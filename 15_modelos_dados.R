# ==============================================================================
#  FUNÇÕES - Modelos: catálogo, preditores, dados e partições
# ------------------------------------------------------------------------------
#  Prepara tudo o que os modelos precisam:
#    - o catálogo de algoritmos e a escolha do usuário;
#    - os PREDITORES: variáveis escalonadas (z-score) ou eixos da PCA, os mesmos
#      para o presente e para os cenários;
#    - os DADOS de cada espécie: presença (1) / background (0), valores dos
#      preditores, pesos e a partição treino/teste da validação cruzada.
# ==============================================================================


# ------------------------------------------------------------------------------
# listar_modelos()
#   Catálogo dos algoritmos. 'dados' diz o que o modelo usa:
#     "presença"               -> só os pontos de presença (envelopes/distâncias)
#     "presença e background"  -> presenças contra o background
#   O JSDM ajusta todas as espécies juntas e fica fora do consenso (seção 19).
# ------------------------------------------------------------------------------
listar_modelos <- function() {
  data.frame(
    codigo = c("bioclim", "domain", "mahalanobis", "glm", "glmnet", "gam", "mars",
               "cta", "rf", "brt", "maxent", "svm", "ann", "esm", "jsdm"),
    nome = c("Bioclim (envelope climático)",
             "Domain (distância de Gower)",
             "Mahalanobis (distância ambiental)",
             "GLM (regressão logística)",
             "GLMNET (GLM com lasso / elastic net)",
             "GAM (modelo aditivo generalizado)",
             "MARS (splines de regressão adaptativas)",
             "CTA (árvore de classificação)",
             "RF (random forest)",
             "BRT (boosted regression trees)",
             "Maxent (maxnet)",
             "SVM (máquina de vetores de suporte)",
             "ANN (rede neural artificial)",
             "ESM (conjunto de modelos pequenos)",
             "JSDM (modelo conjunto de espécies)"),
    dados = c(rep("presença", 3), rep("presença e background", 11),
              "presença e pseudoausência (todas as espécies)"),
    pacote = c(NA, NA, NA, NA, "glmnet", "mgcv", "earth", "rpart", "randomForest",
               "gbm", "maxnet", "e1071", "nnet", NA, "gllvm"),
    stringsAsFactors = FALSE
  )
}


# pacotes necessários para os modelos escolhidos
pacotes_dos_modelos <- function(modelos) {
  catalogo <- listar_modelos()
  pac <- catalogo$pacote[catalogo$codigo %in% modelos]
  unique(pac[!is.na(pac)])
}


# ------------------------------------------------------------------------------
# escolher_modelos()
#   - 'modelos' preenchido no main: confere se os códigos existem;
#   - vazio: mostra o catálogo e pergunta no console.
#   Confere também se os pacotes necessários estão instalados.
#   Devolve o vetor de códigos escolhidos.
# ------------------------------------------------------------------------------
escolher_modelos <- function(modelos = c()) {

  catalogo <- listar_modelos()

  if (length(modelos) == 0) {
    if (!interactive()) {
      stop("Não é possível perguntar fora de uma sessão interativa. ",
           "Preencha 'modelos' no main.", call. = FALSE)
    }
    cat("\nModelos de distribuição disponíveis:\n")
    n <- nrow(catalogo)
    escolhidos <- perguntar_opcoes(c(paste0(catalogo$nome, "  [", catalogo$dados, "]"),
                                     "all (todos os modelos)"),
                                   paste0("Número(s) do(s) modelo(s) (ex.: 1-", n, "): "),
                                   "Quais modelos de distribuição rodar?")
    modelos <- if ((n + 1) %in% escolhidos) catalogo$codigo else catalogo$codigo[escolhidos]
  } else {
    modelos <- tolower(trimws(modelos))
    if ("all" %in% modelos) modelos <- catalogo$codigo
    invalidos <- setdiff(modelos, catalogo$codigo)
    if (length(invalidos) > 0) {
      stop("Modelo(s) inválido(s): ", paste(invalidos, collapse = ", "),
           "\nOpções: ", paste(catalogo$codigo, collapse = ", "), call. = FALSE)
    }
  }

  faltando <- pacotes_dos_modelos(modelos)
  faltando <- faltando[!vapply(faltando, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltando) > 0) {
    warning("Pacote(s) não instalado(s): ", paste(faltando, collapse = ", "),
            ". Os modelos que dependem deles vão falhar. Rode a seção 1 de novo.",
            call. = FALSE, immediate. = TRUE)
  }

  cat("\n================ MODELOS ESCOLHIDOS ================\n")
  for (m in modelos) {
    linha <- catalogo[catalogo$codigo == m, ]
    cat("-", linha$nome, " [", linha$dados, "]\n")
  }
  if ("jsdm" %in% modelos) cat("(o JSDM roda na seção 19 e fica fora do consenso)\n")
  cat("====================================================\n\n")

  return(modelos)
}


# ------------------------------------------------------------------------------
# definir_preditores()
#   Define os rasters que entram nos modelos, os MESMOS no presente e nos
#   cenários:
#     tipo = "variaveis" -> variáveis escalonadas (variaveis_escalonadas/).
#                           'variaveis' = c() usa as variáveis finais da seção 13.
#     tipo = "pca"       -> eixos da PCA (multicolinearidade/pca_eixos/<pca>/).
#                           'pca' = "" escolhe sozinho: pca_todas_variaveis ou,
#                           se não existir, pca_comum.
#                           'eixos' = c() usa todos os eixos gerados; senão só os
#                           indicados (ex.: c("PC1", "PC2", "PC4") ou c(1, 2, 4)).
#   Um cenário só recebe projeção se tiver TODOS os preditores; os demais são
#   avisados. Confere também se os rasters de cada conjunto têm a mesma grade.
#   Devolve list(tipo, nomes, arquivos = lista por conjunto, conjuntos, faltando,
#   pca = nome da PCA (ou NA), variaveis_originais).
# ------------------------------------------------------------------------------
definir_preditores <- function(tipo = "variaveis", variaveis = c(), pca = "",
                               pasta_escalonadas, pasta_pca, variaveis_finais = c(),
                               eixos = c()) {

  tipo <- match.arg(tipo, c("variaveis", "pca"))

  if (tipo == "variaveis") {
    nomes <- if (length(variaveis) > 0) trimws(variaveis) else variaveis_finais
    if (length(nomes) == 0) {
      stop("Não há variáveis para os modelos: preencha 'variaveis_modelos' ou rode a seção 13.",
           call. = FALSE)
    }
    pasta_base <- pasta_escalonadas
    descricao  <- "variáveis escalonadas (z-score)"
    pca_usada  <- NA_character_
    originais  <- nomes
  } else {
    if (pca == "") {
      pca <- if (dir.exists(file.path(pasta_pca, "pca_todas_variaveis"))) "pca_todas_variaveis" else
        if (dir.exists(file.path(pasta_pca, "pca_comum"))) "pca_comum" else "pca_preditoras_completa"
    }
    pasta_base <- file.path(pasta_pca, pca)
    if (!dir.exists(pasta_base)) {
      stop("Não encontrei a PCA '", pca, "' em ", pasta_pca, ". Rode a seção 12.", call. = FALSE)
    }
    eixos_gerados <- list.files(file.path(pasta_base, "preditoras"), pattern = "^PC[0-9]+\\.tif$")
    nomes <- tools::file_path_sans_ext(eixos_gerados[order(as.integer(gsub("\\D", "", eixos_gerados)))])
    if (length(eixos) > 0) {
      pedidos <- paste0("PC", as.integer(gsub("\\D", "", as.character(eixos))))
      faltam  <- setdiff(pedidos, nomes)
      if (length(faltam) > 0) {
        stop("Eixo(s) não encontrado(s) na PCA '", pca, "': ", paste(faltam, collapse = ", "),
             ". Eixos gerados: ", paste(nomes, collapse = ", "), call. = FALSE)
      }
      nomes <- nomes[nomes %in% pedidos]
    }
    descricao <- paste0("eixos da PCA (", pca, ")")
    pca_usada <- pca
    arquivo_modelo <- file.path(pasta_base, "pca_modelo.rds")
    originais <- if (file.exists(arquivo_modelo)) readRDS(arquivo_modelo)$variaveis else character(0)
  }

  conjuntos <- list.dirs(pasta_base, full.names = FALSE, recursive = FALSE)
  conjuntos <- c("preditoras", setdiff(conjuntos, "preditoras"))
  if (!dir.exists(file.path(pasta_base, "preditoras"))) {
    stop("Não encontrei os rasters das preditoras em ", pasta_base, call. = FALSE)
  }

  arquivos <- list()
  faltando <- list()
  for (cj in conjuntos) {
    caminhos <- file.path(pasta_base, cj, paste0(nomes, ".tif"))
    existe   <- file.exists(caminhos)
    if (all(existe)) {
      arquivos[[cj]] <- setNames(caminhos, nomes)
    } else {
      faltando[[cj]] <- nomes[!existe]
    }
  }

  if (is.null(arquivos[["preditoras"]])) {
    stop("Faltam nas preditoras os preditores: ", paste(faltando[["preditoras"]], collapse = ", "),
         call. = FALSE)
  }

  # todos os rasters de um conjunto precisam ter a mesma grade
  for (cj in names(arquivos)) {
    ref <- terra::rast(arquivos[[cj]][1])
    for (a in arquivos[[cj]][-1]) {
      if (!isTRUE(terra::compareGeom(ref, terra::rast(a), stopOnError = FALSE))) {
        stop("Em '", cj, "', o raster ", basename(a), " não tem a mesma grade de ",
             basename(arquivos[[cj]][1]), ". Veja o relatório da seção 7.", call. = FALSE)
      }
    }
  }

  cat("\n================ PREDITORES DOS MODELOS ================\n")
  cat("Tipo:", descricao, "\n")
  cat("Preditores (", length(nomes), "):", paste(nomes, collapse = ", "), "\n")
  cat("Conjuntos com todos os preditores (recebem mapas):",
      paste(names(arquivos), collapse = ", "), "\n")
  for (cj in names(faltando)) {
    cat("Sem projeção em '", cj, "': faltam ", paste(faltando[[cj]], collapse = ", "), "\n", sep = "")
  }
  cat("========================================================\n")

  list(tipo = tipo, descricao = descricao, nomes = nomes, arquivos = arquivos,
       conjuntos = names(arquivos), faltando = faltando,
       pca = pca_usada, variaveis_originais = originais)
}


# ------------------------------------------------------------------------------
# folds_estratificados()
#   Sorteia 'k' grupos de forma que presenças e background fiquem espalhados
#   por todos os grupos (cada grupo recebe a sua parte de cada classe).
# ------------------------------------------------------------------------------
folds_estratificados <- function(y, k) {
  grupo <- integer(length(y))
  for (classe in unique(y)) {
    i <- which(y == classe)
    grupo[i] <- sample(rep(seq_len(k), length.out = length(i)))
  }
  grupo
}


# ------------------------------------------------------------------------------
# particao_blocos()
#   Partição espacial em 4 blocos com o MESMO número de presenças (método
#   "block" do ENMeval): corta pela mediana da longitude das presenças e, em
#   cada metade, pela mediana da latitude. O background segue as mesmas linhas.
#   Assim o modelo é testado numa região que não viu no treino.
# ------------------------------------------------------------------------------
particao_blocos <- function(x, y, presenca) {
  xp <- x[presenca == 1]
  yp <- y[presenca == 1]
  corte_x   <- stats::median(xp)
  corte_esq <- stats::median(yp[xp <= corte_x])
  corte_dir <- stats::median(yp[xp >  corte_x])
  ifelse(x <= corte_x,
         ifelse(y <= corte_esq, 1L, 2L),
         ifelse(y <= corte_dir, 3L, 4L))
}


# ------------------------------------------------------------------------------
# particao_treino_teste()
#   Separa uma parte dos pontos para TESTE (hold-out). Devolve 0 = treino, 1 = teste.
#     "aleatoria" -> sorteia 'fracao' das presenças e 'fracao' do background
#     "blocos"    -> teste = uma faixa espacial com 'fracao' das presenças, no eixo
#                    (longitude ou latitude) de maior extensão das presenças; o
#                    background da mesma faixa também vai para o teste
#   Se a faixa espacial ficar sem presença ou background no teste (ou no treino),
#   usa o sorteio e avisa.
# ------------------------------------------------------------------------------
particao_treino_teste <- function(x, y, presenca, fracao = 0.3, metodo = "aleatoria") {
  sorteio <- function() {
    teste <- integer(length(presenca))
    for (classe in unique(presenca)) {
      i <- which(presenca == classe)
      n <- if (classe == 1) max(1, round(fracao * length(i))) else round(fracao * length(i))
      teste[sample(i, min(n, length(i)))] <- 1L
    }
    teste
  }
  if (metodo != "blocos") return(sorteio())

  xp <- x[presenca == 1]; yp <- y[presenca == 1]
  eixo <- if (diff(range(xp)) >= diff(range(yp))) x else y
  corte <- stats::quantile(eixo[presenca == 1], 1 - fracao, names = FALSE)
  teste <- as.integer(eixo > corte)
  ok <- all(vapply(0:1, function(g) any(presenca[teste == g] == 1) && any(presenca[teste == g] == 0),
                   logical(1)))
  if (!ok) {
    warning("Partição espacial treino/teste sem presença ou background em um dos lados; ",
            "usando sorteio aleatório.", call. = FALSE, immediate. = TRUE)
    return(sorteio())
  }
  teste
}


# ------------------------------------------------------------------------------
# preparar_dados_modelos()
#   Para cada espécie:
#     - lê os preditores das preditoras nos pontos de presença e background;
#     - descarta pontos sem valor em algum preditor;
#     - calcula os pesos (presenças e background somam o mesmo total);
#     - cria os grupos de cada esquema de validação pedido:
#         "blocos"    -> coluna grupo_blocos (4 blocos espaciais)
#         "kfold"     -> coluna grupo_kfold (n_folds grupos sorteados)
#         "jackknife" -> não precisa de coluna (uma presença de fora por vez)
#         "teste"     -> coluna grupo_teste (0 = treino, 1 = teste), com a fração
#                        'particao_teste' dos pontos no teste ('particao_metodo':
#                        "aleatoria" ou "blocos")
#   Espécies com menos de 'minimo_presencas' presenças ficam de fora (aviso).
#   Salva os dados de cada espécie em pasta_saida/<espécie>.csv.
#   Devolve uma lista por espécie: $dados (tabela) e $preditores (nomes).
# ------------------------------------------------------------------------------
preparar_dados_modelos <- function(pontos, preditores, pasta_saida, minimo_presencas = 10,
                                   tipo_validacao = "blocos", n_folds = 5, semente = 123,
                                   particao_teste = NULL, particao_metodo = "aleatoria") {

  validacoes <- if (pulado(tipo_validacao)) character(0) else normalizar_validacoes(tipo_validacao)
  sem_particao <- is.null(particao_teste) || length(particao_teste) == 0 || is.na(particao_teste[1])
  if ("teste" %in% validacoes && sem_particao) {
    if (length(setdiff(validacoes, "teste")) == 0) {
      stop("tipo_validacao = \"teste\" precisa de 'particao_teste' (fração, ex.: 0.3).", call. = FALSE)
    }
    warning("Validação \"teste\" ignorada: 'particao_teste' = NULL.", call. = FALSE, immediate. = TRUE)
    validacoes <- setdiff(validacoes, "teste")
  }
  if ("teste" %in% validacoes) {
    particao_teste <- as.numeric(particao_teste[1])
    if (particao_teste <= 0 || particao_teste >= 1) {
      stop("'particao_teste' deve estar entre 0 e 1 (ex.: 0.3).", call. = FALSE)
    }
    particao_metodo <- match.arg(tolower(trimws(particao_metodo)), c("aleatoria", "blocos"))
  }
  if (!"especie" %in% names(pontos)) pontos$especie <- "especie"
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  message("Lendo os preditores nos ", nrow(pontos), " pontos...")
  valores <- extrair_valores_pontos(pontos, preditores$arquivos[["preditoras"]])
  names(valores) <- preditores$nomes

  cat("\n================ DADOS DOS MODELOS ================\n")
  descricao <- c(blocos = "4 blocos espaciais", kfold = paste(n_folds, "grupos sorteados (k-fold)"),
                 jackknife = "jackknife (uma presença de fora por vez)",
                 teste = if ("teste" %in% validacoes)
                   paste0("treino/teste (", round(100 * particao_teste), "% para teste; ",
                          particao_metodo, ")") else "treino/teste")
  if (length(validacoes) == 0) {
    cat("Validação: nenhuma (tipo_validacao = NA)\n\n")
  } else {
    cat("Validação:", paste(descricao[validacoes], collapse = " | "),
        "\n(principal:", validacoes[1], ")\n\n")
  }

  resultado <- list()
  resumo    <- list()

  for (especie in unique(pontos$especie)) {
    linhas <- which(pontos$especie == especie)
    dados  <- data.frame(presenca = as.integer(pontos$presenca[linhas]),
                         x = pontos$x[linhas], y = pontos$y[linhas],
                         valores[linhas, , drop = FALSE], check.names = FALSE)
    dados  <- dados[stats::complete.cases(dados), , drop = FALSE]
    n1 <- sum(dados$presenca == 1)
    n0 <- sum(dados$presenca == 0)

    if (n1 < minimo_presencas || n0 == 0) {
      warning("Espécie '", especie, "' fora dos modelos: ", n1, " presença(s) e ", n0,
              " ponto(s) de background (mínimo de presenças: ", minimo_presencas, ").",
              call. = FALSE, immediate. = TRUE)
      next
    }

    dados$peso <- ifelse(dados$presenca == 1, 1, n1 / n0)
    set.seed(semente)

    if ("blocos" %in% validacoes) {
      dados$grupo_blocos <- particao_blocos(dados$x, dados$y, dados$presenca)
      completo <- vapply(1:4, function(k) {
        any(dados$presenca[dados$grupo_blocos == k] == 1) &&
          any(dados$presenca[dados$grupo_blocos == k] == 0)
      }, logical(1))
      if (!all(completo)) {
        warning("Espécie '", especie, "': algum bloco ficou sem presença ou sem background. ",
                "Os blocos foram trocados por 4 grupos sorteados.", call. = FALSE, immediate. = TRUE)
        dados$grupo_blocos <- folds_estratificados(dados$presenca, 4)
      }
    }
    if ("kfold" %in% validacoes) {
      dados$grupo_kfold <- folds_estratificados(dados$presenca, n_folds)
    }
    if ("teste" %in% validacoes) {
      dados$grupo_teste <- particao_treino_teste(dados$x, dados$y, dados$presenca,
                                                 particao_teste, particao_metodo)
      if (sum(dados$presenca[dados$grupo_teste == 0] == 1) < 5) {
        warning("Espécie '", especie, "': menos de 5 presenças no treino do hold-out.",
                call. = FALSE, immediate. = TRUE)
      }
    }

    utils::write.csv(dados, file.path(pasta_saida, paste0(nome_seguro(especie), ".csv")),
                     row.names = FALSE, fileEncoding = "UTF-8")
    resultado[[as.character(especie)]] <- list(dados = dados, preditores = preditores$nomes)

    linha <- data.frame(especie = especie, presencas = n1, background = n0)
    if ("blocos" %in% validacoes) linha$presencas_por_bloco <- paste(tapply(dados$presenca, dados$grupo_blocos, sum), collapse = "/")
    if ("teste"  %in% validacoes) linha$presencas_treino_teste <- paste(sum(dados$presenca[dados$grupo_teste == 0]), sum(dados$presenca[dados$grupo_teste == 1]), sep = "/")
    if ("kfold"  %in% validacoes) linha$presencas_por_grupo <- paste(tapply(dados$presenca, dados$grupo_kfold, sum), collapse = "/")
    resumo[[as.character(especie)]] <- linha
  }

  if (length(resultado) == 0) {
    stop("Nenhuma espécie tem presenças suficientes para os modelos.", call. = FALSE)
  }

  print(do.call(rbind, resumo), row.names = FALSE)
  cat("Dados salvos em:", pasta_saida, "\n")
  cat("===================================================\n")
  return(resultado)
}
