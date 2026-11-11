# ==============================================================================
#  FUNÇÕES - Rodadas dos modelos: variáveis, eixos da PCA ou os dois
# ------------------------------------------------------------------------------
#  Os modelos podem usar como preditores:
#    "variaveis" -> as variáveis escolhidas (escalonadas, z-score);
#    "pca"       -> os eixos da PCA (o script mostra os eixos sugeridos e você
#                   roda com eles ou escolhe outros);
#    "ambos"     -> as duas coisas, em RODADAS SEPARADAS: primeiro com as
#                   variáveis, depois com os eixos. Cada rodada tem a sua pasta
#                   (modelos/variaveis e modelos/pca), então nada se mistura.
#  Com um tipo só, os resultados continuam em modelos/ (como antes).
#
#  Quando as preditoras e os cenários têm variáveis DIFERENTES (a seção 12 gera
#  "pca_preditoras_completa" e "pca_comum") e 'pca_modelos' está em branco, o tipo
#  "pca" vira DUAS rodadas, porque os eixos de uma PCA não valem na outra:
#    pca_preditoras_completa -> só as preditoras, com todas as variáveis delas (sem projeção);
#    pca_comum           -> preditoras e cenários, só com as variáveis comuns (com projeção).
#  Cada uma tem as suas pastas (modelos/pca_preditoras_completa e modelos/pca_comum).
#
#  Todos os CSVs dos modelos (mapas, limiares, avaliação, consenso etc.) ganham
#  as colunas 'tipo_preditores', 'preditores' e 'variaveis_originais'.
# ==============================================================================


# TRUE se 'preditores_modelos' é exatamente "pca" (os modelos só usam eixos da PCA;
# então os testes de multicolinearidade e a escolha final das variáveis não são necessários)
so_pca <- function(tipo) {
  is.character(tipo) && length(tipo) == 1 && !is.na(tipo) && tolower(trimws(tipo)) == "pca"
}


# ------------------------------------------------------------------------------
# perguntar_tipo_preditores()
#   'tipo' = "variaveis", "pca" ou "ambos": devolve o valor.
#   'tipo' = "" (ou vazio): pergunta no console.
# ------------------------------------------------------------------------------
perguntar_tipo_preditores <- function(tipo = "") {

  validos <- c("variaveis", "pca", "ambos")
  tipo <- if (length(tipo) == 0 || is.na(tipo[1])) "" else tolower(trimws(tipo[1]))

  if (tipo %in% validos) return(tipo)
  if (tipo != "") {
    stop("'preditores_modelos' inválido: '", tipo, "'. Use \"variaveis\", \"pca\", \"ambos\" ",
         "ou \"\" (para o script perguntar).", call. = FALSE)
  }
  if (!interactive()) {
    stop("Não é possível perguntar fora de uma sessão interativa. ",
         "Preencha 'preditores_modelos' no main (\"variaveis\", \"pca\" ou \"ambos\").", call. = FALSE)
  }

  cat("\nOs modelos serão feitos com quais preditores?\n")
  r <- perguntar_opcoes(
    c("Variáveis escolhidas (escalonadas, z-score)",
      "Eixos da PCA",
      "Os dois (rodados separadamente: primeiro as variáveis, depois os eixos)"),
    "Número da opção: ", "Os modelos serão feitos com quais preditores?")
  if (3 %in% r || all(1:2 %in% r)) "ambos" else validos[r[1]]
}


# ------------------------------------------------------------------------------
# escolher_pca_modelos()
#   Devolve o nome da PCA a usar: 'pca' se informado; senão pca_todas_variaveis
#   ou, se não existir, pca_comum.
# ------------------------------------------------------------------------------
escolher_pca_modelos <- function(pca = "", pasta_pca) {
  if (length(pca) == 0 || is.na(pca[1]) || pca[1] == "") {
    pca <- if (dir.exists(file.path(pasta_pca, "pca_todas_variaveis"))) "pca_todas_variaveis" else
      if (dir.exists(file.path(pasta_pca, "pca_comum"))) "pca_comum" else "pca_preditoras_completa"
    if (pca == "pca_preditoras_completa") {
      message("Aviso: não há PCA comum (foi pulada). Usando só a PCA das preditoras, ",
              "sem projeção nos cenários.")
    }
  }
  pca[1]
}


# ------------------------------------------------------------------------------
# pcas_das_rodadas()
#   Quais PCAs viram rodadas. 'pca' informado -> só ela. Em branco: se só há
#   "pca_todas_variaveis", ela; se há "pca_preditoras_completa" e "pca_comum" (variáveis
#   diferentes entre preditoras e cenários), as DUAS.
# ------------------------------------------------------------------------------
pcas_das_rodadas <- function(pca = "", pasta_pca) {
  if (length(pca) > 0 && !is.na(pca[1]) && pca[1] != "") return(pca[1])
  existe <- function(p) dir.exists(file.path(pasta_pca, p))
  if (existe("pca_todas_variaveis")) return("pca_todas_variaveis")
  if (existe("pca_preditoras_completa") && existe("pca_comum")) return(c("pca_preditoras_completa", "pca_comum"))
  escolher_pca_modelos("", pasta_pca)
}

# nome da rodada para mensagens
rotulo_rodada <- function(rodada) {
  if (identical(rodada$tipo, "variaveis")) return("Variáveis escolhidas")
  if (is.null(rodada$pca) || is.na(rodada$pca)) "Eixos da PCA" else paste0("Eixos da PCA (", rodada$pca, ")")
}


# ------------------------------------------------------------------------------
# escolher_eixos_pca()
#   Mostra os eixos gerados na seção 12 (variância de cada um e acumulada) e a
#   SUGESTÃO (todos os eixos gerados, que seguem a regra de 'n_eixos_pca' /
#   'variancia_pca'). Depois pergunta:
#     - rodar com os eixos sugeridos; ou
#     - você define quais eixos usar (ex.: 1-3,5).
#   'eixos' preenchido no main (números ou "PC1", "PC2"...) pula a pergunta.
#   'eixos' = NULL usa a regra automática da multicolinearidade (variância
#   acumulada >= 1 - 1/p, com p = nº de variáveis da PCA), sem perguntar.
#   'eixos' = "prompt" (character(0)) mostra a tabela e pergunta.
#   Fora de sessão interativa, usa a sugestão.
#   Só dá para escolher entre os eixos que já têm raster. Para ter mais eixos,
#   aumente 'n_eixos_pca' (ou 'variancia_pca') e rode de novo a seção 12.
#   Devolve os nomes dos eixos escolhidos ("PC1", "PC2", ...).
# ------------------------------------------------------------------------------
escolher_eixos_pca <- function(pca = "", pasta_pca, eixos = c()) {

  pca        <- escolher_pca_modelos(pca, pasta_pca)
  pasta_base <- file.path(pasta_pca, pca)
  if (!dir.exists(file.path(pasta_base, "preditoras"))) {
    stop("Não encontrei a PCA '", pca, "' em ", pasta_pca, ". Rode a seção 12.", call. = FALSE)
  }

  arquivos <- list.files(file.path(pasta_base, "preditoras"), pattern = "^PC[0-9]+\\.tif$")
  gerados  <- sort(as.integer(gsub("\\D", "", arquivos)))
  if (length(gerados) == 0) stop("Não há eixos em ", pasta_base, ". Rode a seção 12.", call. = FALSE)

  # eixos_pca_modelos = NULL: regra automática da multicolinearidade (1 - 1/p), sem perguntar
  if (is.null(eixos)) {
    arquivo_var <- file.path(pasta_base, "pca_variancia.csv")
    if (file.exists(arquivo_var)) {
      tab <- utils::read.csv(arquivo_var, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      p   <- nrow(tab)                       # nº de variáveis = nº de componentes
      alvo <- 1 - 1 / p
      n   <- which(tab$acumulada >= alvo - 1e-9)[1]
      if (is.na(n)) n <- p
      escolhidos <- intersect(paste0("PC", seq_len(n)), paste0("PC", gerados))
      if (length(escolhidos) > 0) {
        message("Eixos da PCA '", pca, "' (regra automática 100% - 100/", p, " = ",
                round(alvo * 100, 1), "%): ", paste(escolhidos, collapse = ", "),
                " (", round(tab$acumulada[n] * 100, 1), "% da variância)")
        return(escolhidos)
      }
    }
    message("Não consegui aplicar a regra automática (pca_variancia.csv ausente): ",
            "usando todos os eixos gerados.")
    return(paste0("PC", gerados))
  }

  # eixos informados no main
  if (length(eixos) > 0 && "all" %in% tolower(as.character(eixos))) {
    return(paste0("PC", gerados))
  }
  if (length(eixos) > 0) {
    numeros <- as.integer(gsub("\\D", "", as.character(eixos)))
    invalidos <- setdiff(numeros, gerados)
    if (any(is.na(numeros)) || length(invalidos) > 0) {
      stop("Eixo(s) indisponível(is) na PCA '", pca, "': ",
           paste(eixos[is.na(numeros) | numeros %in% invalidos], collapse = ", "),
           ". Eixos gerados: ", paste0("PC", gerados, collapse = ", "),
           ". Para ter mais eixos, aumente 'n_eixos_pca' e rode a seção 12 de novo.", call. = FALSE)
    }
    return(paste0("PC", sort(unique(numeros))))
  }

  # tabela dos eixos
  arquivo_var <- file.path(pasta_base, "pca_variancia.csv")
  cat("\n================ EIXOS DA PCA PARA OS MODELOS ================\n")
  cat("PCA:", pca, "\n")
  if (file.exists(arquivo_var)) {
    tab <- utils::read.csv(arquivo_var, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    tab <- tab[tab$componente %in% paste0("PC", gerados), , drop = FALSE]
    mostrar <- data.frame(eixo = tab$componente,
                          variancia_pct = arredondar_sdm(100 * tab$variancia),
                          acumulada_pct = arredondar_sdm(100 * tab$acumulada))
    print(mostrar, row.names = FALSE)
    cat("\nSugestão: os ", length(gerados), " eixos gerados (PC", min(gerados), " a PC", max(gerados),
        "), que somam ", mostrar$acumulada_pct[nrow(mostrar)], "% da variância.\n", sep = "")
  } else {
    cat("Eixos gerados:", paste0("PC", gerados, collapse = ", "), "\n")
  }
  cat("==============================================================\n")

  if (!interactive()) {
    message("Fora de sessão interativa: usando os eixos sugeridos. ",
            "Para escolher, preencha 'eixos_pca_modelos' no main.")
    return(paste0("PC", gerados))
  }

  repeat {
    resposta <- tolower(trimws(ler_entrada("Rodar os modelos com os eixos sugeridos? (s/n): ",
                                          contexto = paste0("Eixos da PCA '", pca, "' sugeridos: ",
                                                            paste0("PC", gerados, collapse = ", "),
                                                            "\n(ENTER ou s = usar os sugeridos; n = escolher os eixos)"),
                                          titulo = "Eixos da PCA para os modelos")))
    if (resposta %in% c("", "s", "sim")) return(paste0("PC", gerados))
    if (resposta %in% c("n", "nao", "não")) break
    message("Resposta inválida: digite s (sim) ou n (não).")
  }

  cat("\nEixos disponíveis:", paste0("PC", gerados, collapse = ", "), "\n")
  repeat {
    resposta <- ler_entrada("Digite os números dos eixos a usar (ex.: 1-3,5): ",
                             contexto = paste0("Eixos disponíveis: ", paste0("PC", gerados, collapse = ", ")),
                             titulo = "Quais eixos da PCA usar?")
    numeros  <- ler_numeros(resposta, max(gerados))
    if (!is.null(numeros) && all(numeros %in% gerados)) break
    message("Resposta inválida. Use números de eixos gerados (", paste(gerados, collapse = ", "),
            "), separados por vírgula ou com hífen.")
  }
  paste0("PC", sort(numeros))
}


# ------------------------------------------------------------------------------
# montar_rodadas_modelos()
#   Define o que será rodado. 'tipo' = "variaveis", "pca", "ambos" ou "" (pergunta).
#   Devolve uma lista de rodadas; cada rodada tem:
#     nome, tipo, pasta_modelos (onde salvar) e preditores (de definir_preditores()).
#   Um tipo só -> pasta_modelos = modelos/. Os dois -> modelos/variaveis e modelos/pca.
#   Todas as perguntas são feitas aqui, antes de qualquer modelo rodar.
# ------------------------------------------------------------------------------
montar_rodadas_modelos <- function(tipo = "", variaveis = c(), pca = "", eixos = c(),
                                   pasta_escalonadas, pasta_pca, pasta_modelos,
                                   variaveis_finais = c(), ambiente = globalenv()) {

  tipo  <- perguntar_tipo_preditores(tipo)
  tipos <- if (tipo == "ambos") c("variaveis", "pca") else tipo
  rodadas <- list()

  for (t in tipos) {
    garantir_rasters_preditores(t, ambiente)

    if (t == "variaveis") {
      preditores <- definir_preditores(tipo = "variaveis", variaveis = variaveis,
                                       pasta_escalonadas = pasta_escalonadas, pasta_pca = pasta_pca,
                                       variaveis_finais = variaveis_finais)
      rodadas[["variaveis"]] <- list(nome = "variaveis", tipo = "variaveis", pca = NA_character_,
                                     pasta_modelos = pasta_modelos, preditores = preditores)
    } else {
      pcas <- pcas_das_rodadas(pca, pasta_pca)
      if (length(pcas) > 1) {
        cat("\nPreditoras e cenários têm variáveis diferentes: os modelos com PCA serão feitos em ",
            "DUAS rodadas, pois os eixos de uma PCA não valem na outra:\n",
            "  - pca_preditoras_completa: só as preditoras, com todas as variáveis delas (sem projeção);\n",
            "  - pca_comum: preditoras e cenários, só com as variáveis comuns (com projeção).\n", sep = "")
      }
      for (p in pcas) {
        eixos_escolhidos <- escolher_eixos_pca(p, pasta_pca, eixos)
        preditores <- definir_preditores(tipo = "pca", pca = p, eixos = eixos_escolhidos,
                                         pasta_escalonadas = pasta_escalonadas, pasta_pca = pasta_pca)
        nome <- if (length(pcas) > 1) p else "pca"
        rodadas[[nome]] <- list(nome = nome, tipo = "pca", pca = p,
                                pasta_modelos = pasta_modelos, preditores = preditores)
      }
    }
  }

  # com mais de uma rodada, cada uma salva na sua subpasta
  if (length(rodadas) > 1) {
    for (n in names(rodadas)) rodadas[[n]]$pasta_modelos <- file.path(pasta_modelos, rodadas[[n]]$nome)
  }

  cat("\n================ RODADAS DOS MODELOS ================\n")
  for (r in rodadas) {
    cat("-", rotulo_rodada(r), "(", length(r$preditores$nomes), "preditores ) ->",
        r$pasta_modelos, "\n")
  }
  if (length(rodadas) > 1) cat("As rodadas são feitas uma depois da outra, separadamente.\n")
  cat("=====================================================\n")

  rodadas
}


# ------------------------------------------------------------------------------
# selecionar_rodadas()
#   Escolhe quais rodadas de 'rodadas' (de montar_rodadas_modelos()) executar.
#   'selecao':
#     "all" ou NULL      -> todas
#     "prompt"           -> lista as rodadas numeradas (com "all" por último) e
#                           pergunta (ex.: 1, 1,3 ou 2-3); com uma rodada só, usa ela
#     nomes (ex.: "pca", c("variaveis", "pca_comum")) ou números (ex.: 2, c(1, 3))
#   Devolve a lista das rodadas escolhidas, na ordem de 'rodadas'.
#   Uso:  for (rodada in selecionar_rodadas(rodadas_modelos, "pca")) { ... }
# ------------------------------------------------------------------------------
selecionar_rodadas <- function(rodadas, selecao = "prompt") {

  if (length(rodadas) == 0) stop("Não há rodadas definidas.", call. = FALSE)
  nomes <- names(rodadas)
  rotulos <- vapply(seq_along(rodadas), function(i) {
    paste0(nomes[i], ": ", rotulo_rodada(rodadas[[i]]), "  (", rodadas[[i]]$pasta_modelos, ")")
  }, character(1))

  if (is.null(selecao) || length(selecao) == 0 ||
      (length(selecao) == 1 && !is.na(selecao) && tolower(as.character(selecao)) == "all")) {
    return(rodadas)
  }
  if (length(rodadas) == 1) return(rodadas)

  if (length(selecao) == 1 && !is.na(selecao) && tolower(as.character(selecao)) == "prompt") {
    cat("\nRodadas de modelos disponíveis:\n")
    r <- perguntar_opcoes(c(rotulos, "all: todas as rodadas"),
                          "Número(s) da(s) rodada(s) a executar: ",
                          "Quais rodadas de modelos executar?")
    if ((length(rodadas) + 1) %in% r) return(rodadas)
    return(rodadas[sort(unique(r))])
  }

  if (is.numeric(selecao)) {
    if (any(is.na(selecao) | selecao < 1 | selecao > length(rodadas))) {
      stop("Rodada(s) inexistente(s): ", paste(selecao, collapse = ", "),
           ". Disponíveis: ", paste0(seq_along(rodadas), " = ", nomes, collapse = "; "), call. = FALSE)
    }
    return(rodadas[sort(unique(selecao))])
  }
  selecao <- as.character(selecao)
  invalidas <- setdiff(selecao, nomes)
  if (length(invalidas) > 0) {
    stop("Rodada(s) inexistente(s): ", paste(invalidas, collapse = ", "),
         ". Disponíveis: ", paste(nomes, collapse = ", "), call. = FALSE)
  }
  rodadas[nomes[nomes %in% selecao]]
}


# ------------------------------------------------------------------------------
# ativar_rodada()
#   'rodada' pode ser a própria rodada ou a mesma seleção de selecionar_rodadas()
#   (nome ou número, ex.: ativar_rodada("pca") ou ativar_rodada(2)); como só uma
#   fica em uso, com mais de uma pergunta qual (ou dá erro fora de sessão interativa).
#   Coloca a rodada "em uso": muda a pasta dos modelos (pastas_saida$modelos),
#   define 'preditores' e apaga da sessão os objetos da rodada anterior
#   (dados, avaliação, modelos finais, mapas, consenso...), para nada se misturar.
#   Para trocar de rodada depois: ativar_rodada(rodadas_modelos[["pca"]]).
# ------------------------------------------------------------------------------
ativar_rodada <- function(rodada, ambiente = globalenv()) {

  if (!is.list(rodada)) {
    if (!objeto_existe("rodadas_modelos", ambiente)) {
      stop("'rodadas_modelos' não existe na sessão: rode a seção 14.", call. = FALSE)
    }
    escolhidas <- selecionar_rodadas(get("rodadas_modelos", envir = ambiente), rodada)
    if (length(escolhidas) > 1) {
      if (!interactive()) {
        stop("Mais de uma rodada selecionada; ative uma por vez.", call. = FALSE)
      }
      cat("\nSó uma rodada fica em uso. Qual ativar?\n")
      n <- perguntar_opcoes(vapply(escolhidas, function(r) paste0(r$nome, ": ", rotulo_rodada(r)),
                                   character(1)), "Número da rodada: ",
                           "Qual rodada ativar?")
      escolhidas <- escolhidas[n[1]]
    }
    rodada <- escolhidas[[1]]
  }

  pastas <- obter_pastas_saida(ambiente)
  pastas$modelos <- rodada$pasta_modelos
  dir.create(pastas$modelos, recursive = TRUE, showWarnings = FALSE)
  assign("pastas_saida", pastas, envir = ambiente)
  assign("preditores", rodada$preditores, envir = ambiente)
  assign("rodada_modelos", rodada, envir = ambiente)

  antigos <- c("dados_modelos", "ajuste_maxent", "avaliacao", "modelos_finais", "limiares",
               "mapas", "consenso", "jsdm", "independentes", "validacao_independente",
               "extrapolacao")
  for (obj in antigos) {
    if (exists(obj, envir = ambiente, inherits = FALSE)) rm(list = obj, envir = ambiente)
  }

  cat("\n##############################################################\n")
  cat("# RODADA:", toupper(rotulo_rodada(rodada)), "\n")
  cat("# Preditores:", paste(rodada$preditores$nomes, collapse = ", "), "\n")
  cat("# Resultados em:", rodada$pasta_modelos, "\n")
  cat("##############################################################\n")

  invisible(rodada)
}


# ------------------------------------------------------------------------------
# acrescentar_preditores()
#   Acrescenta à tabela as colunas:
#     tipo_preditores     -> "variaveis" ou "pca"
#     preditores          -> lista dos preditores usados nos modelos (separados por "; ")
#     variaveis_originais -> variáveis escolhidas (tipo "variaveis") ou
#                            variáveis que formaram a PCA (tipo "pca")
#   'preditores' é o objeto de definir_preditores(). Se for NULL, devolve a tabela igual.
# ------------------------------------------------------------------------------
acrescentar_preditores <- function(tabela, preditores) {
  if (is.null(tabela) || is.null(preditores) || !is.data.frame(tabela)) return(tabela)
  tabela$tipo_preditores     <- preditores$tipo
  tabela$preditores          <- paste(preditores$nomes, collapse = "; ")
  tabela$variaveis_originais <- if (length(preditores$variaveis_originais) > 0) {
    paste(preditores$variaveis_originais, collapse = "; ")
  } else NA_character_
  tabela
}


# ------------------------------------------------------------------------------
# obter_rodada_ativa()
#   Para rodar uma seção (19 a 25) isolada, fora do loop das rodadas:
#     - se já há uma rodada ativa na sessão, usa essa;
#     - senão, usa as 'rodadas_modelos' da seção 14: com uma rodada só, ativa
#       ela; com duas, pergunta qual usar;
#     - se a seção 14 ainda não rodou, define as rodadas agora (pergunta se os
#       modelos usam variáveis, eixos da PCA ou os dois).
#   Ativar a rodada define 'preditores' e a pasta dos modelos (ativar_rodada()).
# ------------------------------------------------------------------------------
obter_rodada_ativa <- function(ambiente = globalenv()) {

  if (objeto_existe("rodada_modelos", ambiente)) {
    return(get("rodada_modelos", envir = ambiente))
  }
  if (!objeto_existe("rodadas_modelos", ambiente)) {
    message("Aviso: as rodadas dos modelos (seção 14) não foram definidas nesta sessão. ",
            "Vou defini-las agora.")
    pastas <- obter_pastas_saida(ambiente)
    rodadas <- montar_rodadas_modelos(
      tipo              = ler_parametro("preditores_modelos", ambiente),
      variaveis         = ler_parametro("variaveis_modelos", ambiente),
      pca               = ler_parametro("pca_modelos", ambiente),
      eixos             = ler_parametro("eixos_pca_modelos", ambiente),
      pasta_escalonadas = pastas$variaveis_escalonadas,
      pasta_pca         = file.path(pastas$multicolinearidade, "pca_eixos"),
      pasta_modelos     = file.path(ler_parametro("pasta_projeto", ambiente), "modelos"),
      variaveis_finais  = obter_variaveis_finais(ambiente),
      ambiente          = ambiente)
    assign("rodadas_modelos", rodadas, envir = ambiente)
  }

  rodadas <- get("rodadas_modelos", envir = ambiente)
  if (length(rodadas) == 1) {
    escolhida <- rodadas[[1]]
  } else {
    if (!interactive()) {
      stop("Há mais de uma rodada em 'rodadas_modelos'. Escolha uma com ",
           "ativar_rodada(rodadas_modelos[[\"pca\"]]) (ou [[\"variaveis\"]]).", call. = FALSE)
    }
    nomes <- vapply(rodadas, function(r) {
      paste0(rotulo_rodada(r), "  (", r$pasta_modelos, ")")
    }, character(1))
    cat("\nHá mais de uma rodada de modelos. Qual usar nesta seção?\n")
    n <- perguntar_opcoes(nomes, "Número da rodada: ", "Qual rodada de modelos usar nesta seção?")
    escolhida <- rodadas[[n[1]]]
  }

  ativar_rodada(escolhida, ambiente)
  escolhida
}
