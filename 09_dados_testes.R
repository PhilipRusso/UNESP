# ==============================================================================
#  FUNÇÕES - Dados para os testes de multicolinearidade
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
#
#  Dois tipos de dados:
#    - SEM resposta : amostra de células da área de estudo, uma tabela por
#                     conjunto (preditoras e cada cenário). Usada por VIF, filtro
#                     por correlação, cluster e PCA.
#    - COM resposta : valores nos pontos de presença (1) e background (0) de
#                     cada espécie, só nas variáveis preditoras. Usada por lasso,
#                     ridge, elastic net, PCR, PLSR, CPPLS e Boruta.
#
#  Os valores são consultados ARQUIVO POR ARQUIVO (sem empilhar os rasters).
#  Todas as variáveis são padronizadas em z-score (média 0, desvio 1).
# ==============================================================================


# ------------------------------------------------------------------------------
# nome_curto_variavel()
#   Nome da variável a partir do nome do arquivo, SEM o período e a versão,
#   para que a mesma variável tenha o mesmo nome nas preditoras e nos cenários:
#       "bio01_1981-2010_V.2.1.tif" -> "bio01"
#       "bio01_2071-2100_V.2.1.tif" -> "bio01"
#       "wc2.1_30s_elev.tif"        -> "wc2.1_30s_elev"
# ------------------------------------------------------------------------------
nome_curto_variavel <- function(arquivo) {
  nome <- nome_curto_bruto(arquivo)

  # correspondência definida pelo usuário (variáveis com nomes diferentes nos
  # cenários; ver 13b_nomes_variaveis.R). A chave é o nome do arquivo sem extensão.
  mapa <- .sdm_renomeio$mapa
  if (length(mapa) > 0) {
    chave <- tools::file_path_sans_ext(basename(arquivo))
    pos   <- match(chave, names(mapa))
    nome[!is.na(pos)] <- unname(mapa[pos[!is.na(pos)]])
  }
  return(nome)
}

# nome da variável SEM a correspondência definida pelo usuário
nome_curto_bruto <- function(arquivo) {
  nome <- tools::file_path_sans_ext(basename(arquivo))
  nome <- gsub("_[0-9]{4}-[0-9]{4}", "", nome)       # período (ex.: _1981-2010)
  nome <- gsub("_V\\.?[0-9]+(\\.[0-9]+)*$", "", nome) # versão (ex.: _V.2.1)
  return(nome)
}


# ------------------------------------------------------------------------------
# extrair_valores_pontos()
#   Para cada ponto (colunas x e y, na projeção dos rasters), lê o valor de
#   cada variável. Consulta um arquivo de cada vez.
#   Devolve uma tabela: uma linha por ponto, uma coluna por variável.
# ------------------------------------------------------------------------------
extrair_valores_pontos <- function(pontos_xy, arquivos_variaveis) {

  referencia <- terra::rast(arquivos_variaveis[1])[[1]]
  pontos     <- terra::vect(as.matrix(pontos_xy[, c("x", "y")]), type = "points",
                            crs = terra::crs(referencia))

  nomes   <- make.unique(vapply(arquivos_variaveis, nome_curto_variavel, character(1)),
                         sep = "_")
  valores <- lapply(arquivos_variaveis, function(arquivo) {
    terra::extract(terra::rast(arquivo)[[1]], pontos)[, 2]
  })
  names(valores) <- nomes

  return(as.data.frame(valores, check.names = FALSE))
}


# ------------------------------------------------------------------------------
# amostrar_area()
#   Sorteia 'n_amostra' células válidas da área de estudo (a partir do
#   primeiro raster do conjunto) e lê o valor de todas as variáveis nelas.
#   'n_amostra' = NULL usa TODAS as células válidas (sem sorteio; pode ser pesado
#   em áreas grandes).
#   Remove células com NA em alguma variável e variáveis sem variação.
#   Devolve uma tabela com uma coluna por variável.
# ------------------------------------------------------------------------------
amostrar_area <- function(arquivos_variaveis, n_amostra = 10000, semente = 123) {

  referencia <- terra::rast(arquivos_variaveis[1])[[1]]

  if (is.null(n_amostra)) {
    celulas <- terra::cells(referencia)       # todas as células com valor
    message("Testes de multicolinearidade com TODAS as células válidas: ", length(celulas))
    if (length(celulas) > 1e6) {
      warning("Mais de 1 milhão de células (", length(celulas), "): os testes podem demorar e ",
              "usar muita memória. Para sortear, defina 'n_amostra_testes' (ex.: 100000).",
              call. = FALSE, immediate. = TRUE)
    }
    sorteio <- terra::xyFromCell(referencia, celulas)
    sorteio <- data.frame(x = sorteio[, 1], y = sorteio[, 2])
  } else {
    set.seed(semente)   # o sorteio é sempre o mesmo
    sorteio <- terra::spatSample(referencia, size = n_amostra, method = "random",
                                 na.rm = TRUE, xy = TRUE, values = FALSE)
    sorteio <- data.frame(x = sorteio[, "x"], y = sorteio[, "y"])
  }

  dados <- extrair_valores_pontos(sorteio, arquivos_variaveis)
  dados <- dados[complete.cases(dados), , drop = FALSE]

  if (nrow(dados) < 30) {
    stop("Foram obtidas só ", nrow(dados), " células válidas na amostra da área.",
         call. = FALSE)
  }

  constantes <- vapply(dados, function(x) stats::sd(x) == 0, logical(1))
  if (any(constantes)) {
    warning("Variáveis constantes removidas: ",
            paste(names(dados)[constantes], collapse = ", "),
            call. = FALSE, immediate. = TRUE)
    dados <- dados[, !constantes, drop = FALSE]
  }

  rownames(dados) <- NULL
  return(dados)
}


# ------------------------------------------------------------------------------
# padronizar_zscore()
#   Padroniza cada variável em z-score: (valor - média) / desvio.
#   - 'parametros' = NULL : calcula a média e o desvio a partir de 'dados'.
#   - 'parametros' informado (média e desvio de outro conjunto, em geral as
#     variáveis PREDITORAS): usa esses valores. Variáveis que não estão em
#     'parametros' usam a própria média e o próprio desvio.
#   Devolve uma lista: $dados (padronizados) e $parametros (média e desvio
#   usados, para poderem ser reaproveitados).
# ------------------------------------------------------------------------------
padronizar_zscore <- function(dados, parametros = NULL) {

  proprios <- list(media  = vapply(dados, mean, numeric(1)),
                   desvio = vapply(dados, stats::sd, numeric(1)))

  if (is.null(parametros)) {
    usados <- proprios
  } else {
    sem_parametro <- setdiff(names(dados), names(parametros$media))
    if (length(sem_parametro) > 0) {
      message("Aviso: sem média/desvio das variáveis preditoras para: ",
              paste(sem_parametro, collapse = ", "),
              ". Usando a média e o desvio do próprio conjunto.")
    }
    usados <- list(media = proprios$media, desvio = proprios$desvio)
    comuns <- intersect(names(dados), names(parametros$media))
    usados$media[comuns]  <- parametros$media[comuns]
    usados$desvio[comuns] <- parametros$desvio[comuns]
  }

  padronizados <- as.data.frame(
    sweep(sweep(as.matrix(dados), 2, usados$media[names(dados)], "-"),
          2, usados$desvio[names(dados)], "/")
  )

  return(list(dados = padronizados, parametros = usados))
}


# ------------------------------------------------------------------------------
# preparar_dados_testes()
#   Prepara os dados dos testes SEM resposta para cada conjunto (preditoras e
#   cada cenário): amostra da área + z-score.
#   O z-score dos cenários usa a média e o desvio das variáveis PREDITORAS, para
#   que a diferença entre presente e futuro não seja apagada pela padronização.
#   Devolve uma lista por conjunto:
#       dados_testes[["preditoras"]]$dados       -> tabela em z-score
#       dados_testes[["preditoras"]]$parametros  -> média e desvio usados
# ------------------------------------------------------------------------------
preparar_dados_testes <- function(variaveis_cortadas, n_amostra = 10000, semente = 123) {

  conjuntos <- names(variaveis_cortadas)
  conjuntos <- c("preditoras", setdiff(conjuntos, "preditoras"))   # preditoras primeiro

  resultado <- list()
  parametros_preditoras <- NULL

  for (conjunto in conjuntos) {
    message("Amostrando a área: ", conjunto, " (", length(variaveis_cortadas[[conjunto]]),
            " variáveis)")

    bruto <- amostrar_area(variaveis_cortadas[[conjunto]], n_amostra, semente)
    z     <- padronizar_zscore(bruto, parametros_preditoras)
    resultado[[conjunto]] <- z

    if (conjunto == "preditoras") parametros_preditoras <- z$parametros
    message("   ", nrow(z$dados), " células x ", ncol(z$dados), " variáveis")
  }

  return(resultado[names(variaveis_cortadas)])
}


# ------------------------------------------------------------------------------
# preparar_dados_resposta()
#   Prepara os dados dos testes COM resposta, uma tabela por espécie:
#       $x          -> matriz de variáveis em z-score
#       $y          -> resposta: 1 = presença, 0 = background
#       $parametros -> média e desvio usados no z-score
#   O z-score usa a média e o desvio dos pontos da PRÓPRIA espécie (presenças
#   + background), porque é neles que os modelos são ajustados. Com a média e o
#   desvio da área toda, uma variável que varia pouco dentro do buffer da
#   espécie perderia peso nos testes sem que isso tivesse a ver com a espécie.
#   Espécies com menos de 'minimo_presencas' presenças são ignoradas (aviso).
#   Variáveis constantes dentro da espécie são removidas.
#   'pontos' é a tabela salva por salvar_pontos() (colunas: [especie,] presenca,
#   x, y).
# ------------------------------------------------------------------------------
preparar_dados_resposta <- function(pontos, arquivos_preditoras, minimo_presencas = 10) {

  if (!"especie" %in% names(pontos)) pontos$especie <- "unica"

  message("Lendo os valores das variáveis nos ", nrow(pontos), " pontos...")
  valores <- extrair_valores_pontos(pontos, arquivos_preditoras)

  resultado <- list()
  for (especie in unique(pontos$especie)) {
    linhas <- which(pontos$especie == especie)
    y      <- pontos$presenca[linhas]
    dados  <- valores[linhas, , drop = FALSE]

    if (sum(y == 1) < minimo_presencas) {
      warning("Espécie '", especie, "' ignorada: só ", sum(y == 1),
              " presença(s) (mínimo: ", minimo_presencas, ").",
              call. = FALSE, immediate. = TRUE)
      next
    }

    completos <- complete.cases(dados)
    dados     <- dados[completos, , drop = FALSE]
    y         <- y[completos]

    constantes <- vapply(dados, function(v) stats::sd(v) == 0, logical(1))
    dados      <- dados[, !constantes, drop = FALSE]

    z <- padronizar_zscore(dados)   # média e desvio dos pontos desta espécie
    resultado[[as.character(especie)]] <- list(x = as.matrix(z$dados), y = as.integer(y),
                                               parametros = z$parametros)
  }

  if (length(resultado) == 0) {
    stop("Nenhuma espécie com presenças suficientes para os testes com resposta.",
         call. = FALSE)
  }

  return(resultado)
}
