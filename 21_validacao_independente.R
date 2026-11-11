# ==============================================================================
#  FUNÇÕES - Validação independente
# ------------------------------------------------------------------------------
#  Avalia os modelos com dados que NÃO entraram no ajuste (por exemplo, coletas
#  novas, outro banco de dados ou outra região). É a forma mais forte de
#  avaliação, porque os dados de teste não têm nenhuma ligação com o treino.
#
#  O arquivo segue o formato dos registros (mesmas colunas e separadores).
#  Pode ter uma coluna de presença (1) / ausência (0). Se não tiver, todos os
#  pontos são presenças e a comparação usa o background da espécie.
#
#  São avaliados os modelos finais, o consenso (mapas das preditoras) e o JSDM
#  (se tiver rodado). Para cada um:
#    - AUC, TSS e Kappa máximos e Boyce;
#    - em cada limiar dos mapas binários: omissão (presenças independentes que
#      caem fora do mapa), sensibilidade, especificidade, TSS e Kappa.
# ==============================================================================


# ------------------------------------------------------------------------------
# perguntar_dados_independentes()
#   'usar' = TRUE ou FALSE: devolve o valor.
#   'usar' = NULL: pergunta no console se há dados independentes.
# ------------------------------------------------------------------------------
perguntar_dados_independentes <- function(usar = NULL) {

  if (is.logical(usar) && length(usar) == 1 && !is.na(usar)) return(usar)

  if (!interactive()) {
    message("Validação independente: não é possível perguntar fora de uma sessão interativa. ",
            "Preencha 'dados_independentes' no main (TRUE ou FALSE).")
    return(FALSE)
  }
  cat("\nValidação independente: avalia os modelos com dados que NÃO foram usados no ajuste.\n")
  resposta <- perguntar_opcoes(c("Sim, tenho outro conjunto de dados", "Não"),
                               "Você tem um conjunto de dados independente para avaliar os modelos? ",
                               "Validação independente")
  resposta[1] == 1
}


# ------------------------------------------------------------------------------
# detectar_formato_arquivo()
#   Descobre o separador de colunas (";", "," ou tabulação) e o decimal (",", ".")
#   de um arquivo de texto olhando as primeiras linhas. Serve para o arquivo
#   independente, que pode ter formato diferente do arquivo de registros.
#   Devolve list(sep, dec), como definir_formato_registros().
# ------------------------------------------------------------------------------
detectar_formato_arquivo <- function(caminho) {

  linhas <- readLines(caminho, n = 20, warn = FALSE, encoding = "latin1")
  linhas <- linhas[nzchar(trimws(linhas))]
  if (length(linhas) < 2) stop("O arquivo está vazio ou só tem o cabeçalho: ", caminho, call. = FALSE)

  conta <- function(caractere) sum(nchar(gsub(paste0("[^", caractere, "]"), "", linhas[1])))
  cabecalho <- c(";" = conta(";"), "\t" = conta("\t"), "," = conta(","))
  sep <- if (cabecalho[[1]] > 0 || cabecalho[[2]] > 0) {
    if (cabecalho[[1]] >= cabecalho[[2]]) ";" else "\t"
  } else ","

  dados <- linhas[-1]
  dec <- if (sep != "," && any(grepl("[0-9],[0-9]", dados))) "," else "."
  list(sep = sep, dec = dec)
}


# ------------------------------------------------------------------------------
# ler_dados_independentes()
#   Lê o arquivo (ou abre a janela se 'caminho' for ""). O arquivo tem 3 colunas,
#   NESTA ORDEM: 1) espécie, 2) longitude, 3) latitude (primeira linha =
#   cabeçalho; os nomes do cabeçalho não importam). O separador de colunas e o
#   decimal são detectados no próprio arquivo (podem ser diferentes dos do
#   arquivo de registros). Todos os pontos são tratados como presenças.
#   Descarta coordenadas inválidas, passa para a projeção dos rasters e lê os
#   preditores (preditoras) em cada ponto. Pontos sem valor ficam de fora.
#   Devolve uma tabela: especie, presenca, x, y e os preditores.
# ------------------------------------------------------------------------------
ler_dados_independentes <- function(caminho, crs_registros, preditores) {

  caminho <- selecionar_arquivo(caminho, "Selecione os dados independentes (.csv ou .txt)",
                                c("csv", "txt"))
  formato <- detectar_formato_arquivo(caminho)
  message("Dados independentes: separador de colunas '",
          if (formato$sep == "\t") "tabulação" else formato$sep,
          "' e decimal '", formato$dec, "' (detectados no arquivo).")
  tabela  <- ler_registros(caminho, formato)

  if (ncol(tabela) < 3) {
    stop("O arquivo de dados independentes precisa de 3 colunas: espécie, longitude e latitude ",
         "(nesta ordem). Foram encontradas ", ncol(tabela), ". ",
         "Confira o separador de colunas do arquivo.", call. = FALSE)
  }
  if (ncol(tabela) > 3) {
    message("Aviso: o arquivo tem ", ncol(tabela), " colunas. Serão usadas só as 3 primeiras ",
            "(espécie, longitude, latitude): ", paste(names(tabela)[1:3], collapse = ", "), ".")
  }

  dados <- data.frame(
    especie   = trimws(as.character(tabela[[1]])),
    presenca  = 1L,
    longitude = suppressWarnings(as.numeric(tabela[[2]])),
    latitude  = suppressWarnings(as.numeric(tabela[[3]])),
    stringsAsFactors = FALSE)
  n_arquivo <- nrow(dados)
  validos <- !is.na(dados$longitude) & !is.na(dados$latitude) & abs(dados$longitude) <= 180 &
    abs(dados$latitude) <= 90 & !is.na(dados$especie) & dados$especie != ""
  dados <- dados[validos, , drop = FALSE]

  referencia <- terra::rast(preditores$arquivos[["preditoras"]][1])
  pontos <- terra::vect(dados, geom = c("longitude", "latitude"), crs = crs_registros)
  if (!terra::same.crs(pontos, referencia)) pontos <- terra::project(pontos, terra::crs(referencia))
  xy <- terra::crds(pontos)
  dados$x <- xy[, 1]
  dados$y <- xy[, 2]

  valores <- extrair_valores_pontos(dados, preditores$arquivos[["preditoras"]])
  names(valores) <- preditores$nomes
  com_valor <- stats::complete.cases(valores)
  dados <- cbind(dados[com_valor, c("especie", "presenca", "x", "y")],
                 valores[com_valor, , drop = FALSE])
  rownames(dados) <- NULL

  cat("\n================ DADOS INDEPENDENTES ================\n")
  cat("Arquivo:", caminho, "\n")
  cat("Linhas no arquivo:", n_arquivo, "| coordenada ou presença inválida:", sum(!validos),
      "| fora dos rasters (sem valor):", sum(!com_valor), "| usados:", nrow(dados), "\n")
  cat("Todos os pontos independentes são tratados como presenças.\n")
  if (nrow(dados) == 0) stop("Nenhum ponto independente pôde ser usado.", call. = FALSE)
  resumo <- data.frame(especie   = sort(unique(dados$especie)))
  resumo$presencas <- vapply(resumo$especie, function(e) sum(dados$presenca[dados$especie == e] == 1), numeric(1))
  resumo$ausencias <- vapply(resumo$especie, function(e) sum(dados$presenca[dados$especie == e] == 0), numeric(1))
  print(resumo, row.names = FALSE)
  cat("=====================================================\n")
  return(dados)
}


# ------------------------------------------------------------------------------
# avaliar_independente()
#   Avalia modelos finais, consenso e JSDM com os dados independentes.
#   Avisa quantas presenças independentes caem em células já usadas no treino
#   (essas não são realmente independentes).
#   Salva em pasta_saida: resumo.csv (métricas gerais) e por_limiar.csv
#   (omissão e demais métricas em cada limiar). Devolve list(resumo, por_limiar).
# ------------------------------------------------------------------------------
avaliar_independente <- function(independentes, dados_modelos, modelos_finais, limiares,
                                 preditores, consenso = NULL, jsdm = NULL, pasta_saida) {

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  referencia <- terra::rast(preditores$arquivos[["preditoras"]][1])
  tab_resumo <- list()
  tab_limiar <- list()

  registrar <- function(especie, fonte, nome, p1, p0, lim, tipo_ausencia) {
    m <- avaliar_previsoes(c(p1, p0), c(rep(1, length(p1)), rep(0, length(p0))))
    tab_resumo[[length(tab_resumo) + 1]] <<- data.frame(
      especie = especie, fonte = fonte, nome = nome, presencas = sum(!is.na(p1)),
      ausencias = sum(!is.na(p0)), tipo_ausencia = tipo_ausencia,
      auc = m$auc, tss_max = m$tss, kappa_max = m$kappa, boyce = m$boyce)
    for (i in seq_len(NROW(lim))) {
      tab_limiar[[length(tab_limiar) + 1]] <<- data.frame(
        especie = especie, fonte = fonte, nome = nome, metodo_limiar = lim$metodo[i],
        limiar = lim$limiar[i], metricas_no_limiar(p1, p0, lim$limiar[i]))
    }
  }

  especies <- intersect(unique(independentes$especie), names(modelos_finais))
  sem_modelo <- setdiff(unique(independentes$especie), names(modelos_finais))
  if (length(sem_modelo) > 0) {
    message("Espécies dos dados independentes sem modelo (nomes diferentes ou poucas presenças): ",
            paste(sem_modelo, collapse = ", "))
  }

  for (especie in especies) {
    ind  <- independentes[independentes$especie == especie, , drop = FALSE]
    pres <- ind[ind$presenca == 1, , drop = FALSE]
    if (nrow(pres) == 0) next
    aus  <- ind[ind$presenca == 0, , drop = FALSE]
    tipo_ausencia <- "ausências do arquivo"
    if (nrow(aus) == 0) {
      treino <- dados_modelos[[especie]]$dados
      aus <- treino[treino$presenca == 0, , drop = FALSE]
      tipo_ausencia <- "background da espécie"
    }

    # presenças independentes em células já usadas no treino
    treino_pres <- dados_modelos[[especie]]$dados
    treino_pres <- treino_pres[treino_pres$presenca == 1, , drop = FALSE]
    repetidas <- terra::cellFromXY(referencia, as.matrix(pres[, c("x", "y")])) %in%
      terra::cellFromXY(referencia, as.matrix(treino_pres[, c("x", "y")]))
    if (any(repetidas)) {
      message("  ", especie, ": ", sum(repetidas), " de ", nrow(pres),
              " presença(s) independente(s) caem em células usadas no treino.")
    }

    # modelos finais
    for (codigo in names(modelos_finais[[especie]])) {
      objeto <- modelos_finais[[especie]][[codigo]]
      lim <- if (is.null(limiares$tabela)) NULL else
        limiares$tabela[limiares$tabela$especie == especie & limiares$tabela$modelo == codigo, ]
      registrar(especie, "modelo", codigo, prever_modelo(objeto, pres), prever_modelo(objeto, aus),
                lim, tipo_ausencia)
    }

    # consenso (mapas das preditoras)
    if (!is.null(consenso) && !is.null(consenso$indice)) {
      cont <- consenso$indice[consenso$indice$especie == especie & consenso$indice$conjunto == "preditoras" &
                                consenso$indice$tipo == "continuo", , drop = FALSE]
      for (i in seq_len(nrow(cont))) {
        mapa <- terra::rast(cont$arquivo[i])
        p1 <- terra::extract(mapa, as.matrix(pres[, c("x", "y")]))[, 1]
        p0 <- terra::extract(mapa, as.matrix(aus[, c("x", "y")]))[, 1]
        if (cont$metodo[i] == "votacao") {
          lim <- data.frame(metodo = "maioria", limiar = 0.5)
        } else {
          lim <- NULL
          if (!is.null(consenso$limiares)) {
            lim <- consenso$limiares[consenso$limiares$especie == especie &
                                       consenso$limiares$metodo == cont$metodo[i], ]
            lim <- data.frame(metodo = lim$metodo_limiar, limiar = lim$limiar)
          }
        }
        registrar(especie, "consenso", cont$metodo[i], p1, p0, lim, tipo_ausencia)
      }
    }

    # JSDM
    if (!is.null(jsdm) && !is.null(jsdm$modelo) && especie %in% jsdm$modelo$especies) {
      lim <- if (is.null(jsdm$limiares)) NULL else jsdm$limiares[jsdm$limiares$especie == especie, ]
      registrar(especie, "jsdm", "jsdm", prever_jsdm(jsdm$modelo, pres)[, especie],
                prever_jsdm(jsdm$modelo, aus)[, especie], lim, tipo_ausencia)
    }
  }

  if (length(tab_resumo) == 0) {
    message("Nenhuma espécie dos dados independentes pôde ser avaliada.")
    return(invisible(NULL))
  }
  resumo     <- do.call(rbind, tab_resumo)
  por_limiar <- if (length(tab_limiar) > 0) do.call(rbind, tab_limiar) else NULL
  utils::write.csv(acrescentar_preditores(resumo, preditores),
                   file.path(pasta_saida, "resumo.csv"), row.names = FALSE,
                   fileEncoding = "UTF-8")
  if (!is.null(por_limiar)) {
    utils::write.csv(acrescentar_preditores(por_limiar, preditores),
                     file.path(pasta_saida, "por_limiar.csv"), row.names = FALSE,
                     fileEncoding = "UTF-8")
  }

  cat("\n================ VALIDAÇÃO INDEPENDENTE ================\n")
  for (especie in unique(resumo$especie)) {
    cat("\n--", especie, "(comparação com:", resumo$tipo_ausencia[resumo$especie == especie][1], ") --\n")
    r <- resumo[resumo$especie == especie, c("fonte", "nome", "presencas", "auc", "tss_max",
                                             "kappa_max", "boyce")]
    r[, 4:7] <- arredondar_sdm(r[, 4:7])
    print(r[order(-r$tss_max), ], row.names = FALSE)
    if (!is.null(por_limiar)) {
      o <- por_limiar[por_limiar$especie == especie, c("fonte", "nome", "metodo_limiar", "omissao")]
      o <- stats::reshape(o, idvar = c("fonte", "nome"), timevar = "metodo_limiar", direction = "wide")
      names(o) <- sub("^omissao\\.", "omissao_", names(o))
      o[, -(1:2)] <- arredondar_sdm(o[, -(1:2)])
      cat("Omissão das presenças independentes em cada limiar:\n")
      print(o, row.names = FALSE)
    }
  }
  cat("\nResultados salvos em:", pasta_saida, "\n")
  cat("========================================================\n")

  invisible(list(resumo = resumo, por_limiar = por_limiar))
}
