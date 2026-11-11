# ==============================================================================
#  FUNÇÕES - Verificação dos rasters cortados e dos registros
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
#  Os relatórios são salvos em CSV na pasta verificacao/ do projeto.
# ==============================================================================


# ------------------------------------------------------------------------------
# mesma_geometria()
#   Compara UMA propriedade do raster 'r' com a do raster 'ref'.
#   propriedade: "crs" (projeção), "ext" (extensão), "rowcol" (nº de linhas e
#   colunas) ou "res" (tamanho da célula). Devolve TRUE ou FALSE.
# ------------------------------------------------------------------------------
mesma_geometria <- function(r, ref, propriedade) {

  opcoes <- list(crs = FALSE, ext = FALSE, rowcol = FALSE, res = FALSE)
  opcoes[[propriedade]] <- TRUE

  resultado <- do.call(terra::compareGeom,
                       c(list(r, ref, stopOnError = FALSE), opcoes))
  return(isTRUE(resultado))
}


# ------------------------------------------------------------------------------
# cobertura_mascara()
#   Mede quanto da máscara está bem representado no raster 'r':
#     sem_dados   : % da área da máscara onde o raster tem NA (sem valor)
#     fora_raster : % da área da máscara que fica FORA da extensão do raster
#   Devolve um vetor com os dois valores.
# ------------------------------------------------------------------------------
cobertura_mascara <- function(r, mascara) {

  if (inherits(mascara, "SpatVector")) {

    # células do raster que estão dentro da máscara
    dentro    <- terra::rasterize(mascara, r, field = 1, touches = TRUE)
    n_dentro  <- terra::global(!is.na(dentro), "sum", na.rm = TRUE)[[1]]
    n_sem     <- terra::global(is.na(r) & !is.na(dentro), "sum", na.rm = TRUE)[[1]]

    # parte da máscara fora da extensão do raster
    caixa     <- terra::as.polygons(terra::ext(r), crs = terra::crs(r))
    fora      <- terra::erase(mascara, caixa)
    area_fora <- if (nrow(fora) > 0) sum(terra::expanse(fora)) else 0
    pct_fora  <- 100 * area_fora / sum(terra::expanse(mascara))

  } else {

    # máscara raster: leva para a grade do raster e usa as células não-NA
    m <- mascara
    if (!terra::compareGeom(r, m, stopOnError = FALSE)) {
      m <- terra::resample(m, r, method = "near")
    }
    dentro   <- !is.na(m)
    n_dentro <- terra::global(dentro, "sum", na.rm = TRUE)[[1]]
    n_sem    <- terra::global(is.na(r) & dentro, "sum", na.rm = TRUE)[[1]]

    # compara as caixas (extensões) da máscara e do raster
    em <- as.vector(terra::ext(mascara))
    er <- as.vector(terra::ext(r))
    largura  <- max(0, min(em[2], er[2]) - max(em[1], er[1]))
    altura   <- max(0, min(em[4], er[4]) - max(em[3], er[3]))
    pct_fora <- 100 * (1 - (largura * altura) /
                         ((em[2] - em[1]) * (em[4] - em[3])))
  }

  pct_sem <- if (n_dentro > 0) 100 * n_sem / n_dentro else NA_real_
  return(c(sem_dados = pct_sem, fora_raster = pct_fora))
}


# ------------------------------------------------------------------------------
# verificar_um_raster()
#   Verifica UM arquivo e devolve uma linha (data.frame) com:
#     - consegue abrir/ler?  tem projeção?  está todo NA?
#     - projeção, extensão, linhas/colunas e resolução iguais à referência?
#     - cobertura da máscara (ver cobertura_mascara)
# ------------------------------------------------------------------------------
verificar_um_raster <- function(caminho, conjunto, referencia, mascara) {

  linha <- data.frame(
    conjunto = conjunto, arquivo = basename(caminho),
    leitura_ok = FALSE, crs_ok = NA, extensao_ok = NA,
    linhas_colunas_ok = NA, resolucao_ok = NA,
    res_x = NA_real_, res_y = NA_real_, todo_na = NA,
    pct_mascara_sem_dados = NA_real_, pct_mascara_fora_raster = NA_real_,
    observacao = "", stringsAsFactors = FALSE
  )

  # 1) consegue abrir e ler os valores?
  r <- tryCatch(terra::rast(caminho), error = function(e) NULL)
  if (is.null(r)) {
    linha$observacao <- "não foi possível abrir o arquivo"
    return(linha)
  }
  r1 <- r[[1]]
  estat <- tryCatch(terra::global(r1, c("min", "max"), na.rm = TRUE),
                    error = function(e) NULL)
  if (is.null(estat)) {
    linha$observacao <- "erro ao ler os valores"
    return(linha)
  }
  linha$leitura_ok <- TRUE
  linha$todo_na    <- is.na(estat$min) || is.nan(estat$min)
  linha$res_x      <- terra::res(r1)[1]
  linha$res_y      <- terra::res(r1)[2]

  if (terra::crs(r1) == "") linha$observacao <- "sem sistema de coordenadas"

  # 2) geometria igual à da referência?
  linha$crs_ok            <- mesma_geometria(r1, referencia, "crs")
  linha$extensao_ok       <- mesma_geometria(r1, referencia, "ext")
  linha$linhas_colunas_ok <- mesma_geometria(r1, referencia, "rowcol")
  linha$resolucao_ok      <- mesma_geometria(r1, referencia, "res")

  # 3) cobertura da máscara
  cob <- tryCatch(cobertura_mascara(r1, mascara), error = function(e) NULL)
  if (is.null(cob)) {
    linha$observacao <- paste(linha$observacao, "cobertura não calculada")
  } else {
    linha$pct_mascara_sem_dados   <- arredondar_sdm(cob[["sem_dados"]])
    linha$pct_mascara_fora_raster <- arredondar_sdm(cob[["fora_raster"]])
  }

  return(linha)
}


# ------------------------------------------------------------------------------
# verificar_rasters()
#   Verifica TODOS os rasters cortados (preditoras e cenários), comparando cada um
#   com o primeiro raster das variáveis preditoras (a referência).
#   Mostra um resumo no console, salva a tabela completa em CSV e a devolve.
# ------------------------------------------------------------------------------
verificar_rasters <- function(variaveis_cortadas, mascara, pasta_saida) {

  referencia <- terra::rast(variaveis_cortadas[["preditoras"]][1])[[1]]

  linhas <- list()
  for (conjunto in names(variaveis_cortadas)) {
    message("Verificando: ", conjunto)
    for (caminho in variaveis_cortadas[[conjunto]]) {
      linhas[[length(linhas) + 1]] <- verificar_um_raster(caminho, conjunto,
                                                          referencia, mascara)
    }
  }
  tabela <- do.call(rbind, linhas)
  n      <- nrow(tabela)

  # ---- resumo no console ----
  cat("\n================ VERIFICAÇÃO DOS RASTERS ================\n")
  cat("Referência:", basename(variaveis_cortadas[["preditoras"]][1]), "\n")
  cat("Rasters verificados        :", n, "\n")
  cat("Abriram e foram lidos      :", sum(tabela$leitura_ok), "de", n, "\n")
  cat("Mesma projeção (CRS)       :", sum(tabela$crs_ok %in% TRUE), "de", n, "\n")
  cat("Mesma extensão             :", sum(tabela$extensao_ok %in% TRUE), "de", n, "\n")
  cat("Mesmo nº de linhas/colunas :", sum(tabela$linhas_colunas_ok %in% TRUE), "de", n, "\n")
  cat("Mesma resolução            :", sum(tabela$resolucao_ok %in% TRUE), "de", n, "\n")
  cat("Tamanho da célula (x, y)   :",
      paste(unique(paste(round(tabela$res_x, 6), round(tabela$res_y, 6), sep = ", ")),
            collapse = " | "), "\n")
  cat("Rasters totalmente vazios  :", sum(tabela$todo_na %in% TRUE), "\n")

  problemas <- tabela[!tabela$leitura_ok |
                        tabela$crs_ok %in% FALSE | tabela$extensao_ok %in% FALSE |
                        tabela$linhas_colunas_ok %in% FALSE |
                        tabela$resolucao_ok %in% FALSE |
                        tabela$todo_na %in% TRUE |
                        tabela$observacao != "", ]
  if (nrow(problemas) == 0) {
    cat("\nNenhum problema de leitura ou de geometria encontrado.\n")
  } else {
    cat("\nARQUIVOS COM PROBLEMA:\n")
    print(problemas[, c("conjunto", "arquivo", "leitura_ok", "crs_ok", "extensao_ok",
                        "linhas_colunas_ok", "resolucao_ok", "todo_na", "observacao")],
          row.names = FALSE)
  }

  cat("\n---- Cobertura da máscara ----\n")
  fora <- tabela$pct_mascara_fora_raster
  sem  <- tabela$pct_mascara_sem_dados

  # (tolerância de 0,01% para diferenças mínimas de borda)
  descobertos <- tabela[(!is.na(fora) & fora >= 0.01) | (!is.na(sem) & sem > 0), ]

  if (nrow(descobertos) == 0) {
    cat("Todos os rasters cobrem completamente a área da máscara.\n")
  } else {
    cat(nrow(descobertos), "de", n, "rasters não representam toda a área da máscara.\n")
    cat("  fora_raster : % da máscara que fica FORA da extensão do raster\n")
    cat("  sem_dados   : % da máscara dentro do raster, mas com NA\n\n")
    resumo <- descobertos[, c("conjunto", "arquivo", "pct_mascara_fora_raster",
                              "pct_mascara_sem_dados")]
    names(resumo) <- c("conjunto", "arquivo", "fora_raster", "sem_dados")
    print(head(resumo, 20), row.names = FALSE)
    if (nrow(resumo) > 20) cat("... (lista completa no CSV)\n")
  }
  cat("==========================================================\n\n")

  destino <- file.path(pasta_saida, "verificacao_rasters.csv")
  utils::write.csv(tabela, destino, row.names = FALSE, fileEncoding = "UTF-8")
  message("Tabela completa salva em: ", destino)

  return(invisible(tabela))
}


# ------------------------------------------------------------------------------
# ler_registros()
#   Lê o arquivo de registros (.csv ou .txt) usando o separador de colunas e o
#   separador decimal definidos no main. A primeira linha deve ser o cabeçalho.
#   Tenta UTF-8 (com ou sem BOM) e, se der problema, tenta latin1.
#   'nrows' limita as linhas lidas (-1 = todas); serve para olhar só o cabeçalho.
# ------------------------------------------------------------------------------
ler_registros <- function(caminho, formato, nrows = -1) {

  ler <- function(codificacao) {
    utils::read.table(caminho, header = TRUE, sep = formato$sep, dec = formato$dec,
                      quote = "\"", comment.char = "", stringsAsFactors = FALSE,
                      check.names = FALSE, strip.white = TRUE, nrows = nrows,
                      fileEncoding = codificacao)
  }

  tryCatch(
    ler("UTF-8-BOM"),
    error   = function(e) ler("latin1"),
    warning = function(w) {
      if (grepl("invalid input", conditionMessage(w))) ler("latin1") else ler("UTF-8-BOM")
    }
  )
}


# ------------------------------------------------------------------------------
# verificar_registros()
#   Confere se os registros de ocorrência caem dentro da área de estudo (a
#   máscara usada no corte). Para cada registro verifica:
#     - se as coordenadas são números válidos;
#     - se estão no intervalo possível (longitude -180 a 180, latitude -90 a 90);
#     - se ficam dentro da máscara.
#   Registros com problema são salvos em CSV (com o número da linha no arquivo).
# ------------------------------------------------------------------------------
verificar_registros <- function(entradas, mascara, coluna_longitude,
                                coluna_latitude, crs_registros, pasta_saida) {

  registros <- ler_registros(entradas$registros, entradas$formato_registros)

  faltando <- setdiff(c(coluna_longitude, coluna_latitude), names(registros))
  if (length(faltando) > 0) {
    stop("Coluna(s) não encontrada(s) nos registros: ",
         paste(faltando, collapse = ", "),
         "\nColunas disponíveis: ", paste(names(registros), collapse = ", "),
         "\nConfira 'coluna_longitude' e 'coluna_latitude' no main, e também ",
         "o separador de colunas.", call. = FALSE)
  }

  n   <- nrow(registros)
  lon <- suppressWarnings(as.numeric(registros[[coluna_longitude]]))
  lat <- suppressWarnings(as.numeric(registros[[coluna_latitude]]))

  sem_coordenada <- is.na(lon) | is.na(lat)
  fora_intervalo <- !sem_coordenada & (abs(lon) > 180 | abs(lat) > 90)
  validos        <- !sem_coordenada & !fora_intervalo

  # pontos válidos -> objeto espacial, na mesma projeção da máscara
  pontos <- terra::vect(data.frame(x = lon[validos], y = lat[validos]),
                        geom = c("x", "y"), crs = crs_registros)
  if (!terra::same.crs(pontos, mascara)) {
    pontos <- terra::project(pontos, terra::crs(mascara))
  }

  # dentro da área de estudo?
  if (inherits(mascara, "SpatVector")) {
    dentro <- terra::is.related(pontos, mascara, "intersects")
  } else {
    dentro <- !is.na(terra::extract(mascara, pontos)[, 2])
  }

  # tabela de problemas
  motivo <- rep("", n)
  motivo[sem_coordenada] <- "coordenada ausente ou não numérica"
  motivo[fora_intervalo] <- "coordenada fora do intervalo possível"
  motivo[which(validos)[!dentro]] <- "fora da área de estudo"

  problemas <- data.frame(linha_no_arquivo = seq_len(n) + 1,
                          longitude = registros[[coluna_longitude]],
                          latitude  = registros[[coluna_latitude]],
                          motivo    = motivo, stringsAsFactors = FALSE)
  problemas <- problemas[problemas$motivo != "", ]

  # ---- resumo no console ----
  cat("\n================ VERIFICAÇÃO DOS REGISTROS ================\n")
  cat("Total de registros                  :", n, "\n")
  cat("Sem coordenada válida               :", sum(sem_coordenada), "\n")
  cat("Coordenada fora do intervalo        :", sum(fora_intervalo), "\n")
  cat("Dentro da área de estudo            :", sum(dentro), "\n")
  cat("Fora da área de estudo              :", sum(!dentro), "\n")

  if (sum(sem_coordenada) > 0.5 * n) {
    cat("\nATENÇÃO: mais da metade das coordenadas não é numérica. Confira o ",
        "separador decimal e o de colunas no main.\n", sep = "")
  }

  if (nrow(problemas) == 0) {
    cat("\nTodos os registros caem dentro da área de estudo.\n")
  } else {
    destino <- file.path(pasta_saida, "registros_com_problema.csv")
    utils::write.csv(problemas, destino, row.names = FALSE, fileEncoding = "UTF-8")
    cat("\nRegistros com problema salvos em:", destino, "\n")
    cat("Primeiros registros com problema:\n")
    print(head(problemas, 10), row.names = FALSE)
  }
  cat("===========================================================\n\n")

  return(invisible(list(total = n, validos = sum(validos),
                        dentro = sum(dentro), fora = sum(!dentro),
                        problemas = problemas)))
}
