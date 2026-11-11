# ==============================================================================
#  FUNÇÕES - Variáveis escalonadas (rasters em z-score)
# ------------------------------------------------------------------------------
#  Salva uma cópia de cada variável cortada em z-score:
#       z = (valor - média) / desvio
#  Preditoras E cenários usam a média e o desvio das PREDITORAS (os mesmos da seção 12).
#  Assim, nas preditoras cada variável fica com média 0 e desvio 1, e nos cenários
#  os valores mostram a mudança em "desvios das preditoras" (ex.: z = 1,5 numa
#  célula = 1,5 desvio acima da média atual da área).
#  Os rasters originais (variaveis_cortadas/) não são alterados.
#
#  Os arquivos levam o nome curto da variável (ex.: bio01.tif), igual em todos
#  os conjuntos, para facilitar a projeção nos cenários depois.
# ==============================================================================


# ------------------------------------------------------------------------------
# escalonar_raster()
#   Escalona UM raster com a média e o desvio informados e salva em
#   'arquivo_saida' (compressão LZW). NA continua NA.
# ------------------------------------------------------------------------------
escalonar_raster <- function(arquivo, media, desvio, nome, arquivo_saida) {

  r <- terra::rast(arquivo)[[1]]
  z <- (r - media) / desvio
  names(z) <- nome

  terra::writeRaster(z, arquivo_saida, overwrite = TRUE, datatype = "FLT4S",
                     gdal = c("COMPRESS=LZW"))
  return(arquivo_saida)
}


# ------------------------------------------------------------------------------
# salvar_variaveis_escalonadas()
#   Para cada conjunto (preditoras e cenários), escalona todas as variáveis que
#   têm média e desvio nas PREDITORAS ('parametros', da seção 12) e salva em
#   pasta_saida/<conjunto>/<variável>.tif.
#   Preditoras de um cenário que não existem nas preditoras são puladas (aviso):
#   sem a média e o desvio das preditoras, não há como escaloná-las de forma
#   comparável.
#   Salva também pasta_saida/parametros_zscore.csv (variável, média, desvio).
#   Devolve a lista de arquivos gerados por conjunto.
# ------------------------------------------------------------------------------
salvar_variaveis_escalonadas <- function(variaveis_cortadas, parametros, pasta_saida) {

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  tabela_par <- data.frame(variavel = names(parametros$media),
                           media    = as.numeric(parametros$media),
                           desvio   = as.numeric(parametros$desvio[names(parametros$media)]))
  utils::write.csv(tabela_par, file.path(pasta_saida, "parametros_zscore.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")

  cat("\n================ VARIÁVEIS ESCALONADAS (z-score) ================\n")
  cat("Média e desvio usados: das PREDITORAS (", nrow(tabela_par), "variáveis)\n")

  resultado <- list()
  for (conjunto in names(variaveis_cortadas)) {
    arquivos <- variaveis_cortadas[[conjunto]]
    nomes    <- make.unique(vapply(arquivos, nome_curto_variavel, character(1)), sep = "_")
    pasta    <- file.path(pasta_saida, conjunto)
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

    sem_parametro <- setdiff(nomes, tabela_par$variavel)
    if (length(sem_parametro) > 0) {
      message("Aviso [", conjunto, "]: sem média/desvio nas preditoras, não escalonadas: ",
              paste(sem_parametro, collapse = ", "))
    }

    usar <- which(nomes %in% tabela_par$variavel)
    cat(conjunto, ":", length(usar), "variáveis\n")

    gerados <- character(0)
    for (i in usar) {
      v <- nomes[i]
      gerados <- c(gerados, escalonar_raster(
        arquivo       = arquivos[i],
        media         = parametros$media[[v]],
        desvio        = parametros$desvio[[v]],
        nome          = v,
        arquivo_saida = file.path(pasta, paste0(v, ".tif"))
      ))
    }
    resultado[[conjunto]] <- gerados
  }

  cat("Salvo em:", pasta_saida, "\n")
  cat("=================================================================\n")
  return(resultado)
}
