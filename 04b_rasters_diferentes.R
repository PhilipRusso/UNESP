# ==============================================================================
#  FUNÇÕES - Rasters com geometria diferente (extensão, tamanho de célula, ...)
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
#  Depois da verificação (seção 7), se os rasters cortados não tiverem todos a
#  MESMA grade (projeção, extensão, linhas/colunas e tamanho de célula), as
#  etapas seguintes param com erros como "[rast] extents do not match".
#  Aqui o script avisa quais são os rasters diferentes e deixa escolher:
#    "reamostrar" : todos passam para a grade do raster de MENOR célula. Como o
#                   destino sempre tem células menores (ou iguais), cada célula
#                   grande é apenas DIVIDIDA em células menores com o mesmo valor
#                   (vizinho mais próximo): não há cálculo de novos valores.
#                   Os arquivos originais vão para a subpasta
#                   "originais_antes_da_reamostragem" de cada conjunto.
#    "excluir"    : ficam só os rasters da geometria mais comum; os outros saem
#                   da análise (os arquivos não são apagados, só deixam de ser
#                   usados). A lista fica em verificacao/rasters_excluidos.csv.
#                   Se uma variável é excluída em um conjunto, ela é excluída
#                   também dos outros (preditoras e cenários).
#  A escolha vem do parâmetro 'rasters_diferentes' (seção 2 do main):
#  "reamostrar", "excluir" ou "prompt" (o script pergunta).
# ==============================================================================


# ------------------------------------------------------------------------------
# agrupar_geometrias()
#   Agrupa os rasters de 'variaveis_cortadas' pela geometria (projeção,
#   extensão, nº de linhas/colunas e tamanho de célula). Devolve uma lista:
#     $itens : conjunto, caminho, arquivo e grupo de cada raster
#     $grupos: resumo de cada grupo (n, tamanho da célula, colunas x linhas, projeção)
#     $moldes: um raster-molde (só a grade) por grupo
# ------------------------------------------------------------------------------
agrupar_geometrias <- function(variaveis_cortadas) {

  itens <- data.frame(conjunto = character(0), caminho = character(0),
                      arquivo = character(0), grupo = integer(0),
                      stringsAsFactors = FALSE)
  moldes <- list()

  for (conjunto in names(variaveis_cortadas)) {
    for (caminho in variaveis_cortadas[[conjunto]]) {
      r <- tryCatch(terra::rast(caminho)[[1]], error = function(e) NULL)
      if (is.null(r)) next   # arquivo ilegível: já aparece na verificação

      grupo <- 0L
      for (k in seq_along(moldes)) {
        igual <- terra::compareGeom(r, moldes[[k]], crs = TRUE, ext = TRUE,
                                    rowcol = TRUE, res = TRUE, stopOnError = FALSE)
        if (isTRUE(igual)) { grupo <- k; break }
      }
      if (grupo == 0L) {
        moldes[[length(moldes) + 1]] <- r
        grupo <- length(moldes)
      }
      itens[nrow(itens) + 1, ] <- list(conjunto, caminho, basename(caminho), grupo)
    }
  }

  nome_crs <- function(r) {
    d <- tryCatch(terra::crs(r, describe = TRUE), error = function(e) NULL)
    if (is.null(d) || is.na(d$name[1])) return("(sem nome)")
    as.character(d$name[1])
  }

  grupos <- data.frame(
    grupo   = seq_along(moldes),
    n       = tabulate(itens$grupo, length(moldes)),
    res_x   = vapply(moldes, function(r) terra::res(r)[1], numeric(1)),
    res_y   = vapply(moldes, function(r) terra::res(r)[2], numeric(1)),
    colunas = vapply(moldes, terra::ncol, numeric(1)),
    linhas  = vapply(moldes, terra::nrow, numeric(1)),
    crs     = vapply(moldes, nome_crs, character(1)),
    stringsAsFactors = FALSE
  )
  grupos$area_celula <- grupos$res_x * grupos$res_y

  return(list(itens = itens, grupos = grupos, moldes = moldes))
}


# ------------------------------------------------------------------------------
# ler_rasters_excluidos()
#   Nomes dos arquivos excluídos da análise (verificacao/rasters_excluidos.csv).
#   Devolve character(0) se não houver nenhum.
# ------------------------------------------------------------------------------
ler_rasters_excluidos <- function(pasta_verificacao) {

  caminho <- file.path(pasta_verificacao, "rasters_excluidos.csv")
  if (!file.exists(caminho)) return(character(0))

  tabela <- tryCatch(utils::read.csv(caminho, stringsAsFactors = FALSE,
                                     fileEncoding = "UTF-8"),
                     error = function(e) NULL)
  if (is.null(tabela) || !"arquivo" %in% names(tabela)) return(character(0))
  return(unique(as.character(tabela$arquivo)))
}


# ------------------------------------------------------------------------------
# reamostrar_raster()
#   Reamostra UM raster para a grade de 'molde' (vizinho mais próximo), reaplica
#   a máscara na grade nova e substitui o arquivo. O original é guardado em
#   <pasta do raster>/originais_antes_da_reamostragem/.
#   Devolve o caminho do arquivo (o mesmo).
# ------------------------------------------------------------------------------
reamostrar_raster <- function(caminho, molde, mascara) {

  r    <- terra::rast(caminho)
  novo <- terra::resample(r, molde, method = "near")

  # a máscara foi aplicada na grade antiga: reaplica na grade nova
  m <- igualar_crs(mascara, molde)
  if (inherits(m, "SpatRaster") &&
      !isTRUE(terra::compareGeom(novo, m, stopOnError = FALSE))) {
    m <- terra::resample(m, novo, method = "near")
  }
  novo <- terra::mask(novo, m)

  temporario <- file.path(dirname(caminho), paste0("~reamostrando_", basename(caminho)))
  terra::writeRaster(novo, temporario, overwrite = TRUE,
                     datatype = terra::datatype(r)[1], gdal = c("COMPRESS=LZW"))
  rm(r, novo); invisible(gc())   # solta o arquivo (necessário no Windows)

  pasta_orig <- file.path(dirname(caminho), "originais_antes_da_reamostragem")
  dir.create(pasta_orig, showWarnings = FALSE)
  destino_orig <- file.path(pasta_orig, basename(caminho))

  if (file.exists(destino_orig)) {
    file.remove(caminho)                       # já há uma cópia do original guardada
  } else if (!file.rename(caminho, destino_orig)) {
    stop("Não foi possível mover o original: ", caminho, call. = FALSE)
  }
  if (!file.rename(temporario, caminho)) {
    stop("Não foi possível salvar o raster reamostrado: ", caminho, call. = FALSE)
  }

  return(caminho)
}


# ------------------------------------------------------------------------------
# tratar_rasters_diferentes()
#   1) agrupa os rasters cortados pela geometria;
#   2) se todos forem iguais, não faz nada;
#   3) se houver diferenças, mostra os rasters com problema e aplica a escolha
#      de 'rasters_diferentes' ("reamostrar", "excluir" ou "prompt");
#   4) refaz a verificação e devolve 'variaveis_cortadas' atualizado.
# ------------------------------------------------------------------------------
tratar_rasters_diferentes <- function(variaveis_cortadas, mascara, pasta_verificacao) {

  ag     <- agrupar_geometrias(variaveis_cortadas)
  grupos <- ag$grupos
  itens  <- ag$itens

  if (nrow(grupos) <= 1) {
    message("Todos os rasters cortados têm a mesma geometria.")
    return(variaveis_cortadas)
  }

  # grupo de referência para REAMOSTRAR: menor célula (empate: mais rasters)
  ordem_fino <- order(grupos$area_celula, -grupos$n)
  g_fino     <- grupos$grupo[ordem_fino[1]]
  # grupo que FICA ao excluir: o mais comum (empate: menor célula)
  g_comum    <- grupos$grupo[order(-grupos$n, grupos$area_celula)[1]]

  # ---- aviso ----
  cat("\n=========== RASTERS COM PARÂMETROS DIFERENTES ===========\n")
  cat("Os", nrow(itens), "rasters cortados NÃO têm todos a mesma grade; as etapas\n")
  cat("seguintes parariam com \"[rast] extents do not match\".\n\n")
  resumo <- grupos[, c("grupo", "n", "res_x", "res_y", "colunas", "linhas", "crs")]
  names(resumo) <- c("grupo", "rasters", "célula_x", "célula_y", "colunas", "linhas", "projeção")
  print(resumo, row.names = FALSE)
  cat("\nGrupo de MENOR célula (destino da reamostragem): ", g_fino, "\n", sep = "")
  cat("Grupo mais comum (o que fica ao excluir)        : ", g_comum, "\n\n", sep = "")

  fora_fino <- itens[itens$grupo != g_fino, ]
  cat("Rasters diferentes do grupo de menor célula (", nrow(fora_fino), "):\n", sep = "")
  print(head(data.frame(conjunto = fora_fino$conjunto, arquivo = fora_fino$arquivo,
                        grupo = fora_fino$grupo), 30), row.names = FALSE)
  if (nrow(fora_fino) > 30) cat("... (", nrow(fora_fino) - 30, " a mais)\n", sep = "")
  cat("==========================================================\n\n")

  # ---- escolha: reamostrar ou excluir ----
  escolha <- ler_parametro("rasters_diferentes")

  if (escolha == "reamostrar") {

    molde <- ag$moldes[[g_fino]]
    cat("Reamostrando", nrow(fora_fino), "raster(s) para a grade do grupo", g_fino,
        "(vizinho mais próximo: as células grandes são só divididas)...\n")
    for (i in seq_len(nrow(fora_fino))) {
      message("   [", i, "/", nrow(fora_fino), "] ", fora_fino$conjunto[i], "/",
              fora_fino$arquivo[i])
      reamostrar_raster(fora_fino$caminho[i], molde, mascara)
    }
    message("Reamostragem concluída. Originais guardados em ",
            "'originais_antes_da_reamostragem' de cada pasta.")

  } else {   # "excluir"

    sair    <- itens[itens$grupo != g_comum, ]
    arqs    <- unique(sair$arquivo)   # a variável sai de TODOS os conjuntos
    message("Excluindo da análise ", length(arqs), " variável(is): ",
            paste(arqs, collapse = ", "))

    for (conjunto in names(variaveis_cortadas)) {
      variaveis_cortadas[[conjunto]] <-
        variaveis_cortadas[[conjunto]][!basename(variaveis_cortadas[[conjunto]]) %in% arqs]
    }

    dir.create(pasta_verificacao, recursive = TRUE, showWarnings = FALSE)
    anteriores <- ler_rasters_excluidos(pasta_verificacao)
    todos      <- unique(c(anteriores, arqs))
    utils::write.csv(data.frame(arquivo = todos,
                                motivo = "geometria diferente da maioria dos rasters",
                                stringsAsFactors = FALSE),
                     file.path(pasta_verificacao, "rasters_excluidos.csv"),
                     row.names = FALSE, fileEncoding = "UTF-8")

    if (length(variaveis_cortadas[["preditoras"]]) == 0) {
      stop("Nenhuma variável preditora sobrou depois da exclusão.", call. = FALSE)
    }
  }

  # ---- confere de novo ----
  ag2 <- agrupar_geometrias(variaveis_cortadas)
  if (nrow(ag2$grupos) > 1) {
    warning("Ainda há rasters com geometria diferente depois do tratamento. ",
            "Confira a tabela de verificação.", call. = FALSE, immediate. = TRUE)
  } else {
    message("Agora todos os rasters têm a mesma geometria.")
  }

  attr(variaveis_cortadas, "tratado") <- TRUE   # avisa o main para refazer a verificação
  return(variaveis_cortadas)
}
