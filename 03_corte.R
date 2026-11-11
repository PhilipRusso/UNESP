# ==============================================================================
#  FUNÇÕES - Ajuste de projeção e corte das variáveis
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
#  Regra do corte: tudo o que estiver FORA da máscara vira NA (valor nulo).
#  Não usamos zero, porque o zero seria tratado como um valor real do ambiente
#  nas análises seguintes.
# ==============================================================================


# ------------------------------------------------------------------------------
# igualar_crs()
#   Se a projeção (CRS) da máscara for diferente da 'referencia' (um raster),
#   avisa e reprojeta a máscara para a projeção da referência.
#   Os rasters NÃO são alterados.
# ------------------------------------------------------------------------------
igualar_crs <- function(mascara, referencia) {

  if (terra::same.crs(mascara, referencia)) return(mascara)

  warning("A projeção da máscara é diferente da projeção dos rasters. ",
          "A máscara será reprojetada.", call. = FALSE, immediate. = TRUE)

  if (inherits(mascara, "SpatVector")) {
    return(terra::project(mascara, terra::crs(referencia)))
  } else {
    return(terra::project(mascara, terra::crs(referencia), method = "near"))
  }
}


# ------------------------------------------------------------------------------
# ajustar_crs_mascara()
#   Compara a projeção da máscara com a do primeiro .tif da pasta de variáveis
#   preditoras e reprojeta a máscara se for preciso.
#   Devolve a máscara (já na projeção dos rasters).
# ------------------------------------------------------------------------------
ajustar_crs_mascara <- function(mascara, pasta_var_preditoras) {

  arquivos   <- list.files(pasta_var_preditoras, pattern = "\\.tif$",
                           ignore.case = TRUE, full.names = TRUE)
  referencia <- terra::rast(arquivos[1])

  return(igualar_crs(mascara, referencia))
}


# ------------------------------------------------------------------------------
# salvar_mascara()
#   Salva a máscara usada no corte na pasta 'pasta_mask'.
#     - shapefile -> mascara_corte.shp
#     - raster    -> mascara_corte.tif
#   Devolve o caminho do arquivo salvo.
# ------------------------------------------------------------------------------
salvar_mascara <- function(mascara, pasta_mask) {

  if (inherits(mascara, "SpatVector")) {
    destino <- file.path(pasta_mask, "mascara_corte.shp")
    terra::writeVector(mascara, destino, overwrite = TRUE)
  } else {
    destino <- file.path(pasta_mask, "mascara_corte.tif")
    terra::writeRaster(mascara, destino, overwrite = TRUE)
  }

  message("Máscara salva em: ", destino)
  return(destino)
}


# ------------------------------------------------------------------------------
# cortar_raster()
#   Corta UM raster (.tif) pela máscara e salva o resultado em 'pasta_saida',
#   com o mesmo nome do arquivo original.
#     1) crop : reduz o raster à extensão da máscara
#     2) mask : o que está fora da máscara vira NA
#   Devolve o caminho do arquivo salvo.
# ------------------------------------------------------------------------------
cortar_raster <- function(caminho_tif, mascara, pasta_saida) {

  raster  <- terra::rast(caminho_tif)
  mascara <- igualar_crs(mascara, raster)   # garante a mesma projeção

  cortado <- terra::crop(raster, mascara, snap = "out")

  if (inherits(mascara, "SpatRaster")) {
    # máscara raster: precisa estar na mesma grade do raster cortado
    if (!terra::compareGeom(cortado, mascara, stopOnError = FALSE)) {
      mascara <- terra::resample(mascara, cortado, method = "near")
    }
  }
  cortado <- terra::mask(cortado, mascara)

  destino <- file.path(pasta_saida, basename(caminho_tif))
  terra::writeRaster(cortado, destino, overwrite = TRUE,
                     gdal = c("COMPRESS=LZW"))

  return(destino)
}


# ------------------------------------------------------------------------------
# cortar_pasta()
#   Corta TODOS os .tif de uma pasta e salva em 'pasta_saida' (criada se não
#   existir). Mostra o andamento no console.
#   Devolve os caminhos dos arquivos salvos.
# ------------------------------------------------------------------------------
cortar_pasta <- function(pasta_entrada, mascara, pasta_saida) {

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  arquivos <- list.files(pasta_entrada, pattern = "\\.tif$",
                         ignore.case = TRUE, full.names = TRUE)

  salvos <- character(0)
  for (i in seq_along(arquivos)) {
    message("   [", i, "/", length(arquivos), "] ", basename(arquivos[i]))
    salvos <- c(salvos, cortar_raster(arquivos[i], mascara, pasta_saida))
  }

  return(salvos)
}


# ------------------------------------------------------------------------------
# cortar_todas_variaveis()
#   Corta as variáveis PREDITORAS e as de TODOS os cenários, cada um em uma
#   subpasta própria de 'pasta_saida':
#       variaveis_cortadas/preditoras/
#       variaveis_cortadas/<nome do cenário>/
#   Devolve uma lista com os caminhos salvos de cada conjunto.
# ------------------------------------------------------------------------------
cortar_todas_variaveis <- function(entradas, mascara, pasta_saida) {

  resultado <- list()

  message("Cortando variáveis PREDITORAS...")
  resultado[["preditoras"]] <- cortar_pasta(entradas$var_preditoras, mascara,
                                        file.path(pasta_saida, "preditoras"))

  for (nome in names(entradas$cenarios)) {
    message("Cortando cenário: ", nome, "...")
    resultado[[nome]] <- cortar_pasta(entradas$cenarios[[nome]], mascara,
                                      file.path(pasta_saida, nome))
  }

  message("Corte concluído. Arquivos salvos em: ", pasta_saida)
  return(resultado)
}
