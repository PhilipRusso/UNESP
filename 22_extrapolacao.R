# ==============================================================================
#  FUNÇÕES - Extrapolação: MESS (Multivariate Environmental Similarity Surface)
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
#
#  Um modelo só "conhece" os ambientes dos pontos em que foi ajustado
#  (presenças + background). Onde o mapa usa valores dos preditores FORA da
#  faixa desses pontos, o modelo está EXTRAPOLANDO: o resultado depende de como
#  cada algoritmo trata o desconhecido (uns repetem o valor da borda, outros
#  continuam a curva) e não dos dados.
#
#  MESS (Elith et al. 2010): para cada célula e cada preditor, compara o valor
#  da célula (p) com os valores de referência:
#     f = % dos pontos de referência com valor MENOR que p
#     f = 0          -> p abaixo do mínimo: (p - mínimo) / (máximo - mínimo) x 100
#     0 < f <= 50    -> 2 x f
#     50 < f < 100   -> 2 x (100 - f)
#     f = 100        -> p acima do máximo: (máximo - p) / (máximo - mínimo) x 100
#  O MESS da célula é o MENOR valor entre os preditores:
#     negativo -> pelo menos um preditor fora da faixa de referência (extrapolação);
#                 quanto mais negativo, mais longe da faixa
#     0 a 100  -> todos dentro da faixa (100 = valor típico, perto da mediana)
#  A "variável mais diferente" (MoD) é o preditor que deu esse menor valor:
#  mostra qual variável causa a extrapolação em cada lugar.
#
#  Referência: os pontos usados no ajuste de CADA espécie (presenças +
#  background). Por isso o MESS é calculado por espécie, nas preditoras e em cada
#  cenário. Mesmo nas preditoras pode haver extrapolação, longe do buffer onde o
#  background foi sorteado.
# ==============================================================================


# ------------------------------------------------------------------------------
# similaridade_mess()
#   Similaridade de cada valor de 'p' (vetor) com os valores de 'referencia'
#   de UM preditor, pelas regras do MESS (ver o cabeçalho). Devolve um vetor.
# ------------------------------------------------------------------------------
similaridade_mess <- function(p, referencia) {
  r <- sort(referencia[!is.na(referencia)])
  n <- length(r)
  minimo <- r[1]
  maximo <- r[n]
  amplitude <- maximo - minimo
  if (amplitude == 0) amplitude <- 1   # preditor constante na referência

  f <- 100 * findInterval(p, r, left.open = TRUE) / n   # % de pontos com valor < p

  ifelse(f == 0,   (p - minimo) / amplitude * 100,
  ifelse(f <= 50,  2 * f,
  ifelse(f < 100,  2 * (100 - f),
                   (maximo - p) / amplitude * 100)))
}


# ------------------------------------------------------------------------------
# prever_mess()
#   Função usada por terra::predict(): recebe os valores das células ('data',
#   uma coluna por preditor) e devolve, para cada célula, o MESS e o número do
#   preditor mais diferente (MoD).
# ------------------------------------------------------------------------------
prever_mess <- function(model, data, ...) {
  if (nrow(data) == 0) return(data.frame(mess = numeric(0), mod = integer(0)))

  similaridade <- vapply(names(model), function(v) similaridade_mess(data[[v]], model[[v]]),
                         numeric(nrow(data)))
  similaridade <- matrix(similaridade, nrow = nrow(data))

  data.frame(mess = apply(similaridade, 1, min),
             mod  = max.col(-similaridade, ties.method = "first"))
}


# ------------------------------------------------------------------------------
# calcular_extrapolacao()
#   Para cada espécie (referência = presenças + background dela) e cada conjunto
#   com todos os preditores (preditoras e cenários), calcula o MESS e salva em
#   pasta_saida/<espécie>/<conjunto>/:
#     mess.tif                     -> MESS contínuo (negativo = extrapolação)
#     extrapolacao.tif             -> 1 = extrapolação (MESS < 0); 0 = dentro da faixa
#     variavel_mais_diferente.tif  -> preditor que deu o menor valor (MoD)
#   Salva também resumo_extrapolacao.csv: % da área de estudo em extrapolação e
#   o preditor que mais causa a extrapolação, por espécie e conjunto.
#   Devolve o resumo.
# ------------------------------------------------------------------------------
calcular_extrapolacao <- function(dados_modelos, preditores, pasta_saida) {

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  legenda <- data.frame(valor = seq_along(preditores$nomes), preditor = preditores$nomes)
  resumo  <- list()

  cat("\n================ EXTRAPOLAÇÃO (MESS) ================\n")
  cat("Referência: presenças + background de cada espécie\n")

  for (conjunto in preditores$conjuntos) {
    pilha <- terra::rast(preditores$arquivos[[conjunto]])
    names(pilha) <- preditores$nomes

    for (especie in names(dados_modelos)) {
      referencia <- dados_modelos[[especie]]$dados[, preditores$nomes, drop = FALSE]
      pasta <- file.path(pasta_saida, nome_seguro(especie), conjunto)
      dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

      resultado <- terra::predict(pilha, as.list(referencia), fun = prever_mess, na.rm = TRUE)
      mess  <- resultado[[1]]
      mod   <- resultado[[2]]
      names(mess) <- "mess"
      levels(mod) <- data.frame(valor = legenda$valor, preditor = legenda$preditor)

      terra::writeRaster(mess, file.path(pasta, "mess.tif"), overwrite = TRUE,
                         datatype = "FLT4S", gdal = c("COMPRESS=LZW"))
      terra::writeRaster(mess < 0, file.path(pasta, "extrapolacao.tif"), overwrite = TRUE,
                         datatype = "INT1U", gdal = c("COMPRESS=LZW"))
      terra::writeRaster(mod, file.path(pasta, "variavel_mais_diferente.tif"), overwrite = TRUE,
                         datatype = "INT1U", gdal = c("COMPRESS=LZW"))

      # resumo: % da área em extrapolação e o preditor que mais a causa
      valores <- terra::values(c(mess, resultado[[2]]), mat = TRUE)
      valores <- valores[!is.na(valores[, 1]), , drop = FALSE]
      fora    <- valores[, 1] < 0
      causa   <- if (any(fora)) {
        contagem <- table(factor(valores[fora, 2], levels = legenda$valor))
        legenda$preditor[which.max(contagem)]
      } else {
        NA
      }
      resumo[[length(resumo) + 1]] <- data.frame(
        especie = especie, conjunto = conjunto,
        area_extrapolacao_pct = arredondar_sdm(100 * mean(fora)),
        preditor_principal = causa,
        mess_minimo = arredondar_sdm(min(valores[, 1])))
    }
  }

  resumo <- do.call(rbind, resumo)
  utils::write.csv(acrescentar_preditores(resumo, preditores),
                   file.path(pasta_saida, "resumo_extrapolacao.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(legenda, file.path(pasta_saida, "legenda_variavel_mais_diferente.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")

  print(resumo, row.names = FALSE)
  cat("(area_extrapolacao_pct: % da área de estudo com MESS < 0;\n",
      " preditor_principal: o que mais aparece como variável mais diferente nessa área)\n")
  cat("Resultados salvos em:", pasta_saida, "\n")
  cat("=====================================================\n")

  return(resumo)
}
