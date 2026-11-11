# ==============================================================================
#  FUNÇÕES - Modelos finais, limiares e mapas
# ------------------------------------------------------------------------------
#  Depois da avaliação, cada algoritmo é ajustado de novo com TODOS os pontos
#  da espécie (o modelo final) e projetado nos rasters dos preditores de cada
#  conjunto (preditoras e cenários que têm todos os preditores).
#
#  LIMIARES: transformam a adequabilidade (0 a 1) em presença (1) / ausência (0).
#    max_tss    -> maior TSS (equilibra presenças acertadas e background excluído)
#    max_kappa  -> maior Kappa
#    sens_espec -> sensibilidade igual à especificidade
#    min_roc    -> menor distância ao canto (0, 1) da curva ROC
#    max_acuracia -> maior proporção de acertos (presenças e background)
#    media_pres -> média da adequabilidade nas presenças (mapa bem restrito)
#    mtp        -> menor adequabilidade entre as presenças: nenhuma presença
#                  conhecida fica fora (mapa mais amplo)
#    p10        -> deixa 10% das presenças de fora (tolera registros duvidosos;
#                  mapa mais restrito que o MTP)
#    fixo       -> um valor escolhido (ex.: 0,5)
#  Todos (menos o fixo) vêm das previsões da validação cruzada principal: cada
#  ponto previsto por um modelo que não o viu (ver limiares_de_previsoes()).
#
#  Saídas em modelos/mapas/<espécie>/<conjunto>/:
#       <modelo>.tif               -> adequabilidade contínua (0 a 1)
#       <modelo>_bin_<limiar>.tif  -> presença (1) / ausência (0) em cada limiar
# ==============================================================================


# ------------------------------------------------------------------------------
# ajustar_modelos_finais()
#   Ajusta cada algoritmo com todos os pontos de cada espécie e salva todos os
#   modelos em pasta_saida/modelos_finais.rds (para usar depois sem reajustar).
#   Se 'ajuste_maxent' (seção 16) for informado, o Maxent de cada espécie usa
#   as classes e o regmult escolhidos para ela.
#   Com 'somente_treino' = TRUE e a validação "teste" (hold-out) nos dados, os
#   modelos finais usam só as linhas de treino (grupo_teste = 0); com FALSE
#   (padrão) usam todos os pontos (reajuste após a avaliação).
#   Devolve uma lista: modelos_finais[[espécie]][[modelo]].
# ------------------------------------------------------------------------------
ajustar_modelos_finais <- function(dados_modelos, modelos, ajustes, pasta_saida, semente = 123,
                                   ajuste_maxent = NULL, somente_treino = FALSE) {

  modelos <- setdiff(modelos, "jsdm")
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  resultado <- list()

  cat("\n================ MODELOS FINAIS ================\n")
  for (especie in names(dados_modelos)) {
    dados      <- dados_modelos[[especie]]$dados
    preditores <- dados_modelos[[especie]]$preditores
    ajustes_sp <- ajustes_da_especie(ajustes, ajuste_maxent, especie)
    if (isTRUE(somente_treino) && "grupo_teste" %in% names(dados)) {
      dados <- dados[dados$grupo_teste == 0, , drop = FALSE]
    }
    resultado[[especie]] <- list()
    ok <- character(0)

    for (codigo in modelos) {
      objeto <- tryCatch(ajustar_modelo(codigo, dados, preditores, ajustes_sp, semente),
                         error = function(e) {
                           message("  [", especie, " / ", codigo, "] falhou: ", conditionMessage(e))
                           NULL
                         })
      if (!is.null(objeto)) {
        resultado[[especie]][[codigo]] <- objeto
        ok <- c(ok, codigo)
      }
    }
    cat(especie, ": ", length(ok), " modelo(s) ajustado(s): ", paste(ok, collapse = ", "), "\n", sep = "")
  }
  saveRDS(resultado, file.path(pasta_saida, "modelos_finais.rds"))
  cat("Modelos salvos em:", pasta_saida, "\n")
  cat("================================================\n")

  return(resultado)
}


# ------------------------------------------------------------------------------
# calcular_limiares()
#   Calcula os limiares pedidos para cada espécie e modelo final.
#   Salva limiares.csv (tabela) e limiares.rds (tabela + previsões do modelo
#   final nos pontos de treino, usadas também no consenso).
#   Devolve list(tabela, previsoes_treino, metodos, limiar_fixo).
# ------------------------------------------------------------------------------
calcular_limiares <- function(modelos_finais, dados_modelos, avaliacao, metodos,
                              limiar_fixo = 0.5, pasta_saida, preditores = NULL) {

  metodos <- normalizar_metodos_limiar(metodos)
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  tabela <- list()
  treino <- list()

  for (especie in names(modelos_finais)) {
    dados <- dados_modelos[[especie]]$dados
    treino[[especie]] <- data.frame(presenca = dados$presenca)

    for (codigo in names(modelos_finais[[especie]])) {
      p_treino <- tryCatch(prever_modelo(modelos_finais[[especie]][[codigo]], dados),
                           error = function(e) rep(NA_real_, nrow(dados)))
      treino[[especie]][[codigo]] <- p_treino
      oof <- avaliacao$previsoes[[especie]][[codigo]]
      valores <- limiares_de_previsoes(oof, p_treino, dados$presenca, metodos, limiar_fixo)
      tabela[[length(tabela) + 1]] <- data.frame(especie = especie, modelo = codigo,
                                                 metodo = names(valores), limiar = as.numeric(valores))
    }
  }

  tabela <- do.call(rbind, tabela)
  resultado <- list(tabela = tabela, previsoes_treino = treino, metodos = metodos,
                    limiar_fixo = limiar_fixo)
  utils::write.csv(acrescentar_preditores(tabela, preditores),
                   file.path(pasta_saida, "limiares.csv"), row.names = FALSE,
                   fileEncoding = "UTF-8")
  saveRDS(resultado, file.path(pasta_saida, "limiares.rds"))

  cat("\n================ LIMIARES ================\n")
  largura <- stats::reshape(tabela, idvar = c("especie", "modelo"), timevar = "metodo",
                            direction = "wide")
  names(largura) <- sub("^limiar\\.", "", names(largura))
  largura[, -(1:2)] <- arredondar_sdm(largura[, -(1:2)])
  print(largura, row.names = FALSE)
  cat("Limiares salvos em:", pasta_saida, "\n")
  cat("==========================================\n")

  return(resultado)
}


# ------------------------------------------------------------------------------
# prever_mapa()
#   Aplica um modelo a todas as células de uma pilha de preditores e salva o
#   raster. Células com NA em algum preditor continuam NA.
# ------------------------------------------------------------------------------
prever_mapa <- function(objeto, pilha, arquivo) {
  prever_celulas <- function(model, data, ...) {
    if (nrow(data) == 0) return(numeric(0))
    prever_modelo(model, data)
  }
  mapa <- terra::predict(pilha, objeto, fun = prever_celulas, na.rm = TRUE)
  names(mapa) <- objeto$codigo
  terra::writeRaster(mapa, arquivo, overwrite = TRUE, datatype = "FLT4S",
                     gdal = c("COMPRESS=LZW"))
  return(mapa)
}

# salva o mapa binário (1 = adequabilidade >= limiar)
salvar_binario <- function(mapa, limiar, arquivo) {
  terra::writeRaster(mapa >= limiar, arquivo, overwrite = TRUE, datatype = "INT1U",
                     gdal = c("COMPRESS=LZW"))
  arquivo
}


# ------------------------------------------------------------------------------
# gerar_mapas()
#   Projeta todos os modelos finais em cada conjunto com todos os preditores e,
#   se 'salvar_binarios' = TRUE, salva um mapa binário para cada limiar da
#   tabela 'limiares'. Salva o índice dos arquivos em pasta_saida/mapas.csv
#   (uma linha por arquivo) e devolve essa tabela.
# ------------------------------------------------------------------------------
gerar_mapas <- function(modelos_finais, preditores, limiares, pasta_saida,
                        salvar_binarios = TRUE) {

  indice <- list()
  cat("\n================ MAPAS ================\n")

  for (conjunto in preditores$conjuntos) {
    pilha <- terra::rast(preditores$arquivos[[conjunto]])
    names(pilha) <- preditores$nomes

    for (especie in names(modelos_finais)) {
      pasta <- file.path(pasta_saida, nome_seguro(especie), conjunto)
      dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
      cat(conjunto, "/", especie, ": ")

      for (codigo in names(modelos_finais[[especie]])) {
        arquivo <- file.path(pasta, paste0(codigo, ".tif"))
        mapa <- tryCatch(prever_mapa(modelos_finais[[especie]][[codigo]], pilha, arquivo),
                         error = function(e) {
                           message("\n  [", codigo, "] falhou: ", conditionMessage(e))
                           NULL
                         })
        if (is.null(mapa)) next
        indice[[length(indice) + 1]] <- data.frame(especie = especie, conjunto = conjunto,
                                                   modelo = codigo, tipo = "continuo",
                                                   metodo_limiar = NA, limiar = NA,
                                                   arquivo = arquivo)
        if (salvar_binarios && !is.null(limiares)) {
          lim <- limiares[limiares$especie == especie & limiares$modelo == codigo &
                            !is.na(limiares$limiar), , drop = FALSE]
          if (conjunto == preditores$conjuntos[1] && any(lim$limiar <= 0)) {
            message("\n  Aviso: limiar = 0 em ", codigo, " (", paste(lim$metodo[lim$limiar <= 0], collapse = ", "),
                    "): algumas presenças tiveram adequabilidade 0 na validação, então o mapa binário ",
                    "nesse limiar marca toda a área.")
          }
          for (i in seq_len(nrow(lim))) {
            arquivo_bin <- file.path(pasta, paste0(codigo, "_bin_", lim$metodo[i], ".tif"))
            salvar_binario(mapa, lim$limiar[i], arquivo_bin)
            indice[[length(indice) + 1]] <- data.frame(especie = especie, conjunto = conjunto,
                                                       modelo = codigo, tipo = "binario",
                                                       metodo_limiar = lim$metodo[i],
                                                       limiar = lim$limiar[i], arquivo = arquivo_bin)
          }
        }
        cat(codigo, "")
      }
      cat("\n")
    }
  }

  indice <- do.call(rbind, indice)
  utils::write.csv(acrescentar_preditores(indice, preditores),
                   file.path(pasta_saida, "mapas.csv"), row.names = FALSE,
                   fileEncoding = "UTF-8")
  cat("Mapas salvos em:", pasta_saida, "\n")
  cat("=======================================\n")
  return(indice)
}
