# ==============================================================================
#  FUNÇÕES - Consenso (ensemble) dos modelos
# ------------------------------------------------------------------------------
#  Junta os mapas dos algoritmos de cada espécie em um mapa só. Entram no
#  consenso só os modelos com TSS médio (validação principal) >= 'limiar_tss'.
#  Métodos:
#    media_ponderada -> média das adequabilidades, com peso = TSS médio
#    media_ponderada_kappa    -> idem, com peso = Kappa médio
#    media_ponderada_auc      -> idem, com peso = 2 x AUC médio - 1 (0 = acaso, como o TSS)
#    media_ponderada_acuracia -> idem, com peso = proporção de acertos média, com as
#                                presenças e o background pesando igual (acurácia
#                                balanceada = (TSS + 1) / 2); sem esse equilíbrio a
#                                proporção de acertos seria quase igual em todos os
#                                modelos, porque o background é muito maior que as presenças
#    media           -> média simples das adequabilidades
#    mediana         -> mediana das adequabilidades (menos sensível a um
#                       modelo muito diferente dos outros)
#    votacao         -> proporção de modelos que preveem presença (mapas
#                       binários no limiar 'limiar_votacao')
#  Mapas binários do consenso:
#    - média ponderada, média e mediana: os limiares são calculados para o
#      próprio consenso, combinando do mesmo jeito as previsões da validação
#      cruzada (max_tss, max_kappa, sens_espec) e as do treino (mtp, p10);
#    - votação: presença onde a MAIORIA dos modelos prevê presença (>= 0,5).
#  O JSDM fica fora do consenso.
#  Saídas em modelos/consenso/<espécie>/<conjunto>/:
#       consenso_<método>.tif               -> contínuo
#       consenso_<método>_bin_<limiar>.tif  -> presença / ausência
# ==============================================================================


# métodos de média ponderada e a medida de avaliação que dá o peso de cada modelo
metodos_ponderados <- c("media_ponderada", "media_ponderada_kappa", "media_ponderada_auc",
                        "media_ponderada_acuracia")

# peso de cada modelo ('av': tabela com modelo, tss_media, kappa_media, auc_media);
# negativos viram 0 e, se todos forem 0, os pesos ficam iguais
pesos_do_metodo <- function(metodo, av, modelos) {
  i <- match(modelos, av$modelo)
  p <- switch(metodo,
              media_ponderada          = av$tss_media[i],
              media_ponderada_kappa    = av$kappa_media[i],
              media_ponderada_auc      = 2 * av$auc_media[i] - 1,
              media_ponderada_acuracia = (av$tss_media[i] + 1) / 2,
              rep(1, length(modelos)))
  p <- pmax(p, 0)
  p[is.na(p)] <- 0
  if (sum(p) == 0) p <- rep(1, length(modelos))
  p
}

# combina previsões de vários modelos (colunas) do mesmo jeito que os mapas
combinar_previsoes <- function(tabela, modelos, metodo, pesos) {
  X <- as.matrix(tabela[, modelos, drop = FALSE])
  if (metodo %in% metodos_ponderados) metodo <- "media_ponderada"
  switch(metodo,
         media_ponderada = as.numeric(X %*% pesos / sum(pesos)),
         media           = rowMeans(X),
         mediana         = apply(X, 1, stats::median))
}


# ------------------------------------------------------------------------------
# gerar_consenso()
#   mapas     : índice de gerar_mapas()
#   avaliacao : resultado de avaliar_modelos() (resumo e previsões)
#   limiares  : resultado de calcular_limiares() (previsões de treino)
#   Se nenhum modelo de uma espécie passar do limiar de TSS, usa todos (aviso).
#   Salva consenso.csv (índice), limiares_consenso.csv, modelos_no_consenso.csv
#   e consenso.rds. Devolve list(indice, limiares, modelos).
# ------------------------------------------------------------------------------
gerar_consenso <- function(mapas, avaliacao, limiares, metodos, limiar_tss = 0.5,
                           metodos_limiar = c("max_tss"), limiar_fixo = 0.5,
                           limiar_votacao = "max_tss", pasta_saida, salvar_binarios = TRUE,
                           preditores = NULL) {

  metodos <- match.arg(metodos, c(metodos_ponderados, "media", "mediana", "votacao"),
                       several.ok = TRUE)
  sem_limiar <- pulado(metodos_limiar)
  metodos_limiar <- if (sem_limiar) character(0) else normalizar_metodos_limiar(metodos_limiar)
  if (sem_limiar) {
    salvar_binarios <- FALSE
    if ("votacao" %in% metodos) {
      message("Sem 'metodos_limiar' não há mapas binários: o consenso \"votacao\" foi retirado.")
      metodos <- setdiff(metodos, "votacao")
    }
    if (length(metodos) == 0) {
      message("Nenhum método de consenso restou: consenso pulado.")
      return(invisible(NULL))
    }
  }
  resumo <- avaliacao$resumo[avaliacao$resumo$validacao == avaliacao$principal, , drop = FALSE]
  continuos <- mapas[mapas$tipo == "continuo" & mapas$modelo != "jsdm", , drop = FALSE]
  indice <- list()
  tab_limiares <- list()
  usados <- list()

  cat("\n================ CONSENSO ================\n")
  cat("Entram os modelos com TSS médio >=", limiar_tss, "(validação:", avaliacao$principal, ")\n")

  for (especie in unique(continuos$especie)) {
    av <- resumo[resumo$especie == especie & resumo$modelo %in% continuos$modelo[continuos$especie == especie],
                 c("modelo", "tss_media", "kappa_media", "auc_media")]
    entram <- av$modelo[!is.na(av$tss_media) & av$tss_media >= limiar_tss]
    if (length(entram) == 0) {
      warning("'", especie, "': nenhum modelo com TSS >= ", limiar_tss,
              ". O consenso usa todos os modelos.", call. = FALSE, immediate. = TRUE)
      entram <- av$modelo
    }
    pesos <- pesos_do_metodo("media_ponderada", av, entram)
    tabela_uso <- data.frame(especie = especie, modelo = entram,
                             tss_media = arredondar_sdm(av$tss_media[match(entram, av$modelo)]),
                             kappa_media = arredondar_sdm(av$kappa_media[match(entram, av$modelo)]),
                             auc_media = arredondar_sdm(av$auc_media[match(entram, av$modelo)]),
                             peso = arredondar_sdm(pesos / sum(pesos)))
    for (m in intersect(metodos, metodos_ponderados)) {
      p <- pesos_do_metodo(m, av, entram)
      tabela_uso[[paste0("peso_", m)]] <- arredondar_sdm(p / sum(p))
    }
    usados[[length(usados) + 1]] <- tabela_uso

    # limiares do próprio consenso (média ponderada, média, mediana)
    oof    <- avaliacao$previsoes[[especie]]
    treino <- limiares$previsoes_treino[[especie]]
    lim_consenso <- list()
    for (metodo in if (sem_limiar) character(0) else
                   intersect(metodos, c(metodos_ponderados, "media", "mediana"))) {
      ok <- intersect(entram, intersect(names(oof), names(treino)))
      ok <- ok[vapply(ok, function(m) any(!is.na(oof[[m]])), logical(1))]
      w  <- pesos_do_metodo(metodo, av, ok)
      v  <- limiares_de_previsoes(
        oof      = if (length(ok)) combinar_previsoes(oof, ok, metodo, w) else NULL,
        treino   = if (length(ok)) combinar_previsoes(treino, ok, metodo, w) else rep(NA, nrow(treino)),
        presenca = oof$presenca, metodos = metodos_limiar, limiar_fixo = limiar_fixo)
      lim_consenso[[metodo]] <- v
      tab_limiares[[length(tab_limiares) + 1]] <- data.frame(especie = especie, metodo = metodo,
                                                             metodo_limiar = names(v),
                                                             limiar = as.numeric(v))
    }

    for (conjunto in unique(continuos$conjunto[continuos$especie == especie])) {
      sub <- continuos[continuos$especie == especie & continuos$conjunto == conjunto &
                         continuos$modelo %in% entram, , drop = FALSE]
      if (nrow(sub) == 0) next
      w   <- NULL   # pesos de cada método: calculados no laço dos métodos
      pasta <- file.path(pasta_saida, nome_seguro(especie), conjunto)
      dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
      pilha <- terra::rast(sub$arquivo)

      registrar <- function(metodo, tipo, metodo_limiar, limiar, arquivo) {
        indice[[length(indice) + 1]] <<- data.frame(especie = especie, conjunto = conjunto,
                                                    metodo = metodo, tipo = tipo,
                                                    metodo_limiar = metodo_limiar,
                                                    limiar = limiar, arquivo = arquivo)
      }

      for (metodo in metodos) {
        w <- pesos_do_metodo(metodo, av, sub$modelo)
        mapa <- switch(if (metodo %in% metodos_ponderados) "media_ponderada" else metodo,
          media_ponderada = {
            soma <- pilha[[1]] * w[1]
            for (i in seq_len(terra::nlyr(pilha))[-1]) soma <- soma + pilha[[i]] * w[i]
            soma / sum(w)
          },
          media   = terra::app(pilha, "mean"),
          mediana = terra::app(pilha, "median"),
          votacao = {
            bin <- mapas[mapas$especie == especie & mapas$conjunto == conjunto &
                           mapas$tipo == "binario" & mapas$metodo_limiar == limiar_votacao &
                           mapas$modelo %in% entram, , drop = FALSE]
            if (nrow(bin) == 0) {
              message("  Votação: não há mapas binários no limiar '", limiar_votacao,
                      "' para ", especie, " (inclua-o em 'metodos_limiar'). Pulada.")
              NULL
            } else {
              terra::app(terra::rast(bin$arquivo), "mean")
            }
          })
        if (is.null(mapa)) next

        names(mapa) <- paste0("consenso_", metodo)
        arquivo <- file.path(pasta, paste0("consenso_", metodo, ".tif"))
        terra::writeRaster(mapa, arquivo, overwrite = TRUE, datatype = "FLT4S",
                           gdal = c("COMPRESS=LZW"))
        registrar(metodo, "continuo", NA, NA, arquivo)

        if (salvar_binarios) {
          if (metodo == "votacao") {
            arquivo_bin <- file.path(pasta, "consenso_votacao_bin_maioria.tif")
            registrar(metodo, "binario", "maioria", 0.5, salvar_binario(mapa, 0.5, arquivo_bin))
          } else {
            v <- lim_consenso[[metodo]]
            for (m in names(v)[!is.na(v)]) {
              arquivo_bin <- file.path(pasta, paste0("consenso_", metodo, "_bin_", m, ".tif"))
              registrar(metodo, "binario", m, v[[m]], salvar_binario(mapa, v[[m]], arquivo_bin))
            }
          }
        }
      }
      cat(especie, "/", conjunto, ":", nrow(sub), "modelo(s):", paste(sub$modelo, collapse = ", "), "\n")
    }
  }

  resultado <- list(indice = do.call(rbind, indice), limiares = do.call(rbind, tab_limiares),
                    modelos = do.call(rbind, usados))
  utils::write.csv(acrescentar_preditores(resultado$indice, preditores),
                   file.path(pasta_saida, "consenso.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(resultado$limiares)) {
    utils::write.csv(acrescentar_preditores(resultado$limiares, preditores),
                     file.path(pasta_saida, "limiares_consenso.csv"),
                     row.names = FALSE, fileEncoding = "UTF-8")
  }
  utils::write.csv(acrescentar_preditores(resultado$modelos, preditores),
                   file.path(pasta_saida, "modelos_no_consenso.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  saveRDS(resultado, file.path(pasta_saida, "consenso.rds"))
  cat("Consenso salvo em:", pasta_saida, "\n")
  cat("==========================================\n")
  return(resultado)
}
