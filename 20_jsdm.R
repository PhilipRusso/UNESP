# ==============================================================================
#  FUNÇÕES - JSDM (modelo conjunto de distribuição de espécies)
# ------------------------------------------------------------------------------
#  O JSDM ajusta todas as espécies JUNTAS. Além do efeito do ambiente em cada
#  espécie, ele estima variáveis latentes: padrões comuns que o ambiente não
#  explica (por exemplo, espécies que aparecem juntas mais do que o esperado).
#  Daí sai a matriz de correlação residual entre as espécies.
#
#  Os dados precisam estar no formato sítios x espécies (presença/ausência nos
#  MESMOS pontos). Como os registros são só de presença, os sítios são as
#  células com presença de qualquer espécie ou com background, e a espécie sem
#  registro na célula conta como AUSÊNCIA. Essa ausência é suposta, então as
#  correlações entre espécies devem ser lidas com cuidado. Por isso o JSDM fica
#  fora do consenso.
#
#  Pacote: gllvm (modelos lineares generalizados com variáveis latentes), com
#  ligação probit e termos lineares e quadráticos para cada preditor.
#  Os mapas mostram a probabilidade explicada só pelo ambiente (variáveis
#  latentes fixadas em zero), que é o que dá para projetar em outros lugares e
#  nos cenários.
# ==============================================================================


# ------------------------------------------------------------------------------
# montar_sitios_jsdm()
#   Transforma os pontos (presença e background de todas as espécies) em:
#     $Y  -> matriz sítios x espécies (1 = registro da espécie na célula)
#     $X  -> valores dos preditores em cada sítio
#     $xy -> coordenadas dos sítios (centro da célula)
#   Cada célula do raster vira um sítio só.
# ------------------------------------------------------------------------------
montar_sitios_jsdm <- function(pontos, preditores) {

  referencia <- terra::rast(preditores$arquivos[["preditoras"]][1])
  celula     <- terra::cellFromXY(referencia, as.matrix(pontos[, c("x", "y")]))
  ok         <- !is.na(celula)

  especies <- sort(unique(pontos$especie))
  celulas  <- sort(unique(celula[ok]))
  Y <- matrix(0L, length(celulas), length(especies), dimnames = list(NULL, especies))
  pres <- ok & pontos$presenca == 1
  Y[cbind(match(celula[pres], celulas), match(pontos$especie[pres], especies))] <- 1L

  xy <- terra::xyFromCell(referencia, celulas)
  X  <- extrair_valores_pontos(data.frame(x = xy[, 1], y = xy[, 2]), preditores$arquivos[["preditoras"]])
  names(X) <- preditores$nomes

  completos <- stats::complete.cases(X)
  list(Y = Y[completos, , drop = FALSE], X = X[completos, , drop = FALSE],
       xy = xy[completos, , drop = FALSE])
}


# ------------------------------------------------------------------------------
# ajustar_jsdm() / prever_jsdm()
#   Ajuste com gllvm e previsão só com o ambiente:
#       probabilidade = pnorm(intercepto + soma(coeficiente x preditor))
#   prever_jsdm() devolve uma matriz células x espécies.
# ------------------------------------------------------------------------------
ajustar_jsdm <- function(Y, X, preditores, n_latentes = 2, semente = 123) {
  Xq <- as.data.frame(matriz_quadratica(X, preditores))
  d  <- max(1, min(n_latentes, ncol(Y) - 1))
  ajuste <- gllvm::gllvm(y = Y, X = Xq, family = stats::binomial(link = "probit"),
                         num.lv = d, seed = semente)
  list(gllvm = ajuste, preditores = preditores, especies = colnames(Y))
}

prever_jsdm <- function(modelo, novos) {
  Xq <- matriz_quadratica(as.data.frame(novos), modelo$preditores)
  b0 <- modelo$gllvm$params$beta0
  B  <- as.matrix(modelo$gllvm$params$Xcoef)
  if (!is.null(colnames(B)) && all(colnames(Xq) %in% colnames(B))) {
    B <- B[, colnames(Xq), drop = FALSE]
  }
  eta <- sweep(Xq %*% t(B), 2, b0, "+")
  p   <- stats::pnorm(eta)
  colnames(p) <- modelo$especies
  p
}


# ------------------------------------------------------------------------------
# rodar_jsdm()
#   1) monta os sítios; 2) validação cruzada (blocos ou k-fold, sobre os
#   sítios) com AUC, TSS, Kappa, Boyce e omissão por espécie; 3) ajusta o
#   modelo final com todos os sítios; 4) calcula os limiares; 5) salva a
#   correlação residual entre espécies; 6) faz os mapas contínuos e binários
#   de cada espécie em cada conjunto com todos os preditores.
#   O jackknife não se aplica ao JSDM (ele deixa sítios de fora, não uma
#   presença): se for a validação principal, o JSDM usa k-fold.
#   Saídas em pasta_saida (modelos/jsdm/), incluindo jsdm_resultado.rds.
# ------------------------------------------------------------------------------
rodar_jsdm <- function(pontos, preditores, ajustes, pasta_saida, tipo_validacao = "blocos",
                       n_folds = 5, salvar_binarios = TRUE, metodos_limiar = "max_tss",
                       limiar_fixo = 0.5, semente = 123) {

  if (!"especie" %in% names(pontos) || length(unique(pontos$especie)) < 2) {
    message("O JSDM precisa de pelo menos 2 espécies. Seção pulada.")
    return(invisible(NULL))
  }
  sem_limiar <- pulado(metodos_limiar)
  metodos_limiar <- if (sem_limiar) character(0) else normalizar_metodos_limiar(metodos_limiar)
  validacao <- normalizar_validacoes(tipo_validacao)[1]
  if (validacao %in% c("jackknife", "teste")) {
    message("O esquema '", validacao, "' não se aplica ao JSDM: usando k-fold com ", n_folds, " grupos.")
    validacao <- "kfold"
  }
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  sitios   <- montar_sitios_jsdm(pontos, preditores)
  Y        <- sitios$Y
  especies <- colnames(Y)

  cat("\n================ JSDM ================\n")
  cat("Sítios:", nrow(Y), "| Espécies:", ncol(Y), "| Validação:", validacao, "\n")
  cat("Presenças por espécie:", paste0(especies, " = ", colSums(Y), collapse = "; "), "\n")

  # ---- partição dos sítios ----
  set.seed(semente)
  algum <- as.integer(rowSums(Y) > 0)
  grupo <- if (validacao == "blocos") {
    particao_blocos(sitios$xy[, 1], sitios$xy[, 2], algum)
  } else {
    folds_estratificados(algum, n_folds)
  }

  # ---- validação cruzada ----
  previsoes <- matrix(NA_real_, nrow(Y), ncol(Y), dimnames = list(NULL, especies))
  linhas <- list()
  for (k in sort(unique(grupo))) {
    treino <- grupo != k
    com_registro <- colSums(Y[treino, , drop = FALSE]) > 0
    r <- tryCatch({
      m <- ajustar_jsdm(Y[treino, com_registro, drop = FALSE], sitios$X[treino, , drop = FALSE],
                        preditores$nomes, ajustes$jsdm_latentes, semente)
      list(teste = prever_jsdm(m, sitios$X[!treino, , drop = FALSE]),
           treino = prever_jsdm(m, sitios$X[treino, , drop = FALSE]))
    }, error = function(e) {
      message("  Grupo ", k, " falhou: ", conditionMessage(e))
      NULL
    })
    if (is.null(r)) next
    previsoes[!treino, colnames(r$teste)] <- r$teste
    for (sp in colnames(r$teste)) {
      m  <- avaliar_previsoes(r$teste[, sp], Y[!treino, sp])
      tr <- r$treino[Y[treino, sp] == 1, sp]
      p1 <- r$teste[Y[!treino, sp] == 1, sp]
      linhas[[length(linhas) + 1]] <- data.frame(
        especie = sp, grupo = k, auc = m$auc, tss = m$tss, kappa = m$kappa, boyce = m$boyce,
        omissao_mtp = if (length(p1)) mean(p1 < limiar_mtp(tr)) else NA,
        omissao_p10 = if (length(p1)) mean(p1 < limiar_p10(tr)) else NA)
    }
  }

  media  <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  grupos <- if (length(linhas) > 0) do.call(rbind, linhas) else NULL
  avaliacao <- do.call(rbind, lapply(especies, function(sp) {
    g <- if (is.null(grupos)) NULL else grupos[grupos$especie == sp, , drop = FALSE]
    data.frame(especie = sp, modelo = "jsdm", validacao = validacao,
               auc_media = media(g$auc), tss_media = media(g$tss), kappa_media = media(g$kappa),
               boyce_media = media(g$boyce), omissao_mtp = media(g$omissao_mtp),
               omissao_p10 = media(g$omissao_p10),
               grupos_ok = if (is.null(g)) 0 else sum(!is.na(g$auc)))
  }))
  utils::write.csv(acrescentar_preditores(avaliacao, preditores),
                   file.path(pasta_saida, "avaliacao_jsdm.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  cat("\nAvaliação (validação cruzada):\n")
  mostrar <- avaliacao[, c("especie", "auc_media", "tss_media", "kappa_media", "boyce_media",
                           "omissao_mtp", "omissao_p10")]
  mostrar[, -1] <- arredondar_sdm(mostrar[, -1])
  names(mostrar) <- sub("_media", "", names(mostrar))
  print(mostrar, row.names = FALSE)

  # ---- modelo final, limiares e correlação residual ----
  final <- tryCatch(ajustar_jsdm(Y, sitios$X, preditores$nomes, ajustes$jsdm_latentes, semente),
                    error = function(e) {
                      message("O ajuste final do JSDM falhou: ", conditionMessage(e))
                      NULL
                    })
  if (is.null(final)) return(invisible(list(avaliacao = avaliacao)))
  saveRDS(final, file.path(pasta_saida, "jsdm_modelo.rds"))

  p_final  <- prever_jsdm(final, sitios$X)
  limiares <- NULL   # sem 'metodos_limiar' (NA): só o mapa contínuo
  if (!sem_limiar) {
    limiares <- do.call(rbind, lapply(especies, function(sp) {
      v <- limiares_de_previsoes(previsoes[, sp], p_final[, sp], Y[, sp], metodos_limiar, limiar_fixo)
      data.frame(especie = sp, modelo = "jsdm", metodo = names(v), limiar = as.numeric(v))
    }))
    utils::write.csv(acrescentar_preditores(limiares, preditores),
                     file.path(pasta_saida, "limiares_jsdm.csv"),
                     row.names = FALSE, fileEncoding = "UTF-8")
  }

  correlacao <- tryCatch(gllvm::getResidualCor(final$gllvm), error = function(e) NULL)
  if (!is.null(correlacao)) {
    dimnames(correlacao) <- list(especies, especies)
    utils::write.csv(arredondar_sdm(correlacao), file.path(pasta_saida, "correlacao_residual.csv"),
                     fileEncoding = "UTF-8")
    cat("\nCorrelação residual entre as espécies (o que o ambiente não explica):\n")
    print(arredondar_sdm(correlacao))
  }

  # ---- mapas ----
  prever_celulas <- function(model, data, ...) {
    if (nrow(data) == 0) return(matrix(numeric(0), 0, length(model$especies)))
    prever_jsdm(model, data)
  }
  indice <- list()
  for (conjunto in preditores$conjuntos) {
    pilha <- terra::rast(preditores$arquivos[[conjunto]])
    names(pilha) <- preditores$nomes
    mapas <- tryCatch(terra::predict(pilha, final, fun = prever_celulas, na.rm = TRUE),
                      error = function(e) {
                        message("Mapas do JSDM em '", conjunto, "' falharam: ", conditionMessage(e))
                        NULL
                      })
    if (is.null(mapas)) next
    names(mapas) <- especies

    for (sp in especies) {
      pasta <- file.path(pasta_saida, "mapas", nome_seguro(sp))
      dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
      arquivo <- file.path(pasta, paste0(conjunto, ".tif"))
      terra::writeRaster(mapas[[sp]], arquivo, overwrite = TRUE, datatype = "FLT4S",
                         gdal = c("COMPRESS=LZW"))
      indice[[length(indice) + 1]] <- data.frame(especie = sp, conjunto = conjunto, tipo = "continuo",
                                                 metodo_limiar = NA, limiar = NA, arquivo = arquivo)
      if (salvar_binarios && !is.null(limiares)) {
        lim <- limiares[limiares$especie == sp & !is.na(limiares$limiar), , drop = FALSE]
        for (i in seq_len(nrow(lim))) {
          arquivo_bin <- file.path(pasta, paste0(conjunto, "_bin_", lim$metodo[i], ".tif"))
          salvar_binario(mapas[[sp]], lim$limiar[i], arquivo_bin)
          indice[[length(indice) + 1]] <- data.frame(especie = sp, conjunto = conjunto, tipo = "binario",
                                                     metodo_limiar = lim$metodo[i],
                                                     limiar = lim$limiar[i], arquivo = arquivo_bin)
        }
      }
    }
    cat("Mapas do JSDM:", conjunto, "\n")
  }

  resultado <- list(avaliacao = avaliacao, limiares = limiares, modelo = final,
                    correlacao = correlacao, mapas = do.call(rbind, indice))
  saveRDS(resultado, file.path(pasta_saida, "jsdm_resultado.rds"))
  cat("Saídas em:", pasta_saida, "\n")
  cat("======================================\n")
  invisible(resultado)
}
