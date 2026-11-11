# ==============================================================================
#  FUNÇÕES - Importância das variáveis
# ------------------------------------------------------------------------------
#  Mostra quais preditores mais pesam nos modelos, por três caminhos que se
#  complementam ('metodos_importancia' no main):
#
#    "permutacao" -> embaralha UM preditor por vez e vê quanto o AUC do modelo
#                    final cai. Serve para QUALQUER algoritmo. Quanto maior a
#                    queda, mais o modelo depende do preditor.
#    "jackknife"  -> como no Maxent (.jar): para cada preditor, o modelo é
#                    refeito (a) SEM ele e (b) SÓ com ele, e avaliado pela validação
#                    cruzada principal. Dá duas medidas:
#                      jackknife_sem: queda do AUC ao retirar o preditor
#                                     (informação que só ele tem);
#                      jackknife_so : AUC usando só o preditor
#                                     (quanto ele explica sozinho).
#                    É lento (refaz o modelo 2 x nº de preditores vezes).
#    "regressao"  -> um GLM com os preditores padronizados (z-score): para cada
#                    preditor, a queda de deviance ao retirar seus termos (teste da
#                    razão de verossimilhança), o coeficiente padronizado e o sinal
#                    do efeito. Não depende dos algoritmos escolhidos. Os p-valores
#                    são só indicativos: o background não é uma amostra independente.
#
#  Com eixos da PCA como preditores, a importância dos métodos acima é dos EIXOS.
#  Para saber quais VARIÁVEIS ORIGINAIS pesam mais, mesmo com o modelo feito com
#  eixos ('importancia_variaveis_originais' = TRUE), são calculados também:
#
#    "permutacao_variaveis" -> embaralha cada variável original (valor bruto no
#                    ponto), recalcula os eixos da PCA com as cargas da PCA e
#                    prevê com o modelo final; queda do AUC = importância da variável.
#    "carga_variaveis"      -> reparte a importância de cada eixo (queda do AUC na
#                    permutação dos eixos) entre as variáveis, pelo quadrado das
#                    cargas (soma das cargas ao quadrado de cada eixo = 1). Só quando
#                    "permutacao" está em 'metodos_importancia'.
#    "regressao_variaveis"  -> GLM com as variáveis originais padronizadas (só quando
#                    "regressao" está em 'metodos_importancia').
#
#  Saídas em modelos/importancia/: importancia.csv (tabela única), mais um CSV de
#  detalhes por método. Todos com as colunas dos preditores usados.
# ==============================================================================


# ------------------------------------------------------------------------------
# normalizar_metodos_importancia()
# ------------------------------------------------------------------------------
normalizar_metodos_importancia <- function(metodos) {
  metodos   <- unique(tolower(trimws(metodos)))
  validos   <- c("permutacao", "jackknife", "regressao")
  invalidos <- setdiff(metodos, validos)
  if (length(invalidos) > 0) {
    stop("Método(s) de importância inválido(s): ", paste(invalidos, collapse = ", "),
         "\nOpções: ", paste(validos, collapse = ", "), call. = FALSE)
  }
  metodos
}


# ------------------------------------------------------------------------------
# importancia_permutacao()
#   Queda média do AUC (nas 'n_repeticoes' permutações) ao embaralhar cada
#   preditor, com o modelo final e os pontos da espécie.
# ------------------------------------------------------------------------------
importancia_permutacao <- function(objeto, dados, preditores, n_repeticoes = 10, semente = 123) {

  p_base   <- prever_modelo(objeto, dados)
  auc_base <- calcular_auc(p_base[dados$presenca == 1], p_base[dados$presenca == 0])
  set.seed(semente)

  linhas <- lapply(preditores, function(v) {
    quedas <- vapply(seq_len(n_repeticoes), function(i) {
      d <- dados
      d[[v]] <- sample(d[[v]])
      p <- prever_modelo(objeto, d)
      auc_base - calcular_auc(p[d$presenca == 1], p[d$presenca == 0])
    }, numeric(1))
    data.frame(variavel = v, auc_base = auc_base, queda_auc = mean(quedas, na.rm = TRUE),
               queda_auc_dp = stats::sd(quedas, na.rm = TRUE), repeticoes = n_repeticoes)
  })
  do.call(rbind, linhas)
}


# ------------------------------------------------------------------------------
# esquema_importancia()
#   Escolhe a validação cruzada do jackknife de variáveis: a primeira de
#   'validacoes' que não seja o jackknife de presenças. Se só houver essa, ou se
#   a coluna de grupos não existir, cria grupos k-fold.
#   Devolve list(dados, validacao).
# ------------------------------------------------------------------------------
esquema_importancia <- function(dados, validacoes, n_folds = 5, semente = 123) {
  v <- setdiff(normalizar_validacoes(validacoes), c("jackknife", "teste"))
  v <- if (length(v) == 0) "kfold" else v[1]
  coluna <- paste0("grupo_", v)
  if (!coluna %in% names(dados)) {
    set.seed(semente)
    dados[[coluna]] <- folds_estratificados(dados$presenca, n_folds)
  }
  list(dados = dados, validacao = v)
}


# ------------------------------------------------------------------------------
# importancia_jackknife()
#   Para cada preditor: AUC e TSS da validação cruzada refazendo o modelo sem o
#   preditor e só com ele, comparados com o modelo completo.
# ------------------------------------------------------------------------------
importancia_jackknife <- function(codigo, dados, preditores, ajustes, validacao, semente = 123) {

  geral <- function(vars) {
    r <- tryCatch(validar_modelo(codigo, dados, vars, ajustes, validacao, semente)$geral,
                  error = function(e) NULL)
    if (is.null(r)) c(auc = NA_real_, tss = NA_real_) else c(auc = r$auc, tss = r$tss)
  }

  completo <- geral(preditores)
  linhas <- lapply(preditores, function(v) {
    sem <- if (length(preditores) > 1) geral(setdiff(preditores, v)) else c(auc = NA_real_, tss = NA_real_)
    so  <- geral(v)
    data.frame(variavel = v, validacao = validacao,
               auc_completo = completo[["auc"]], tss_completo = completo[["tss"]],
               auc_sem = sem[["auc"]], tss_sem = sem[["tss"]],
               auc_so = so[["auc"]], tss_so = so[["tss"]],
               queda_auc_sem = completo[["auc"]] - sem[["auc"]],
               queda_tss_sem = completo[["tss"]] - sem[["tss"]])
  })
  do.call(rbind, linhas)
}


# ------------------------------------------------------------------------------
# PCA -> importância das VARIÁVEIS ORIGINAIS
# ------------------------------------------------------------------------------

# lê o modelo da PCA usada nos preditores e os rasters das variáveis preditoras.
# Devolve list(pca_modelo, arquivos) ou NULL (com aviso) se não for possível.
carregar_pca_importancia <- function(preditores, pasta_pca, arquivos_variaveis) {
  if (is.null(preditores) || !identical(preditores$tipo, "pca")) return(NULL)
  arquivo <- file.path(pasta_pca, preditores$pca, "pca_modelo.rds")
  if (!file.exists(arquivo)) {
    message("Importância das variáveis originais pulada: não encontrei ", arquivo)
    return(NULL)
  }
  pca_modelo <- readRDS(arquivo)
  nomes <- nome_curto_variavel(basename(arquivos_variaveis))
  faltam <- setdiff(pca_modelo$variaveis, nomes)
  if (length(faltam) > 0) {
    message("Importância das variáveis originais pulada: faltam os rasters de ",
            paste(faltam, collapse = ", "))
    return(NULL)
  }
  list(pca_modelo = pca_modelo,
       arquivos = arquivos_variaveis[match(pca_modelo$variaveis, nomes)])
}

# argumentos da importância das variáveis originais (para seção 22 e obter_importancia)
argumentos_importancia_pca <- function(ambiente = globalenv()) {
  ativo <- isTRUE(ler_parametro("importancia_variaveis_originais", ambiente))
  p <- obter_preditores(ambiente)
  if (!ativo || !identical(p$tipo, "pca")) return(NULL)
  carregar_pca_importancia(p, file.path(obter_pastas_saida(ambiente)$multicolinearidade, "pca_eixos"),
                           obter_variaveis_cortadas(ambiente)[["preditoras"]])
}

# valores brutos das variáveis da PCA nos pontos de 'dados'
valores_originais_pca <- function(dados, pca_info) {
  v <- extrair_valores_pontos(dados[, c("x", "y")], pca_info$arquivos)
  names(v) <- pca_info$pca_modelo$variaveis
  v
}

# eixos da PCA (colunas 'eixos') calculados a partir dos valores brutos 'X'
eixos_de_originais <- function(X, pca_modelo, eixos) {
  vars <- pca_modelo$variaveis
  z <- as.matrix(X[, vars, drop = FALSE])
  z <- sweep(z, 2, pca_modelo$media[vars], "-")
  z <- sweep(z, 2, pca_modelo$desvio[vars], "/")
  z <- sweep(z, 2, pca_modelo$centro[vars], "-")
  esc <- pca_modelo$escala
  esc <- if (is.null(esc) || isFALSE(esc)) rep(1, length(vars)) else esc[vars]
  z <- sweep(z, 2, esc, "/")
  s <- z %*% pca_modelo$rotacao[vars, eixos, drop = FALSE]
  colnames(s) <- eixos
  s
}

# queda média do AUC ao embaralhar cada variável ORIGINAL (eixos recalculados)
importancia_permutacao_pca <- function(objeto, dados, X, pca_modelo, eixos,
                                       n_repeticoes = 10, semente = 123) {
  d <- dados
  auc_de <- function(Xi) {
    d[eixos] <- as.data.frame(eixos_de_originais(Xi, pca_modelo, eixos))
    p <- prever_modelo(objeto, d)
    calcular_auc(p[d$presenca == 1], p[d$presenca == 0])
  }
  auc_base <- auc_de(X)
  set.seed(semente)
  linhas <- lapply(pca_modelo$variaveis, function(v) {
    quedas <- vapply(seq_len(n_repeticoes), function(i) {
      Xi <- X
      Xi[[v]] <- sample(Xi[[v]])
      auc_base - auc_de(Xi)
    }, numeric(1))
    data.frame(variavel = v, auc_base = auc_base, queda_auc = mean(quedas, na.rm = TRUE),
               queda_auc_dp = stats::sd(quedas, na.rm = TRUE), repeticoes = n_repeticoes)
  })
  do.call(rbind, linhas)
}

# reparte a importância dos eixos (queda do AUC, negativos = 0) entre as variáveis
# originais pelo quadrado das cargas
importancia_cargas_pca <- function(tab_eixos, pca_modelo) {
  eixos <- intersect(tab_eixos$variavel, colnames(pca_modelo$rotacao))
  w <- pmax(tab_eixos$queda_auc[match(eixos, tab_eixos$variavel)], 0)
  w[is.na(w)] <- 0
  cargas2 <- pca_modelo$rotacao[pca_modelo$variaveis, eixos, drop = FALSE]^2
  data.frame(variavel = pca_modelo$variaveis,
             contribuicao_auc = as.numeric(cargas2 %*% w),
             eixos_usados = paste(eixos, collapse = "; "))
}


# ------------------------------------------------------------------------------
# importancia_regressao()
#   GLM binomial com os preditores padronizados (termos lineares e, se
#   'glm_termos' = "quadratico", também quadráticos). Para cada preditor:
#   queda de deviance ao retirar seus termos, % da deviance nula, p-valor
#   (qui-quadrado), coeficiente linear padronizado e sinal.
# ------------------------------------------------------------------------------
importancia_regressao <- function(dados, preditores, ajustes) {

  d <- dados
  d[preditores] <- lapply(d[preditores], function(x) {
    s <- stats::sd(x)
    if (is.na(s) || s == 0) rep(0, length(x)) else (x - mean(x)) / s
  })
  quadratico <- !identical(ajustes$glm_termos, "linear")
  termos <- function(v) if (quadratico) c(crase(v), paste0("I(", crase(v), "^2)")) else crase(v)
  formula_de <- function(vars) {
    rhs <- if (length(vars) == 0) "1" else paste(unlist(lapply(vars, termos)), collapse = " + ")
    stats::as.formula(paste("presenca ~", rhs))
  }
  ajustar <- function(vars) {
    suppressWarnings(stats::glm(formula_de(vars), data = d, family = stats::binomial(),
                                weights = peso))
  }

  completo <- ajustar(preditores)
  coefs    <- stats::coef(completo)

  linhas <- lapply(preditores, function(v) {
    sem <- tryCatch(ajustar(setdiff(preditores, v)), error = function(e) NULL)
    queda <- if (is.null(sem)) NA_real_ else stats::deviance(sem) - stats::deviance(completo)
    gl    <- length(termos(v))
    nome_coef <- c(v, crase(v))
    nome_coef <- nome_coef[nome_coef %in% names(coefs)]
    beta  <- if (length(nome_coef) > 0) unname(coefs[nome_coef[1]]) else NA_real_
    data.frame(variavel = v,
               queda_deviance = queda,
               pct_deviance_nula = 100 * queda / completo$null.deviance,
               p_valor = if (is.na(queda)) NA_real_ else stats::pchisq(max(queda, 0), gl, lower.tail = FALSE),
               coeficiente_padronizado = beta,
               sinal = if (is.na(beta)) NA_character_ else if (beta > 0) "+" else "-",
               termos = if (quadratico) "linear + quadratico" else "linear")
  })
  do.call(rbind, linhas)
}


# organiza uma medida de importância em colunas padrão
# (importancia, importancia_pct, ranking), dentro de espécie / modelo / método
padronizar_importancia <- function(tabela, medida) {
  tabela$importancia <- medida
  base <- pmax(tabela$importancia, 0)
  total <- sum(base, na.rm = TRUE)
  tabela$importancia_pct <- if (total > 0) 100 * base / total else NA_real_
  tabela$ranking <- rank(-tabela$importancia, ties.method = "min", na.last = "keep")
  tabela
}


# ------------------------------------------------------------------------------
# calcular_importancia()
#   Roda os métodos pedidos para cada espécie e algoritmo ('modelos').
#   Salva pasta_saida/importancia.csv (tabela única), um CSV de detalhes por
#   método e importancia.rds. Devolve list(tabela, permutacao, jackknife,
#   regressao).
#   'modelos' vazio = todos os modelos finais (o JSDM não entra).
# ------------------------------------------------------------------------------
calcular_importancia <- function(dados_modelos, modelos_finais, modelos = c(), metodos,
                                 ajustes, validacoes = "blocos", n_folds = 5,
                                 n_permutacoes = 10, pasta_saida, semente = 123,
                                 ajuste_maxent = NULL, preditores = NULL, pca_info = NULL) {

  # pca_info: de carregar_pca_importancia(); liga a importância das variáveis
  # ORIGINAIS quando os modelos usam eixos da PCA
  metodos <- normalizar_metodos_importancia(metodos)
  if (pulado(validacoes) && "jackknife" %in% metodos) {
    message("'tipo_validacao' = NA: o jackknife de variáveis (usa validação cruzada) foi pulado.")
    metodos <- setdiff(metodos, "jackknife")
    if (length(metodos) == 0) {
      message("Nenhum método de importância restou: seção pulada.")
      return(invisible(NULL))
    }
  }
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  ajustes <- completar_ajustes(ajustes)

  detalhes <- list(permutacao = list(), jackknife = list(), regressao = list(),
                   permutacao_variaveis = list(), carga_variaveis = list(),
                   regressao_variaveis = list())
  longa    <- list()

  cat("\n================ IMPORTÂNCIA DAS VARIÁVEIS ================\n")
  cat("Métodos:", paste(metodos, collapse = ", "), "\n")
  if (!is.null(pca_info)) {
    cat("Modelos com eixos da PCA: também será medida a importância das variáveis originais (",
        paste(pca_info$pca_modelo$variaveis, collapse = ", "), ").\n", sep = "")
  }
  if ("jackknife" %in% metodos) {
    cat("Aviso: o jackknife refaz cada modelo 2 x (nº de preditores) vezes; pode demorar.\n")
  }

  for (especie in names(dados_modelos)) {
    dados       <- dados_modelos[[especie]]$dados
    preditores_sp <- dados_modelos[[especie]]$preditores
    ajustes_sp  <- ajustes_da_especie(ajustes, ajuste_maxent, especie)
    codigos     <- names(modelos_finais[[especie]])
    codigos     <- setdiff(codigos, "jsdm")
    if (length(modelos) > 0 && !"all" %in% tolower(modelos)) codigos <- intersect(codigos, modelos)
    cat("\n", especie, "\n", sep = "")

    guardar <- function(tabela, metodo, codigo, medida) {
      tabela <- cbind(especie = especie, modelo = codigo, metodo = metodo, tabela,
                      stringsAsFactors = FALSE)
      chave  <- if (grepl("^jackknife", metodo)) "jackknife" else metodo
      if (is.null(detalhes[[chave]])) chave <- "regressao"
      lista  <- detalhes[[chave]]
      lista[[length(lista) + 1]] <- tabela
      detalhes[[chave]] <<- lista
      p <- padronizar_importancia(tabela[, c("especie", "modelo", "metodo", "variavel")], medida)
      longa[[length(longa) + 1]] <<- p
    }

    # regressão: não depende do algoritmo
    if ("regressao" %in% metodos) {
      cat("  regressão (GLM)... ")
      r <- tryCatch(importancia_regressao(dados, preditores_sp, ajustes_sp),
                    error = function(e) { message("falhou: ", conditionMessage(e)); NULL })
      if (!is.null(r)) {
        guardar(r, "regressao", "glm_regressao", r$queda_deviance)
        cat("ok\n")
      }
    }

    # variáveis originais da PCA: valores brutos nos pontos
    X_orig <- NULL
    if (!is.null(pca_info)) {
      X_orig <- tryCatch(valores_originais_pca(dados, pca_info),
                         error = function(e) { message("variáveis originais: ", conditionMessage(e)); NULL })
      if (!is.null(X_orig) && any(!stats::complete.cases(X_orig))) {
        message("Aviso: ", sum(!stats::complete.cases(X_orig)), " ponto(s) de '", especie,
                "' sem valor em alguma variável original: ficam de fora desta parte.")
      }
    }
    if (!is.null(X_orig) && "regressao" %in% metodos) {
      cc <- stats::complete.cases(X_orig)
      dv <- cbind(dados[cc, c("presenca", "peso"), drop = FALSE], X_orig[cc, , drop = FALSE])
      r <- tryCatch(importancia_regressao(dv, pca_info$pca_modelo$variaveis, ajustes_sp),
                    error = function(e) { message("regressão (originais) falhou: ", conditionMessage(e)); NULL })
      if (!is.null(r)) {
        guardar(r, "regressao_variaveis", "glm_regressao", r$queda_deviance)
        cat("  regressão (GLM) nas variáveis originais... ok\n")
      }
    }

    esquema <- if ("jackknife" %in% metodos) esquema_importancia(dados, validacoes, n_folds, semente) else NULL

    for (codigo in codigos) {
      cat("  ", codigo, ": ", sep = "")

      if ("permutacao" %in% metodos) {
        r <- tryCatch(importancia_permutacao(modelos_finais[[especie]][[codigo]], dados,
                                             preditores_sp, n_permutacoes, semente),
                      error = function(e) { message("[permutação falhou: ", conditionMessage(e), "] "); NULL })
        if (!is.null(r)) {
          guardar(r, "permutacao", codigo, r$queda_auc); cat("permutação ")
          if (!is.null(X_orig)) {
            rc <- importancia_cargas_pca(r, pca_info$pca_modelo)
            guardar(rc, "carga_variaveis", codigo, rc$contribuicao_auc)
          }
        }
      }

      if (!is.null(X_orig)) {
        cc <- stats::complete.cases(X_orig)
        r <- tryCatch(importancia_permutacao_pca(modelos_finais[[especie]][[codigo]],
                                                 dados[cc, , drop = FALSE], X_orig[cc, , drop = FALSE],
                                                 pca_info$pca_modelo, preditores_sp,
                                                 n_permutacoes, semente),
                      error = function(e) { message("[permutação das originais falhou: ", conditionMessage(e), "] "); NULL })
        if (!is.null(r)) { guardar(r, "permutacao_variaveis", codigo, r$queda_auc); cat("permutação(originais) ") }
      }

      if ("jackknife" %in% metodos) {
        r <- tryCatch(importancia_jackknife(codigo, esquema$dados, preditores_sp, ajustes_sp,
                                            esquema$validacao, semente),
                      error = function(e) { message("[jackknife falhou: ", conditionMessage(e), "] "); NULL })
        if (!is.null(r)) {
          guardar(r, "jackknife_sem", codigo, r$queda_auc_sem)
          guardar(r, "jackknife_so", codigo, r$auc_so - 0.5)
          cat("jackknife ")
        }
      }
      cat("\n")
    }
  }

  tabela <- if (length(longa) > 0) do.call(rbind, longa) else NULL
  resultado <- list(
    tabela     = tabela,
    permutacao = if (length(detalhes$permutacao) > 0) do.call(rbind, detalhes$permutacao) else NULL,
    jackknife  = if (length(detalhes$jackknife) > 0) do.call(rbind, detalhes$jackknife) else NULL,
    regressao  = if (length(detalhes$regressao) > 0) do.call(rbind, detalhes$regressao) else NULL,
    permutacao_variaveis = if (length(detalhes$permutacao_variaveis) > 0) do.call(rbind, detalhes$permutacao_variaveis) else NULL,
    carga_variaveis      = if (length(detalhes$carga_variaveis) > 0) do.call(rbind, detalhes$carga_variaveis) else NULL,
    regressao_variaveis  = if (length(detalhes$regressao_variaveis) > 0) do.call(rbind, detalhes$regressao_variaveis) else NULL)

  # o jackknife entra duas vezes (sem / só) nos detalhes: mantém uma linha por variável
  if (!is.null(resultado$jackknife)) {
    resultado$jackknife <- resultado$jackknife[resultado$jackknife$metodo == "jackknife_sem", , drop = FALSE]
    resultado$jackknife$metodo <- "jackknife"
  }

  if (is.null(tabela)) {
    message("Nenhuma importância pôde ser calculada.")
    return(invisible(resultado))
  }

  gravar <- function(x, nome) {
    if (is.null(x)) return(invisible(NULL))
    utils::write.csv(acrescentar_preditores(x, preditores), file.path(pasta_saida, nome),
                     row.names = FALSE, fileEncoding = "UTF-8")
  }
  gravar(tabela, "importancia.csv")
  gravar(resultado$permutacao, "importancia_permutacao.csv")
  gravar(resultado$jackknife, "importancia_jackknife.csv")
  gravar(resultado$regressao, "importancia_regressao.csv")
  gravar(resultado$permutacao_variaveis, "importancia_permutacao_variaveis.csv")
  gravar(resultado$carga_variaveis, "importancia_carga_variaveis.csv")
  gravar(resultado$regressao_variaveis, "importancia_regressao_variaveis.csv")
  saveRDS(resultado, file.path(pasta_saida, "importancia.rds"))

  # resumo na tela: média da importância (%) entre os modelos, 5 primeiras variáveis
  cat("\n---------------- RESUMO (importância média entre modelos, %) ----------------\n")
  for (especie in unique(tabela$especie)) {
    for (metodo in unique(tabela$metodo[tabela$especie == especie])) {
      sub <- tabela[tabela$especie == especie & tabela$metodo == metodo, , drop = FALSE]
      media <- sort(tapply(sub$importancia_pct, sub$variavel, mean, na.rm = TRUE), decreasing = TRUE)
      media <- utils::head(media, 5)
      cat(especie, "/", metodo, ": ",
          paste0(names(media), " (", round(media, 1), ")", collapse = ", "), "\n", sep = "")
    }
  }
  cat("Importância salva em:", pasta_saida, "\n")
  cat("===========================================================\n")

  invisible(resultado)
}


# ------------------------------------------------------------------------------
# obter_importancia()
#   Resultado da seção 22: da sessão, do arquivo salvo ou calculado agora com os
#   parâmetros da seção 2.
# ------------------------------------------------------------------------------
obter_importancia <- function(ambiente = globalenv()) {
  if (objeto_existe("importancia", ambiente)) {
    return(get("importancia", envir = ambiente))
  }
  pasta <- file.path(obter_pastas_saida(ambiente)$modelos, "importancia")
  arquivo <- file.path(pasta, "importancia.rds")
  if (file.exists(arquivo)) {
    message("Aviso: 'importancia' não encontrado na sessão. Usando: ", arquivo)
    return(readRDS(arquivo))
  }
  message("Aviso: não há importância salva. Será calculada agora.")
  calcular_importancia(
    dados_modelos  = obter_dados_modelos(ambiente),
    modelos_finais = obter_modelos_finais(ambiente),
    modelos        = ler_parametro("modelos_importancia", ambiente),
    metodos        = ler_parametro("metodos_importancia", ambiente),
    ajustes        = ler_parametro("ajustes_modelos", ambiente),
    validacoes     = ler_parametro("tipo_validacao", ambiente),
    n_folds        = ler_parametro("n_folds", ambiente),
    n_permutacoes  = ler_parametro("n_permutacoes", ambiente),
    pasta_saida    = pasta,
    semente        = ler_parametro("semente", ambiente),
    ajuste_maxent  = obter_ajuste_maxent(ambiente),
    preditores     = obter_preditores(ambiente),
    pca_info       = argumentos_importancia_pca(ambiente))
}
