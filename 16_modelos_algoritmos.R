# ==============================================================================
#  FUNÇÕES - Modelos: os algoritmos
# ------------------------------------------------------------------------------
#  Todos os algoritmos seguem a mesma "interface":
#       ajustar_<modelo>(treino, preditores, ajustes)  -> modelo ajustado
#       prever_<modelo>(modelo, novos)                 -> adequabilidade de 0 a 1
#  'treino' tem as colunas: presenca (1/0), peso, e uma coluna por preditor.
#  As funções ajustar_modelo() e prever_modelo() escolhem o algoritmo pelo
#  código, então o resto do script trata todos do mesmo jeito.
#
#  Pesos: presenças e background somam o mesmo total (coluna 'peso'), para que
#  os muitos pontos de background não dominem o ajuste. O RF usa outra forma
#  de equilíbrio (sorteia o mesmo número de presenças e de background em cada
#  árvore) e o Maxent não usa pesos (ele já trata o background como amostra
#  do ambiente disponível).
# ==============================================================================


# ---- ajustes (parâmetros dos algoritmos) ---------------------------------------

# ------------------------------------------------------------------------------
# ajustes_padrao() / completar_ajustes()
#   Valores usuais de todos os parâmetros dos algoritmos. Quem edita é a lista
#   'ajustes_modelos' do main; o que faltar nela (por exemplo, numa sessão
#   iniciada com um main antigo) é completado com estes valores.
#   NA em 'rf_mtry' e 'svm_gamma' = padrão do pacote.
# ------------------------------------------------------------------------------
ajustes_padrao <- function() {
  list(
    glm_termos       = "quadratico",   # GLM e ESM: "quadratico" (linear + quadrático) ou "linear"
    glmnet_alfa      = 1,
    glmnet_lambda    = "lambda.min",
    glmnet_folds     = NA,
    glmnet_padronizar = TRUE,
    gam_k            = 4,
    gam_metodo       = "REML",
    gam_select       = FALSE,
    gam_gamma        = 1,
    mars_grau        = 1,
    mars_nk          = NA,
    mars_penalidade  = NA,
    mars_thresh      = 0.001,
    cta_cp           = 0.01,
    cta_minsplit     = 10,
    cta_maxdepth     = 30,
    cta_minbucket    = NA,
    rf_arvores       = 500,
    rf_mtry          = NA,
    rf_tamanho_no    = 1,
    rf_balancear     = TRUE,
    rf_reposicao     = TRUE,
    brt_taxa         = 0.01,
    brt_complexidade = 3,
    brt_max_arvores  = 5000,
    brt_bag          = 0.75,
    brt_min_obs      = 5,
    brt_cv_folds     = 5,
    brt_distribuicao = "bernoulli",
    maxent_classes   = "default",
    maxent_regmult   = 1,
    maxent_maxit     = 100000,
    maxent_beta_lqp  = 1,
    maxent_beta_hinge = 1,
    maxent_beta_threshold = 1,
    maxent_beta_categorical = 1,
    svm_custo        = 1,
    svm_kernel       = "radial",
    svm_gamma        = NA,
    svm_pesos_classe = TRUE,
    svm_tolerancia   = 0.001,
    ann_neuronios    = 5,
    ann_decaimento   = 0.1,
    ann_maxit        = 1000,
    ann_erro         = "ce",
    ann_repeticoes   = 1,
    ann_abstol       = 1e-4,
    ann_reltol       = 1e-8,
    ann_rang         = 0.5,
    ann_skip         = FALSE,
    esm_folds        = 3,
    esm_limite_d     = 0,
    jsdm_latentes    = 2
  )
}

completar_ajustes <- function(ajustes) {
  padrao <- ajustes_padrao()
  if (is.null(ajustes)) ajustes <- list()
  for (nome in names(padrao)) {
    if (is.null(ajustes[[nome]]) || length(ajustes[[nome]]) == 0) ajustes[[nome]] <- padrao[[nome]]
  }
  conferir <- function(nome, validos) {
    if (!ajustes[[nome]] %in% validos) {
      stop("'", nome, "' em 'ajustes_modelos' deve ser um destes: ",
           paste0("\"", validos, "\"", collapse = ", "), call. = FALSE)
    }
  }
  conferir("glm_termos", c("quadratico", "linear"))
  conferir("glmnet_lambda", c("lambda.min", "lambda.1se"))
  conferir("gam_metodo", c("REML", "GCV.Cp", "ML"))
  conferir("ann_erro", c("ce", "sse"))
  conferir("brt_distribuicao", c("bernoulli", "adaboost"))
  conferir("svm_kernel", c("radial", "linear", "polynomial", "sigmoid"))
  ajustes
}


# ---- auxiliares ---------------------------------------------------------------

# nomes entre crases, para funcionar em fórmulas mesmo com nomes estranhos
crase <- function(v) paste0("`", v, "`")

# fórmula com termo linear e quadrático de cada preditor
formula_quadratica <- function(preditores, resposta = "presenca") {
  termos <- c(crase(preditores), paste0("I(", crase(preditores), "^2)"))
  stats::as.formula(paste(resposta, "~", paste(termos, collapse = " + ")))
}

# fórmula só com os termos lineares
formula_linear <- function(preditores, resposta = "presenca") {
  stats::as.formula(paste(resposta, "~", paste(crase(preditores), collapse = " + ")))
}

# matriz com os preditores e seus quadrados (para o glmnet)
matriz_quadratica <- function(dados, preditores) {
  X  <- as.matrix(dados[, preditores, drop = FALSE])
  X2 <- X^2
  colnames(X2) <- paste0(preditores, "_2")
  cbind(X, X2)
}

# presenças do treino (para os modelos só de presença)
so_presencas <- function(treino, preditores) {
  as.matrix(treino[treino$presenca == 1, preditores, drop = FALSE])
}


# ---- modelos só de presença ---------------------------------------------------

# Bioclim: para cada variável, onde o valor cai na distribuição das presenças
# (percentil). Perto da mediana = 1; nos extremos = perto de 0; fora da faixa
# das presenças = 0. O valor final é o da variável mais limitante (mínimo).
ajustar_bioclim <- function(treino, preditores, ajustes) {
  pres <- so_presencas(treino, preditores)
  list(ordenados = lapply(seq_len(ncol(pres)), function(j) sort(pres[, j])),
       preditores = preditores)
}

prever_bioclim <- function(modelo, novos) {
  X <- as.matrix(novos[, modelo$preditores, drop = FALSE])
  s <- matrix(NA_real_, nrow(X), ncol(X))
  for (j in seq_len(ncol(X))) {
    v      <- modelo$ordenados[[j]]
    menor  <- findInterval(X[, j], v, left.open = TRUE)   # nº de presenças < valor
    ate    <- findInterval(X[, j], v)                     # nº de presenças <= valor
    perc   <- (menor + 0.5 * (ate - menor)) / length(v)   # percentil
    s[, j] <- 2 * pmin(perc, 1 - perc)
  }
  apply(s, 1, min)
}

# Domain: semelhança ambiental com a presença MAIS parecida (distância de
# Gower: diferença em cada variável dividida pela amplitude nas presenças).
ajustar_domain <- function(treino, preditores, ajustes) {
  pres <- so_presencas(treino, preditores)
  amplitude <- apply(pres, 2, function(v) diff(range(v)))
  amplitude[amplitude == 0] <- 1
  list(pres = pres, amplitude = amplitude, preditores = preditores)
}

prever_domain <- function(modelo, novos) {
  X <- as.matrix(novos[, modelo$preditores, drop = FALSE])
  d_min <- rep(Inf, nrow(X))
  for (i in seq_len(nrow(modelo$pres))) {
    d <- rowMeans(sweep(abs(sweep(X, 2, modelo$pres[i, ], "-")), 2, modelo$amplitude, "/"))
    d_min <- pmin(d_min, d)
  }
  pmax(0, 1 - d_min)
}

# Mahalanobis: distância ao "centro" das presenças, considerando a correlação
# entre as variáveis. Vira 0 a 1 pela distribuição qui-quadrado (1 = no centro).
ajustar_mahalanobis <- function(treino, preditores, ajustes) {
  pres  <- so_presencas(treino, preditores)
  covar <- stats::cov(pres)
  inversa <- tryCatch(solve(covar), error = function(e) NULL)
  if (is.null(inversa)) {   # poucas presenças ou variáveis colineares
    inversa <- solve(covar + diag(1e-6 * mean(diag(covar)) + 1e-12, ncol(covar)))
  }
  list(centro = colMeans(pres), inversa = inversa, preditores = preditores)
}

prever_mahalanobis <- function(modelo, novos) {
  X  <- as.matrix(novos[, modelo$preditores, drop = FALSE])
  d2 <- stats::mahalanobis(X, modelo$centro, modelo$inversa, inverted = TRUE)
  1 - stats::pchisq(d2, df = length(modelo$preditores))
}


# ---- modelos de presença e background ----------------------------------------

# GLM: regressão logística com termos lineares e quadráticos (curvas em sino).
# Com 'glm_termos' = "linear", só os termos lineares (curvas sempre crescentes
# ou decrescentes). Vale também para o ESM, que usa GLMs.
ajustar_glm <- function(treino, preditores, ajustes) {
  formula_glm <- if (identical(ajustes$glm_termos, "linear")) formula_linear(preditores) else
    formula_quadratica(preditores)
  suppressWarnings(stats::glm(formula_glm, data = treino,
                              family = stats::binomial(), weights = peso))
}
prever_glm <- function(modelo, novos) {
  as.numeric(stats::predict(modelo, newdata = novos, type = "response"))
}

# GLMNET: o mesmo GLM com penalização (lasso/elastic net), que encolhe os
# coeficientes e zera os pouco úteis. A força da penalização (lambda) é
# escolhida por validação cruzada interna ('glmnet_lambda': "lambda.min" = menor
# erro; "lambda.1se" = penalização maior, modelo mais simples).
ajustar_glmnet <- function(treino, preditores, ajustes) {
  X <- matriz_quadratica(treino, preditores)
  n_grupos <- if (is.na(ajustes$glmnet_folds)) {
    max(3, min(5, floor(sum(treino$presenca == 1) / 8)))
  } else max(3, round(ajustes$glmnet_folds))
  grupos   <- folds_estratificados(treino$presenca, n_grupos)
  cv <- suppressWarnings(glmnet::cv.glmnet(X, treino$presenca, family = "binomial",
                                           weights = treino$peso, alpha = ajustes$glmnet_alfa,
                                           standardize = isTRUE(ajustes$glmnet_padronizar),
                                           foldid = grupos))
  list(cv = cv, preditores = preditores, lambda = ajustes$glmnet_lambda)
}
prever_glmnet <- function(modelo, novos) {
  X <- matriz_quadratica(novos, modelo$preditores)
  lambda <- if (is.null(modelo$lambda)) "lambda.min" else modelo$lambda
  as.numeric(stats::predict(modelo$cv, newx = X, s = lambda, type = "response"))
}

# GAM: curvas suaves (splines) para cada preditor; 'gam_k' limita a
# flexibilidade das curvas.
ajustar_gam <- function(treino, preditores, ajustes) {
  termos <- paste0("s(", crase(preditores), ", k = ", ajustes$gam_k, ")")
  f <- stats::as.formula(paste("presenca ~", paste(termos, collapse = " + ")))
  mgcv::gam(f, data = treino, family = stats::binomial(), weights = peso,
            method = ajustes$gam_metodo, select = isTRUE(ajustes$gam_select),
            gamma = ajustes$gam_gamma)
}
prever_gam <- function(modelo, novos) {
  as.numeric(stats::predict(modelo, newdata = novos, type = "response"))
}

# MARS: retas por partes (funções "dobradiça") escolhidas automaticamente.
# 'mars_nk' = máximo de termos antes da poda; 'mars_penalidade' = custo por termo
# na poda (NA = padrão do pacote: 2 sem interações, 3 com interações).
ajustar_mars <- function(treino, preditores, ajustes) {
  argumentos <- list(x = treino[, preditores, drop = FALSE], y = treino$presenca,
                     weights = treino$peso, glm = list(family = stats::binomial()),
                     degree = ajustes$mars_grau, thresh = ajustes$mars_thresh)
  if (!is.na(ajustes$mars_nk)) argumentos$nk <- ajustes$mars_nk
  if (!is.na(ajustes$mars_penalidade)) argumentos$penalty <- ajustes$mars_penalidade
  list(earth = suppressWarnings(do.call(earth::earth, argumentos)),
       preditores = preditores)
}
prever_mars <- function(modelo, novos) {
  as.numeric(stats::predict(modelo$earth, newdata = novos[, modelo$preditores, drop = FALSE],
                            type = "response"))
}

# CTA: uma única árvore de classificação (perguntas do tipo "bio01 < x?").
ajustar_cta <- function(treino, preditores, ajustes) {
  treino$classe <- factor(treino$presenca, levels = c(0, 1))
  f <- stats::as.formula(paste("classe ~", paste(crase(preditores), collapse = " + ")))
  rpart::rpart(f, data = treino, weights = peso, method = "class",
               control = rpart::rpart.control(
                 cp = ajustes$cta_cp, minsplit = ajustes$cta_minsplit,
                 minbucket = if (is.na(ajustes$cta_minbucket)) round(ajustes$cta_minsplit / 3) else
                   ajustes$cta_minbucket,
                 maxdepth = min(30, ajustes$cta_maxdepth), xval = 0))
}
prever_cta <- function(modelo, novos) {
  as.numeric(stats::predict(modelo, newdata = novos, type = "prob")[, "1"])
}

# RF: muitas árvores, cada uma com uma amostra dos dados. Cada árvore sorteia
# o MESMO número de presenças e de background (down-sampling; 'rf_balancear' = FALSE
# usa todos os dados). 'rf_reposicao' = sorteio com reposição.
ajustar_rf <- function(treino, preditores, ajustes) {
  y  <- factor(treino$presenca, levels = c(0, 1))
  n1 <- sum(y == "1")
  list(rf = randomForest::randomForest(x = treino[, preditores, drop = FALSE], y = y,
                                       ntree = ajustes$rf_arvores,
                                       mtry = if (is.na(ajustes$rf_mtry)) {
                                         max(1, floor(sqrt(length(preditores))))
                                       } else min(ajustes$rf_mtry, length(preditores)),
                                       nodesize = ajustes$rf_tamanho_no,
                                       sampsize = if (isTRUE(ajustes$rf_balancear)) {
                                         c("0" = n1, "1" = n1)
                                       } else length(y),
                                       strata = y, replace = isTRUE(ajustes$rf_reposicao)),
       preditores = preditores)
}
prever_rf <- function(modelo, novos) {
  as.numeric(stats::predict(modelo$rf, newdata = novos[, modelo$preditores, drop = FALSE],
                            type = "prob")[, "1"])
}

# BRT: árvores pequenas somadas em sequência, cada uma corrigindo o erro das
# anteriores. O número de árvores é escolhido por validação cruzada interna.
ajustar_brt <- function(treino, preditores, ajustes) {
  f <- stats::as.formula(paste("presenca ~", paste(crase(preditores), collapse = " + ")))
  # a validação cruzada interna do gbm falha com 1 preditor só; nesse caso o
  # nº de árvores é escolhido pelos dados fora da amostra de cada árvore (OOB)
  usar_cv <- length(preditores) > 1
  utils::capture.output(   # esconde o progresso que o gbm imprime na tela
    modelo <- gbm::gbm(f, data = treino, distribution = ajustes$brt_distribuicao, weights = peso,
                       n.trees = ajustes$brt_max_arvores,
                       interaction.depth = ajustes$brt_complexidade,
                       shrinkage = ajustes$brt_taxa, bag.fraction = ajustes$brt_bag,
                       n.minobsinnode = ajustes$brt_min_obs,
                       cv.folds = if (usar_cv) ajustes$brt_cv_folds else 0,
                       n.cores = 1, verbose = FALSE)
  )
  n_arvores <- suppressWarnings(suppressMessages(
    gbm::gbm.perf(modelo, method = if (usar_cv) "cv" else "OOB", plot.it = FALSE)))
  list(gbm = modelo, n_arvores = max(1, as.integer(n_arvores)))
}
prever_brt <- function(modelo, novos) {
  as.numeric(gbm::predict.gbm(modelo$gbm, newdata = novos, n.trees = modelo$n_arvores,
                              type = "response"))
}

# Maxent (pacote maxnet): compara o ambiente das presenças com o do
# background usando "features" (lineares, quadráticas, dobradiças...).
# Saída "cloglog", de 0 a 1.
ajustar_maxent <- function(treino, preditores, ajustes) {
  dados <- treino[, preditores, drop = FALSE]
  formula_mx <- maxnet::maxnet.formula(treino$presenca, dados, classes = ajustes$maxent_classes)
  argumentos <- list(p = treino$presenca, data = dados, f = formula_mx,
                     regmult = ajustes$maxent_regmult,
                     regfun = regularizacao_maxent(ajustes))
  if (!is.na(ajustes$maxent_maxit) && ajustes$maxent_maxit != 100000) {
    argumentos$maxit <- ajustes$maxent_maxit    # repassado ao glmnet
  }
  list(maxnet = do.call(maxnet::maxnet, argumentos), preditores = preditores)
}

# Regularização do Maxent: parte da regularização padrão do maxnet e multiplica
# cada tipo de feature pelo fator escolhido (1 = padrão). O 'maxent_regmult'
# continua multiplicando tudo.
regularizacao_maxent <- function(ajustes) {
  fatores <- c(lqp = ajustes$maxent_beta_lqp, hinge = ajustes$maxent_beta_hinge,
               threshold = ajustes$maxent_beta_threshold,
               categorical = ajustes$maxent_beta_categorical)
  function(p, m) {
    reg <- maxnet::maxnet.default.regularization(p, m)
    nomes <- colnames(m)
    reg[grepl("^hinge\\(", nomes)]       <- reg[grepl("^hinge\\(", nomes)] * fatores[["hinge"]]
    reg[grepl("^thresholds\\(", nomes)]  <- reg[grepl("^thresholds\\(", nomes)] * fatores[["threshold"]]
    reg[grepl("^categorical\\(", nomes)] <- reg[grepl("^categorical\\(", nomes)] * fatores[["categorical"]]
    lqp <- !grepl("^(hinge|thresholds|categorical)\\(", nomes)
    reg[lqp] <- reg[lqp] * fatores[["lqp"]]
    reg
  }
}
prever_maxent <- function(modelo, novos) {
  as.numeric(stats::predict(modelo$maxnet, newdata = novos[, modelo$preditores, drop = FALSE],
                            type = "cloglog"))
}

# SVM: procura a fronteira que melhor separa presenças e background no
# espaço ambiental (núcleo radial). As classes recebem pesos equilibrados.
ajustar_svm <- function(treino, preditores, ajustes) {
  y <- factor(treino$presenca, levels = c(0, 1))
  pesos_classe <- if (isTRUE(ajustes$svm_pesos_classe)) {
    c("0" = 1, "1" = sum(y == "0") / sum(y == "1"))
  } else c("0" = 1, "1" = 1)
  list(svm = e1071::svm(x = as.matrix(treino[, preditores, drop = FALSE]), y = y,
                        type = "C-classification", kernel = ajustes$svm_kernel,
                        cost = ajustes$svm_custo,
                        gamma = if (is.na(ajustes$svm_gamma)) 1 / length(preditores) else ajustes$svm_gamma,
                        tolerance = ajustes$svm_tolerancia,
                        probability = TRUE, class.weights = pesos_classe),
       preditores = preditores)
}
prever_svm <- function(modelo, novos) {
  previsao <- stats::predict(modelo$svm, as.matrix(novos[, modelo$preditores, drop = FALSE]),
                             probability = TRUE)
  as.numeric(attr(previsao, "probabilities")[, "1"])
}

# ANN: rede neural com uma camada oculta; 'ann_decaimento' evita o sobreajuste.
# 'ann_repeticoes' treina várias redes com pesos iniciais diferentes e fica com a
# de menor erro; 'ann_erro': "ce" (entropia cruzada) ou "sse" (soma dos quadrados).
ajustar_ann <- function(treino, preditores, ajustes) {
  x <- as.matrix(treino[, preditores, drop = FALSE])
  melhor <- NULL
  for (r in seq_len(max(1, round(ajustes$ann_repeticoes)))) {
    rede <- nnet::nnet(x = x, y = treino$presenca,
                       weights = treino$peso, size = ajustes$ann_neuronios,
                       decay = ajustes$ann_decaimento, maxit = ajustes$ann_maxit,
                       entropy = identical(ajustes$ann_erro, "ce"),
                       abstol = ajustes$ann_abstol, reltol = ajustes$ann_reltol,
                       rang = ajustes$ann_rang, skip = isTRUE(ajustes$ann_skip),
                       trace = FALSE)
    if (is.null(melhor) || rede$value < melhor$value) melhor <- rede
  }
  list(nnet = melhor, preditores = preditores)
}
prever_ann <- function(modelo, novos) {
  as.numeric(stats::predict(modelo$nnet, as.matrix(novos[, modelo$preditores, drop = FALSE]),
                            type = "raw"))
}

# ESM (conjunto de modelos pequenos): um GLM para cada PAR de preditores.
# Cada par é avaliado por validação cruzada interna (D de Somers = 2*AUC - 1);
# entram os pares melhores que 'esm_limite_d' (0 = acaso), com peso D. Indicado para
# espécies com poucos registros, porque cada modelo tem poucos parâmetros.
ajustar_esm <- function(treino, preditores, ajustes) {
  if (length(preditores) < 2) stop("o ESM precisa de pelo menos 2 preditores", call. = FALSE)
  pares  <- utils::combn(preditores, 2, simplify = FALSE)
  n_folds_esm <- max(2, round(ajustes$esm_folds))
  grupos <- folds_estratificados(treino$presenca, n_folds_esm)

  avaliar_par <- function(par) {
    auc <- vapply(seq_len(n_folds_esm), function(k) {
      m <- ajustar_glm(treino[grupos != k, ], par, ajustes)
      p <- prever_glm(m, treino[grupos == k, ])
      calcular_auc(p[treino$presenca[grupos == k] == 1], p[treino$presenca[grupos == k] == 0])
    }, numeric(1))
    2 * mean(auc, na.rm = TRUE) - 1
  }
  d <- vapply(pares, avaliar_par, numeric(1))
  usar <- which(!is.na(d) & d > ajustes$esm_limite_d)
  if (length(usar) == 0) stop("nenhum par de preditores ficou melhor que o acaso", call. = FALSE)

  list(pares   = pares[usar],
       modelos = lapply(pares[usar], function(par) ajustar_glm(treino, par, ajustes)),
       pesos   = d[usar])
}
prever_esm <- function(modelo, novos) {
  previsoes <- vapply(modelo$modelos, prever_glm, numeric(nrow(novos)), novos = novos)
  previsoes <- matrix(previsoes, nrow = nrow(novos))
  as.numeric(previsoes %*% modelo$pesos / sum(modelo$pesos))
}


# ---- escolha do algoritmo pelo código ----------------------------------------

# ------------------------------------------------------------------------------
# ajustar_modelo() / prever_modelo()
#   Chamam ajustar_<codigo>() e prever_<codigo>(). O resultado do ajuste é
#   guardado numa lista com o código e os preditores, para a previsão saber
#   qual algoritmo usar. A previsão é limitada ao intervalo de 0 a 1.
# ------------------------------------------------------------------------------
ajustar_modelo <- function(codigo, treino, preditores, ajustes, semente = 123) {
  ajustes <- completar_ajustes(ajustes)
  set.seed(semente)
  ajuste <- get(paste0("ajustar_", codigo), mode = "function")
  list(codigo = codigo, preditores = preditores,
       modelo = ajuste(treino, preditores, ajustes))
}

prever_modelo <- function(objeto, novos) {
  # carrega o pacote do algoritmo (necessário quando o modelo veio de um
  # arquivo salvo, numa sessão em que o pacote ainda não foi usado)
  catalogo <- listar_modelos()
  pacote   <- catalogo$pacote[catalogo$codigo == objeto$codigo]
  if (length(pacote) == 1 && !is.na(pacote)) requireNamespace(pacote, quietly = TRUE)

  previsao <- get(paste0("prever_", objeto$codigo), mode = "function")
  p <- previsao(objeto$modelo, as.data.frame(novos))
  pmin(pmax(as.numeric(p), 0), 1)
}
