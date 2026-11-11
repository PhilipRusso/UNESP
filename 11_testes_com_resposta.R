# ==============================================================================
#  FUNÇÕES - Testes de multicolinearidade COM variável resposta
# ------------------------------------------------------------------------------
#  Usam presença (1) e background (0) de cada espécie, nas variáveis PREDITORAS
#  já em z-score. Métodos: lasso, ridge, elastic net (glmnet), PCR, PLSR,
#  CPPLS (pacote pls) e Boruta.
#
#  Cada método devolve uma lista com:
#       $tabela            -> resultado por variável
#       $mantidas          -> variáveis sugeridas
#       $seleciona_variaveis -> FALSE quando o método só ranqueia (ridge, PCR)
# ==============================================================================


# pacotes necessários para cada método
pacotes_dos_metodos <- function(metodos) {
  mapa <- list(lasso = "glmnet", ridge = "glmnet", elastic_net = "glmnet",
               pcr = "pls", plsr = "pls", cppls = "pls", boruta = "Boruta")
  unique(unlist(mapa[intersect(names(mapa), metodos)]))
}


# ------------------------------------------------------------------------------
# pesos_balanceados()
#   Presenças e background têm pesos que somam o mesmo valor, para que o
#   grande número de pontos de background não domine o ajuste.
# ------------------------------------------------------------------------------
pesos_balanceados <- function(y) {
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  ifelse(y == 1, 0.5 / n1, 0.5 / n0) * length(y)
}


# ------------------------------------------------------------------------------
# testar_glmnet()
#   Regressão logística penalizada. alfa = 1 (lasso), 0 (ridge) ou entre os
#   dois (elastic net). O lambda é escolhido por validação cruzada
#   (maior lambda dentro de 1 erro-padrão do melhor).
#   Mantém as variáveis com coeficiente diferente de zero.
# ------------------------------------------------------------------------------
testar_glmnet <- function(x, y, alfa, semente = 123) {

  set.seed(semente)
  pesos <- pesos_balanceados(y)

  # Validação cruzada ESTRATIFICADA: cada bloco recebe presenças e background.
  # Com poucas presenças, usa menos blocos (idealmente >= 8 presenças por bloco).
  n_blocos <- max(3, min(5, floor(sum(y == 1) / 8)))
  bloco <- integer(length(y))
  for (classe in c(0, 1)) {
    i <- which(y == classe)
    bloco[i] <- sample(rep(seq_len(n_blocos), length.out = length(i)))
  }

  # lambda.min.ratio maior evita lambdas minúsculos, onde o ajuste não converge
  cv <- suppressWarnings(
    glmnet::cv.glmnet(x, y, family = "binomial", alpha = alfa, weights = pesos,
                      standardize = FALSE, foldid = bloco, lambda.min.ratio = 0.01)
  )

  coef <- as.matrix(glmnet::coef.glmnet(cv, s = "lambda.1se"))[-1, 1]
  if (all(coef == 0)) {
    coef <- as.matrix(glmnet::coef.glmnet(cv, s = "lambda.min"))[-1, 1]
  }

  seleciona <- alfa > 0
  tabela <- data.frame(variavel = names(coef), coeficiente = arredondar_sdm(as.numeric(coef)),
                       mantida = if (seleciona) coef != 0 else NA,
                       stringsAsFactors = FALSE)
  tabela <- tabela[order(-abs(tabela$coeficiente)), ]

  list(tabela = tabela,
       mantidas = if (seleciona) names(coef)[coef != 0] else colnames(x),
       seleciona_variaveis = seleciona, lambda = cv$lambda.1se)
}


# ------------------------------------------------------------------------------
# escolher_ncomp()
#   Número de componentes por validação cruzada (menor número cujo erro está
#   dentro de 1 erro-padrão do mínimo).
# ------------------------------------------------------------------------------
escolher_ncomp <- function(modelo) {
  n <- tryCatch(pls::selectNcomp(modelo, method = "onesigma", plot = FALSE),
                error = function(e) NA)
  if (is.na(n) || n < 1) {
    rmsep <- as.numeric(pls::RMSEP(modelo, estimate = "CV")$val[1, 1, -1])
    n <- which.min(rmsep)
  }
  max(1, as.integer(n))
}


# ------------------------------------------------------------------------------
# calcular_vip()
#   VIP (importância da variável na projeção) para modelos PLS/CPPLS.
#   Variáveis com VIP >= 1 são consideradas importantes.
# ------------------------------------------------------------------------------
calcular_vip <- function(modelo, ncomp) {
  pesos  <- unclass(modelo$loading.weights)[, seq_len(ncomp), drop = FALSE]
  escore <- unclass(modelo$scores)[, seq_len(ncomp), drop = FALSE]
  q      <- unclass(modelo$Yloadings)[, seq_len(ncomp), drop = FALSE]

  ss <- as.numeric(colSums(escore^2) * colSums(q^2))
  pesos_norm <- sweep(pesos, 2, sqrt(colSums(pesos^2)), "/")
  vip <- sqrt(nrow(pesos) * (pesos_norm^2 %*% ss) / sum(ss))
  setNames(as.numeric(vip), rownames(pesos))
}


# ------------------------------------------------------------------------------
# testar_pls()
#   metodo = "pcr", "plsr" ou "cppls".
#   PLSR e CPPLS: sugerem variáveis com VIP >= 1.
#   PCR: só mostra a importância (|coeficiente| padronizado), sem seleção.
# ------------------------------------------------------------------------------
testar_pls <- function(x, y, metodo, semente = 123) {

  # o pacote pls precisa estar carregado (suas funções internas chamam outras do pacote)
  suppressPackageStartupMessages(library(pls))

  set.seed(semente)
  dados <- data.frame(y = y)
  dados$x <- x

  funcao <- switch(metodo, pcr = pls::pcr, plsr = pls::plsr, cppls = pls::cppls)
  ncomp_max <- min(10, ncol(x) - 1)

  modelo <- funcao(y ~ x, data = dados, ncomp = ncomp_max, scale = FALSE,
                   validation = "CV", segments = 5)
  ncomp <- escolher_ncomp(modelo)

  if (metodo == "pcr") {
    coef <- drop(stats::coef(modelo, ncomp = ncomp))
    tabela <- data.frame(variavel = colnames(x), importancia = arredondar_sdm(abs(coef)),
                         stringsAsFactors = FALSE)
    tabela <- tabela[order(-tabela$importancia), ]
    return(list(tabela = tabela, mantidas = colnames(x),
                seleciona_variaveis = FALSE, ncomp = ncomp))
  }

  vip <- calcular_vip(modelo, ncomp)
  tabela <- data.frame(variavel = names(vip), vip = arredondar_sdm(vip),
                       mantida = vip >= 1, stringsAsFactors = FALSE)
  tabela <- tabela[order(-tabela$vip), ]

  list(tabela = tabela, mantidas = names(vip)[vip >= 1],
       seleciona_variaveis = TRUE, ncomp = ncomp)
}


# ------------------------------------------------------------------------------
# testar_boruta()
#   Boruta compara cada variável com cópias embaralhadas dela mesma, usando
#   random forest. Mantém as "Confirmed". Limita a 5000 linhas por velocidade.
# ------------------------------------------------------------------------------
testar_boruta <- function(x, y, semente = 123, max_linhas = 5000) {

  set.seed(semente)
  if (nrow(x) > max_linhas) {
    linhas <- sample(nrow(x), max_linhas)
    x <- x[linhas, , drop = FALSE]
    y <- y[linhas]
  }

  b <- Boruta::Boruta(x = as.data.frame(x), y = factor(y), doTrace = 0)
  decisao <- as.character(b$finalDecision)
  names(decisao) <- names(b$finalDecision)
  importancia <- apply(b$ImpHistory[, names(decisao), drop = FALSE], 2,
                       function(v) stats::median(v[is.finite(v)]))

  tabela <- data.frame(variavel = names(decisao), decisao = decisao,
                       importancia = arredondar_sdm(as.numeric(importancia)),
                       stringsAsFactors = FALSE)
  tabela <- tabela[order(-tabela$importancia), ]

  list(tabela = tabela, mantidas = names(decisao)[decisao == "Confirmed"],
       seleciona_variaveis = TRUE)
}


# nome seguro para pasta (sem espaços ou caracteres especiais)
nome_seguro <- function(texto) gsub("[^A-Za-z0-9_.-]+", "_", texto)


# ------------------------------------------------------------------------------
# rodar_testes_com_resposta()
#   dados_resposta: lista por espécie com $x (matriz) e $y (0/1).
#   Roda cada método escolhido em cada espécie; erro em um método não
#   interrompe os demais. Salva CSV em pasta_saida/<especie>/<metodo>.csv.
#   Devolve resultados[[especie]][[metodo]].
# ------------------------------------------------------------------------------
rodar_testes_com_resposta <- function(dados_resposta, metodos, pasta_saida,
                                      alfa_elastic_net = 0.5, semente = 123) {

  metodos <- intersect(c("lasso", "ridge", "elastic_net", "pcr", "plsr", "cppls", "boruta"),
                       metodos)
  resultados <- list()

  if (length(metodos) == 0) {
    message("Nenhum teste com resposta foi escolhido.")
    return(resultados)
  }

  for (pac in pacotes_dos_metodos(metodos)) {
    if (!requireNamespace(pac, quietly = TRUE)) {
      message("Pacote '", pac, "' não instalado; tentando instalar...")
      try(utils::install.packages(pac, quiet = TRUE), silent = TRUE)
    }
  }

  for (especie in names(dados_resposta)) {
    x <- dados_resposta[[especie]]$x
    y <- dados_resposta[[especie]]$y
    pasta <- file.path(pasta_saida, nome_seguro(especie))
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

    cat("\n================ TESTES COM RESPOSTA:", especie, "================\n")
    cat("Presenças:", sum(y == 1), "| Background:", sum(y == 0),
        "| Variáveis:", ncol(x), "\n\n")
    resultados[[especie]] <- list()

    for (m in metodos) {
      r <- tryCatch(
        switch(m,
               lasso       = testar_glmnet(x, y, 1, semente),
               ridge       = testar_glmnet(x, y, 0, semente),
               elastic_net = testar_glmnet(x, y, alfa_elastic_net, semente),
               pcr         = testar_pls(x, y, "pcr", semente),
               plsr        = testar_pls(x, y, "plsr", semente),
               cppls       = testar_pls(x, y, "cppls", semente),
               boruta      = testar_boruta(x, y, semente)),
        error = function(e) {
          message("  [", m, "] falhou: ", conditionMessage(e))
          NULL
        })

      if (is.null(r)) next

      resultados[[especie]][[m]] <- r
      utils::write.csv(r$tabela, file.path(pasta, paste0(m, ".csv")),
                       row.names = FALSE, fileEncoding = "UTF-8")
      if (r$seleciona_variaveis) {
        cat(sprintf("%-12s mantém %d de %d variáveis\n", m, length(r$mantidas), ncol(x)))
      } else {
        cat(sprintf("%-12s apenas ranking de importância (não seleciona)\n", m))
      }
    }
    cat("Resultados salvos em:", pasta, "\n")
  }

  return(resultados)
}
