# ==============================================================================
#  FUNÇÕES - Testes de multicolinearidade SEM variável resposta
# ------------------------------------------------------------------------------
#  Métodos: VIF (com remoção iterativa), filtro por correlação par a par,
#  cluster de variáveis e PCA. Usam só R base.
#
#  Todos recebem 'dados': tabela em z-score, uma coluna por variável.
#  Cada método devolve uma lista com:
#       $tabela   -> resultado por variável (será salvo em CSV)
#       $mantidas -> variáveis que o método SUGERE manter
#  O usuário escolhe as variáveis finais no último passo.
# ==============================================================================


# ------------------------------------------------------------------------------
# calcular_vif()
#   VIF (fator de inflação da variância) de cada variável: o quanto a variância
#   do coeficiente aumenta porque a variável é explicada pelas demais.
#       VIF = 1 / (1 - R2), em que R2 vem da regressão da variável nas outras.
#   Calculado pela diagonal da inversa da matriz de correlação (resultado
#   idêntico ao das regressões, mas muito mais rápido).
#   Referência comum: VIF acima de 5 ou 10 indica multicolinearidade forte.
# ------------------------------------------------------------------------------
calcular_vif <- function(dados) {

  R <- stats::cor(dados)

  inversa <- tryCatch(solve(R), error = function(e) NULL)
  if (is.null(inversa)) {
    # colinearidade perfeita: a matriz não tem inversa; usa uma pequena folga
    inversa <- solve(R + diag(1e-8, ncol(R)))
  }

  vif <- diag(inversa)
  names(vif) <- colnames(dados)
  return(vif)
}


# ------------------------------------------------------------------------------
# vif_iterativo()
#   Remove, uma de cada vez, a variável de MAIOR VIF e recalcula, até todas
#   ficarem com VIF menor ou igual a 'limite'.
#   Devolve $tabela (variável, VIF inicial, VIF final, situação) e $mantidas.
# ------------------------------------------------------------------------------
vif_iterativo <- function(dados, limite = 10) {

  restantes    <- names(dados)
  vif_inicial  <- calcular_vif(dados)
  removidas    <- character(0)
  vif_removida <- numeric(0)
  vif_atual    <- vif_inicial

  while (length(restantes) > 2 && max(vif_atual) > limite) {
    pior         <- names(which.max(vif_atual))
    removidas    <- c(removidas, pior)
    vif_removida <- c(vif_removida, max(vif_atual))
    restantes    <- setdiff(restantes, pior)
    vif_atual    <- calcular_vif(dados[, restantes, drop = FALSE])
  }

  vif_final <- rep(NA_real_, length(vif_inicial))
  names(vif_final) <- names(vif_inicial)
  vif_final[restantes] <- vif_atual[restantes]

  situacao <- rep("mantida", length(vif_inicial))
  names(situacao) <- names(vif_inicial)
  situacao[removidas] <- paste0("removida (", seq_along(removidas), "ª)")

  tabela <- data.frame(
    variavel    = names(vif_inicial),
    vif_inicial = arredondar_sdm(as.numeric(vif_inicial)),
    vif_final   = arredondar_sdm(as.numeric(vif_final)),
    situacao    = as.character(situacao),
    stringsAsFactors = FALSE
  )

  return(list(tabela = tabela, mantidas = restantes, limite = limite))
}


# ------------------------------------------------------------------------------
# filtro_correlacao()
#   Enquanto existir um par de variáveis com |correlação| maior que 'limiar':
#   pega o par mais correlacionado e remove a variável que, em média, é mais
#   correlacionada com todas as outras.
#   Devolve $tabela (variável, situação, motivo) e $mantidas.
# ------------------------------------------------------------------------------
filtro_correlacao <- function(dados, limiar = 0.7) {

  R <- abs(stats::cor(dados))
  diag(R) <- 0

  restantes <- colnames(R)
  motivo    <- setNames(rep("", length(restantes)), restantes)
  situacao  <- setNames(rep("mantida", length(restantes)), restantes)

  repeat {
    sub <- R[restantes, restantes, drop = FALSE]
    if (length(restantes) < 2 || max(sub) <= limiar) break

    posicao <- which(sub == max(sub), arr.ind = TRUE)[1, ]
    a <- restantes[posicao[1]]
    b <- restantes[posicao[2]]

    remover <- if (mean(sub[a, ]) >= mean(sub[b, ])) a else b
    outro   <- if (remover == a) b else a

    situacao[remover] <- "removida"
    motivo[remover]   <- paste0("|r| = ", round(max(sub), 2), " com ", outro)
    restantes <- setdiff(restantes, remover)
  }

  tabela <- data.frame(variavel = names(situacao), situacao = as.character(situacao),
                       motivo = as.character(motivo), stringsAsFactors = FALSE)

  return(list(tabela = tabela, mantidas = restantes, limiar = limiar))
}


# ------------------------------------------------------------------------------
# cluster_variaveis()
#   Agrupa as variáveis por semelhança, usando a distância  1 - |correlação|
#   (método de ligação "average"). O dendrograma é cortado na altura
#   1 - limiar: variáveis no mesmo grupo têm |r| médio de pelo menos 'limiar'.
#   De cada grupo, a variável REPRESENTANTE é a mais correlacionada, em média,
#   com as outras do grupo.
#   Devolve $tabela (variável, grupo, representante), $mantidas (os
#   representantes) e $arvore (para desenhar o dendrograma).
# ------------------------------------------------------------------------------
cluster_variaveis <- function(dados, limiar = 0.7) {

  R      <- abs(stats::cor(dados))
  arvore <- stats::hclust(stats::as.dist(1 - R), method = "average")
  grupos <- stats::cutree(arvore, h = 1 - limiar)

  representante <- setNames(rep(FALSE, length(grupos)), names(grupos))
  for (g in unique(grupos)) {
    membros <- names(grupos)[grupos == g]
    if (length(membros) == 1) {
      representante[membros] <- TRUE
    } else {
      media_r <- sapply(membros, function(v) mean(R[v, setdiff(membros, v)]))
      representante[names(which.max(media_r))] <- TRUE
    }
  }

  tabela <- data.frame(variavel = names(grupos), grupo = as.integer(grupos),
                       representante = as.logical(representante),
                       stringsAsFactors = FALSE)
  tabela <- tabela[order(tabela$grupo), ]

  return(list(tabela = tabela, mantidas = names(representante)[representante],
              arvore = arvore, limiar = limiar))
}


# ------------------------------------------------------------------------------
# regra_eixos_pca()
#   Define quantos eixos (componentes) da PCA ficam. Ordem de prioridade:
#     1) 'n_eixos' informado          -> usa esse número;
#     2) 'variancia_alvo' informada   -> eixos até somar essa variância (0 a 1);
#     3) nada informado (regra automática): com p variáveis, cada eixo "médio"
#        vale 1/p da variância. Ficam os eixos que somam  1 - 1/p.
#        Ex.: 20 variáveis -> 1/20 = 5% -> eixos que somam 95%;
#             8 variáveis  -> 1/8 = 12,5% -> eixos que somam 87,5%.
#   Devolve list(n, alvo, motivo).
# ------------------------------------------------------------------------------
regra_eixos_pca <- function(autovalores, n_eixos = NULL, variancia_alvo = NULL) {

  p <- length(autovalores)
  acumulada <- cumsum(autovalores) / sum(autovalores)

  if (!is.null(n_eixos)) {
    n <- min(max(1, round(n_eixos)), p)
    return(list(n = n, alvo = acumulada[n],
                motivo = paste0("número de eixos definido por você (", n, ")")))
  }

  if (!is.null(variancia_alvo)) {
    alvo   <- variancia_alvo
    motivo <- paste0("variância alvo definida por você (", round(alvo * 100, 1), "%)")
  } else {
    alvo   <- 1 - 1 / p
    motivo <- paste0("regra automática: 100% - 100/", p, " = ", round(alvo * 100, 1), "%")
  }

  n <- which(acumulada >= alvo - 1e-9)[1]
  if (is.na(n)) n <- p

  list(n = n, alvo = alvo, motivo = motivo)
}


# ------------------------------------------------------------------------------
# calcular_pca()
#   Análise de componentes principais sobre a matriz de CORRELAÇÃO do conjunto
#   (scale. = TRUE): cada variável é padronizada com a média e o desvio do
#   próprio conjunto, então nenhuma pesa mais por variar mais.
#     - preditoras: nada muda (já estão em z-score, com desvio 1);
#     - cenários: chegam escalonados com a média e o desvio das PREDITORAS. Sem
#       esta padronização, uma variável que passou a variar mais no cenário
#       ganharia peso no diagnóstico.
#   Número de eixos: ver regra_eixos_pca() (n_eixos > variancia_alvo > regra
#   automática 1 - 1/p).
#   Aqui a PCA serve de diagnóstico e de ranking de importância (ela NÃO
#   escolhe variáveis):
#     - 'importancia' de cada variável: soma, nos eixos retidos, de
#       carga^2 x variância do eixo, dividida pela variância retida. Os valores
#       somam 1 entre as variáveis.
#   Devolve $tabela (variância por componente), $cargas, $importancia,
#   $n_componentes, $regra, $modelo (o prcomp, para projetar depois).
# ------------------------------------------------------------------------------
calcular_pca <- function(dados, variancia_alvo = NULL, n_eixos = NULL) {

  if (ncol(dados) < 2) stop("A PCA precisa de pelo menos 2 variáveis.", call. = FALSE)

  pca <- stats::prcomp(dados, center = TRUE, scale. = TRUE)

  autovalores <- pca$sdev^2
  tabela <- data.frame(
    componente  = paste0("PC", seq_along(autovalores)),
    autovalor   = arredondar_sdm(autovalores),
    variancia   = arredondar_sdm(autovalores / sum(autovalores)),
    acumulada   = arredondar_sdm(cumsum(autovalores) / sum(autovalores)),
    stringsAsFactors = FALSE
  )

  regra  <- regra_eixos_pca(autovalores, n_eixos, variancia_alvo)
  n_comp <- regra$n

  cargas <- pca$rotation[, seq_len(n_comp), drop = FALSE]
  pesos  <- autovalores[seq_len(n_comp)]
  nota   <- as.numeric((cargas^2) %*% pesos) / sum(pesos)

  importancia <- data.frame(
    variavel    = rownames(cargas),
    importancia = arredondar_sdm(nota),
    stringsAsFactors = FALSE
  )
  # eixo em que a variável tem a maior |carga| (informativo)
  importancia$eixo_de_maior_carga <- paste0("PC", apply(abs(cargas), 1, which.max))
  importancia <- importancia[order(-importancia$importancia), ]

  return(list(tabela = tabela, cargas = arredondar_sdm(pca$rotation),
              importancia = importancia, mantidas = colnames(dados),
              seleciona_variaveis = FALSE, modelo = pca,
              n_componentes = n_comp, regra = regra))
}


# ------------------------------------------------------------------------------
# salvar_figura_cluster() / salvar_figura_pca()
#   Salvam em PNG o dendrograma do cluster e a variância explicada da PCA.
# ------------------------------------------------------------------------------
salvar_figura_cluster <- function(resultado, arquivo) {
  grDevices::png(arquivo, width = 1400, height = 900, res = 130)
  graphics::plot(resultado$arvore, main = "Cluster das variáveis (1 - |r|)",
                 xlab = "", sub = "", ylab = "Distância (1 - |r|)")
  graphics::abline(h = 1 - resultado$limiar, lty = 2, col = "red")
  grDevices::dev.off()
}

salvar_figura_pca <- function(resultado, arquivo) {
  grDevices::png(arquivo, width = 1200, height = 800, res = 130)
  graphics::barplot(resultado$tabela$variancia * 100,
                    names.arg = resultado$tabela$componente,
                    ylab = "Variância explicada (%)", las = 2,
                    main = "PCA: variância explicada por componente")
  graphics::lines(seq_along(resultado$tabela$acumulada) * 1.2 - 0.5,
                  resultado$tabela$acumulada * 100, type = "b", col = "red")
  grDevices::dev.off()
}


# ------------------------------------------------------------------------------
# rodar_testes_sem_resposta()
#   Roda, em CADA conjunto (preditoras e cenários, separadamente), os testes
#   escolhidos que não precisam de resposta, mostra um resumo e salva as
#   tabelas (CSV) e figuras (PNG) em  pasta_saida/<conjunto>/.
#   Devolve uma lista:  resultados[[conjunto]][[metodo]]
# ------------------------------------------------------------------------------
rodar_testes_sem_resposta <- function(dados_testes, metodos, pasta_saida,
                                      limite_vif = 10, limiar_filtro = 0.7,
                                      limiar_cluster = 0.7, variancia_pca = NULL,
                                      n_eixos_pca = NULL) {

  metodos <- intersect(c("vif", "filtro_correlacao", "cluster", "pca"), metodos)
  resultados <- list()

  if (length(metodos) == 0) {
    message("Nenhum teste sem resposta foi escolhido.")
    return(resultados)
  }

  gravar <- function(tabela, pasta, nome) {
    utils::write.csv(tabela, file.path(pasta, nome), row.names = FALSE,
                     fileEncoding = "UTF-8")
  }

  for (conjunto in names(dados_testes)) {
    dados <- dados_testes[[conjunto]]$dados
    pasta <- file.path(pasta_saida, conjunto)
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

    cat("\n================ TESTES SEM RESPOSTA:", conjunto, "================\n")
    cat("Variáveis:", ncol(dados), "| Células:", nrow(dados), "\n\n")
    resultados[[conjunto]] <- list()

    if ("vif" %in% metodos) {
      r <- vif_iterativo(dados, limite_vif)
      resultados[[conjunto]][["vif"]] <- r
      gravar(r$tabela, pasta, "vif.csv")
      cat("VIF (limite", limite_vif, "): mantém", length(r$mantidas), "de", ncol(dados),
          "| VIF inicial máximo:", max(r$tabela$vif_inicial), "\n")
    }

    if ("filtro_correlacao" %in% metodos) {
      r <- filtro_correlacao(dados, limiar_filtro)
      resultados[[conjunto]][["filtro_correlacao"]] <- r
      gravar(r$tabela, pasta, "filtro_correlacao.csv")
      cat("Filtro por correlação (|r| >", limiar_filtro, "): mantém", length(r$mantidas),
          "de", ncol(dados), "\n")
    }

    if ("cluster" %in% metodos) {
      r <- cluster_variaveis(dados, limiar_cluster)
      resultados[[conjunto]][["cluster"]] <- r
      gravar(r$tabela, pasta, "cluster_variaveis.csv")
      salvar_figura_cluster(r, file.path(pasta, "cluster_dendrograma.png"))
      cat("Cluster (|r| >=", limiar_cluster, "): ", max(r$tabela$grupo), "grupos; mantém",
          length(r$mantidas), "representantes\n")
    }

    if ("pca" %in% metodos) {
      r <- calcular_pca(dados, variancia_pca, n_eixos_pca)
      resultados[[conjunto]][["pca"]] <- r
      gravar(r$tabela, pasta, "pca_variancia.csv")
      gravar(data.frame(variavel = rownames(r$cargas), r$cargas), pasta, "pca_cargas.csv")
      gravar(r$importancia, pasta, "pca_importancia.csv")
      salvar_figura_pca(r, file.path(pasta, "pca_variancia.png"))
      cat("PCA:", r$n_componentes, "eixos de", ncol(dados), "(", r$regra$motivo, ");",
          "variância acumulada:", round(r$tabela$acumulada[r$n_componentes] * 100, 1),
          "%\n     gera ranking de importância (não seleciona variáveis)\n")
    }

    cat("Resultados salvos em:", pasta, "\n")
    cat("====================================================\n")
  }

  return(resultados)
}
