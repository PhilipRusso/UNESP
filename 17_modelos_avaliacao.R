# ==============================================================================
#  FUNÇÕES - Modelos: avaliação (métricas, limiares e validação cruzada)
# ------------------------------------------------------------------------------
#  Métricas (presença contra background, que faz o papel de ausência):
#    AUC     : chance de uma presença ter previsão maior que um ponto de
#              background (0,5 = acaso; 1 = perfeito).
#    TSS     : sensibilidade + especificidade - 1, no limiar que maximiza esse
#              valor (0 = acaso; 1 = perfeito).
#    Kappa   : concordância entre o mapa binário e os dados, descontado o
#              acaso, no limiar que maximiza o Kappa. Depende da proporção
#              presença/background, por isso o TSS também é mostrado.
#    Boyce   : correlação de Spearman entre a adequabilidade e a razão
#              "presenças observadas / esperadas" (-1 a 1).
#    Omissão : proporção das presenças de TESTE que ficam abaixo do limiar
#              calculado no TREINO: MTP (menor valor entre as presenças de
#              treino; o esperado é 0) e P10 (10% das presenças de treino ficam
#              de fora; o esperado é 0,10). Omissão bem acima do esperado
#              indica sobreajuste.
#
#  Esquemas de validação cruzada:
#    "blocos"    : 4 blocos espaciais (o modelo é testado numa região que não viu)
#    "kfold"     : k grupos sorteados
#    "jackknife" : deixa UMA presença de fora por vez (n modelos para n
#                  presenças). Indicado para poucas presenças. Inclui o teste
#                  de Pearson et al. (2007): a chance de acertar tantas
#                  presenças deixadas de fora só pelo tamanho da área prevista.
#
#  Ajuste do Maxent: testa combinações de features e regularização com a
#  mesma validação cruzada e escolhe a melhor para cada espécie
#  (selecionar_ajuste_maxent()).
# ==============================================================================


# ---- métricas ----------------------------------------------------------------

# AUC pela estatística de Mann-Whitney (ordenação dos valores)
calcular_auc <- function(p_presenca, p_ausencia) {
  p_presenca <- p_presenca[!is.na(p_presenca)]
  p_ausencia <- p_ausencia[!is.na(p_ausencia)]
  n1 <- length(p_presenca)
  n0 <- length(p_ausencia)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  postos <- rank(c(p_presenca, p_ausencia))
  (sum(postos[seq_len(n1)]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ------------------------------------------------------------------------------
# curva_limiares()
#   Para cada limiar possível: sensibilidade (presenças >= limiar),
#   especificidade (background < limiar), TSS, Kappa e acurácia (proporção de acertos).
# ------------------------------------------------------------------------------
curva_limiares <- function(p_presenca, p_ausencia) {
  p1 <- sort(p_presenca[!is.na(p_presenca)])
  p0 <- sort(p_ausencia[!is.na(p_ausencia)])
  n1 <- length(p1)
  n0 <- length(p0)
  if (n1 == 0 || n0 == 0) return(NULL)

  limiar <- sort(unique(c(p1, p0)))
  fn <- findInterval(limiar, p1, left.open = TRUE)   # presenças abaixo do limiar
  tn <- findInterval(limiar, p0, left.open = TRUE)   # background abaixo do limiar
  tp <- n1 - fn
  fp <- n0 - tn
  n  <- n1 + n0
  po <- (tp + tn) / n
  pe <- ((tp + fp) * (tp + fn) + (fn + tn) * (fp + tn)) / n^2
  data.frame(limiar = limiar, sensibilidade = tp / n1, especificidade = tn / n0,
             tss = tp / n1 + tn / n0 - 1, kappa = ifelse(pe < 1, (po - pe) / (1 - pe), 0),
             acuracia = po)
}

# Índice de Boyce contínuo (Hirzel et al. 2006), como no ecospat: janelas
# móveis de 10% da amplitude; F = proporção das presenças / do background.
calcular_boyce <- function(p_presenca, p_fundo, n_janelas = 101, largura = 0.1) {
  p_presenca <- p_presenca[!is.na(p_presenca)]
  p_fundo    <- p_fundo[!is.na(p_fundo)]
  if (length(p_presenca) < 2 || length(p_fundo) < 2) return(NA_real_)

  minimo <- min(c(p_presenca, p_fundo))
  maximo <- max(c(p_presenca, p_fundo))
  if (maximo - minimo < 1e-12) return(NA_real_)

  w      <- (maximo - minimo) * largura
  inicio <- seq(minimo, maximo - w, length.out = n_janelas)
  P <- vapply(inicio, function(a) mean(p_presenca >= a & p_presenca <= a + w), numeric(1))
  E <- vapply(inicio, function(a) mean(p_fundo >= a & p_fundo <= a + w), numeric(1))
  razao <- P / E
  meio  <- inicio + w / 2

  ok     <- is.finite(razao)
  razao  <- razao[ok]
  meio   <- meio[ok]
  manter <- c(TRUE, diff(razao) != 0)
  razao  <- razao[manter]
  meio   <- meio[manter]
  if (length(razao) < 3 || stats::sd(razao) == 0) return(NA_real_)

  stats::cor(razao, meio, method = "spearman")
}

# ------------------------------------------------------------------------------
# avaliar_previsoes()
#   Todas as métricas de um vetor de previsões e da resposta (1/0), com os
#   limiares de maior TSS, de maior Kappa e de sensibilidade = especificidade.
# ------------------------------------------------------------------------------
avaliar_previsoes <- function(previsao, presenca) {
  p1 <- previsao[presenca == 1]
  p0 <- previsao[presenca == 0]
  curva <- curva_limiares(p1, p0)
  if (is.null(curva)) {
    return(data.frame(auc = NA, tss = NA, kappa = NA, boyce = NA, limiar_max_tss = NA,
                      limiar_max_kappa = NA, limiar_sens_espec = NA,
                      sensibilidade = NA, especificidade = NA))
  }
  i_tss <- which.max(curva$tss)
  data.frame(auc = calcular_auc(p1, p0), tss = curva$tss[i_tss],
             kappa = max(curva$kappa), boyce = calcular_boyce(p1, p0),
             limiar_max_tss = curva$limiar[i_tss],
             limiar_max_kappa = curva$limiar[which.max(curva$kappa)],
             limiar_sens_espec = curva$limiar[which.min(abs(curva$sensibilidade - curva$especificidade))],
             sensibilidade = curva$sensibilidade[i_tss],
             especificidade = curva$especificidade[i_tss])
}

# ------------------------------------------------------------------------------
# metricas_no_limiar()
#   Omissão, sensibilidade, especificidade, TSS e Kappa num limiar dado.
# ------------------------------------------------------------------------------
metricas_no_limiar <- function(p_presenca, p_ausencia, limiar) {
  p1 <- p_presenca[!is.na(p_presenca)]
  p0 <- p_ausencia[!is.na(p_ausencia)]
  if (is.na(limiar) || length(p1) == 0) {
    return(data.frame(omissao = NA, sensibilidade = NA, especificidade = NA, tss = NA, kappa = NA))
  }
  tp <- sum(p1 >= limiar); fn <- sum(p1 < limiar)
  tn <- sum(p0 < limiar);  fp <- sum(p0 >= limiar)
  n  <- tp + fn + tn + fp
  sens  <- tp / (tp + fn)
  espec <- if (tn + fp > 0) tn / (tn + fp) else NA
  po <- (tp + tn) / n
  pe <- ((tp + fp) * (tp + fn) + (fn + tn) * (fp + tn)) / n^2
  data.frame(omissao = 1 - sens, sensibilidade = sens, especificidade = espec,
             tss = sens + espec - 1, kappa = if (pe < 1) (po - pe) / (1 - pe) else NA)
}


# ---- limiares ----------------------------------------------------------------

# menor adequabilidade entre as presenças de treino (MTP)
limiar_mtp <- function(p_treino_presencas) {
  p <- p_treino_presencas[!is.na(p_treino_presencas)]
  if (length(p) == 0) NA_real_ else min(p)
}

# valor que deixa 10% das presenças de treino de fora (P10), como no ENMeval
limiar_p10 <- function(p_treino_presencas) {
  p <- sort(p_treino_presencas[!is.na(p_treino_presencas)], decreasing = TRUE)
  n <- length(p)
  if (n == 0) return(NA_real_)
  p[max(1, if (n < 10) floor(0.9 * n) else ceiling(0.9 * n))]
}

# confere os nomes dos métodos de limiar
normalizar_metodos_limiar <- function(metodos) {
  metodos   <- unique(tolower(trimws(metodos)))
  validos   <- c("max_tss", "max_kappa", "sens_espec", "mtp", "p10", "min_roc", "max_acuracia",
                 "media_pres", "fixo")
  invalidos <- setdiff(metodos, validos)
  if (length(invalidos) > 0) {
    stop("Limiar(es) inválido(s): ", paste(invalidos, collapse = ", "),
         "\nOpções: ", paste(validos, collapse = ", "), call. = FALSE)
  }
  metodos
}

# ------------------------------------------------------------------------------
# limiares_de_previsoes()
#   Calcula os limiares dos mapas binários a partir das previsões da validação
#   cruzada ('oof': cada ponto previsto por um modelo que não o viu no ajuste):
#     max_tss, max_kappa, sens_espec, min_roc, max_acuracia -> presenças e background;
#     mtp, p10, media_pres                                  -> presenças;
#     fixo                            -> 'limiar_fixo'.
#   Por que mtp e p10 não usam as previsões de treino: modelos que "decoram" os
#   pontos de treino (o Domain sempre dá 1 a eles; RF e BRT chegam perto) dariam
#   limiares altos demais e mapas quase vazios. As previsões de treino ('treino')
#   só são usadas se faltar a validação cruzada.
# ------------------------------------------------------------------------------
limiares_de_previsoes <- function(oof, treino, presenca, metodos, limiar_fixo = 0.5) {
  v <- setNames(rep(NA_real_, length(metodos)), metodos)
  if (!is.null(oof) && any(!is.na(oof))) {
    curva <- curva_limiares(oof[presenca == 1], oof[presenca == 0])
    if (!is.null(curva)) {
      if ("max_tss"    %in% metodos) v["max_tss"]    <- curva$limiar[which.max(curva$tss)]
      if ("max_kappa"  %in% metodos) v["max_kappa"]  <- curva$limiar[which.max(curva$kappa)]
      if ("sens_espec" %in% metodos) v["sens_espec"] <- curva$limiar[which.min(abs(curva$sensibilidade - curva$especificidade))]
      if ("min_roc"    %in% metodos) v["min_roc"]    <- curva$limiar[which.min((1 - curva$sensibilidade)^2 + (1 - curva$especificidade)^2)]
      if ("max_acuracia" %in% metodos) v["max_acuracia"] <- curva$limiar[which.max(curva$acuracia)]
    }
  }
  presencas <- if (!is.null(oof) && any(!is.na(oof[presenca == 1]))) oof[presenca == 1] else treino[presenca == 1]
  if ("mtp"  %in% metodos) v["mtp"]  <- limiar_mtp(presencas)
  if ("p10"  %in% metodos) v["p10"]  <- limiar_p10(presencas)
  if ("media_pres" %in% metodos) v["media_pres"] <- mean(presencas, na.rm = TRUE)
  if ("fixo" %in% metodos) v["fixo"] <- limiar_fixo
  v
}


# ---- esquemas de validação ---------------------------------------------------

# confere os nomes dos esquemas ("aleatoria" é o nome antigo do "kfold")
normalizar_validacoes <- function(tipo_validacao) {
  v <- tolower(trimws(tipo_validacao))
  v[v == "aleatoria"] <- "kfold"
  v[v %in% c("holdout", "hold-out", "treino_teste")] <- "teste"
  invalidos <- setdiff(v, c("blocos", "kfold", "jackknife", "teste"))
  if (length(invalidos) > 0) {
    stop("Validação inválida: ", paste(invalidos, collapse = ", "),
         "\nOpções: blocos, kfold, jackknife, teste", call. = FALSE)
  }
  unique(v)
}

# chance de acertar 'acertos' ou mais presenças deixadas de fora só pelo
# tamanho da área prevista em cada modelo (distribuição binomial de Poisson)
p_valor_jackknife <- function(acertos, areas) {
  dist <- 1
  for (a in areas) dist <- c(dist * (1 - a), 0) + c(0, dist * a)
  sum(dist[(acertos + 1):length(dist)])
}

# ------------------------------------------------------------------------------
# validar_modelo()
#   Validação de UM algoritmo em UMA espécie por "blocos", "kfold" ou "teste"
#   (hold-out: treino/teste fixos, coluna grupo_teste com 0 = treino e 1 = teste)
#   (os grupos estão nas colunas grupo_blocos / grupo_kfold / grupo_teste dos dados). Em cada grupo:
#   ajusta sem ele, prevê nele e calcula as métricas e a omissão (limiares MTP e
#   P10 do treino). Devolve $grupos, $geral (todas as previsões fora do treino
#   juntas), $previsoes e $erros.
# ------------------------------------------------------------------------------
validar_modelo <- function(codigo, dados, preditores, ajustes, validacao = "blocos",
                           semente = 123) {

  if (validacao == "jackknife") {
    return(validar_jackknife(codigo, dados, preditores, ajustes, semente))
  }
  grupo     <- dados[[paste0("grupo_", validacao)]]
  previsoes <- rep(NA_real_, nrow(dados))
  linhas    <- list()
  erros     <- character(0)

  # "teste" (hold-out): um único ajuste com o treino (grupo_teste = 0), avaliado
  # nas linhas de teste (grupo_teste = 1); as previsões do treino ficam NA
  grupos_k <- if (validacao == "teste") 1L else sort(unique(grupo))
  for (k in grupos_k) {
    treino <- dados[grupo != k, , drop = FALSE]
    teste  <- dados[grupo == k, , drop = FALSE]

    r <- tryCatch({
      objeto  <- ajustar_modelo(codigo, treino, preditores, ajustes, semente)
      p_teste <- prever_modelo(objeto, teste)
      p_trein <- prever_modelo(objeto, treino[treino$presenca == 1, , drop = FALSE])
      list(teste = p_teste, mtp = limiar_mtp(p_trein), p10 = limiar_p10(p_trein))
    }, error = function(e) {
      erros <<- c(erros, conditionMessage(e))
      NULL
    })

    if (is.null(r)) {
      linhas[[length(linhas) + 1]] <- data.frame(grupo = k, auc = NA, tss = NA, kappa = NA,
                                                 boyce = NA, omissao_mtp = NA, omissao_p10 = NA)
      next
    }
    previsoes[grupo == k] <- r$teste
    m  <- avaliar_previsoes(r$teste, teste$presenca)
    p1 <- r$teste[teste$presenca == 1]
    linhas[[length(linhas) + 1]] <- data.frame(
      grupo = k, auc = m$auc, tss = m$tss, kappa = m$kappa, boyce = m$boyce,
      omissao_mtp = if (length(p1)) mean(p1 < r$mtp) else NA,
      omissao_p10 = if (length(p1)) mean(p1 < r$p10) else NA)
  }

  list(grupos = do.call(rbind, linhas), geral = avaliar_previsoes(previsoes, dados$presenca),
       previsoes = previsoes, erros = unique(erros), jackknife = NULL)
}

# ------------------------------------------------------------------------------
# validar_jackknife()
#   Para cada presença: ajusta o modelo sem ela e vê se ela fica acima dos
#   limiares MTP e P10 do treino. A área prevista como presença é estimada
#   pela proporção do background acima do limiar.
#     - omissão = proporção das presenças deixadas de fora que o modelo errou;
#     - p-valor de Pearson et al. (2007) para MTP e P10 (p < 0,05 = o modelo
#       acerta mais do que o tamanho da área prevista explicaria);
#     - AUC de cada modelo: posição da presença deixada de fora entre as
#       previsões do background.
#   O background não sai do treino; nas métricas gerais ele usa a previsão do
#   modelo com todos os pontos.
# ------------------------------------------------------------------------------
validar_jackknife <- function(codigo, dados, preditores, ajustes, semente = 123) {

  presencas <- which(dados$presenca == 1)
  fundo     <- dados[dados$presenca == 0, , drop = FALSE]
  previsoes <- rep(NA_real_, nrow(dados))
  erros     <- character(0)

  completo <- tryCatch(ajustar_modelo(codigo, dados, preditores, ajustes, semente),
                       error = function(e) { erros <<- c(erros, conditionMessage(e)); NULL })
  if (!is.null(completo)) previsoes[dados$presenca == 0] <- prever_modelo(completo, fundo)

  linhas <- list()
  for (i in presencas) {
    r <- tryCatch({
      m     <- ajustar_modelo(codigo, dados[-i, , drop = FALSE], preditores, ajustes, semente)
      p_i   <- prever_modelo(m, dados[i, , drop = FALSE])
      p_tr  <- prever_modelo(m, dados[setdiff(presencas, i), , drop = FALSE])
      p_bg  <- prever_modelo(m, fundo)
      mtp   <- limiar_mtp(p_tr)
      p10   <- limiar_p10(p_tr)
      data.frame(grupo = i, previsao = p_i,
                 auc = mean(p_bg < p_i) + 0.5 * mean(p_bg == p_i),
                 omissao_mtp = as.numeric(p_i < mtp), area_mtp = mean(p_bg >= mtp),
                 omissao_p10 = as.numeric(p_i < p10), area_p10 = mean(p_bg >= p10))
    }, error = function(e) { erros <<- c(erros, conditionMessage(e)); NULL })
    if (!is.null(r)) {
      previsoes[i] <- r$previsao
      linhas[[length(linhas) + 1]] <- r
    }
  }

  tab <- if (length(linhas) > 0) do.call(rbind, linhas) else NULL
  jack <- NULL
  grupos <- data.frame(grupo = presencas, auc = NA, tss = NA, kappa = NA, boyce = NA,
                       omissao_mtp = NA, omissao_p10 = NA)
  if (!is.null(tab)) {
    grupos <- data.frame(grupo = tab$grupo, auc = tab$auc, tss = NA, kappa = NA, boyce = NA,
                         omissao_mtp = tab$omissao_mtp, omissao_p10 = tab$omissao_p10)
    jack <- list(p_mtp = p_valor_jackknife(sum(tab$omissao_mtp == 0), tab$area_mtp),
                 p_p10 = p_valor_jackknife(sum(tab$omissao_p10 == 0), tab$area_p10))
  }

  list(grupos = grupos, geral = avaliar_previsoes(previsoes, dados$presenca),
       previsoes = previsoes, erros = unique(erros), jackknife = jack)
}


# ---- ajuste do Maxent --------------------------------------------------------

# ------------------------------------------------------------------------------
# ajustes_da_especie()
#   Devolve 'ajustes' com as classes e o regmult do Maxent escolhidos para a
#   espécie em 'ajuste_maxent' (tabela de selecionar_ajuste_maxent()). Sem a
#   tabela, ou sem a espécie nela, devolve 'ajustes' como está.
# ------------------------------------------------------------------------------
ajustes_da_especie <- function(ajustes, ajuste_maxent = NULL, especie) {
  if (is.null(ajuste_maxent)) return(ajustes)
  linha <- ajuste_maxent[ajuste_maxent$especie == especie, , drop = FALSE]
  if (nrow(linha) == 0) return(ajustes)
  ajustes$maxent_classes <- as.character(linha$classes[1])
  ajustes$maxent_regmult <- as.numeric(linha$regmult[1])
  return(ajustes)
}


# ------------------------------------------------------------------------------
# selecionar_ajuste_maxent()
#   O Maxent muda muito com as "features" (classes) e com a regularização
#   (regmult): poucas features e regmult alto dão curvas suaves; muitas
#   features e regmult baixo dão curvas complexas, que podem sobreajustar.
#   Esta função testa todas as combinações de 'classes' x 'regmult' com a
#   MESMA validação cruzada principal (o primeiro esquema de 'validacoes', com
#   os mesmos grupos) e escolhe, para cada espécie:
#     1) a menor omissão P10 média (o modelo deixa de fora poucas presenças de
#        teste: sinal de que não está sobreajustado);
#     2) em caso de empate, a maior AUC média;
#     3) se ainda empatar, o modelo mais simples (regmult maior, menos classes).
#   É o critério "sequencial" usado no pacote ENMeval.
#   Salva em pasta_saida:
#     ajuste_maxent_testes.csv -> resultado de todas as combinações
#     ajuste_maxent.csv        -> a combinação escolhida para cada espécie
#   Devolve a tabela escolhida: especie, classes, regmult, omissao_p10, auc.
# ------------------------------------------------------------------------------
selecionar_ajuste_maxent <- function(dados_modelos, ajustes, validacoes = "blocos",
                                     classes = c("l", "lq", "h", "lqh", "lqph"),
                                     regmult = c(0.5, 1, 2, 3, 4),
                                     pasta_saida, semente = 123) {

  validacao   <- normalizar_validacoes(validacoes)[1]
  combinacoes <- expand.grid(classes = classes, regmult = regmult, stringsAsFactors = FALSE)
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  media <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

  testes     <- list()
  escolhidos <- list()

  cat("\n================ AJUSTE DO MAXENT ================\n")
  cat(nrow(combinacoes), "combinações (classes x regmult) | validação:", validacao, "\n")

  for (especie in names(dados_modelos)) {
    dados      <- dados_modelos[[especie]]$dados
    preditores <- dados_modelos[[especie]]$preditores
    cat(especie, "")

    linhas <- list()
    for (i in seq_len(nrow(combinacoes))) {
      teste <- ajustes
      teste$maxent_classes <- combinacoes$classes[i]
      teste$maxent_regmult <- combinacoes$regmult[i]
      v <- validar_modelo("maxent", dados, preditores, teste, validacao, semente)
      linhas[[i]] <- data.frame(especie = especie, classes = combinacoes$classes[i],
                                regmult = combinacoes$regmult[i],
                                omissao_p10 = media(v$grupos$omissao_p10),
                                auc = media(v$grupos$auc),
                                grupos_ok = sum(!is.na(v$grupos$auc)),
                                grupos_total = nrow(v$grupos))
      cat(".")
    }
    cat("\n")
    tabela <- do.call(rbind, linhas)
    testes[[especie]] <- tabela

    validas <- tabela[!is.na(tabela$omissao_p10) & !is.na(tabela$auc), , drop = FALSE]
    if (nrow(validas) == 0) {
      warning("Maxent: nenhuma combinação rodou para '", especie, "'. Serão usados ",
              "maxent_classes e maxent_regmult de 'ajustes_modelos'.",
              call. = FALSE, immediate. = TRUE)
      next
    }
    ordem <- order(round(validas$omissao_p10, 6), -validas$auc, -validas$regmult,
                   nchar(validas$classes))
    escolhidos[[especie]] <- validas[ordem[1], c("especie", "classes", "regmult",
                                                 "omissao_p10", "auc")]
  }

  testes <- do.call(rbind, testes)
  rownames(testes) <- NULL
  utils::write.csv(testes, file.path(pasta_saida, "ajuste_maxent_testes.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")

  resultado <- NULL
  if (length(escolhidos) > 0) {
    resultado <- do.call(rbind, escolhidos)
    rownames(resultado) <- NULL
    utils::write.csv(resultado, file.path(pasta_saida, "ajuste_maxent.csv"),
                     row.names = FALSE, fileEncoding = "UTF-8")
    mostrar <- resultado
    mostrar$omissao_p10 <- arredondar_sdm(mostrar$omissao_p10)
    mostrar$auc         <- arredondar_sdm(mostrar$auc)
    cat("\nCombinação escolhida (menor omissão P10; empate -> maior AUC):\n")
    print(mostrar, row.names = FALSE)
  }
  cat("Testes salvos em:", pasta_saida, "\n")
  cat("==================================================\n")

  return(resultado)
}


# ---- avaliação de todos os modelos -------------------------------------------

# ------------------------------------------------------------------------------
# avaliar_modelos()
#   Roda cada esquema de validação em 'validacoes' para cada espécie e cada
#   algoritmo (menos o JSDM, que tem seção própria). O PRIMEIRO esquema é o
#   principal: as previsões dele dão os limiares e o TSS usado no consenso.
#   Se 'ajuste_maxent' (de selecionar_ajuste_maxent()) for informado, o Maxent
#   de cada espécie usa as classes e o regmult escolhidos para ela.
#   Salva em pasta_saida:
#     avaliacao_grupos.csv  -> métricas de cada grupo (ou de cada presença,
#                              no jackknife)
#     avaliacao_resumo.csv  -> médias das métricas por esquema e modelo
#     avaliacao.rds         -> tudo, para as próximas seções
#   Devolve list(resumo, grupos, previsoes, principal, validacoes).
# ------------------------------------------------------------------------------
avaliar_modelos <- function(dados_modelos, modelos, ajustes, validacoes = "blocos",
                            pasta_saida, semente = 123, ajuste_maxent = NULL,
                            preditores = NULL) {

  modelos    <- setdiff(modelos, "jsdm")
  validacoes <- normalizar_validacoes(validacoes)
  if ("teste" %in% validacoes &&
      !all(vapply(dados_modelos, function(d) "grupo_teste" %in% names(d$dados), logical(1)))) {
    warning("Validação \"teste\" ignorada: 'particao_teste' não foi definido ao preparar os dados.",
            call. = FALSE, immediate. = TRUE)
    validacoes <- setdiff(validacoes, "teste")
  }
  if (length(validacoes) == 0) stop("Nenhuma validação restou para avaliar.", call. = FALSE)
  if (length(modelos) == 0) stop("Nenhum modelo para avaliar (além do JSDM).", call. = FALSE)
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  media <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  dp    <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else stats::sd(x, na.rm = TRUE)

  tab_grupos <- list()
  tab_resumo <- list()
  previsoes  <- list()

  for (especie in names(dados_modelos)) {
    dados      <- dados_modelos[[especie]]$dados
    preditores_sp <- dados_modelos[[especie]]$preditores
    ajustes_sp <- ajustes_da_especie(ajustes, ajuste_maxent, especie)
    previsoes[[especie]] <- data.frame(presenca = dados$presenca)

    for (validacao in validacoes) {
      cat("\n================ AVALIAÇÃO:", especie, "-", validacao, "================\n")
      if (validacao == "jackknife") {
        cat("(", sum(dados$presenca == 1), "modelos por algoritmo, um para cada presença)\n")
      }
      linhas_sp <- list()

      for (codigo in modelos) {
        cat(sprintf("  %-12s", codigo))
        tempo <- system.time(v <- validar_modelo(codigo, dados, preditores_sp, ajustes_sp, validacao, semente))
        cat(sprintf(" (%.0f s)\n", tempo[["elapsed"]]))
        if (length(v$erros) > 0) cat("    falhou: ", v$erros[1], "\n", sep = "")

        if (validacao == validacoes[1]) previsoes[[especie]][[codigo]] <- v$previsoes
        jk <- validacao == "jackknife"
        tab_grupos[[length(tab_grupos) + 1]] <- data.frame(especie = especie, validacao = validacao,
                                                           modelo = codigo, v$grupos)
        linha <- data.frame(
          especie = especie, validacao = validacao, modelo = codigo,
          auc_media   = media(v$grupos$auc), auc_dp = dp(v$grupos$auc),
          tss_media   = if (jk) v$geral$tss   else media(v$grupos$tss),
          tss_dp      = if (jk) NA            else dp(v$grupos$tss),
          kappa_media = if (jk) v$geral$kappa else media(v$grupos$kappa),
          boyce_media = if (jk) v$geral$boyce else media(v$grupos$boyce),
          omissao_mtp = media(v$grupos$omissao_mtp),
          omissao_p10 = media(v$grupos$omissao_p10),
          p_jackknife_mtp = if (jk && !is.null(v$jackknife)) v$jackknife$p_mtp else NA,
          p_jackknife_p10 = if (jk && !is.null(v$jackknife)) v$jackknife$p_p10 else NA,
          limiar_max_tss = v$geral$limiar_max_tss,
          grupos_ok = sum(!is.na(v$grupos$auc)), grupos_total = nrow(v$grupos))
        tab_resumo[[length(tab_resumo) + 1]] <- linha
        linhas_sp[[length(linhas_sp) + 1]]   <- linha
      }

      mostrar <- do.call(rbind, linhas_sp)
      colunas <- c("modelo", "auc_media", "tss_media", "kappa_media", "boyce_media",
                   "omissao_mtp", "omissao_p10",
                   if (validacao == "jackknife") c("p_jackknife_mtp", "p_jackknife_p10"))
      mostrar <- mostrar[order(-mostrar$tss_media), colunas]
      mostrar[, -1] <- arredondar_sdm(mostrar[, -1])
      names(mostrar) <- sub("_media", "", names(mostrar))
      cat("\n")
      print(mostrar, row.names = FALSE)
    }
  }

  resultado <- list(resumo = do.call(rbind, tab_resumo), grupos = do.call(rbind, tab_grupos),
                    previsoes = previsoes, principal = validacoes[1], validacoes = validacoes)
  utils::write.csv(acrescentar_preditores(resultado$grupos, preditores),
                   file.path(pasta_saida, "avaliacao_grupos.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(acrescentar_preditores(resultado$resumo, preditores),
                   file.path(pasta_saida, "avaliacao_resumo.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  saveRDS(resultado, file.path(pasta_saida, "avaliacao.rds"))
  cat("\nValidação principal (limiares e consenso):", validacoes[1], "\n")
  cat("Avaliação salva em:", pasta_saida, "\n")

  return(resultado)
}
