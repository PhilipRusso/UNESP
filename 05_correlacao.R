# ==============================================================================
#  FUNÇÕES - Correlação entre as variáveis ambientais
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
#  A correlação pode ser calculada com TODAS as células da área de estudo
#  (padrão) ou com uma AMOSTRA aleatória de células (mais rápido, útil quando
#  a área é muito grande).
# ==============================================================================


# ------------------------------------------------------------------------------
# calcular_correlacao()
#   1) empilha os rasters (cada .tif vira uma variável);
#   2) calcula a matriz de correlação entre as variáveis:
#        - n_amostra = NULL  -> usa TODAS as células da área de estudo
#        - n_amostra = número -> sorteia esse nº de células e usa só elas
#   Variáveis constantes (sem variação) são removidas, pois não têm correlação.
#   Devolve uma lista:
#       $matriz      -> matriz de correlação (variáveis x variáveis)
#       $covariancia -> matriz de covariância (valores originais, sem padronizar)
#       $amostra    -> tabela com as células sorteadas (NULL se usou todas)
#       $n_celulas  -> nº de células usadas no cálculo
# ------------------------------------------------------------------------------
calcular_correlacao <- function(arquivos_variaveis, n_amostra = NULL,
                                metodo = "pearson", semente = 123) {

  if (!metodo %in% c("pearson", "spearman")) {
    stop("'metodo_correlacao' deve ser \"pearson\" ou \"spearman\".", call. = FALSE)
  }

  pilha   <- terra::rast(arquivos_variaveis)
  amostra <- NULL

  if (is.null(n_amostra)) {

    # ---- TODAS as células ----
    n_celulas <- terra::global(!is.na(pilha[[1]]), "sum")[[1]]
    message("Calculando a correlação com todas as células (", n_celulas, ")...")

    if (metodo == "pearson") {
      # layerCor lê o raster em blocos: não precisa carregar tudo na memória
      resultado <- terra::layerCor(pilha, "pearson")
      matriz <- if (!is.null(resultado[["pearson"]])) resultado[["pearson"]] else resultado[[1]]

      resultado_cov <- terra::layerCor(pilha, "cov")
      covariancia   <- if (!is.null(resultado_cov[["covariance"]])) resultado_cov[["covariance"]] else resultado_cov[[1]]
    } else {
      # Spearman precisa dos valores (ordenação): usa mais memória
      message("Aviso: Spearman com todas as células carrega os valores na memória.")
      valores <- terra::values(pilha, dataframe = TRUE)
      valores <- valores[complete.cases(valores), , drop = FALSE]
      matriz  <- stats::cor(valores, method = "spearman")
      covariancia <- stats::cov(valores)
    }

  } else {

    # ---- AMOSTRA de células ----
    set.seed(semente)   # garante que o sorteio seja sempre o mesmo
    amostra <- terra::spatSample(pilha, size = n_amostra, method = "random",
                                 na.rm = TRUE, as.df = TRUE)
    amostra <- amostra[complete.cases(amostra), , drop = FALSE]

    if (nrow(amostra) < 30) {
      stop("Foram sorteadas só ", nrow(amostra), " células válidas. ",
           "Confira se os rasters cortados têm valores dentro da máscara.",
           call. = FALSE)
    }
    if (nrow(amostra) < n_amostra) {
      message("Aviso: foram obtidas ", nrow(amostra), " células válidas (pedidas: ",
              n_amostra, ").")
    }

    # variáveis sem variação não permitem calcular correlação
    desvios <- sapply(amostra, stats::sd)
    if (any(desvios == 0)) {
      warning("Variáveis constantes removidas: ",
              paste(names(desvios)[desvios == 0], collapse = ", "),
              call. = FALSE, immediate. = TRUE)
      amostra <- amostra[, desvios > 0, drop = FALSE]
    }

    matriz      <- stats::cor(amostra, method = metodo)
    covariancia <- stats::cov(amostra)
    n_celulas   <- nrow(amostra)
  }

  # variáveis constantes aparecem como NA na diagonal (caso de todas as células)
  constantes <- is.na(diag(matriz))
  if (any(constantes)) {
    warning("Variáveis constantes removidas: ",
            paste(rownames(matriz)[constantes], collapse = ", "),
            call. = FALSE, immediate. = TRUE)
    matriz <- matriz[!constantes, !constantes]
  }
  covariancia <- covariancia[rownames(matriz), rownames(matriz)]

  return(list(matriz = matriz, covariancia = covariancia,
              amostra = amostra, n_celulas = n_celulas))
}


# ------------------------------------------------------------------------------
# resumir_correlacao()
#   Mostra no console a matriz de correlação COMPLETA (todos os pares de
#   variáveis) e salva em 'pasta_saida':
#     matriz_correlacao.csv    -> matriz completa, com os nomes das variáveis
#     matriz_covariancia.csv   -> covariância (só salva, não é mostrada no console)
#     amostra_variaveis.csv    -> valores das células sorteadas (só se usar amostra)
#
#   No console, as colunas aparecem numeradas (1, 2, 3...) e os nomes das
#   variáveis ficam na legenda à esquerda, para a matriz caber na tela.
#   A matriz completa fica no CSV (nenhuma aba/janela é aberta, para não travar o R).
#   'nome_conjunto' (ex.: "preditoras", "LGM") só identifica o conjunto no resumo.
# ------------------------------------------------------------------------------
resumir_correlacao <- function(correlacao, pasta_saida, nome_conjunto = "", figura = FALSE,
                               limiar = 0.7, metodo = NULL, formatos = "png", resolucao = 300) {

  matriz <- correlacao$matriz
  n_var  <- ncol(matriz)

  cat("\n================ CORRELAÇÃO ENTRE VARIÁVEIS ================\n")
  if (nome_conjunto != "") cat("Conjunto                       :", nome_conjunto, "\n")
  cat("Variáveis                      :", n_var, "\n")
  if (is.null(correlacao$amostra)) {
    cat("Células usadas                 :", correlacao$n_celulas,
        "(todas as células válidas)\n")
  } else {
    cat("Células na amostra             :", correlacao$n_celulas, "\n")
  }

  # versão compacta para o console: colunas numeradas, nomes nas linhas
  compacta <- round(matriz, 2)
  rownames(compacta) <- paste0("[", seq_len(n_var), "] ", rownames(matriz))
  colnames(compacta) <- seq_len(n_var)

  cat("\nMatriz de correlação (todos os pares):\n")
  print(compacta)
  cat("==============================================================\n\n")

  utils::write.csv(matriz, file.path(pasta_saida, "matriz_correlacao.csv"),
                   fileEncoding = "UTF-8")
  utils::write.csv(correlacao$covariancia, file.path(pasta_saida, "matriz_covariancia.csv"),
                   fileEncoding = "UTF-8")
  if (!is.null(correlacao$amostra)) {
    utils::write.csv(correlacao$amostra, file.path(pasta_saida, "amostra_variaveis.csv"),
                     row.names = FALSE, fileEncoding = "UTF-8")
  }
  message("Arquivos da correlação salvos em: ", pasta_saida)

  if (isTRUE(figura) && length(formatos) > 0) {
    sub_titulo <- paste0("Em negrito e fundo escuro: |r| ",
                         if (isTRUE(unname(capabilities("cairo")))) "\u2265" else ">=", " ",
                         sub(".", ",", as.character(limiar), fixed = TRUE),
                         if (!is.null(metodo)) paste0("  \u00b7  ", tools::toTitleCase(metodo)),
                         "  \u00b7  ", correlacao$n_celulas, " células")
    arquivos <- desenhar_mapa_correlacao(
      matriz, file.path(pasta_saida, "mapa_correlacao"),
      titulo = paste0("Correlação entre as variáveis", if (nome_conjunto != "") paste0(" - ", nome_conjunto)),
      subtitulo = sub_titulo, limiar = limiar, formatos = formatos, resolucao = resolucao)
    if (length(arquivos) > 0) message("Figura da correlação: ", paste(basename(arquivos), collapse = ", "))
  }

  return(invisible(NULL))
}


# ------------------------------------------------------------------------------
# calcular_correlacao_todos()
#   Calcula a correlação SEPARADAMENTE para cada conjunto de variáveis:
#   as preditoras e cada cenário. Os conjuntos NÃO são misturados nem comparados.
#   Com 'figura' = TRUE, também salva o mapa de calor (mapa_correlacao.png/.pdf) no estilo
#   de apresentação; |r| >= 'limiar_forte' fica com fundo escuro e negrito.
#   Os resultados de cada conjunto são salvos em uma subpasta própria:
#       correlacao/preditoras/
#       correlacao/<nome do cenário>/
#   Devolve uma lista com o resultado de cada conjunto:
#       correlacoes[["preditoras"]]$matriz, correlacoes[["LGM"]]$matriz, ...
# ------------------------------------------------------------------------------
calcular_correlacao_todos <- function(variaveis_cortadas, n_amostra = NULL,
                                      metodo = "pearson", semente = 123,
                                      pasta_saida, figura = FALSE, limiar_forte = 0.7,
                                      formatos = "png", resolucao = 300) {

  correlacoes <- list()

  for (conjunto in names(variaveis_cortadas)) {
    message("Calculando correlação: ", conjunto)

    correlacoes[[conjunto]] <- calcular_correlacao(
      arquivos_variaveis = variaveis_cortadas[[conjunto]],
      n_amostra          = n_amostra,
      metodo             = metodo,
      semente            = semente
    )

    pasta_conjunto <- file.path(pasta_saida, conjunto)
    dir.create(pasta_conjunto, recursive = TRUE, showWarnings = FALSE)
    resumir_correlacao(correlacoes[[conjunto]], pasta_conjunto, conjunto, figura = figura,
                       limiar = limiar_forte, metodo = metodo, formatos = formatos,
                       resolucao = resolucao)
    invisible(gc())   # libera memória antes do próximo conjunto
  }

  return(correlacoes)
}


# ------------------------------------------------------------------------------
# desenhar_mapa_correlacao()
#   Figura da matriz de correlação (mapa de calor) no estilo de apresentação:
#     - cores divergentes: marrom (correlação negativa) -> claro (zero) -> verde-azulado
#       escuro (positiva);
#     - quadros separados por linhas brancas, com o valor de r (vírgula decimal) em cada um;
#     - |r| >= 'limiar': fundo escuro e número em negrito branco;
#     - nomes das variáveis à esquerda e inclinados embaixo; título e subtítulo à esquerda.
#   Salva arquivo_base.png e/ou arquivo_base.pdf (formatos). Devolve os arquivos.
# ------------------------------------------------------------------------------
cor_correlacao <- function(r) {
  pos  <- c(-1, -0.7, -0.35, 0, 0.35, 0.7, 1)
  cols <- grDevices::col2rgb(c("#7B3014", "#A5522F", "#E5A088", "#F2F2F0",
                               "#9FC1C6", "#3E7B80", "#004B50"))
  r <- max(-1, min(1, r))
  canal <- vapply(1:3, function(k) stats::approx(pos, cols[k, ], xout = r)$y, numeric(1))
  grDevices::rgb(canal[1], canal[2], canal[3], maxColorValue = 255)
}

desenhar_mapa_correlacao <- function(matriz, arquivo_base, titulo = "Correlação entre as variáveis",
                                     subtitulo = NULL, limiar = 0.7, formatos = "png",
                                     resolucao = 300) {
  n     <- ncol(matriz)
  nomes <- colnames(matriz)
  cairo <- isTRUE(unname(capabilities("cairo")))
  menos <- if (cairo) "−" else "-"
  ge    <- if (cairo) "≥" else ">="
  virgula <- function(x) sub(".", ",", as.character(x), fixed = TRUE)
  if (is.null(subtitulo)) {
    subtitulo <- paste0("Em negrito e fundo escuro: |r| ", ge, " ", virgula(limiar))
  }

  # tamanhos (polegadas): quadros menores quando há muitas variáveis
  celula_cm <- if (n <= 8) 1.7 else if (n <= 14) 1.3 else if (n <= 24) 0.95 else 0.7
  cex_num   <- max(0.45, min(1, celula_cm / 1.7))
  cex_nome  <- max(0.6, min(0.95, celula_cm / 1.7 + 0.1))
  maior     <- max(nchar(nomes))
  cel_in    <- celula_cm / 2.54
  esq_in    <- (maior * 0.2 * cex_nome + 0.6) / 2.54
  baixo_in  <- (maior * 0.2 * cex_nome * 0.72 + 0.6) / 2.54
  topo_in   <- 1.0
  dir_in    <- 0.4
  largura   <- max(esq_in + n * cel_in + dir_in,
                   0.13 * max(nchar(titulo), nchar(subtitulo)) + 0.6)
  dir_in    <- largura - esq_in - n * cel_in
  altura    <- baixo_in + n * cel_in + topo_in

  texto_r <- function(r) {
    if (is.na(r)) return("")
    r <- round(r, 2)
    if (r == 0) r <- 0
    if (abs(r) == 1) return(if (r > 0) "1" else paste0(menos, "1"))
    s <- formatC(r, format = "f", digits = 2, decimal.mark = ",")
    sub("^-", menos, s)
  }

  desenhar <- function() {
    din <- graphics::par("din")
    graphics::par(mai = c(baixo_in, esq_in, topo_in, dir_in), xaxs = "i", yaxs = "i")
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, n), ylim = c(n, 0))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        r     <- matriz[i, j]
        forte <- !is.na(r) && abs(r) >= limiar
        cor   <- if (is.na(r)) "grey90" else
          cor_correlacao(if (forte) sign(r) * max(abs(r), 0.75) else r)
        graphics::rect(j - 1, i - 1, j, i, col = cor, border = "white", lwd = 3)
        if (celula_cm >= 0.6) {
          graphics::text(j - 0.5, i - 0.5, texto_r(r), cex = cex_num,
                         font = if (forte) 2 else 1, col = if (forte) "white" else "#26323A")
        }
      }
    }
    for (i in seq_len(n)) {
      graphics::text(-0.12, i - 0.5, nomes[i], adj = c(1, 0.5), xpd = NA, cex = cex_nome,
                     col = "#26323A")
    }
    for (j in seq_len(n)) {
      graphics::text(j - 0.3, n + 0.2, nomes[j], srt = 45, adj = c(1, 1), xpd = NA,
                     cex = cex_nome, col = "#26323A")
    }
    x0 <- graphics::grconvertX(0.2 / din[1], "ndc", "user")
    graphics::text(x0, graphics::grconvertY(1 - 0.33 / din[2], "ndc", "user"), titulo,
                   adj = c(0, 0.5), xpd = NA, font = 2, cex = 1.25, col = "#1C1C1C")
    graphics::text(x0, graphics::grconvertY(1 - 0.68 / din[2], "ndc", "user"), subtitulo,
                   adj = c(0, 0.5), xpd = NA, cex = 0.95, col = "#6B6B6B")
  }

  dir.create(dirname(arquivo_base), recursive = TRUE, showWarnings = FALSE)
  gerados <- character(0)
  for (formato in unique(tolower(formatos))) {
    arquivo <- paste0(arquivo_base, ".", formato)
    if (formato == "png") {
      if (cairo) grDevices::png(arquivo, width = largura, height = altura, units = "in",
                                res = resolucao, type = "cairo", bg = "white")
      else grDevices::png(arquivo, width = largura, height = altura, units = "in",
                          res = resolucao, bg = "white")
    } else if (formato == "pdf") {
      if (cairo) grDevices::cairo_pdf(arquivo, width = largura, height = altura)
      else grDevices::pdf(arquivo, width = largura, height = altura)
    } else next
    ok <- tryCatch({ desenhar(); TRUE }, error = function(e) {
      message("  Figura da correlação não gerada (", basename(arquivo), "): ", conditionMessage(e))
      FALSE
    })
    grDevices::dev.off()
    if (ok) gerados <- c(gerados, arquivo) else unlink(arquivo)
  }
  invisible(gerados)
}
