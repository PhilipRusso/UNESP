# ==============================================================================
#  FUNÇÕES - Autocorrelação espacial: I de Moran, resíduos e correlograma
# ------------------------------------------------------------------------------
#  Testa se os pontos próximos se parecem mais (ou menos) do que o acaso:
#
#    "presenca" -> autocorrelação dos DADOS (presença = 1 / background = 0);
#    "residuos" -> autocorrelação dos RESÍDUOS de cada modelo final
#                  (resíduo = observado - previsto, nos pontos de presença e background).
#                  Resíduos autocorrelacionados indicam que o modelo deixou de captar
#                  um padrão espacial (variável faltando, escala, viés de coleta).
#
#  Para cada espécie e alvo:
#    - I de Moran GLOBAL: pares de pontos a até 'distancia_max_km' (peso 1) e
#      p-valor pela aproximação normal (variância sob randomização, Cliff & Ord);
#    - CORRELOGRAMA: o I de Moran em classes de distância (anéis), com o p-valor de cada
#      classe (círculo cheio = p < 0,05);
#    - MAPA de resíduos suavizado (média local com núcleo gaussiano) por modelo.
#  Distâncias em km (haversine se as coordenadas forem geográficas).
#  Com muitos pontos, usa uma amostra de 'n_max' pontos (a matriz de distâncias é
#  n x n).
#
#  Cuidado: no background (pseudoausências) o resíduo depende de como os pontos foram
#  sorteados (buffer, viés de coleta); a autocorrelação dos resíduos mistura o erro do
#  modelo com o desenho amostral. 'pontos = "presencas"' usa só as presenças.
#
#  Saídas em pasta_saida:
#    moran_global.csv, moran_correlograma.csv, moran_residuos_pontos.csv, moran.rds
#    <espécie>/moran_<modelo>.png|pdf  -> mapa dos resíduos + correlograma
#    <espécie>/moran_correlogramas.png|pdf -> correlogramas de todos os modelos juntos
# ==============================================================================


# ---- auxiliares ----------------------------------------------------------------

fmt_numero_br <- function(x, digitos = 2, menos = "-") {
  s <- formatC(x, format = "f", digits = digitos, decimal.mark = ",")
  sub("^-", menos, s)
}

# distâncias (km) entre dois conjuntos de pontos; devolve matriz length(x1) x length(x2)
distancias_cruzadas_km <- function(x1, y1, x2, y2, geografico, fator_km = 0.001) {
  if (geografico) {
    rad <- pi / 180
    lat1 <- y1 * rad; lat2 <- y2 * rad
    lon1 <- x1 * rad; lon2 <- x2 * rad
    a <- sin(outer(lat1, lat2, "-") / 2)^2 +
      outer(cos(lat1), cos(lat2)) * sin(outer(lon1, lon2, "-") / 2)^2
    6371 * 2 * asin(pmin(sqrt(a), 1))
  } else {
    sqrt(outer(x1, x2, "-")^2 + outer(y1, y2, function(a, b) (a - b)^2)) * fator_km
  }
}

# I de Moran de cada coluna de Z (valores centralizados) com pesos binários simétricos W.
# p-valor bilateral pela variância sob randomização (Cliff & Ord, 1981).
# Devolve data.frame (I, esperado, z, p, pares) com uma linha por coluna de Z.
moran_multiplo <- function(Z, W, minimo_pares = 10) {
  n <- nrow(Z)
  m <- ncol(Z)
  S0 <- sum(W)
  res <- data.frame(I = rep(NA_real_, m), esperado = rep(-1 / (n - 1), m),
                    z = rep(NA_real_, m), p = rep(NA_real_, m), pares = rep(S0, m))
  if (S0 < minimo_pares || n < 5) return(res)

  S1 <- 2 * S0                       # W binária e simétrica
  S2 <- 4 * sum(rowSums(W)^2)
  WZ <- W %*% Z
  E  <- -1 / (n - 1)
  for (j in seq_len(m)) {
    z  <- Z[, j]
    ss <- sum(z^2)
    if (!is.finite(ss) || ss == 0) next
    I  <- (n / S0) * sum(z * WZ[, j]) / ss
    k  <- (sum(z^4) / n) / (ss / n)^2
    A  <- n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * S0^2)
    B  <- k * ((n^2 - n) * S1 - 2 * n * S2 + 6 * S0^2)
    V  <- (A - B) / ((n - 1) * (n - 2) * (n - 3) * S0^2) - E^2
    if (!is.finite(V) || V <= 0) next
    zsc <- (I - E) / sqrt(V)
    res$I[j] <- I
    res$z[j] <- zsc
    res$p[j] <- 2 * stats::pnorm(-abs(zsc))
  }
  res
}

# escolhe até n_max linhas (mantém presenças e background em proporção equilibrada)
selecionar_linhas_moran <- function(dados, pontos = "todos", n_max = 2000, semente = 123) {
  linhas <- if (pontos == "presencas") which(dados$presenca == 1) else seq_len(nrow(dados))
  if (!is.null(n_max) && length(linhas) > n_max) {
    set.seed(semente)
    i1 <- linhas[dados$presenca[linhas] == 1]
    i0 <- linhas[dados$presenca[linhas] == 0]
    n1 <- min(length(i1), floor(n_max / 2))
    n0 <- min(length(i0), n_max - n1)
    n1 <- min(length(i1), n_max - n0)
    linhas <- sort(c(if (n1 > 0) i1[sample.int(length(i1), n1)],
                     if (n0 > 0) i0[sample.int(length(i0), n0)]))
  }
  linhas
}

# média local (núcleo gaussiano) dos resíduos numa grade; R = matriz n x m (um modelo por coluna).
# Devolve list(raster = SpatRaster de m camadas, h_km).
mapa_residuos_suavizado <- function(x, y, R, ref, geografico, fator_km, h_km, max_celulas = 150) {
  fator <- max(1, ceiling(max(terra::nrow(ref), terra::ncol(ref)) / max_celulas))
  grade <- if (fator > 1) terra::aggregate(ref[[1]], fact = fator, fun = "mean", na.rm = TRUE) else ref[[1]]
  celulas <- which(!is.na(terra::values(grade)[, 1]))
  xy <- terra::xyFromCell(grade, celulas)
  saida <- matrix(NA_real_, nrow = length(celulas), ncol = ncol(R))
  bloco <- 400
  for (ini in seq(1, length(celulas), by = bloco)) {
    i <- ini:min(ini + bloco - 1, length(celulas))
    D <- distancias_cruzadas_km(xy[i, 1], xy[i, 2], x, y, geografico, fator_km)
    w <- exp(-0.5 * (D / h_km)^2)
    sw <- rowSums(w)
    v <- (w %*% R) / sw
    v[sw < 0.5, ] <- NA_real_               # sem pontos por perto: não extrapola
    saida[i, ] <- v
  }
  camadas <- terra::rast(grade, nlyrs = ncol(R))
  valores <- matrix(NA_real_, nrow = terra::ncell(grade), ncol = ncol(R))
  valores[celulas, ] <- saida
  terra::values(camadas) <- valores
  names(camadas) <- colnames(R)
  camadas
}


# ---- figuras --------------------------------------------------------------------

# salva em PNG e/ou PDF (polegadas), com cairo quando disponível (acentos e sinal de menos)
salvar_figura_sdm <- function(arquivo_base, desenhar, largura, altura, formatos, resolucao = 300) {
  dir.create(dirname(arquivo_base), recursive = TRUE, showWarnings = FALSE)
  cairo <- isTRUE(unname(capabilities("cairo")))
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
      message("  Figura não gerada (", basename(arquivo), "): ", conditionMessage(e))
      FALSE
    })
    grDevices::dev.off()
    if (ok) gerados <- c(gerados, arquivo) else unlink(arquivo)
  }
  invisible(gerados)
}

# título (negrito) e subtítulo (cinza) alinhados à esquerda do painel atual
titulo_painel <- function(titulo, subtitulo = NULL) {
  graphics::mtext(titulo, side = 3, line = if (is.null(subtitulo)) 0.6 else 1.7, adj = 0,
                  font = 2, cex = 1.05, col = "#1C1C1C")
  if (!is.null(subtitulo)) {
    graphics::mtext(subtitulo, side = 3, line = 0.5, adj = 0, cex = 0.85, col = "#6B6B6B")
  }
}

# largura (polegadas) que a legenda de 'rotulos' ocupa; medida num dispositivo temporário
# (com folga de 15%, porque a fonte do PNG pode ser mais larga que a do PDF)
largura_legenda_pol <- function(rotulos, cex = 0.85) {
  atual <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  graphics::plot.new()
  w <- max(graphics::strwidth(rotulos, units = "inches", cex = cex, font = 2))
  grDevices::dev.off()
  if (atual > 1) grDevices::dev.set(atual)
  1.15 * w
}

# correlograma: 'series' = lista de list(nome, cor, tab) com tab$d_medio, tab$I, tab$p
# A legenda (nomes coloridos, alinhados à esquerda) fica À DIREITA, FORA da área do gráfico.
desenhar_correlograma <- function(series, titulo, subtitulo = NULL, menos = "-",
                                  ylab = "I de Moran dos resíduos") {
  rotulos <- vapply(series, function(s) s$nome, character(1))
  folga   <- 0.15                                   # espaço entre o gráfico e a legenda (pol.)
  graphics::par(mar = c(4.2, 4.6, 3.4, 1), bty = "n", las = 1)
  pol_linha <- graphics::par("mai")[4]              # polegadas de 1 linha de margem
  graphics::par(mar = c(4.2, 4.6, 3.4, (largura_legenda_pol(rotulos) + folga + 0.1) / pol_linha))

  d_max <- max(vapply(series, function(s) max(s$tab$d_medio, na.rm = TRUE), numeric(1)))
  y_all <- unlist(lapply(series, function(s) s$tab$I))
  y_all <- c(y_all[is.finite(y_all)], 0)
  pad   <- 0.1 * diff(range(y_all))
  # os eixos começam e terminam em marcas (ticks) e se encontram no canto inferior esquerdo
  marcas_y <- pretty(c(min(y_all) - pad, max(y_all) + pad), 5)
  marcas_x <- pretty(c(0, d_max * 1.02), 5)
  ylim <- range(marcas_y)
  xlim <- c(0, max(marcas_x))
  graphics::plot(NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "", axes = FALSE,
                 xaxs = "i", yaxs = "i")
  graphics::abline(h = marcas_y, col = "grey90", lwd = 0.6)
  graphics::abline(h = 0, col = "grey55", lwd = 1)
  usr <- graphics::par("usr")
  graphics::segments(usr[1], usr[3], usr[2], usr[3], col = "grey40", lwd = 1, xpd = NA)  # eixo x
  graphics::segments(usr[1], usr[3], usr[1], usr[4], col = "grey40", lwd = 1, xpd = NA)  # eixo y
  graphics::axis(1, at = marcas_x, lwd = 0, lwd.ticks = 1, col.ticks = "grey40",
                 col.axis = "#26323A", cex.axis = 0.85)
  graphics::axis(2, at = marcas_y, labels = fmt_numero_br(marcas_y, 2, menos), lwd = 0,
                 lwd.ticks = 1, col.ticks = "grey40", col.axis = "#26323A", cex.axis = 0.85)
  graphics::mtext("Distância entre os pontos (km)", side = 1, line = 2.7, cex = 0.9, col = "#26323A")
  graphics::mtext(ylab, side = 2, line = 3.4, cex = 0.9, col = "#26323A", las = 0)
  for (k in seq_along(series)) {
    s <- series[[k]]
    t <- s$tab[is.finite(s$tab$I), , drop = FALSE]
    if (nrow(t) == 0) next
    graphics::lines(t$d_medio, t$I, col = s$cor, lwd = 1.1)
    graphics::points(t$d_medio, t$I, pch = 21, cex = 0.5, lwd = 0.9, col = s$cor,
                     bg = ifelse(!is.na(t$p) & t$p < 0.05, s$cor, "white"))
  }

  # legenda fora do gráfico, à direita, alinhada à esquerda (de cima para baixo)
  x_ini <- graphics::grconvertX(graphics::grconvertX(usr[2], "user", "inches") + folga,
                                "inches", "user")
  alt_linha <- 1.7 * graphics::strheight("M", units = "inches", cex = 0.85)
  y_topo_in <- graphics::grconvertY(usr[4], "user", "inches")
  for (k in seq_along(series)) {
    y <- graphics::grconvertY(y_topo_in - (k - 1) * alt_linha, "inches", "user")
    graphics::text(x_ini, y, series[[k]]$nome, col = series[[k]]$cor, font = 2,
                   adj = c(0, 1), cex = 0.85, xpd = NA)
  }
  titulo_painel(titulo, subtitulo)
}

# barra de cores horizontal (resíduos) com os rótulos das pontas
desenhar_barra_residuos <- function(limite, menos = "-") {
  graphics::par(mar = c(1.6, 1.2, 1.5, 1.2))
  graphics::plot.new()
  graphics::plot.window(xlim = c(-limite, limite), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  cores <- vapply(seq(-1, 1, length.out = 200), cor_correlacao, character(1))
  graphics::rasterImage(grDevices::as.raster(matrix(cores, nrow = 1)), -limite, 0.1, limite, 0.9)
  graphics::mtext("Resíduo (observado − previsto)", side = 3, line = 0.1, adj = 0, font = 2,
                  cex = 0.8, col = "#1C1C1C")
  graphics::text(-limite, -0.1, paste0("min (", fmt_numero_br(-limite, 2, menos), ")"),
                 adj = c(0, 1), xpd = NA, cex = 0.8, col = "#26323A")
  graphics::text(0, -0.1, "0", adj = c(0.5, 1), xpd = NA, cex = 0.8, col = "#26323A")
  graphics::text(limite, -0.1, paste0("max (", fmt_numero_br(limite, 2, menos), ")"),
                 adj = c(1, 1), xpd = NA, cex = 0.8, col = "#26323A")
}

# contorno da área de estudo (SpatVector) a partir da máscara (vetor ou raster) ou, sem
# máscara, da área com valor do raster de referência. Devolve NULL se não conseguir.
contorno_mascara <- function(mascara, ref, max_celulas = 200) {
  tryCatch({
    m <- if (is.null(mascara)) ref[[1]] else mascara
    if (inherits(m, "SpatRaster")) {
      m <- m[[1]]
      fator <- max(1, ceiling(max(terra::nrow(m), terra::ncol(m)) / max_celulas))
      if (fator > 1) m <- terra::aggregate(m, fact = fator, fun = "max", na.rm = TRUE)
      v <- terra::as.polygons(terra::ifel(is.na(m), NA, 1))
    } else {
      v <- if (terra::same.crs(m, ref)) m else terra::project(m, terra::crs(ref))
      v <- terra::aggregate(v)
    }
    v
  }, error = function(e) NULL)
}

# figura de um modelo: mapa dos resíduos (esquerda) + correlograma (direita)
figura_moran_modelo <- function(especie, modelo, mapa, series, base, formatos, resolucao,
                                subtitulo_mapa, subtitulo_corr, contorno = NULL) {
  cairo <- isTRUE(unname(capabilities("cairo")))
  menos <- if (cairo) "−" else "-"
  if (is.null(mapa)) {
    desenhar <- function() desenhar_correlograma(series, paste0("Correlograma - ", toupper(modelo)),
                                                 subtitulo_corr, menos)
    return(salvar_figura_sdm(base, desenhar, 8.4, 4.6, formatos, resolucao))
  }
  v <- terra::values(mapa)[, 1]
  v <- v[is.finite(v)]
  limite <- if (length(v) > 0) max(abs(stats::quantile(v, c(0.02, 0.98), names = FALSE))) else 0.5
  if (!is.finite(limite) || limite == 0) limite <- 0.5
  proporcao <- min(max(terra::nrow(mapa) / terra::ncol(mapa), 0.55), 1.5)
  cores <- vapply(seq(-1, 1, length.out = 100), cor_correlacao, character(1))
  w1 <- 4.9; w2 <- 7.0
  alt <- w1 * proporcao + 0.9

  desenhar <- function() {
    graphics::layout(matrix(1:4, nrow = 2, byrow = TRUE),
                     widths = graphics::lcm(c(w1, w2) * 2.54), heights = graphics::lcm(c(alt, 0.95) * 2.54))
    if (!is.null(contorno)) {
      # máscara (área de estudo) ao fundo, resíduos por cima e o contorno da máscara de novo
      terra::plot(contorno, col = "grey94", border = "grey35", lwd = 0.8, axes = FALSE,
                  box = FALSE, mar = c(0.4, 0.4, 3.4, 0.4), main = "")
      terra::plot(mapa, col = cores, range = c(-limite, limite), legend = FALSE, axes = FALSE,
                  box = FALSE, add = TRUE)
      terra::lines(contorno, col = "grey25", lwd = 0.8)
    } else {
      terra::plot(mapa, col = cores, range = c(-limite, limite), legend = FALSE, axes = FALSE,
                  box = FALSE, mar = c(0.4, 0.4, 3.4, 0.4))
    }
    titulo_painel(paste0("Resíduos do modelo ", toupper(modelo)), subtitulo_mapa)
    desenhar_correlograma(series, "Correlograma", subtitulo_corr, menos)
    desenhar_barra_residuos(limite, menos)
    graphics::plot.new()
  }
  # o PNG tem um nº inteiro de pixels (polegadas x resolução). Se o tamanho do layout
  # (em lcm) passar disso por uma fração de pixel, o R dá "figure region too large":
  # por isso o tamanho do arquivo é arredondado PARA CIMA para um nº inteiro de pixels.
  para_pixel <- function(x) ceiling(x * resolucao - 1e-6) / resolucao
  salvar_figura_sdm(base, desenhar, para_pixel(w1 + w2), para_pixel(alt + 0.95), formatos, resolucao)
}

# correlogramas de todos os modelos (e dos dados) de uma espécie, num só painel
figura_moran_comparativa <- function(especie, series, base, formatos, resolucao, subtitulo) {
  cairo <- isTRUE(unname(capabilities("cairo")))
  menos <- if (cairo) "−" else "-"
  desenhar <- function() desenhar_correlograma(series, paste0("Autocorrelação espacial - ", especie),
                                               subtitulo, menos, ylab = "I de Moran")
  salvar_figura_sdm(base, desenhar, 9.2, 4.8, formatos, resolucao)
}


# ------------------------------------------------------------------------------
# calcular_autocorrelacao()
#   alvos            : "residuos" e/ou "presenca" (ver o cabeçalho do arquivo)
#   n_classes        : nº de classes de distância do correlograma
#   distancia_max_km : alcance do correlograma e do I global (NULL = metade da maior
#                      distância entre os pontos)
#   n_max            : máximo de pontos por espécie (NULL = todos; a matriz de distâncias
#                      é n x n, então muitos pontos exigem muita memória)
#   pontos           : "todos" (presenças + background) ou "presencas"
#   suavizacao_km    : largura do núcleo gaussiano do mapa de resíduos (NULL = 3% da diagonal
#                      da área de estudo)
#   figuras          : TRUE = salva os PNG/PDF ('formatos', 'resolucao')
#   Devolve list(global, correlograma, residuos, ...) e salva os CSV.
# ------------------------------------------------------------------------------
calcular_autocorrelacao <- function(dados_modelos, modelos_finais, preditores,
                                    alvos = c("residuos", "presenca"), n_classes = 15,
                                    distancia_max_km = NULL, n_max = 2000, pontos = "todos",
                                    suavizacao_km = NULL, pasta_saida, figuras = TRUE,
                                    formatos = "png", resolucao = 300, semente = 123,
                                    mascara = NULL) {

  # "prompt" ainda literal (seção 2 rodada de novo sem o registro dos parâmetros): pergunta
  if (parametro_prompt(alvos)) alvos <- perguntar_parametro("moran_alvos")

  alvos <- unique(tolower(trimws(alvos)))
  invalidos <- setdiff(alvos, c("residuos", "presenca"))
  if (length(invalidos) > 0 || length(alvos) == 0) {
    stop("'moran_alvos' inválido: ", paste(invalidos, collapse = ", "),
         "\nOpções: residuos, presenca", call. = FALSE)
  }
  pontos <- match.arg(tolower(trimws(pontos)), c("todos", "presencas"))
  if (pontos == "presencas" && "presenca" %in% alvos) {
    message("Só presenças: o alvo 'presenca' não varia (tudo = 1) e foi retirado.")
    alvos <- setdiff(alvos, "presenca")
    if (length(alvos) == 0) alvos <- "residuos"
  }
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  ref <- terra::rast(preditores$arquivos[["preditoras"]][1])[[1]]
  # máscara da área de estudo para o mapa dos resíduos (sem ela, usa a área do raster)
  if (is.null(mascara)) mascara <- tryCatch(obter_mascara_corte(), error = function(e) NULL)
  contorno <- contorno_mascara(mascara, ref)
  geografico <- isTRUE(terra::is.lonlat(ref))
  fator_km <- if (geografico) NA_real_ else {
    f <- suppressWarnings(terra::linearUnits(ref))
    if (is.null(f) || !is.finite(f) || f <= 0) 0.001 else f / 1000
  }
  e <- terra::ext(ref)
  diagonal_km <- distancias_cruzadas_km(e[1], e[3], e[2], e[4], geografico, fator_km)[1, 1]
  h_km <- if (is.null(suavizacao_km)) 0.03 * diagonal_km else suavizacao_km
  cairo <- isTRUE(unname(capabilities("cairo")))
  menos <- if (cairo) "−" else "-"

  cat("\n================ AUTOCORRELAÇÃO ESPACIAL (I DE MORAN) ================\n")
  cat("Alvos:", paste(alvos, collapse = ", "), "| pontos:", pontos, "| classes:", n_classes, "\n")
  cat("Distâncias em km", if (geografico) "(haversine)" else "(unidades do raster)", "\n")

  tab_global <- list()
  tab_corr   <- list()
  tab_resid  <- list()
  arquivos   <- character(0)

  for (especie in names(dados_modelos)) {
    dados <- dados_modelos[[especie]]$dados
    cat("\n", especie, "\n", sep = "")

    # ---- alvos: presença e resíduos de cada modelo -----------------------------
    alvos_valores <- data.frame(row.names = seq_len(nrow(dados)))
    previsoes <- data.frame(row.names = seq_len(nrow(dados)))
    if ("presenca" %in% alvos) alvos_valores[["presenca"]] <- as.numeric(dados$presenca)
    modelos <- character(0)
    if ("residuos" %in% alvos) {
      for (codigo in setdiff(names(modelos_finais[[especie]]), "jsdm")) {
        p <- tryCatch(as.numeric(prever_modelo(modelos_finais[[especie]][[codigo]], dados)),
                      error = function(e) rep(NA_real_, nrow(dados)))
        if (mean(is.na(p)) > 0.5) {
          message("  [", codigo, "] sem previsões: ignorado.")
          next
        }
        previsoes[[codigo]] <- p
        alvos_valores[[paste0("res_", codigo)]] <- dados$presenca - p
        modelos <- c(modelos, codigo)
      }
    }
    if (ncol(alvos_valores) == 0) {
      message("  Nada para calcular nesta espécie.")
      next
    }

    linhas <- selecionar_linhas_moran(dados, pontos, n_max, semente)
    linhas <- linhas[stats::complete.cases(alvos_valores[linhas, , drop = FALSE])]
    if (length(linhas) < 30) {
      message("  Menos de 30 pontos utilizáveis: espécie pulada.")
      next
    }
    cat("  pontos usados:", length(linhas), "de", nrow(dados), "\n")

    x <- dados$x[linhas]; y <- dados$y[linhas]
    Z <- as.matrix(alvos_valores[linhas, , drop = FALSE])
    Z <- sweep(Z, 2, colMeans(Z), "-")

    D <- distancias_cruzadas_km(x, y, x, y, geografico, fator_km)
    d_max <- if (is.null(distancia_max_km)) max(D) / 2 else distancia_max_km
    quebras <- seq(0, d_max, length.out = n_classes + 1)

    # global (0, d_max]
    W <- (D > 0 & D <= d_max) * 1
    g <- moran_multiplo(Z, W)
    tab_global[[especie]] <- data.frame(
      especie = especie, alvo = ifelse(colnames(Z) == "presenca", "presenca",
                                       sub("^res_", "residuos_", colnames(Z))),
      n_pontos = length(linhas), distancia_max_km = d_max, g, row.names = NULL)
    rm(W)

    # correlograma
    classes <- list()
    for (k in seq_len(n_classes)) {
      W <- (D > quebras[k] & D <= quebras[k + 1]) * 1
      m <- moran_multiplo(Z, W)
      d_medio <- if (sum(W) > 0) mean(D[W > 0]) else NA_real_
      classes[[k]] <- data.frame(
        especie = especie, alvo = tab_global[[especie]]$alvo, classe = k,
        d_min = quebras[k], d_max = quebras[k + 1], d_medio = d_medio, m, row.names = NULL)
    }
    rm(W, D)
    corr <- do.call(rbind, classes)
    tab_corr[[especie]] <- corr

    # resíduos nos pontos
    if (length(modelos) > 0) {
      tr <- data.frame(especie = especie, x = dados$x[linhas], y = dados$y[linhas],
                       presenca = dados$presenca[linhas])
      for (codigo in modelos) {
        tr[[paste0("previsao_", codigo)]] <- previsoes[[codigo]][linhas]
        tr[[paste0("residuo_", codigo)]]  <- alvos_valores[[paste0("res_", codigo)]][linhas]
      }
      tab_resid[[especie]] <- tr
    }

    cat(sprintf("  I global (até %.0f km): ", d_max))
    print(arredondar_sdm(stats::setNames(g$I, tab_global[[especie]]$alvo)))

    # ---- figuras ---------------------------------------------------------------
    if (isTRUE(figuras) && length(formatos) > 0) {
      pasta_sp <- file.path(pasta_saida, nome_seguro(especie))
      serie <- function(alvo, nome, cor) {
        list(nome = nome, cor = cor, tab = corr[corr$alvo == alvo, , drop = FALSE])
      }
      cor_dados <- "#1B5E63"
      cor_res   <- "#C0562F"
      sub_corr  <- paste0("Círculo cheio: p < 0,05 · ", length(linhas), " pontos")

      mapas <- NULL
      if (length(modelos) > 0) {
        R <- as.matrix(alvos_valores[linhas, paste0("res_", modelos), drop = FALSE])
        colnames(R) <- modelos
        mapas <- tryCatch(mapa_residuos_suavizado(x, y, R, ref, geografico, fator_km, h_km),
                          error = function(e) { message("  mapa de resíduos: ", conditionMessage(e)); NULL })
      }

      for (codigo in modelos) {
        ser <- list(serie(paste0("residuos_", codigo), paste0("Resíduos ", toupper(codigo)), cor_res))
        if ("presenca" %in% alvos) ser[[2]] <- serie("presenca", "Dados (presença)", cor_dados)
        arquivos <- c(arquivos, figura_moran_modelo(
          especie, codigo, if (is.null(mapas)) NULL else mapas[[codigo]], ser,
          file.path(pasta_sp, paste0("moran_", nome_seguro(codigo))), formatos, resolucao,
          paste0(especie, " · média local (σ = ", round(h_km), " km)"), sub_corr,
          contorno = contorno))
      }
      if (length(modelos) == 0 && "presenca" %in% alvos) {
        arquivos <- c(arquivos, figura_moran_modelo(
          especie, "dados", NULL, list(serie("presenca", "Dados (presença)", cor_dados)),
          file.path(pasta_sp, "moran_dados"), formatos, resolucao, NULL, sub_corr))
      }

      # todos juntos
      todas <- list()
      if ("presenca" %in% alvos) todas[[1]] <- serie("presenca", "Dados (presença)", "#222222")
      paleta <- grDevices::hcl.colors(max(length(modelos), 2), "Dark 3")
      for (i in seq_along(modelos)) {
        todas[[length(todas) + 1]] <- serie(paste0("residuos_", modelos[i]), toupper(modelos[i]), paleta[i])
      }
      if (length(todas) > 1 || length(modelos) > 0) {
        arquivos <- c(arquivos, figura_moran_comparativa(
          especie, todas, file.path(pasta_sp, "moran_correlogramas"), formatos, resolucao, sub_corr))
      }
    }
    invisible(gc())
  }

  if (length(tab_global) == 0) {
    message("Nenhuma espécie pôde ser analisada.")
    return(invisible(NULL))
  }
  global <- do.call(rbind, tab_global)
  correlograma <- do.call(rbind, tab_corr)
  residuos <- NULL
  if (length(tab_resid) > 0) {
    colunas <- unique(unlist(lapply(tab_resid, names)))
    tab_resid <- lapply(tab_resid, function(t) {
      for (c in setdiff(colunas, names(t))) t[[c]] <- NA_real_
      t[, colunas, drop = FALSE]
    })
    residuos <- do.call(rbind, tab_resid)
    rownames(residuos) <- NULL
  }
  rownames(global) <- NULL; rownames(correlograma) <- NULL

  gravar <- function(tab, nome) {
    if (is.null(tab)) return(invisible(NULL))
    utils::write.csv(acrescentar_preditores(tab, preditores), file.path(pasta_saida, nome),
                     row.names = FALSE, fileEncoding = "UTF-8")
  }
  gravar(global, "moran_global.csv")
  gravar(correlograma, "moran_correlograma.csv")
  gravar(residuos, "moran_residuos_pontos.csv")
  resultado <- list(global = global, correlograma = correlograma, residuos = residuos,
                    figuras = arquivos)
  saveRDS(resultado, file.path(pasta_saida, "moran.rds"))

  cat("\n---------------- RESUMO: I de Moran global ----------------\n")
  mostrar <- global[, c("especie", "alvo", "n_pontos", "I", "p")]
  mostrar$I <- arredondar_sdm(mostrar$I)
  mostrar$p <- arredondar_sdm(mostrar$p)
  print(mostrar, row.names = FALSE)
  cat("I > 0: pontos próximos se parecem (agrupamento); I < 0: se opõem.\n")
  cat("Resíduos com I alto e p < 0,05 indicam padrão espacial não explicado pelo modelo.\n")
  cat("Autocorrelação salva em:", pasta_saida, "\n")
  cat("===========================================================\n")

  invisible(resultado)
}
