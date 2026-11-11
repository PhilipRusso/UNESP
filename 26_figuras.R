# ==============================================================================
#  FUNÇÕES - Figuras prontas (PNG ou PDF)
# ------------------------------------------------------------------------------
#  Monta figuras a partir dos resultados dos modelos, além dos .tif:
#
#    "modelos"         -> um painel por espécie e conjunto (preditoras / cenários) com o
#                         mapa contínuo de cada algoritmo e do consenso (escala 0 a 1);
#    "binarios"        -> um painel por espécie, conjunto e limiar, com o mapa de
#                         presença/ausência de cada algoritmo e do consenso;
#    "consenso"        -> o mapa contínuo do consenso ao lado dos seus mapas binários;
#    "cenarios"        -> o consenso de cada conjunto lado a lado (preditoras x cenários);
#    "variaveis"       -> mapa de cada preditor usado nos modelos (as variáveis escolhidas
#                         ou, se for o caso, os eixos da PCA), em cada conjunto;
#    "importancia"     -> gráficos de barras da importância das variáveis (seção 22);
#    "curvas_resposta" -> curva de resposta de cada algoritmo a cada preditor, com os
#                         demais preditores na mediana;
#    "extrapolacao"    -> por espécie e conjunto, a partir de extrapolacao/ (seção 21): o MESS,
#                         a área em extrapolação (MESS < 0) e a variável mais diferente.
#
#  Layout ('figuras_layout'): "painel" (um quadro com os mapas), "separadas" (uma
#  figura por mapa) ou os dois. Dá para limitar a espécies, conjuntos, mapas
#  (algoritmo ou "consenso_<método>") e limiares específicos.
#  Formatos: "png" (com a resolução de 'resolucao_figuras') e/ou "pdf" (vetorial).
#  Os pontos de presença podem ser desenhados sobre os mapas ('figuras_pontos'), com
#  barra de escala ('figuras_escala') e seta do norte ('figuras_norte').
#  Os títulos são quebrados em linhas (e a letra reduzida, se preciso) para caberem
#  na figura.
#  Saídas em modelos/figuras/<espécie>/.
# ==============================================================================


# ---- auxiliares ---------------------------------------------------------------

# aceita c("a", "b") e também um texto único "a, b" (separado por vírgula, ponto e vírgula ou espaço)
separar_texto_opcoes <- function(x) {
  x <- unlist(strsplit(as.character(x), "[,;[:space:]]+"))
  unique(tolower(x[nzchar(x)]))
}

normalizar_formatos_figuras <- function(formatos) {
  formatos  <- separar_texto_opcoes(formatos)
  if ("all" %in% formatos) formatos <- c("png", "pdf")
  invalidos <- setdiff(formatos, c("png", "pdf"))
  if (length(invalidos) > 0) {
    stop("Formato(s) de figura inválido(s): ", paste(invalidos, collapse = ", "),
         "\nOpções: png, pdf", call. = FALSE)
  }
  formatos
}

normalizar_tipos_figuras <- function(tipos) {
  tipos     <- separar_texto_opcoes(tipos)
  validos   <- c("modelos", "binarios", "consenso", "cenarios", "variaveis", "importancia",
                 "curvas_resposta", "extrapolacao")
  if ("all" %in% tipos) tipos <- validos
  invalidos <- setdiff(tipos, validos)
  if (length(invalidos) > 0) {
    stop("Tipo(s) de figura inválido(s): ", paste(invalidos, collapse = ", "),
         "\nOpções: ", paste(validos, collapse = ", "), call. = FALSE)
  }
  tipos
}

# opções dos mapas definidas em gerar_figuras() (escala e seta do norte)
.sdm_figuras_opcoes <- new.env()

# Quebra o título em linhas que caibam na largura (polegadas; padrão = a do quadro atual)
# e, se uma palavra sozinha ainda não couber, reduz a letra. Precisa de um dispositivo aberto.
# Devolve list(texto, linhas, n, cex).
ajustar_titulo <- function(texto, cex = 0.9, largura = NULL, font = 1) {
  if (is.null(largura)) largura <- graphics::par("fin")[1]
  largura <- max(0.92 * largura, 0.5)
  linhas <- character(0)
  for (parte in strsplit(as.character(texto), "\n", fixed = TRUE)[[1]]) {
    atual <- ""
    for (p in strsplit(parte, " ", fixed = TRUE)[[1]]) {
      candidato <- if (atual == "") p else paste(atual, p)
      if (atual != "" &&
          graphics::strwidth(candidato, units = "inches", cex = cex, font = font) > largura) {
        linhas <- c(linhas, atual)
        atual <- p
      } else {
        atual <- candidato
      }
    }
    linhas <- c(linhas, atual)
  }
  if (length(linhas) == 0) linhas <- ""
  maior <- max(graphics::strwidth(linhas, units = "inches", cex = cex, font = font))
  if (maior > largura) cex <- max(0.5, cex * largura / maior)
  list(texto = paste(linhas, collapse = "\n"), linhas = linhas, n = length(linhas), cex = cex)
}

# título geral de uma figura (na margem externa): prepara (define oma) e escreve
preparar_titulo_geral <- function(texto, cex = 1.1) {
  tg <- ajustar_titulo(texto, cex, largura = graphics::par("din")[1], font = 2)
  graphics::par(oma = c(0, 0, 0.8 + 1.5 * tg$n, 0))
  tg
}
escrever_titulo_geral <- function(tg) {
  for (k in seq_len(tg$n)) {
    graphics::mtext(tg$linhas[k], side = 3, outer = TRUE, line = (tg$n - k) * 1.4 + 0.3,
                    cex = tg$cex, font = 2)
  }
}

# ------------------------------------------------------------------------------
# Barra de escala e seta do norte ('figuras_escala' / 'figuras_norte')
#   Desenhadas à mão (gráficos base) sobre o mapa atual, no estilo da apresentação:
#   barra em dois tons com os números em cima e a unidade (km) depois do último
#   número; seta do norte pequena, com um "N" em cima.
#   Opções (definidas em gerar_figuras): posição de cada um (canto do mapa),
#   cores normais ou invertidas e fundo semitransparente atrás dos elementos.
# ------------------------------------------------------------------------------
posicoes_elementos_mapa <- c("inferior_direito", "inferior_esquerdo",
                             "superior_direito", "superior_esquerdo")

normalizar_posicao_elemento <- function(x, padrao = "inferior_direito") {
  x <- tolower(trimws(as.character(x)[1]))
  x <- chartr("áàâãéêíóôõúç", "aaaaeeiooouc", x)
  x <- gsub("[ -]+", "_", x)
  if (is.na(x) || !x %in% posicoes_elementos_mapa) padrao else x
}

# maior comprimento "redondo" (1, 2 ou 5 x 10^k) que não passa de 'maximo'
comprimento_barra_escala <- function(maximo) {
  p <- 10^floor(log10(maximo))
  for (m in c(5, 2, 1)) if (m * p <= maximo) return(m * p)
  p
}

texto_numero_escala <- function(x) format(x, decimal.mark = ",", trim = TRUE, scientific = FALSE)

# desenha a barra de escala e/ou a seta do norte no quadro atual
desenhar_escala_norte <- function(r, escala, norte, pos_escala, pos_norte, cores, fundo) {

  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  ux  <- (usr[2] - usr[1]) / pin[1]          # unidades do mapa por polegada (x)
  uy  <- (usr[4] - usr[3]) / pin[2]          # unidades do mapa por polegada (y)

  invertidas <- identical(cores, "invertidas")
  fg <- if (invertidas) "white" else "#222222"
  bg <- if (invertidas) "#111111" else "white"
  cex <- 0.6 * min(1.15, max(0.75, pin[1] / 4))           # Tamanho do tesxto, mude o primeiro multiplicador 0.6
  fonte_h <- graphics::strheight("0", cex = cex) / uy     # altura do texto (pol)
  margem <- 0.12; folga <- 0.035; pad <- 0.06; entre <- 0.15

  # ---- tamanho de cada elemento (em polegadas) ----
  esc <- NULL
  if (isTRUE(escala)) {
    ycen <- mean(usr[3:4])
    km_por_unidade <- if (isTRUE(terra::is.lonlat(r))) 111.32 * cos(ycen * pi / 180) else {
      lu <- tryCatch(terra::linearUnits(r), error = function(e) NA_real_)
      if (length(lu) != 1 || is.na(lu) || lu <= 0) lu <- 1
      lu / 1000
    }
    alvo_pol  <- 0.3 * pin[1]       # Comprimento da barra, mude o multiplicador do pin[1]
    comp_km   <- comprimento_barra_escala(alvo_pol * ux * km_por_unidade)
    comp_pol  <- comp_km / km_por_unidade / ux
    t_zero <- "0"; t_meio <- texto_numero_escala(comp_km / 2); t_fim <- texto_numero_escala(comp_km)
    w <- function(t) graphics::strwidth(t, cex = cex) / ux
    esq   <- w(t_zero) / 2
    dir   <- w(t_fim) / 2 + 0.03 + w("km")
    barra_h <- 0.01 * pin[1]       # Altura da barra, mude o multiplicador do pin[1]
    esc <- list(comp_pol = comp_pol, esq = esq, largura = esq + comp_pol + dir,
                altura = barra_h + folga + fonte_h, barra_h = barra_h,
                t = c(t_zero, t_meio, t_fim), w_fim = w(t_fim))
  }
  nor <- NULL
  if (isTRUE(norte)) {
    seta_h <- 0.04 * pin[1];             # Altura da seta, mude o multiplicador do pin[1]
    seta_w <- 0.01 * pin[1];             # Largura da seta, mude o multiplicador do pin[1]
    nor <- list(seta_h = seta_h, seta_w = seta_w,
                largura = max(2 * seta_w, graphics::strwidth("N", cex = cex, font = 2) / ux),
                altura = fonte_h + folga + seta_h)
  }

  # ---- posições (canto inferior esquerdo de cada caixa, em unidades do mapa) ----
  # O terra amplia a região de plotagem (usr) além da extensão do raster quando mantém a
  # proporção do mapa; a moldura desenhada é a extensão do raster. Os cantos para ancorar
  # os elementos são, então, a extensão do raster recortada pela região de plotagem.
  ex <- tryCatch(as.vector(terra::ext(r)), error = function(e) usr[c(1, 2, 3, 4)])
  lim <- c(max(usr[1], ex[1]), min(usr[2], ex[2]), max(usr[3], ex[3]), min(usr[4], ex[4]))
  if (!all(is.finite(lim)) || lim[2] <= lim[1] || lim[4] <= lim[3]) lim <- usr
  canto_x0 <- function(pos, largura) {
    if (grepl("direito", pos)) lim[2] - (margem + largura) * ux else lim[1] + margem * ux
  }
  canto_y0 <- function(pos, altura) {
    if (grepl("inferior", pos)) lim[3] + margem * uy else lim[4] - (margem + altura) * uy
  }
  if (!is.null(esc)) { esc$x0 <- canto_x0(pos_escala, esc$largura); esc$y0 <- canto_y0(pos_escala, esc$altura) }
  if (!is.null(nor)) { nor$x0 <- canto_x0(pos_norte, nor$largura);   nor$y0 <- canto_y0(pos_norte, nor$altura) }
  # no mesmo canto: escala e norte lado a lado (escala à esquerda, norte à direita)
  if (!is.null(esc) && !is.null(nor) && identical(pos_escala, pos_norte)) {
    total <- esc$largura + entre + nor$largura
    x_ini <- canto_x0(pos_escala, total)
    esc$x0 <- x_ini
    nor$x0 <- x_ini + (esc$largura + entre) * ux
  }

  # ---- fundo semitransparente ----
  if (isTRUE(fundo)) {
    cor_fundo <- grDevices::adjustcolor(bg, alpha.f = 0.75)
    caixas <- list(esc, nor)
    caixas <- caixas[!vapply(caixas, is.null, logical(1))]
    if (length(caixas) == 2 && identical(pos_escala, pos_norte)) {
      caixas <- list(list(x0 = esc$x0, y0 = min(esc$y0, nor$y0),
                          largura = (nor$x0 - esc$x0) / ux + nor$largura,
                          altura = max(esc$altura, nor$altura)))
    }
    for (cx in caixas) {
      graphics::rect(cx$x0 - pad * ux, cx$y0 - pad * uy,
                     cx$x0 + (cx$largura + pad) * ux, cx$y0 + (cx$altura + pad) * uy,
                     col = cor_fundo, border = NA)
    }
  }

  # ---- barra de escala ----
  if (!is.null(esc)) {
    xb <- esc$x0 + esc$esq * ux
    comp <- esc$comp_pol * ux
    yb <- esc$y0
    graphics::rect(xb, yb, xb + comp / 2, yb + esc$barra_h * uy, col = fg, border = fg, lwd = 0.6)
    graphics::rect(xb + comp / 2, yb, xb + comp, yb + esc$barra_h * uy, col = bg, border = fg, lwd = 0.6)
    yt <- yb + (esc$barra_h + folga) * uy
    graphics::text(xb,            yt, esc$t[1], adj = c(0.5, 0), cex = cex, col = fg)
    graphics::text(xb + comp / 2, yt, esc$t[2], adj = c(0.5, 0), cex = cex, col = fg)
    graphics::text(xb + comp,     yt, esc$t[3], adj = c(0.5, 0), cex = cex, col = fg)
    graphics::text(xb + comp + (esc$w_fim / 2 + 0.03) * ux, yt, "km", adj = c(0, 0), cex = cex, col = fg)
  }

  # ---- seta do norte ----
  if (!is.null(nor)) {
    xc <- nor$x0 + nor$largura * ux / 2
    graphics::polygon(xc + c(0, -nor$seta_w, 0, nor$seta_w) * ux,
                      nor$y0 + c(nor$seta_h, 0, 0.3 * nor$seta_h, 0) * uy,
                      col = fg, border = NA)
    graphics::text(xc, nor$y0 + (nor$seta_h + folga) * uy, "N", adj = c(0.5, 0),
                   cex = cex, font = 2, col = fg)
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Contorno da máscara de estudo ('figuras_mascara')
#   A máscara (shapefile ou raster) vira um polígono único, desenhado como linha
#   sobre cada mapa (em todos os mapas do painel).
# ------------------------------------------------------------------------------
preparar_mascara_figuras <- function(mascara) {
  if (is.null(mascara)) return(NULL)
  v <- tryCatch({
    if (inherits(mascara, "SpatRaster")) {
      p <- terra::as.polygons(mascara[[1]], dissolve = TRUE)
      valores <- terra::values(p)[, 1]
      p <- p[!is.na(valores) & valores != 0]
      terra::aggregate(p)
    } else if (inherits(mascara, "SpatVector")) {
      terra::aggregate(mascara)
    } else NULL
  }, error = function(e) {
    message("  (contorno da máscara não preparado: ", conditionMessage(e), ")")
    NULL
  })
  v
}

acrescentar_mascara <- function(r) {
  o <- .sdm_figuras_opcoes
  if (is.null(o$mascara)) return(invisible(NULL))
  tryCatch({
    v <- o$mascara
    if (!terra::same.crs(v, r)) v <- terra::project(v, terra::crs(r))
    terra::lines(v, col = o$mascara_cor, lwd = o$mascara_espessura)
  }, error = function(e) {
    if (!isTRUE(o$avisou_mascara)) {
      message("  (contorno da máscara não desenhado: ", conditionMessage(e), ")")
      o$avisou_mascara <- TRUE
    }
  })
  invisible(NULL)
}

# cor de preenchimento da máscara ('figuras_mascara_preenchimento'): NA = sem preenchimento
# (as células de valor mínimo ficam com a cor mínima da escala de cores)
cor_preenchimento_mascara <- function() {
  f <- .sdm_figuras_opcoes$mascara_fill
  if (is.null(f)) NA else f
}

# Pinta com a cor de preenchimento as células de adequabilidade 0 (por cima do mapa contínuo)
acrescentar_preenchimento_zero <- function(r) {
  cor <- cor_preenchimento_mascara()
  if (is.na(cor)) return(invisible(NULL))
  tryCatch({
    zeros <- terra::ifel(r <= 0, 1, NA)
    terra::plot(zeros, col = cor, legend = FALSE, axes = FALSE, add = TRUE)
  }, error = function(e) {
    if (!isTRUE(.sdm_figuras_opcoes$avisou_preench)) {
      message("  (preenchimento da máscara não desenhado: ", conditionMessage(e), ")")
      .sdm_figuras_opcoes$avisou_preench <- TRUE
    }
  })
  invisible(NULL)
}

# Extensão do mapa com uma folga ('figuras_margem_mapa' = fração da largura/altura), para o
# mapa não encostar nas bordas do quadro. Usada em terra::plot(ext = ...).
extensao_com_margem <- function(r) {
  f <- .sdm_figuras_opcoes$margem_mapa
  e <- terra::ext(r)
  if (!is.numeric(f) || length(f) != 1 || is.na(f) || f <= 0) return(e)
  dx <- (e[2] - e[1]) * f
  dy <- (e[4] - e[3]) * f
  terra::ext(e[1] - dx, e[2] + dx, e[3] - dy, e[4] + dy)
}

# Nos painéis, 'figuras_elementos_painel' = "ultimo" mostra legenda, escala e norte só no
# último mapa do painel. Os laços dos painéis chamam esta função antes de cada mapa.
definir_elementos_painel <- function(i, n) {
  o <- .sdm_figuras_opcoes
  o$mostrar_elementos <- !identical(o$elementos_painel, "ultimo") || i == n
  invisible(NULL)
}
restaurar_elementos_painel <- function() {
  .sdm_figuras_opcoes$mostrar_elementos <- TRUE
  invisible(NULL)
}
elementos_visiveis <- function() !isFALSE(.sdm_figuras_opcoes$mostrar_elementos)

# barra de escala e seta do norte sobre o mapa atual (opções de gerar_figuras)
acrescentar_elementos_mapa <- function(r) {
  o <- .sdm_figuras_opcoes
  if (!elementos_visiveis()) return(invisible(NULL))
  if (!isTRUE(o$escala) && !isTRUE(o$norte)) return(invisible(NULL))
  tryCatch(
    desenhar_escala_norte(r, escala = o$escala, norte = o$norte,
                          pos_escala = o$pos_escala, pos_norte = o$pos_norte,
                          cores = o$cores, fundo = isTRUE(o$fundo)),
    error = function(e) {
      if (!isTRUE(o$avisou)) {
        message("  (escala/norte não desenhados: ", conditionMessage(e), ")")
        o$avisou <- TRUE
      }
    })
  invisible(NULL)
}

# abre o(s) dispositivo(s) gráfico(s), chama desenhar() e fecha
#   largura / altura em cm; devolve os arquivos gerados
salvar_figura <- function(nome_base, desenhar, formatos, largura, altura, resolucao = 300) {
  dir.create(dirname(nome_base), recursive = TRUE, showWarnings = FALSE)
  gerados <- character(0)
  for (formato in formatos) {
    arquivo <- paste0(nome_base, ".", formato)
    if (formato == "png") {
      grDevices::png(arquivo, width = largura, height = altura, units = "cm", res = resolucao)
    } else {
      grDevices::pdf(arquivo, width = largura / 2.54, height = altura / 2.54)
    }
    ok <- tryCatch({ desenhar(); TRUE }, error = function(e) {
      message("\n  Figura não gerada (", basename(arquivo), "): ", conditionMessage(e))
      FALSE
    })
    grDevices::dev.off()
    if (ok) gerados <- c(gerados, arquivo) else unlink(arquivo)
  }
  invisible(gerados)
}

# número de colunas e linhas de um painel com n quadros
grade_painel <- function(n) {
  colunas <- ceiling(sqrt(n))
  c(linhas = ceiling(n / colunas), colunas = colunas)
}

# dimensões (cm) de um painel de mapas: cada quadro com a proporção do raster
dimensoes_painel <- function(arquivo_referencia, n, largura_painel = 10) {
  r <- terra::rast(arquivo_referencia)
  proporcao <- terra::nrow(r) / terra::ncol(r)
  proporcao <- min(max(proporcao, 0.5), 1.6)
  g <- grade_painel(n)
  c(largura = g[["colunas"]] * largura_painel,
    altura  = g[["linhas"]] * (largura_painel * proporcao + 1.2))
}

# mapa contínuo (0 a 1) com os pontos de presença
desenhar_continuo <- function(arquivo, titulo, pontos = NULL, paleta = "YlGnBu") {
  r <- terra::rast(arquivo)
  t <- ajustar_titulo(titulo, 0.9)
  terra::plot(r, range = c(0, 1), col = rev(grDevices::hcl.colors(100, paleta)),
              ext = extensao_com_margem(r), legend = elementos_visiveis(), main = t$texto, cex.main = t$cex, mar = c(2, 2, 0.8 + 1.1 * t$n, 4))
  acrescentar_preenchimento_zero(r)
  if (!is.null(pontos) && nrow(pontos) > 0) {
    graphics::points(pontos$x, pontos$y, pch = 21, bg = "red", col = "white", cex = 0.5, lwd = 0.3)
  }
  acrescentar_mascara(r)
  acrescentar_elementos_mapa(r)
}

# mapa binário (presença em verde, ausência em cinza claro ou na cor de preenchimento da máscara)
desenhar_binario <- function(arquivo, titulo, pontos = NULL) {
  r <- terra::rast(arquivo)
  t <- ajustar_titulo(titulo, 0.9)
  cor_ausencia <- if (is.na(cor_preenchimento_mascara())) "grey92" else cor_preenchimento_mascara()
  terra::plot(r, breaks = c(-0.5, 0.5, 1.5), col = c(cor_ausencia, "#1b7837"), legend = FALSE,
              ext = extensao_com_margem(r),
              main = t$texto, cex.main = t$cex, mar = c(2, 2, 0.8 + 1.1 * t$n, 1))
  if (!is.null(pontos) && nrow(pontos) > 0) {
    graphics::points(pontos$x, pontos$y, pch = 21, bg = "red", col = "white", cex = 0.5, lwd = 0.3)
  }
  acrescentar_mascara(r)
  acrescentar_elementos_mapa(r)
}

# pontos de presença da espécie (ou NULL)
pontos_da_especie <- function(dados_modelos, especie, usar = TRUE) {
  if (!isTRUE(usar) || is.null(dados_modelos[[especie]])) return(NULL)
  d <- dados_modelos[[especie]]$dados
  d[d$presenca == 1, c("x", "y"), drop = FALSE]
}


# ---- painéis e figuras separadas de mapas -----------------------------------

# layout: "painel" (um quadro com todos os mapas), "separadas" (um arquivo por mapa)
# ou os dois; "all" = os dois
normalizar_layout_figuras <- function(layout) {
  layout <- unique(tolower(trimws(layout)))
  layout[layout %in% c("separada", "separado", "separados")] <- "separadas"
  if ("all" %in% layout) layout <- c("painel", "separadas")
  invalidos <- setdiff(layout, c("painel", "separadas"))
  if (length(invalidos) > 0 || length(layout) == 0) {
    stop("'figuras_layout' inválido: ", paste(c(invalidos, if (length(layout) == 0) "(vazio)"), collapse = ", "),
         "\nOpções: painel, separadas, all", call. = FALSE)
  }
  layout
}

# Faz as figuras de uma lista de mapas: o painel e/ou um arquivo por mapa.
#   binario  : TRUE/FALSE por mapa (binário ou contínuo)
#   ids      : nome do arquivo de cada mapa quando "separadas"
emitir_mapas <- function(arquivos, titulos, binario, ids, base_painel, titulo_geral, layout,
                         pontos, paleta, formatos, resolucao, largura_painel) {
  gerados <- character(0)
  desenhar_um <- function(i, titulo) {
    if (binario[i]) desenhar_binario(arquivos[i], titulo, pontos)
    else desenhar_continuo(arquivos[i], titulo, pontos, paleta)
  }

  if ("painel" %in% layout) {
    dim <- dimensoes_painel(arquivos[1], length(arquivos), largura_painel)
    g   <- grade_painel(length(arquivos))
    desenhar <- function() {
      graphics::par(mfrow = c(g[["linhas"]], g[["colunas"]]))
      tg <- preparar_titulo_geral(titulo_geral)
      for (i in seq_along(arquivos)) {
        definir_elementos_painel(i, length(arquivos))
        desenhar_um(i, titulos[i])
      }
      restaurar_elementos_painel()
      escrever_titulo_geral(tg)
    }
    gerados <- c(gerados, salvar_figura(base_painel, desenhar, formatos,
                                        dim[["largura"]], dim[["altura"]] + 1, resolucao))
  }

  if ("separadas" %in% layout) {
    dim <- dimensoes_painel(arquivos[1], 1, largura_painel * 1.3)
    for (i in seq_along(arquivos)) {
      desenhar <- function() desenhar_um(i, paste0(titulo_geral, "\n", titulos[i]))
      gerados <- c(gerados, salvar_figura(file.path(dirname(base_painel), ids[i]), desenhar,
                                          formatos, dim[["largura"]], dim[["altura"]] + 1.8, resolucao))
    }
  }
  gerados
}

# painel (ou figuras separadas) de mapas CONTÍNUOS por espécie e conjunto
figuras_modelos <- function(mapas, consenso, dados_modelos, pasta, formatos, resolucao,
                            largura_painel, paleta, usar_pontos, layout = "painel") {
  gerados <- character(0)
  cont <- mapas[mapas$tipo == "continuo", , drop = FALSE]
  cons <- if (!is.null(consenso$indice)) consenso$indice[consenso$indice$tipo == "continuo", , drop = FALSE] else NULL

  for (especie in unique(cont$especie)) {
    for (conjunto in unique(cont$conjunto[cont$especie == especie])) {
      a <- cont[cont$especie == especie & cont$conjunto == conjunto, , drop = FALSE]
      arquivos <- a$arquivo
      titulos  <- toupper(a$modelo)
      ids      <- paste0(nome_seguro(especie), "_", conjunto, "_", a$modelo)
      if (!is.null(cons)) {
        b <- cons[cons$especie == especie & cons$conjunto == conjunto, , drop = FALSE]
        arquivos <- c(arquivos, b$arquivo)
        titulos  <- c(titulos, paste0("consenso (", b$metodo, ")"))
        ids      <- c(ids, paste0(nome_seguro(especie), "_", conjunto, "_consenso_", b$metodo))
      }
      manter <- file.exists(arquivos)
      arquivos <- arquivos[manter]; titulos <- titulos[manter]; ids <- ids[manter]
      if (length(arquivos) == 0) next
      pontos <- pontos_da_especie(dados_modelos, especie, usar_pontos)
      gerados <- c(gerados, emitir_mapas(
        arquivos, titulos, rep(FALSE, length(arquivos)), ids,
        file.path(pasta, nome_seguro(especie), paste0(nome_seguro(especie), "_", conjunto, "_modelos")),
        paste0(especie, " - ", conjunto, " (adequabilidade)"), layout, pontos, paleta,
        formatos, resolucao, largura_painel))
    }
  }
  gerados
}

# painel (ou figuras separadas) de mapas BINÁRIOS por espécie, conjunto e limiar
figuras_binarios <- function(mapas, consenso, dados_modelos, pasta, formatos, resolucao,
                             largura_painel, usar_pontos, layout = "painel") {
  gerados <- character(0)
  bin  <- mapas[mapas$tipo == "binario", , drop = FALSE]
  if (nrow(bin) == 0) {
    message("Não há mapas binários (salvar_binarios = FALSE ou sem 'metodos_limiar'): figuras de binários puladas.")
    return(gerados)
  }
  cons <- if (!is.null(consenso$indice)) consenso$indice[consenso$indice$tipo == "binario", , drop = FALSE] else NULL

  for (especie in unique(bin$especie)) {
    for (conjunto in unique(bin$conjunto[bin$especie == especie])) {
      sub <- bin[bin$especie == especie & bin$conjunto == conjunto, , drop = FALSE]
      for (limiar in unique(sub$metodo_limiar)) {
        a <- sub[sub$metodo_limiar == limiar, , drop = FALSE]
        arquivos <- a$arquivo
        titulos  <- paste0(toupper(a$modelo), " (", round(a$limiar, 3), ")")
        ids      <- paste0(nome_seguro(especie), "_", conjunto, "_", a$modelo, "_bin_", limiar)
        if (!is.null(cons)) {
          b <- cons[cons$especie == especie & cons$conjunto == conjunto &
                      cons$metodo_limiar %in% limiar, , drop = FALSE]
          arquivos <- c(arquivos, b$arquivo)
          titulos  <- c(titulos, paste0("consenso ", b$metodo, " (", round(b$limiar, 3), ")"))
          ids      <- c(ids, paste0(nome_seguro(especie), "_", conjunto, "_consenso_", b$metodo,
                                    "_bin_", limiar))
        }
        manter <- file.exists(arquivos)
        arquivos <- arquivos[manter]; titulos <- titulos[manter]; ids <- ids[manter]
        if (length(arquivos) == 0) next
        pontos <- pontos_da_especie(dados_modelos, especie, usar_pontos)
        gerados <- c(gerados, emitir_mapas(
          arquivos, titulos, rep(TRUE, length(arquivos)), ids,
          file.path(pasta, nome_seguro(especie),
                    paste0(nome_seguro(especie), "_", conjunto, "_binarios_", limiar)),
          paste0(especie, " - ", conjunto, " - limiar: ", limiar), layout, pontos, "YlGnBu",
          formatos, resolucao, largura_painel))
      }
    }
  }
  gerados
}

# consenso: mapa contínuo ao lado dos mapas binários do mesmo método
figuras_consenso <- function(consenso, dados_modelos, pasta, formatos, resolucao,
                             largura_painel, paleta, usar_pontos, layout = "painel") {
  gerados <- character(0)
  if (is.null(consenso$indice) || nrow(consenso$indice) == 0) {
    message("Não há consenso: figuras do consenso puladas.")
    return(gerados)
  }
  ind <- consenso$indice
  for (especie in unique(ind$especie)) {
    for (conjunto in unique(ind$conjunto[ind$especie == especie])) {
      for (metodo in unique(ind$metodo[ind$especie == especie & ind$conjunto == conjunto])) {
        sub  <- ind[ind$especie == especie & ind$conjunto == conjunto & ind$metodo == metodo, , drop = FALSE]
        cont <- sub[sub$tipo == "continuo", , drop = FALSE]
        bin  <- sub[sub$tipo == "binario", , drop = FALSE]
        if (nrow(cont) == 0 || !file.exists(cont$arquivo[1])) next
        bin <- bin[file.exists(bin$arquivo), , drop = FALSE]
        base_id <- paste0(nome_seguro(especie), "_", conjunto, "_consenso_", metodo)
        arquivos <- c(cont$arquivo[1], bin$arquivo)
        titulos  <- c("adequabilidade", paste0("limiar ", bin$metodo_limiar, " (", round(bin$limiar, 3), ")"))
        ids      <- c(base_id, paste0(base_id, "_bin_", bin$metodo_limiar))
        pontos <- pontos_da_especie(dados_modelos, especie, usar_pontos)
        gerados <- c(gerados, emitir_mapas(
          arquivos, titulos, c(FALSE, rep(TRUE, nrow(bin))), ids,
          file.path(pasta, nome_seguro(especie), base_id),
          paste0(especie, " - ", conjunto, " - consenso: ", metodo), layout, pontos, paleta,
          formatos, resolucao, largura_painel))
      }
    }
  }
  gerados
}

# consenso de cada conjunto (preditoras e cenários) lado a lado
figuras_cenarios <- function(consenso, dados_modelos, pasta, formatos, resolucao,
                             largura_painel, paleta, usar_pontos, layout = "painel") {
  gerados <- character(0)
  if (is.null(consenso$indice) || nrow(consenso$indice) == 0) {
    message("Não há consenso: figuras de cenários puladas.")
    return(gerados)
  }
  ind <- consenso$indice[consenso$indice$tipo == "continuo", , drop = FALSE]
  for (especie in unique(ind$especie)) {
    for (metodo in unique(ind$metodo[ind$especie == especie])) {
      sub <- ind[ind$especie == especie & ind$metodo == metodo, , drop = FALSE]
      sub <- sub[file.exists(sub$arquivo), , drop = FALSE]
      if (nrow(sub) < 2) next
      pontos <- pontos_da_especie(dados_modelos, especie, usar_pontos)
      gerados <- c(gerados, emitir_mapas(
        sub$arquivo, sub$conjunto, rep(FALSE, nrow(sub)),
        paste0(nome_seguro(especie), "_cenarios_", metodo, "_", sub$conjunto),
        file.path(pasta, nome_seguro(especie), paste0(nome_seguro(especie), "_cenarios_", metodo)),
        paste0(especie, " - consenso (", metodo, "): preditoras x cenários"), layout, pontos, paleta,
        formatos, resolucao, largura_painel))
    }
  }
  gerados
}


# ---- extrapolação (MESS) ----------------------------------------------------------
#  Lê os rasters de 'extrapolacao/<espécie>/<conjunto>/' (seção 21) e faz, por espécie e
#  conjunto: o MESS (vermelho = extrapolação, azul = dentro da faixa), a área em
#  extrapolação (MESS < 0) e a variável mais diferente onde há extrapolação.

# margens idênticas nos 3 mapas: o quadro de cada um fica do mesmo tamanho e alinhado
margens_extrapolacao <- function() c(2, 2, 3, 6)

desenhar_mess <- function(arquivo, titulo) {
  r <- terra::rast(arquivo)
  t <- ajustar_titulo(titulo, 0.9)
  rg  <- terra::global(r, "range", na.rm = TRUE)
  lim <- max(abs(unlist(rg)), 1, na.rm = TRUE)
  cores <- grDevices::colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))(101)
  terra::plot(r, range = c(-lim, lim), col = cores, ext = extensao_com_margem(r),
              legend = elementos_visiveis(), main = t$texto, cex.main = t$cex,
              mar = margens_extrapolacao())
  acrescentar_mascara(r)
  acrescentar_elementos_mapa(r)
}

desenhar_extrapolacao_area <- function(arquivo, titulo) {
  r <- terra::rast(arquivo)
  t <- ajustar_titulo(titulo, 0.9)
  terra::plot(r, breaks = c(-0.5, 0.5, 1.5), col = c("grey92", "#B2182B"), legend = FALSE,
              ext = extensao_com_margem(r), main = t$texto, cex.main = t$cex,
              mar = margens_extrapolacao())
  if (elementos_visiveis()) {
    usr <- graphics::par("usr")
    graphics::legend("topright", legend = c("dentro da faixa", "extrapolação"),
                     fill = c("grey92", "#B2182B"), cex = 0.6, bty = "n", inset = 0.01)
  }
  acrescentar_mascara(r)
  acrescentar_elementos_mapa(r)
}

desenhar_variavel_diferente <- function(arquivo_mod, arquivo_ext, nomes, titulo) {
  mod <- terra::rast(arquivo_mod) + 0          # + 0: tira as categorias do raster
  ext <- terra::rast(arquivo_ext)
  t <- ajustar_titulo(titulo, 0.9)
  mod <- terra::ifel(ext > 0, mod, NA)          # só onde há extrapolação
  presentes <- sort(unique(stats::na.omit(terra::values(mod)[, 1])))
  paleta <- grDevices::hcl.colors(max(length(nomes), 3), "Dark 3")
  par_mar <- margens_extrapolacao()
  if (length(presentes) == 0) {
    terra::plot(ext * 0, col = "grey92", legend = FALSE, ext = extensao_com_margem(ext),
                main = t$texto, cex.main = t$cex, mar = par_mar)
  } else {
    terra::plot(mod, breaks = c(presentes - 0.5, max(presentes) + 0.5),
                col = paleta[presentes], legend = FALSE, ext = extensao_com_margem(mod),
                main = t$texto, cex.main = t$cex, mar = par_mar)
    if (elementos_visiveis()) {
      usr <- graphics::par("usr")
      graphics::legend(x = usr[2] + 0.015 * (usr[2] - usr[1]), y = usr[4], xpd = NA,
                       legend = nomes[presentes], fill = paleta[presentes],
                       cex = 0.6, bty = "n", y.intersp = 1.1)
    }
  }
  acrescentar_mascara(mod)
  acrescentar_elementos_mapa(mod)
}

figuras_extrapolacao <- function(pasta_extrapolacao, preditores, pasta, formatos, resolucao,
                                 largura_painel, layout = "painel", especies_sel = NULL,
                                 conjuntos_sel = NULL) {
  gerados <- character(0)
  if (!dir.exists(pasta_extrapolacao)) {
    message("Não há resultados de extrapolação em ", pasta_extrapolacao, ": figuras puladas.")
    return(gerados)
  }
  nomes <- if (!is.null(preditores)) preditores$nomes else NULL
  arq_leg <- file.path(pasta_extrapolacao, "legenda_variavel_mais_diferente.csv")
  if (file.exists(arq_leg)) nomes <- as.character(utils::read.csv(arq_leg)$preditor)
  if (is.null(nomes)) nomes <- character(0)

  for (pasta_esp in list.dirs(pasta_extrapolacao, recursive = FALSE)) {
    especie <- basename(pasta_esp)
    if (!is.null(especies_sel) && !(especie %in% nome_seguro(especies_sel)) &&
        !(especie %in% especies_sel)) next
    for (pasta_cj in list.dirs(pasta_esp, recursive = FALSE)) {
      conjunto <- basename(pasta_cj)
      if (!is.null(conjuntos_sel) && !conjunto %in% conjuntos_sel) next
      a_mess <- file.path(pasta_cj, "mess.tif")
      a_ext  <- file.path(pasta_cj, "extrapolacao.tif")
      a_mod  <- file.path(pasta_cj, "variavel_mais_diferente.tif")
      if (!all(file.exists(c(a_mess, a_ext, a_mod)))) next
      if (length(nomes) == 0) nomes <- as.character(seq_len(max(terra::global(terra::rast(a_mod) + 0, "max", na.rm = TRUE)[1, 1], 1)))

      base <- file.path(pasta, especie, paste0(especie, "_extrapolacao_", conjunto))
      titulo_geral <- paste0(especie, " - extrapolação (MESS) - ", conjunto)
      sub_mess <- "MESS (vermelho = extrapolação)"
      sub_area <- "Área em extrapolação (MESS < 0)"
      sub_mod  <- "Variável mais diferente (onde há extrapolação)"
      desenhar_i <- function(i, titulo) {
        if (i == 1) desenhar_mess(a_mess, titulo)
        else if (i == 2) desenhar_extrapolacao_area(a_ext, titulo)
        else desenhar_variavel_diferente(a_mod, a_ext, nomes, titulo)
      }

      if ("painel" %in% layout) {
        r0 <- terra::rast(a_mess)
        prop <- min(max(terra::nrow(r0) / terra::ncol(r0), 0.5), 1.6)
        desenhar <- function() {
          graphics::par(mfrow = c(1, 3))
          tg <- preparar_titulo_geral(titulo_geral)
          subs <- c(sub_mess, sub_area, sub_mod)
          for (i in 1:3) {
            definir_elementos_painel(i, 3)
            desenhar_i(i, subs[i])
          }
          restaurar_elementos_painel()
          escrever_titulo_geral(tg)
        }
        gerados <- c(gerados, salvar_figura(base, desenhar, formatos, 3 * largura_painel * 1.1,
                                            largura_painel * prop + 3.5, resolucao))
      }
      if ("separadas" %in% layout) {
        r0 <- terra::rast(a_mess)
        prop <- min(max(terra::nrow(r0) / terra::ncol(r0), 0.5), 1.6)
        subs <- c(mess = sub_mess, area = sub_area, variavel = sub_mod)
        for (i in 1:3) {
          local({
            ii <- i
            desenhar <- function() desenhar_i(ii, paste0(titulo_geral, "\n", subs[ii]))
            gerados <<- c(gerados, salvar_figura(paste0(base, "_", names(subs)[ii]), desenhar,
                                                 formatos, largura_painel * 1.3 + (if (ii == 3) 2.5 else 0),
                                                 largura_painel * 1.3 * prop + 2.6, resolucao))
          })
        }
      }
    }
  }
  gerados
}


# ---- preditores usados (variáveis ou eixos da PCA) ---------------------------------

figuras_variaveis <- function(preditores, pasta, formatos, resolucao, largura_painel, layout,
                              conjuntos_sel = NULL) {
  gerados <- character(0)
  if (is.null(preditores) || length(preditores$nomes) == 0) {
    message("Sem preditores definidos: figuras das variáveis puladas.")
    return(gerados)
  }
  cores <- grDevices::hcl.colors(100, "viridis")

  for (conjunto in preditores$conjuntos) {
    if (!is.null(conjuntos_sel) && !conjunto %in% conjuntos_sel) next
    arquivos <- preditores$arquivos[[conjunto]]
    if (is.null(arquivos) || !all(file.exists(arquivos))) next
    pilha <- terra::rast(arquivos)
    names(pilha) <- preditores$nomes
    n <- terra::nlyr(pilha)
    base <- file.path(pasta, "variaveis", paste0(conjunto, "_preditores"))

    desenhar_um <- function(i, titulo) {
      t <- ajustar_titulo(titulo, 0.9)
      terra::plot(pilha[[i]], col = cores, ext = extensao_com_margem(pilha[[i]]), legend = elementos_visiveis(), main = t$texto, cex.main = t$cex,
                  mar = c(2, 2, 0.8 + 1.1 * t$n, 4))
      acrescentar_mascara(pilha[[i]])
      acrescentar_elementos_mapa(pilha[[i]])
    }
    if ("painel" %in% layout) {
      dim <- dimensoes_painel(arquivos[1], n, largura_painel)
      g   <- grade_painel(n)
      desenhar <- function() {
        graphics::par(mfrow = c(g[["linhas"]], g[["colunas"]]))
        tg <- preparar_titulo_geral(paste0("Preditores usados - ", conjunto))
        for (i in seq_len(n)) {
          definir_elementos_painel(i, n)
          desenhar_um(i, preditores$nomes[i])
        }
        restaurar_elementos_painel()
        escrever_titulo_geral(tg)
      }
      gerados <- c(gerados, salvar_figura(base, desenhar, formatos, dim[["largura"]],
                                          dim[["altura"]] + 1, resolucao))
    }
    if ("separadas" %in% layout) {
      dim <- dimensoes_painel(arquivos[1], 1, largura_painel * 1.3)
      for (i in seq_len(n)) {
        desenhar <- function() desenhar_um(i, paste0(preditores$nomes[i], " (", conjunto, ")"))
        gerados <- c(gerados, salvar_figura(
          file.path(dirname(base), paste0(conjunto, "_", nome_seguro(preditores$nomes[i]))),
          desenhar, formatos, dim[["largura"]], dim[["altura"]] + 0.8, resolucao))
      }
    }
  }
  gerados
}


# ---- seleção de mapas específicos ----------------------------------------------------

# Resolve um parâmetro de seleção ('figuras_especies' etc.):
#   "all" ou NULL -> sem filtro (devolve NULL); valores -> confere e devolve;
#   vazio (= "prompt") -> mostra a lista e pergunta (a última opção é "all").
resolver_selecao_figuras <- function(valor, disponiveis, nome, rotulo) {
  disponiveis <- sort(unique(as.character(disponiveis[!is.na(disponiveis)])))
  if (length(disponiveis) == 0 || is.null(valor)) return(NULL)

  if (length(valor) == 0) {
    if (!interactive()) {
      message("'", nome, "' = \"prompt\" fora de sessão interativa: usando todos.")
      return(NULL)
    }
    cat("\n", rotulo, ":\n", sep = "")
    r <- perguntar_opcoes(c(disponiveis, "all (todos)"), "Número(s) (ex.: 1,3): ", rotulo)
    if ((length(disponiveis) + 1) %in% r) return(NULL)
    return(disponiveis[r])
  }

  v <- trimws(as.character(valor))
  if ("all" %in% tolower(v)) return(NULL)
  invalidos <- setdiff(v, disponiveis)
  if (length(invalidos) > 0) {
    stop("'", nome, "': valor(es) não encontrado(s): ", paste(invalidos, collapse = ", "),
         "\nDisponíveis: ", paste(disponiveis, collapse = ", "),
         "\n(Use \"all\" para todos ou \"prompt\" para escolher no console.)", call. = FALSE)
  }
  v
}

# mantém só as linhas da seleção (NULL = sem filtro em cada critério)
filtrar_indice_figuras <- function(d, especies, conjuntos, ids, limiares, e_consenso) {
  if (is.null(d) || nrow(d) == 0) return(d)
  manter <- rep(TRUE, nrow(d))
  if (!is.null(especies))  manter <- manter & d$especie %in% especies
  if (!is.null(conjuntos)) manter <- manter & d$conjunto %in% conjuntos
  if (!is.null(ids)) {
    id <- if (e_consenso) paste0("consenso_", d$metodo) else d$modelo
    manter <- manter & id %in% ids
  }
  if (!is.null(limiares)) manter <- manter & (d$tipo != "binario" | d$metodo_limiar %in% limiares)
  d[manter, , drop = FALSE]
}


# ---- importância das variáveis -------------------------------------------------

# barras horizontais ordenadas (valores nomeados, em %)
desenhar_barras <- function(valores, titulo, cor = "#2c7fb8") {
  valores <- sort(valores[!is.na(valores)])
  t <- ajustar_titulo(titulo, 0.9)
  if (length(valores) == 0) {
    graphics::plot.new()
    graphics::title(t$texto, cex.main = t$cex)
    return(invisible(NULL))
  }
  # margem esquerda do tamanho do maior nome de variável
  esq <- max(graphics::strwidth(names(valores), units = "inches", cex = 0.8)) / graphics::par("csi") + 1.2
  graphics::par(mar = c(4, min(max(esq, 4), 16), 0.8 + 1.1 * t$n, 1))
  graphics::barplot(valores, horiz = TRUE, las = 1, col = cor, border = NA,
                    main = t$texto, cex.main = t$cex, cex.names = 0.8,
                    xlab = "importância (%)", xlim = c(0, max(valores) * 1.1))
}

figuras_importancia <- function(importancia, pasta, formatos, resolucao, largura_painel) {
  gerados <- character(0)
  tab <- importancia$tabela
  if (is.null(tab) || nrow(tab) == 0) {
    message("Não há importância calculada: figuras de importância puladas.")
    return(gerados)
  }
  rotulos <- c(permutacao = "Permutação (queda do AUC)",
               jackknife_sem = "Jackknife: sem a variável (queda do AUC)",
               jackknife_so = "Jackknife: só a variável (AUC)",
               regressao = "Regressão (queda de deviance)")

  for (especie in unique(tab$especie)) {
    for (metodo in unique(tab$metodo[tab$especie == especie])) {
      sub <- tab[tab$especie == especie & tab$metodo == metodo, , drop = FALSE]
      modelos <- unique(sub$modelo)
      media <- tapply(sub$importancia_pct, sub$variavel, mean, na.rm = TRUE)
      media <- setNames(as.vector(media), names(media))
      quadros <- c(as.list(setNames(lapply(modelos, function(m) {
        x <- sub[sub$modelo == m, , drop = FALSE]
        setNames(x$importancia_pct, x$variavel)
      }), toupper(modelos))), if (length(modelos) > 1) list("MÉDIA" = media))
      g <- grade_painel(length(quadros))
      largura <- max(g[["colunas"]] * largura_painel, 1.4 * largura_painel)
      altura  <- g[["linhas"]] * (largura_painel * 0.9) + 2.5
      rotulo  <- if (metodo %in% names(rotulos)) rotulos[[metodo]] else metodo

      desenhar <- function() {
        graphics::par(mfrow = c(g[["linhas"]], g[["colunas"]]))
        tg <- preparar_titulo_geral(paste0(especie, " - ", rotulo))
        for (i in seq_along(quadros)) desenhar_barras(quadros[[i]], names(quadros)[i])
        escrever_titulo_geral(tg)
      }
      gerados <- c(gerados, salvar_figura(
        file.path(pasta, nome_seguro(especie), paste0(nome_seguro(especie), "_importancia_", metodo)),
        desenhar, formatos, largura, altura, resolucao))
    }
  }
  gerados
}


# ---- curvas de resposta ---------------------------------------------------------

# curva de cada preditor (os demais na mediana) para um modelo final
figuras_curvas_resposta <- function(modelos_finais, dados_modelos, pasta, formatos, resolucao,
                                    largura_painel) {
  gerados <- character(0)
  for (especie in names(modelos_finais)) {
    dados <- dados_modelos[[especie]]$dados
    preditores <- dados_modelos[[especie]]$preditores
    base <- as.data.frame(lapply(dados[preditores], stats::median, na.rm = TRUE))

    for (codigo in names(modelos_finais[[especie]])) {
      objeto <- modelos_finais[[especie]][[codigo]]
      g <- grade_painel(length(preditores))

      desenhar <- function() {
        graphics::par(mfrow = c(g[["linhas"]], g[["colunas"]]))
        tg <- preparar_titulo_geral(paste0(especie, " - ", toupper(codigo), ": curvas de resposta"))
        tv <- lapply(preditores, ajustar_titulo, cex = 0.9,
                     largura = graphics::par("din")[1] / g[["colunas"]])
        names(tv) <- preditores
        graphics::par(mar = c(4, 4, 0.8 + 1.1 * max(vapply(tv, function(x) x$n, numeric(1))), 1))
        for (v in preditores) {
          tt <- tv[[v]]
          x  <- dados[[v]]
          xs <- seq(stats::quantile(x, 0.01, na.rm = TRUE), stats::quantile(x, 0.99, na.rm = TRUE),
                    length.out = 60)
          novos <- base[rep(1, length(xs)), , drop = FALSE]
          novos[[v]] <- xs
          y <- tryCatch(prever_modelo(objeto, novos), error = function(e) rep(NA_real_, length(xs)))
          graphics::plot(xs, y, type = "l", lwd = 2, col = "#2c7fb8", ylim = c(0, 1),
                         xlab = v, ylab = "adequabilidade", main = tt$texto, cex.main = tt$cex)
          graphics::rug(x[dados$presenca == 1], col = "#d7301f")
        }
        escrever_titulo_geral(tg)
      }
      largura <- max(g[["colunas"]] * largura_painel * 0.7, 1.4 * largura_painel)
      altura  <- g[["linhas"]] * largura_painel * 0.6 + 2.5
      gerados <- c(gerados, salvar_figura(
        file.path(pasta, nome_seguro(especie), paste0(nome_seguro(especie), "_curvas_", codigo)),
        desenhar, formatos, largura, altura, resolucao))
    }
  }
  gerados
}


# ------------------------------------------------------------------------------
# gerar_figuras()
#   Faz as figuras dos 'tipos' pedidos nos 'formatos' pedidos. Cada figura é
#   independente: se uma falhar, aparece o aviso e as demais continuam.
#   'escala' / 'norte': desenha barra de escala e seta do norte nos mapas;
#   'posicao_escala' / 'posicao_norte': canto do mapa ("inferior_direito", "inferior_esquerdo",
#   "superior_direito", "superior_esquerdo"); 'cores_elementos': "normais" ou "invertidas"
#   (branco); 'fundo_elementos': TRUE = fundo semitransparente atrás dos dois;
#   'mascara_preenchimento': cor de preenchimento da área da máscara, atrás do mapa ("nenhum" = sem);
#   'margem_mapa': folga entre o mapa e as bordas do quadro (fração da largura/altura; 0 = sem folga);
#   'mascara': máscara de estudo (SpatVector/SpatRaster) desenhada como contorno nos mapas
#   (NULL = não desenha; 'mascara_cor' e 'mascara_espessura' controlam a linha);
#   'elementos_painel': "todos" ou "ultimo" - nos painéis, legenda, escala e norte em
#   todos os mapas ou só no último.
#   'importancia' só é necessária para o tipo "importancia"; 'consenso' para
#   "consenso", "cenarios" e como painel extra de "modelos" e "binarios".
#   Devolve a tabela dos arquivos gerados (e salva figuras.csv).
# ------------------------------------------------------------------------------
gerar_figuras <- function(mapas, consenso = NULL, importancia = NULL, modelos_finais = NULL,
                          dados_modelos, tipos, formatos, pasta_saida, resolucao = 300,
                          largura_painel = 10, paleta = "YlGnBu", usar_pontos = TRUE,
                          preditores = NULL, layout = "painel", sel_especies = NULL,
                          sel_conjuntos = NULL, sel_mapas = NULL, sel_limiares = NULL,
                          escala = FALSE, norte = FALSE,
                          posicao_escala = "inferior_direito", posicao_norte = "inferior_direito",
                          cores_elementos = "normais", fundo_elementos = FALSE,
                          elementos_painel = "todos",
                          mascara = NULL, mascara_cor = "black", mascara_espessura = 0.8,
                          margem_mapa = 0.03, mascara_preenchimento = "nenhum",
                          pasta_extrapolacao = file.path(dirname(pasta_saida), "extrapolacao")) {

  .sdm_figuras_opcoes$escala     <- isTRUE(escala)
  .sdm_figuras_opcoes$norte      <- isTRUE(norte)
  .sdm_figuras_opcoes$pos_escala <- normalizar_posicao_elemento(posicao_escala)
  .sdm_figuras_opcoes$pos_norte  <- normalizar_posicao_elemento(posicao_norte)
  .sdm_figuras_opcoes$cores      <- if (identical(tolower(as.character(cores_elementos)[1]), "invertidas")) "invertidas" else "normais"
  .sdm_figuras_opcoes$fundo      <- isTRUE(fundo_elementos)
  .sdm_figuras_opcoes$elementos_painel <- if (identical(tolower(as.character(elementos_painel)[1]), "ultimo")) "ultimo" else "todos"
  .sdm_figuras_opcoes$mostrar_elementos <- TRUE
  .sdm_figuras_opcoes$mascara           <- preparar_mascara_figuras(mascara)
  .sdm_figuras_opcoes$mascara_cor       <- if (length(mascara_cor) == 1 && nzchar(mascara_cor)) mascara_cor else "black"
  .sdm_figuras_opcoes$mascara_espessura <- if (is.numeric(mascara_espessura) && length(mascara_espessura) == 1) mascara_espessura else 0.8
  .sdm_figuras_opcoes$avisou_mascara    <- FALSE
  .sdm_figuras_opcoes$mascara_fill      <- if (length(mascara_preenchimento) == 1 && !is.na(mascara_preenchimento) &&
                                               !tolower(trimws(mascara_preenchimento)) %in% c("", "nenhum", "nenhuma", "none", "na")) mascara_preenchimento else NULL
  .sdm_figuras_opcoes$margem_mapa       <- if (is.numeric(margem_mapa) && length(margem_mapa) == 1 && !is.na(margem_mapa)) margem_mapa else 0
  .sdm_figuras_opcoes$avisou <- FALSE
  tipos    <- normalizar_tipos_figuras(tipos)
  formatos <- normalizar_formatos_figuras(formatos)
  layout   <- normalizar_layout_figuras(layout)
  if (length(formatos) == 0 || length(tipos) == 0) {
    message("Nenhum formato ou tipo de figura escolhido: nada a fazer.")
    return(invisible(NULL))
  }
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  # seleção de mapas específicos (NULL = todos)
  ind_cons <- consenso$indice
  sel_especies  <- resolver_selecao_figuras(sel_especies, c(mapas$especie, ind_cons$especie),
                                            "figuras_especies", "Espécies para as figuras")
  sel_conjuntos <- resolver_selecao_figuras(sel_conjuntos, c(mapas$conjunto, ind_cons$conjunto),
                                            "figuras_conjuntos", "Conjuntos (preditoras / cenários)")
  sel_mapas     <- resolver_selecao_figuras(sel_mapas,
                     c(mapas$modelo, if (!is.null(ind_cons)) paste0("consenso_", ind_cons$metodo)),
                     "figuras_mapas", "Mapas (algoritmo ou consenso_<método>)")
  sel_limiares  <- resolver_selecao_figuras(sel_limiares,
                     c(mapas$metodo_limiar[mapas$tipo == "binario"],
                       ind_cons$metodo_limiar[ind_cons$tipo == "binario"]),
                     "figuras_limiares", "Limiares dos mapas binários")
  mapas <- filtrar_indice_figuras(mapas, sel_especies, sel_conjuntos, sel_mapas, sel_limiares, FALSE)
  if (!is.null(consenso$indice)) {
    consenso$indice <- filtrar_indice_figuras(consenso$indice, sel_especies, sel_conjuntos,
                                              sel_mapas, sel_limiares, TRUE)
  }
  if (!is.null(sel_especies) && !is.null(dados_modelos)) {
    dados_modelos <- dados_modelos[intersect(names(dados_modelos), sel_especies)]
  }

  cat("\n================ FIGURAS ================\n")
  cat("Tipos:", paste(tipos, collapse = ", "), "| formatos:", paste(formatos, collapse = ", "),
      "| layout:", paste(layout, collapse = " + "),
      "| escala:", if (isTRUE(escala)) "sim" else "não", "| norte:", if (isTRUE(norte)) "sim" else "não", "\n")
  if (!is.null(c(sel_especies, sel_conjuntos, sel_mapas, sel_limiares))) {
    cat("Seleção - espécies:", if (is.null(sel_especies)) "todas" else paste(sel_especies, collapse = ", "),
        "| conjuntos:", if (is.null(sel_conjuntos)) "todos" else paste(sel_conjuntos, collapse = ", "),
        "| mapas:", if (is.null(sel_mapas)) "todos" else paste(sel_mapas, collapse = ", "),
        "| limiares:", if (is.null(sel_limiares)) "todos" else paste(sel_limiares, collapse = ", "), "\n")
  }

  gerados <- list()
  executar <- function(tipo, expressao) {
    cat(tipo, "... ")
    r <- tryCatch(expressao, error = function(e) {
      message("falhou: ", conditionMessage(e)); character(0)
    })
    gerados[[tipo]] <<- data.frame(tipo = rep(tipo, length(r)), arquivo = as.character(r),
                                   stringsAsFactors = FALSE)
    cat(length(r), "arquivo(s)\n")
  }

  if ("modelos" %in% tipos) {
    executar("modelos", figuras_modelos(mapas, consenso, dados_modelos, pasta_saida, formatos,
                                        resolucao, largura_painel, paleta, usar_pontos, layout))
  }
  if ("binarios" %in% tipos) {
    executar("binarios", figuras_binarios(mapas, consenso, dados_modelos, pasta_saida, formatos,
                                          resolucao, largura_painel, usar_pontos, layout))
  }
  if ("consenso" %in% tipos) {
    executar("consenso", figuras_consenso(consenso, dados_modelos, pasta_saida, formatos,
                                          resolucao, largura_painel, paleta, usar_pontos, layout))
  }
  if ("cenarios" %in% tipos) {
    executar("cenarios", figuras_cenarios(consenso, dados_modelos, pasta_saida, formatos,
                                          resolucao, largura_painel, paleta, usar_pontos, layout))
  }
  if ("variaveis" %in% tipos) {
    executar("variaveis", figuras_variaveis(preditores, pasta_saida, formatos, resolucao,
                                            largura_painel, layout, sel_conjuntos))
  }
  if ("importancia" %in% tipos) {
    executar("importancia", figuras_importancia(importancia, pasta_saida, formatos, resolucao,
                                                largura_painel))
  }
  if ("extrapolacao" %in% tipos) {
    executar("extrapolacao", figuras_extrapolacao(pasta_extrapolacao, preditores, pasta_saida,
                                                  formatos, resolucao, largura_painel, layout,
                                                  sel_especies, sel_conjuntos))
  }
  if ("curvas_resposta" %in% tipos) {
    executar("curvas_resposta", figuras_curvas_resposta(modelos_finais, dados_modelos, pasta_saida,
                                                        formatos, resolucao, largura_painel))
  }

  indice <- do.call(rbind, gerados)
  if (!is.null(indice) && nrow(indice) > 0) {
    utils::write.csv(acrescentar_preditores(indice, preditores),
                     file.path(pasta_saida, "figuras.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  cat("Figuras salvas em:", pasta_saida, "\n")
  cat("=========================================\n")
  invisible(indice)
}
