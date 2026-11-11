# ==============================================================================
#  FUNÇÕES - Limpeza dos registros e sorteio do background
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
#  Os valores das variáveis são consultados ARQUIVO POR ARQUIVO (sem empilhar
#  os rasters), então esta etapa não depende de as variáveis terem a mesma grade.
#
#  O background pode ser sorteado de dois jeitos:
#    - sem viés: todas as células do buffer têm a mesma chance;
#    - com viés de coleta: a chance de cada célula é proporcional ao esforço
#      amostral nela (mapa de esforço feito a partir dos registros de todas as
#      espécies ou de um arquivo de pontos de coleta). Assim o background tem o
#      mesmo viés dos registros e o modelo não confunde "onde se coleta mais"
#      com "onde a espécie ocorre" (background de grupo-alvo, Phillips et al.
#      2009).
# ==============================================================================


# ------------------------------------------------------------------------------
# adicionar_passo()
#   Acrescenta uma linha à tabela que resume a limpeza dos registros:
#   nome do passo, quantos registros foram removidos e quantos restaram.
# ------------------------------------------------------------------------------
adicionar_passo <- function(passos, nome, n_antes, n_depois) {
  rbind(passos, data.frame(passo = nome, removidos = n_antes - n_depois,
                           restantes = n_depois, stringsAsFactors = FALSE))
}


# ------------------------------------------------------------------------------
# celulas_com_na()
#   Para cada ponto, verifica se ALGUMA variável tem NA (sem valor) na célula
#   onde o ponto cai. Consulta um arquivo de cada vez.
#   Devolve um vetor lógico (TRUE = o ponto cai em célula com NA).
# ------------------------------------------------------------------------------
celulas_com_na <- function(pontos, arquivos_variaveis) {

  tem_na <- rep(FALSE, nrow(pontos))

  for (arquivo in arquivos_variaveis) {
    raster  <- terra::rast(arquivo)[[1]]
    valores <- terra::extract(raster, pontos)[, 2]
    tem_na  <- tem_na | is.na(valores)
  }

  return(tem_na)
}


# ------------------------------------------------------------------------------
# preparar_registros()
#   Lê e limpa os registros de ocorrência. Sempre descarta:
#     1) registros sem coordenada válida;
#     2) registros fora da área de estudo (máscara);
#     3) registros em células com NA em alguma variável.
#   Opcionalmente (parâmetros do main):
#     4) 'remover_duplicados' : registros com a mesma longitude e latitude;
#     5) 'um_por_celula'      : mantém só 1 registro por célula do raster.
#   Se 'coluna_especie' for informada, os passos 4 e 5 comparam os registros
#   SÓ DENTRO DE CADA ESPÉCIE: coordenadas iguais de espécies diferentes são
#   mantidas. Com coluna_especie = "", todos os registros são tratados como
#   uma única espécie.
#   Mostra uma tabela com quantos registros saíram em cada passo.
#   Devolve uma lista:
#       $registros -> linha_no_arquivo, (especie), longitude, latitude
#                     (originais) e x, y (na projeção dos rasters)
#       $passos    -> resumo da limpeza
# ------------------------------------------------------------------------------
preparar_registros <- function(entradas, mascara, arquivos_variaveis,
                               coluna_longitude, coluna_latitude, crs_registros,
                               remover_duplicados = FALSE, um_por_celula = FALSE,
                               coluna_especie = "", titulo = "LIMPEZA DOS REGISTROS") {

  registros <- ler_registros(entradas$registros, entradas$formato_registros)

  usa_especie <- !is.null(coluna_especie) && length(coluna_especie) == 1 &&
    !is.na(coluna_especie) && coluna_especie != ""

  faltando <- setdiff(c(coluna_longitude, coluna_latitude,
                        if (usa_especie) coluna_especie), names(registros))
  if (length(faltando) > 0) {
    stop("Coluna(s) não encontrada(s) nos registros: ",
         paste(faltando, collapse = ", "),
         "\nColunas disponíveis: ", paste(names(registros), collapse = ", "),
         call. = FALSE)
  }

  tabela <- data.frame(
    linha_no_arquivo = seq_len(nrow(registros)) + 1,   # +1 por causa do cabeçalho
    longitude = suppressWarnings(as.numeric(registros[[coluna_longitude]])),
    latitude  = suppressWarnings(as.numeric(registros[[coluna_latitude]]))
  )
  if (usa_especie) {
    # a espécie vai logo depois do número da linha
    especie <- trimws(as.character(registros[[coluna_especie]]))
    especie[is.na(especie) | especie == ""] <- "(sem espécie)"
    tabela <- cbind(tabela[1], especie = especie, tabela[-1])
  }
  passos <- data.frame(passo = "Registros no arquivo", removidos = 0,
                       restantes = nrow(tabela), stringsAsFactors = FALSE)

  # 1) coordenadas válidas
  n_antes <- nrow(tabela)
  validos <- !is.na(tabela$longitude) & !is.na(tabela$latitude) &
    abs(tabela$longitude) <= 180 & abs(tabela$latitude) <= 90
  tabela  <- tabela[validos, ]
  passos  <- adicionar_passo(passos, "Sem coordenada válida", n_antes, nrow(tabela))

  # pontos espaciais, na mesma projeção da máscara
  pontos <- terra::vect(tabela, geom = c("longitude", "latitude"), crs = crs_registros)
  if (!terra::same.crs(pontos, mascara)) {
    pontos <- terra::project(pontos, terra::crs(mascara))
  }
  coordenadas <- terra::crds(pontos)
  tabela$x <- coordenadas[, 1]
  tabela$y <- coordenadas[, 2]

  # 2) dentro da área de estudo
  n_antes <- nrow(tabela)
  if (inherits(mascara, "SpatVector")) {
    dentro <- terra::is.related(pontos, mascara, "intersects")
  } else {
    dentro <- !is.na(terra::extract(mascara, pontos)[, 2])
  }
  tabela <- tabela[dentro, ]
  pontos <- pontos[dentro, ]
  passos <- adicionar_passo(passos, "Fora da área de estudo", n_antes, nrow(tabela))

  # 3) células com NA em alguma variável
  n_antes <- nrow(tabela)
  message("Conferindo células sem valor nas ", length(arquivos_variaveis), " variáveis...")
  com_na <- celulas_com_na(pontos, arquivos_variaveis)
  tabela <- tabela[!com_na, ]
  passos <- adicionar_passo(passos, "Em célula com NA", n_antes, nrow(tabela))

  # 4) duplicados exatos (opcional)
  if (remover_duplicados) {
    n_antes <- nrow(tabela)
    chaves  <- c(if (usa_especie) "especie", "longitude", "latitude")
    tabela  <- tabela[!duplicated(tabela[, chaves, drop = FALSE]), ]
    passos  <- adicionar_passo(passos,
                               if (usa_especie) "Duplicados (mesma espécie e lon/lat)"
                               else "Duplicados (mesma lon/lat)",
                               n_antes, nrow(tabela))
  }

  # 5) um registro por célula (opcional)
  if (um_por_celula) {
    n_antes    <- nrow(tabela)
    referencia <- terra::rast(arquivos_variaveis[1])[[1]]
    celula     <- terra::cellFromXY(referencia, cbind(tabela$x, tabela$y))
    chaves     <- data.frame(celula = celula)
    if (usa_especie) chaves$especie <- tabela$especie
    tabela     <- tabela[!duplicated(chaves), ]
    passos     <- adicionar_passo(passos,
                                  if (usa_especie) "Mais de um por célula (por espécie)"
                                  else "Mais de um por célula",
                                  n_antes, nrow(tabela))
  }

  if (nrow(tabela) == 0) {
    stop("Nenhum registro restou após a limpeza.", call. = FALSE)
  }

  cat("\n================", titulo, "================\n")
  print(passos, row.names = FALSE)
  cat("Duplicados exatos removidos    :", if (remover_duplicados) "sim" else "não", "\n")
  cat("Um registro por célula         :", if (um_por_celula) "sim" else "não", "\n")
  if (usa_especie) {
    cat("Espécies nos registros         :", length(unique(tabela$especie)), "\n")
  }
  cat("=======================================================\n\n")

  rownames(tabela) <- NULL
  return(list(registros = tabela, passos = passos))
}


# ------------------------------------------------------------------------------
# ler_pontos_esforco()
#   Lê um arquivo de pontos de coleta (esforço amostral) no MESMO formato dos
#   registros: mesmos separadores e mesmas colunas de longitude e latitude.
#   Descarta coordenadas inválidas e passa os pontos para a projeção dos
#   rasters. Devolve uma tabela com as colunas x e y.
# ------------------------------------------------------------------------------
ler_pontos_esforco <- function(caminho, formato, coluna_longitude, coluna_latitude,
                               crs_registros, referencia) {

  caminho <- selecionar_arquivo(caminho, "Selecione o arquivo de pontos de coleta (.csv ou .txt)",
                                c("csv", "txt"))
  tabela  <- ler_registros(caminho, formato)

  faltando <- setdiff(c(coluna_longitude, coluna_latitude), names(tabela))
  if (length(faltando) > 0) {
    stop("Coluna(s) não encontrada(s) no arquivo de pontos de coleta: ",
         paste(faltando, collapse = ", "),
         "\nColunas disponíveis: ", paste(names(tabela), collapse = ", "), call. = FALSE)
  }

  coordenadas <- data.frame(
    longitude = suppressWarnings(as.numeric(tabela[[coluna_longitude]])),
    latitude  = suppressWarnings(as.numeric(tabela[[coluna_latitude]]))
  )
  validos <- !is.na(coordenadas$longitude) & !is.na(coordenadas$latitude) &
    abs(coordenadas$longitude) <= 180 & abs(coordenadas$latitude) <= 90
  coordenadas <- coordenadas[validos, , drop = FALSE]
  if (nrow(coordenadas) == 0) {
    stop("Nenhum ponto de coleta com coordenada válida em: ", caminho, call. = FALSE)
  }

  pontos <- terra::vect(coordenadas, geom = c("longitude", "latitude"), crs = crs_registros)
  if (!terra::same.crs(pontos, referencia)) {
    pontos <- terra::project(pontos, terra::crs(referencia))
  }
  xy <- terra::crds(pontos)

  message("Pontos de coleta lidos: ", nrow(xy), " (descartados por coordenada inválida: ",
          sum(!validos), ")")
  return(data.frame(x = xy[, 1], y = xy[, 2]))
}


# ------------------------------------------------------------------------------
# superficie_esforco()
#   Faz o mapa do ESFORÇO AMOSTRAL (de 0 a 1) na grade das variáveis:
#     1) conta quantos pontos de coleta caem em cada célula;
#     2) se 'raio_kernel_km' > 0, espalha cada coleta em volta dela com um
#        kernel gaussiano (desvio = raio_kernel_km): células perto de muitas
#        coletas ficam com valor alto; as que estão a mais de 3 raios de
#        qualquer coleta ficam com 0;
#     3) divide tudo pelo maior valor (o esforço máximo vira 1).
#   Para ser rápido, o kernel é aplicado numa grade mais grossa (células de
#   cerca de 1/4 do raio), que depois volta para a grade das variáveis.
#   Salva o mapa em 'arquivo' (se informado) e devolve o raster.
# ------------------------------------------------------------------------------
superficie_esforco <- function(pontos_xy, arquivos_variaveis, raio_kernel_km = 20,
                               arquivo = NULL) {

  referencia <- terra::rast(arquivos_variaveis[1])[[1]]

  # 1) nº de pontos de coleta em cada célula (pontos fora dos rasters são ignorados)
  celula <- terra::cellFromXY(referencia, as.matrix(pontos_xy[, c("x", "y")]))
  celula <- celula[!is.na(celula)]
  if (length(celula) == 0) {
    stop("Nenhum ponto de coleta cai dentro dos rasters.", call. = FALSE)
  }
  contagem <- rep(0, terra::ncell(referencia))
  por_celula <- table(celula)
  contagem[as.integer(names(por_celula))] <- as.numeric(por_celula)
  esforco <- terra::rast(referencia)
  terra::values(esforco) <- contagem

  # 2) kernel gaussiano
  if (raio_kernel_km > 0) {
    # tamanho da célula em km. Em graus: 1 grau de latitude tem ~111,32 km e
    # 1 grau de longitude encolhe com o cosseno da latitude.
    if (terra::is.lonlat(referencia)) {
      lat_media <- mean(as.vector(terra::ext(referencia))[3:4])
      celula_km <- terra::res(referencia) * 111.32 * c(cos(lat_media * pi / 180), 1)
    } else {
      unidade <- terra::linearUnits(referencia)   # metros por unidade do mapa
      if (is.na(unidade) || unidade == 0) unidade <- 1
      celula_km <- terra::res(referencia) * unidade / 1000
    }

    # grade mais grossa: soma as contagens em blocos de 'fator' x 'fator' células
    fator <- max(1, floor(raio_kernel_km / 4 / max(celula_km)))
    if (fator > 1) esforco <- terra::aggregate(esforco, fact = fator, fun = "sum", na.rm = TRUE)
    celula_km <- celula_km * fator

    # pesos do kernel em volta de cada célula: exp(-d² / (2 x raio²)), até 3 raios
    nx <- ceiling(3 * raio_kernel_km / celula_km[1])
    ny <- ceiling(3 * raio_kernel_km / celula_km[2])
    dx <- matrix((-nx:nx) * celula_km[1], nrow = 2 * ny + 1, ncol = 2 * nx + 1, byrow = TRUE)
    dy <- matrix((-ny:ny) * celula_km[2], nrow = 2 * ny + 1, ncol = 2 * nx + 1)
    pesos <- exp(-(dx^2 + dy^2) / (2 * raio_kernel_km^2))

    # cada célula recebe a soma das contagens vizinhas multiplicadas pelos pesos
    esforco <- terra::focal(esforco, w = pesos, fun = "sum", na.rm = TRUE)
    if (fator > 1) esforco <- terra::resample(esforco, referencia, method = "bilinear")
  }

  # só dentro da área de estudo, e 3) de 0 a 1
  esforco <- terra::mask(esforco, referencia)
  maximo  <- terra::global(esforco, "max", na.rm = TRUE)[1, 1]
  if (is.na(maximo) || maximo <= 0) {
    stop("Nenhum ponto de coleta cai dentro da área de estudo.", call. = FALSE)
  }
  esforco <- esforco / maximo
  names(esforco) <- "esforco"

  if (!is.null(arquivo)) {
    terra::writeRaster(esforco, arquivo, overwrite = TRUE, datatype = "FLT4S",
                       gdal = c("COMPRESS=LZW"))
  }
  return(esforco)
}


# ------------------------------------------------------------------------------
# preparar_esforco()
#   Decide de onde vem o esforço amostral e faz o mapa (superficie_esforco()):
#     vies = "nenhum"    -> sem mapa (devolve NULL): background sem viés;
#     vies = "registros" -> registros ORIGINAIS de TODAS as espécies (o
#                           "grupo-alvo": coletado com o mesmo viés da espécie);
#     vies = "arquivo"   -> pontos de coleta de outro arquivo ('caminho_esforco').
#   Salva o mapa em pasta_saida/esforco_amostral.tif e mostra um resumo.
# ------------------------------------------------------------------------------
preparar_esforco <- function(vies = "nenhum", registros = NULL, arquivos_variaveis,
                             raio_kernel_km = 20, caminho_esforco = "", formato = NULL,
                             coluna_longitude = "", coluna_latitude = "",
                             crs_registros = "EPSG:4326", pasta_saida,
                             entradas = NULL, mascara = NULL) {

  vies <- match.arg(tolower(trimws(vies)), c("nenhum", "registros", "arquivo"))
  if (vies == "nenhum") return(NULL)

  if (vies == "registros") {
    # o esforço usa os registros ORIGINAIS: só descarta coordenadas inválidas,
    # registros fora da área de estudo e em células sem valor. Duplicados e
    # vários registros na mesma célula são MANTIDOS (eles medem o esforço).
    if (is.null(entradas) || is.null(mascara)) {
      stop("Para vies = \"registros\", informe 'entradas' e 'mascara' em preparar_esforco().",
           call. = FALSE)
    }
    minimos <- preparar_registros(
      entradas           = entradas,
      mascara            = mascara,
      arquivos_variaveis = arquivos_variaveis,
      coluna_longitude   = coluna_longitude,
      coluna_latitude    = coluna_latitude,
      crs_registros      = crs_registros,
      remover_duplicados = FALSE,
      um_por_celula      = FALSE,
      coluna_especie     = "",
      titulo             = "REGISTROS PARA O ESFORÇO DE COLETA (limpeza mínima)")
    pontos_xy <- minimos$registros[, c("x", "y")]
    origem    <- paste0("registros originais de todas as espécies, sem remover duplicados nem ",
                        "limitar a 1 por célula (", nrow(pontos_xy), " pontos)")
  } else {
    pontos_xy <- ler_pontos_esforco(caminho_esforco, formato, coluna_longitude,
                                    coluna_latitude, crs_registros,
                                    terra::rast(arquivos_variaveis[1]))
    origem    <- paste0("arquivo de pontos de coleta (", nrow(pontos_xy), " pontos)")
  }

  arquivo <- file.path(pasta_saida, "esforco_amostral.tif")
  esforco <- superficie_esforco(pontos_xy, arquivos_variaveis, raio_kernel_km, arquivo)
  area_com_esforco <- terra::global(esforco > 0.01, "mean", na.rm = TRUE)[1, 1]

  cat("\n================ ESFORÇO AMOSTRAL ================\n")
  cat("Origem        :", origem, "\n")
  cat("Raio do kernel:", raio_kernel_km, "km",
      if (raio_kernel_km == 0) "(só as células com coleta)" else "", "\n")
  cat("Área de estudo com esforço acima de 1% do máximo:",
      round(100 * area_com_esforco, 1), "%\n")
  cat("Mapa salvo em :", arquivo, "\n")
  cat("==================================================\n\n")

  return(esforco)
}


# ------------------------------------------------------------------------------
# gerar_background_especie()
#   Sorteia o background de UMA espécie (ou do conjunto de registros recebido):
#     - dentro de um BUFFER de 'distancia_km' em volta dos registros;
#     - apenas em células com valor em TODAS as variáveis;
#     - se 'raio_pseudoausencia_km' for informado (opcional), fora de um raio
#       desse tamanho em volta de cada registro da espécie; ele precisa ser
#       MENOR que 'distancia_km' (NULL, NA ou 0 = sem exclusão);
#     - quantidade = 'fator' x número de registros, mas nunca menos que
#       'minimo' (piso: espécies com poucos registros também precisam de
#       background suficiente para representar o ambiente do buffer);
#     - sempre no centro das células do primeiro raster.
#   Se 'esforco' (mapa de preparar_esforco()) for informado, a chance de cada
#   célula ser sorteada é proporcional ao esforço amostral nela, e células sem
#   esforço (valor 0) não são sorteadas. Sem 'esforco', todas têm a mesma chance.
#   Se o buffer tiver menos células válidas do que o pedido, usa todas e avisa.
#   Devolve uma tabela com as colunas x e y.
# ------------------------------------------------------------------------------
gerar_background_especie <- function(registros, arquivos_variaveis, distancia_km = 100,
                                     fator = 10, minimo = 0, esforco = NULL,
                                     semente = 123, nome = "", raio_pseudoausencia_km = NULL) {

  identificacao <- if (nome != "") paste0(" (espécie: ", nome, ")") else ""
  vazio <- data.frame(x = numeric(0), y = numeric(0))

  usar_raio <- !is.null(raio_pseudoausencia_km) && length(raio_pseudoausencia_km) == 1 &&
    !is.na(raio_pseudoausencia_km) && raio_pseudoausencia_km > 0
  if (usar_raio && raio_pseudoausencia_km >= distancia_km) {
    stop("'raio_pseudoausencia_km' (", raio_pseudoausencia_km, " km) precisa ser MENOR que ",
         "'distancia_buffer_km' (", distancia_km, " km). Senão não sobra área para o background.",
         call. = FALSE)
  }

  n_pedido   <- max(round(fator * nrow(registros)), minimo)
  referencia <- terra::rast(arquivos_variaveis[1])[[1]]

  # buffer único (união dos círculos) em volta de todos os registros
  pontos <- terra::vect(registros[, c("x", "y")], geom = c("x", "y"),
                        crs = terra::crs(referencia))
  buffer <- terra::aggregate(terra::buffer(pontos, width = distancia_km * 1000))

  # células da referência que estão dentro do buffer (e da máscara)
  area    <- terra::mask(terra::crop(referencia, buffer), buffer)

  # raio de exclusão: não sorteia perto das presenças
  if (usar_raio) {
    exclusao <- terra::aggregate(terra::buffer(pontos, width = raio_pseudoausencia_km * 1000))
    area     <- terra::mask(area, exclusao, inverse = TRUE)
  }
  celulas <- which(!is.na(terra::values(area, mat = FALSE)))

  # com viés de coleta: o peso de cada célula é o esforço amostral nela
  peso <- NULL
  if (!is.null(esforco) && length(celulas) > 0) {
    peso <- terra::extract(esforco, terra::xyFromCell(area, celulas))[["esforco"]]
    peso[is.na(peso)] <- 0
    if (all(peso <= 0)) {
      warning("Não há esforço amostral dentro do buffer", identificacao,
              ". O background desta espécie foi sorteado SEM viés de coleta.",
              call. = FALSE, immediate. = TRUE)
      peso <- NULL
    } else {
      celulas <- celulas[peso > 0]
      peso    <- peso[peso > 0]
    }
  }

  if (length(celulas) == 0) {
    warning("Não há células válidas dentro do buffer de ", distancia_km, " km",
            if (usar_raio) paste0(" fora do raio de ", raio_pseudoausencia_km, " km das presenças") else "",
            identificacao, ". Nenhum ponto de background foi sorteado.",
            call. = FALSE, immediate. = TRUE)
    return(vazio)
  }

  # ordem de sorteio das células
  set.seed(semente)
  if (is.null(peso)) {
    # sem viés: embaralha as células (todas com a mesma chance)
    celulas <- celulas[sample.int(length(celulas))]
  } else {
    # com viés: sorteio ponderado sem reposição. Cada célula recebe uma "chave"
    # aleatória que tende a ser maior quanto maior o seu peso, e as células
    # entram da maior para a menor chave (método de Efraimidis e Spirakis)
    chave   <- log(stats::runif(length(celulas))) / peso
    celulas <- celulas[order(chave, decreasing = TRUE)]
  }

  # vai aceitando, em lotes e nessa ordem, as células que não têm NA
  escolhidos <- vazio
  lote       <- max(n_pedido * 2, 1000)
  inicio     <- 1

  while (nrow(escolhidos) < n_pedido && inicio <= length(celulas)) {
    fim  <- min(inicio + lote - 1, length(celulas))
    xy   <- terra::xyFromCell(area, celulas[inicio:fim])
    pts  <- terra::vect(xy, type = "points", crs = terra::crs(referencia))
    ok   <- !celulas_com_na(pts, arquivos_variaveis)
    escolhidos <- rbind(escolhidos, data.frame(x = xy[ok, 1], y = xy[ok, 2]))
    inicio <- fim + 1
  }

  if (nrow(escolhidos) < n_pedido) {
    warning("O buffer tem só ", nrow(escolhidos), " células válidas",
            if (!is.null(peso)) " com esforço amostral" else "", identificacao,
            "; foram pedidos ", n_pedido, " pontos de background. Aumente ",
            "'distancia_buffer_km'", if (!is.null(peso)) " ou 'raio_kernel_km'" else "",
            if (usar_raio) ", diminua 'raio_pseudoausencia_km'" else "",
            ", ou reduza 'fator_background' ou 'minimo_background'.",
            call. = FALSE, immediate. = TRUE)
  } else {
    escolhidos <- escolhidos[seq_len(n_pedido), ]
  }

  rownames(escolhidos) <- NULL
  return(escolhidos)
}


# ------------------------------------------------------------------------------
# gerar_background()
#   Sorteia o background SEPARADAMENTE para cada espécie (cada uma com o seu
#   buffer, calculado só com os registros dela, e a sua quantidade de pontos).
#   'minimo' e 'esforco': ver gerar_background_especie().
#   Se 'registros' não tiver a coluna "especie", sorteia um único background.
#   Devolve uma tabela com as colunas especie (se houver), x e y.
# ------------------------------------------------------------------------------
gerar_background <- function(registros, arquivos_variaveis, distancia_km = 100,
                             fator = 10, minimo = 0, esforco = NULL, semente = 123,
                             raio_pseudoausencia_km = NULL) {

  if (!"especie" %in% names(registros)) {
    return(gerar_background_especie(registros, arquivos_variaveis, distancia_km,
                                    fator, minimo, esforco, semente,
                                    raio_pseudoausencia_km = raio_pseudoausencia_km))
  }

  especies  <- unique(registros$especie)
  resultado <- list()

  for (i in seq_along(especies)) {
    especie <- especies[i]
    message("Background: ", especie, " (", i, "/", length(especies), ")")

    fundo <- gerar_background_especie(
      registros          = registros[registros$especie %in% especie, ],
      arquivos_variaveis = arquivos_variaveis,
      distancia_km       = distancia_km,
      fator              = fator,
      minimo             = minimo,
      esforco            = esforco,
      semente            = semente,
      nome               = especie,
      raio_pseudoausencia_km = raio_pseudoausencia_km
    )
    fundo$especie <- rep(especie, nrow(fundo))
    resultado[[i]] <- fundo[, c("especie", "x", "y")]
  }

  fundo <- do.call(rbind, resultado)
  rownames(fundo) <- NULL
  return(fundo)
}


# ------------------------------------------------------------------------------
# salvar_pontos()
#   Junta presenças e background em uma tabela e salva em 'pasta_saida':
#     registros_limpos.csv          -> presenças após a limpeza
#     background.csv                -> pontos de background
#     pontos_presenca_background.csv-> os dois juntos (especie, tipo,
#                                      presenca 1/0, x, y)
#   Se houver a coluna "especie", ela é mantida e o resumo é por espécie.
#   Devolve a tabela com os dois juntos.
# ------------------------------------------------------------------------------
salvar_pontos <- function(registros, background, pasta_saida) {

  com_especie <- "especie" %in% names(registros) && "especie" %in% names(background)

  presenca <- data.frame(tipo = "presenca", presenca = 1L,
                         x = registros$x, y = registros$y)
  fundo    <- data.frame(tipo = "background", presenca = 0L,
                         x = background$x, y = background$y)
  if (com_especie) {
    presenca <- cbind(especie = registros$especie, presenca)
    fundo    <- cbind(especie = background$especie, fundo)
  }
  pontos <- rbind(presenca, fundo)

  utils::write.csv(registros, file.path(pasta_saida, "registros_limpos.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(background, file.path(pasta_saida, "background.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(pontos, file.path(pasta_saida, "pontos_presenca_background.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")

  cat("\n================ PONTOS PARA O MODELO ================\n")
  cat("Presenças  :", nrow(presenca), "\n")
  cat("Background :", nrow(fundo), "\n")

  if (com_especie) {
    especies <- sort(unique(presenca$especie))
    resumo <- data.frame(
      especie    = especies,
      presencas  = sapply(especies, function(e) sum(presenca$especie == e)),
      background = sapply(especies, function(e) sum(fundo$especie == e))
    )
    cat("\nPor espécie:\n")
    print(resumo, row.names = FALSE)
  }
  cat("======================================================\n\n")
  message("Pontos salvos em: ", pasta_saida)

  return(invisible(pontos))
}
