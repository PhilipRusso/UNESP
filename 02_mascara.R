# ==============================================================================
#  FUNÇÕES - Máscara da área de estudo
# ------------------------------------------------------------------------------
#  Requer o pacote "terra" (carregado no main).
# ==============================================================================


# ------------------------------------------------------------------------------
# mostrar_tabela_mascara()
#   Mostra a tabela de atributos da máscara, para que o usuário veja quais
#   feições existem (ex.: estados, municípios, biomas) e possa escolher
#   depois se vai cortar para a máscara toda ou só para algumas feições.
#
#   - Máscara em .shp : mostra a tabela e devolve um data.frame.
#       A coluna "feicao" é o número da linha (1, 2, 3...) e serve para
#       indicar a feição escolhida.
#   - Máscara em .tif : não existe tabela de atributos; avisa e devolve NULL.
#
#   max_linhas: quantas linhas imprimir no console (a tabela completa
#   também abre na aba de visualização do RStudio, se disponível).
# ------------------------------------------------------------------------------
mostrar_tabela_mascara <- function(caminho_mascara, max_linhas = 30) {

  extensao <- tolower(tools::file_ext(caminho_mascara))

  if (extensao != "shp") {
    message("A máscara é um raster (.", extensao, "): não há tabela de atributos. ",
            "O corte será feito para a máscara inteira.")
    return(invisible(NULL))
  }

  mascara <- terra::vect(caminho_mascara)
  tabela  <- as.data.frame(mascara)
  tabela  <- cbind(feicao = seq_len(nrow(tabela)), tabela)

  cat("\n================ TABELA DE ATRIBUTOS DA MÁSCARA ================\n")
  cat("Número de feições:", nrow(tabela), "\n")
  cat("Colunas          :", paste(names(tabela)[-1], collapse = ", "), "\n\n")

  if (nrow(tabela) > max_linhas) {
    print(tabela[seq_len(max_linhas), ], row.names = FALSE)
    cat("... (mostrando as primeiras", max_linhas, "de", nrow(tabela), "linhas)\n")
  } else {
    print(tabela, row.names = FALSE)
  }
  cat("================================================================\n\n")

  # No RStudio, abre a tabela completa em uma aba
  if (interactive() && exists("View", mode = "function")) {
    utils::View(tabela, title = "Atributos da máscara")
  }

  return(invisible(tabela))
}


# ------------------------------------------------------------------------------
# ler_numeros()
#   Converte a resposta digitada pelo usuário (ex.: "1,3,5-7") em um vetor de
#   números. Devolve NULL se a resposta for inválida ou estiver fora de 1:maximo.
# ------------------------------------------------------------------------------
ler_numeros <- function(texto, maximo) {

  partes <- trimws(unlist(strsplit(texto, ",")))
  partes <- partes[partes != ""]
  if (length(partes) == 0) return(NULL)

  numeros <- c()
  for (parte in partes) {
    if (grepl("^[0-9]+$", parte)) {
      numeros <- c(numeros, as.integer(parte))
    } else if (grepl("^[0-9]+-[0-9]+$", parte)) {
      limites <- as.integer(strsplit(parte, "-")[[1]])
      numeros <- c(numeros, seq(limites[1], limites[2]))
    } else {
      return(NULL)
    }
  }

  if (any(numeros < 1 | numeros > maximo)) return(NULL)
  return(unique(numeros))
}


# ------------------------------------------------------------------------------
# ler_entrada()
#   Lê uma resposta do usuário. No RStudio, abre uma pequena JANELA para digitar
#   (pacote rstudioapi). Isso é necessário porque, ao rodar várias linhas do main
#   de uma vez (Ctrl+Enter numa seleção, "Run" de uma seção), o readline() do
#   console consome as PRÓXIMAS LINHAS DO SCRIPT como se fossem a resposta, e o
#   script quebra. Fora do RStudio (ou sem o pacote), usa o readline() do console:
#   nesse caso, responda com o script parado na pergunta, uma linha por vez.
#   'contexto' = texto mostrado acima da pergunta na janela (ex.: a lista de opções).
#   Cancelar a janela interrompe com uma mensagem.
# ------------------------------------------------------------------------------
ler_entrada <- function(prompt, contexto = NULL, padrao = "", titulo = NULL) {

  usar_janela <- interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
    tryCatch(rstudioapi::isAvailable() && rstudioapi::hasFun("showPrompt"),
             error = function(e) FALSE)

  if (!usar_janela) return(readline(prompt))

  # título da janela: "SDM - <assunto>" (assunto = 'titulo' ou, se faltar, a primeira linha do contexto)
  assunto <- if (!is.null(titulo)) titulo else if (length(contexto) > 0) contexto[1] else prompt
  assunto <- trimws(strsplit(as.character(assunto), "\n")[[1]][1])
  assunto <- sub("[[:space:]]*[:?][[:space:]]*$", "", assunto)
  if (nchar(assunto) > 70) assunto <- paste0(substr(assunto, 1, 67), "...")
  mensagem <- paste(c(contexto, "", prompt), collapse = "\n")
  resposta <- rstudioapi::showPrompt(title = paste0("SDM - ", assunto), message = mensagem,
                                     default = padrao)
  if (is.null(resposta)) stop("Pergunta cancelada.", call. = FALSE)
  cat(prompt, resposta, "\n", sep = "")      # deixa a resposta registrada no console
  resposta
}


# ------------------------------------------------------------------------------
# perguntar_opcoes()
#   Mostra uma lista numerada e pergunta qual(is) o usuário quer.
#   Repete a pergunta até a resposta ser válida.
#   Devolve os números escolhidos.
# ------------------------------------------------------------------------------
perguntar_opcoes <- function(opcoes, pergunta, titulo = NULL) {

  # sem título, a própria pergunta serve de título da janela
  if (is.null(titulo)) titulo <- sub("[[:space:]]*[:?][[:space:]]*$", "", pergunta)
  lista <- paste0("  [", seq_along(opcoes), "] ", opcoes)
  cat(lista, sep = "\n")
  aviso <- NULL

  repeat {
    resposta <- ler_entrada(pergunta, contexto = c(titulo, aviso, lista), titulo = titulo)
    numeros  <- ler_numeros(resposta, length(opcoes))
    if (!is.null(numeros)) return(numeros)
    aviso <- "Resposta inválida. Use números da lista, separados por vírgula (ex.: 1,3 ou 2-5)."
    message(aviso)
  }
}


# ------------------------------------------------------------------------------
# escolher_valores()
#   Para UMA coluna, mostra os valores existentes e pergunta quais usar.
#   Se houver muitos valores (mais que 'limite_lista'), primeiro pede um texto
#   para filtrar a lista
#   Devolve os valores escolhidos (texto).
# ------------------------------------------------------------------------------
escolher_valores <- function(dados, coluna, limite_lista = 100) {

  valores <- sort(unique(as.character(dados[[coluna]])))
  valores <- valores[!is.na(valores)]

  cat("\nColuna:", coluna, "-", length(valores), "valores diferentes\n")

  if (length(valores) > limite_lista) {
    repeat {
      busca      <- ler_entrada("Muitos valores. Digite parte do texto para filtrar a lista: ",
                                contexto = paste0("Filtrar os valores da coluna ", coluna, " (",
                                                  length(valores), " valores diferentes)"),
                                titulo = paste0("Filtrar valores de ", coluna))
      candidatos <- valores[grepl(tolower(busca), tolower(valores), fixed = TRUE)]
      if (length(candidatos) > 0) {
        valores <- candidatos
        break
      }
      message("Nenhum valor contém '", busca, "'. Tente outro texto.")
    }
  }

  escolhidos <- perguntar_opcoes(valores,
                                 paste0("Número(s) do(s) valor(es) de ", coluna, ": "),
                                 paste0("Quais valores da coluna '", coluna, "' manter na máscara?"))
  return(valores[escolhidos])
}


# ------------------------------------------------------------------------------
# escolher_filtro_interativo()
#   Pergunta ao usuário, no console:
#     1) qual(is) coluna(s) da tabela de atributos usar;
#     2) para cada coluna, qual(is) valor(es) manter.
#   Devolve uma lista no formato  list(coluna1 = c(valores), coluna2 = c(...)).
# ------------------------------------------------------------------------------
escolher_filtro_interativo <- function(dados) {

  if (!interactive()) {
    stop("Não é possível perguntar fora de uma sessão interativa. ",
         "Preencha 'filtro_feicoes' no main.", call. = FALSE)
  }

  colunas  <- names(dados)
  n_unicos <- sapply(dados, function(x) length(unique(x)))

  cat("\nEscolha a(s) coluna(s) para selecionar as feições:\n")
  idx_colunas <- perguntar_opcoes(paste0(colunas, " (", n_unicos, " valores)"),
                                  "Número(s) da(s) coluna(s): ",
                                  "Quais colunas da tabela de atributos usar para selecionar as feições?")

  filtro <- list()
  for (coluna in colunas[idx_colunas]) {
    filtro[[coluna]] <- escolher_valores(dados, coluna)
  }

  return(filtro)
}


# ------------------------------------------------------------------------------
# aplicar_filtro_feicoes()
#   Mantém apenas as feições que atendem ao filtro.
#   Regra:  vários valores na MESMA coluna  -> basta atender a UM deles (OU)
#           colunas DIFERENTES              -> precisa atender a TODAS   (E)
#   Devolve o shapefile (SpatVector) só com as feições selecionadas.
# ------------------------------------------------------------------------------
aplicar_filtro_feicoes <- function(mascara, dados, filtro) {

  manter <- rep(TRUE, nrow(dados))

  for (coluna in names(filtro)) {
    if (!coluna %in% names(dados)) {
      stop("A coluna '", coluna, "' não existe na máscara. Colunas disponíveis: ",
           paste(names(dados), collapse = ", "), call. = FALSE)
    }
    manter <- manter & (as.character(dados[[coluna]]) %in% filtro[[coluna]])
  }

  if (!any(manter)) {
    stop("Nenhuma feição atende ao filtro escolhido.", call. = FALSE)
  }

  return(mascara[manter, ])
}


# ------------------------------------------------------------------------------
# preparar_mascara()
#   Lê a máscara e devolve o objeto que será usado no corte:
#     - .tif                         -> raster (SpatRaster), inteiro
#     - .shp, cortar_por_feicoes = FALSE -> shapefile inteiro
#     - .shp, cortar_por_feicoes = TRUE  -> só as feições escolhidas
#   Se 'filtro_feicoes' estiver vazio, pergunta no console qual(is) coluna(s)
#   e valor(es) usar.
# ------------------------------------------------------------------------------
preparar_mascara <- function(caminho_mascara, cortar_por_feicoes = FALSE,
                             filtro_feicoes = list()) {

  extensao <- tolower(tools::file_ext(caminho_mascara))

  if (extensao != "shp") {
    if (cortar_por_feicoes) {
      warning("Máscara raster não tem feições: será usada inteira.", call. = FALSE)
    }
    return(terra::rast(caminho_mascara))
  }

  mascara <- terra::vect(caminho_mascara)

  if (!cortar_por_feicoes) {
    message("Usando a máscara inteira (", nrow(mascara), " feições).")
    return(mascara)
  }

  dados <- as.data.frame(mascara)
  if (length(filtro_feicoes) == 0) {
    filtro_feicoes <- escolher_filtro_interativo(dados)
  }

  selecionada <- aplicar_filtro_feicoes(mascara, dados, filtro_feicoes)

  cat("\n", nrow(selecionada), "de", nrow(mascara), "feições selecionadas.\n")
  cat("Para repetir esta escolha sem perguntas, copie para o main:\n\n")
  cat("filtro_feicoes <- ", paste(deparse(filtro_feicoes), collapse = "\n"), "\n\n", sep = "")

  return(selecionada)
}
