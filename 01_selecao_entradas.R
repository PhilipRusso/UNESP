# ==============================================================================
#  FUNÇÕES - Seleção e conferência das entradas e pastas de saída
# ------------------------------------------------------------------------------
#  Usa apenas o R base (sem pacotes extras):
#    - no Windows: choose.dir() e choose.files()  (pacote "utils", já vem no R)
#    - em outros sistemas: tcltk                  (também já vem com o R)
# ==============================================================================


# ------------------------------------------------------------------------------
# abrir_janela_pasta()
#   Abre uma janela para o usuário escolher uma PASTA.
#   Retorna o caminho escolhido (texto).
# ------------------------------------------------------------------------------
abrir_janela_pasta <- function(titulo) {
  message(">> ", titulo)   # aviso no console (a janela às vezes abre atrás do RStudio)

  if (.Platform$OS.type == "windows") {
    caminho <- utils::choose.dir(caption = titulo)
  } else {
    caminho <- tcltk::tk_choose.dir(caption = titulo)
  }

  if (length(caminho) == 0 || is.na(caminho) || caminho == "") {
    stop("Nenhuma pasta foi selecionada.", call. = FALSE)
  }
  return(caminho)
}


# ------------------------------------------------------------------------------
# abrir_janela_arquivo()
#   Abre uma janela para o usuário escolher um ARQUIVO, mostrando apenas as
#   extensões permitidas.
#   Retorna o caminho escolhido (texto).
# ------------------------------------------------------------------------------
abrir_janela_arquivo <- function(titulo, extensoes) {
  message(">> ", titulo)

  if (.Platform$OS.type == "windows") {
    padrao  <- paste0("*.", extensoes, collapse = ";")          # ex.: "*.csv;*.txt"
    filtros <- rbind(c(paste0("Arquivos (", padrao, ")"), padrao),
                     c("Todos os arquivos (*.*)", "*.*"))
    caminho <- utils::choose.files(caption = titulo, multi = FALSE, filters = filtros)
  } else {
    filtros <- cbind("Arquivos permitidos", paste0(".", extensoes))
    caminho <- tcltk::tk_choose.files(caption = titulo, multi = FALSE, filters = filtros)
  }

  if (length(caminho) == 0 || is.na(caminho) || caminho == "") {
    stop("Nenhum arquivo foi selecionado.", call. = FALSE)
  }
  return(caminho)
}


# ------------------------------------------------------------------------------
# selecionar_pasta()
#   - Se 'caminho' estiver vazio (""), abre a janela de seleção.
#   - Confere se a pasta existe e se tem arquivos com a extensão esperada.
#   Retorna o caminho completo da pasta.
# ------------------------------------------------------------------------------
selecionar_pasta <- function(caminho = "", titulo = "Selecione uma pasta",
                             extensao = "tif") {

  if (is.null(caminho) || caminho == "") {
    caminho <- abrir_janela_pasta(titulo)
  }

  if (!dir.exists(caminho)) {
    stop("A pasta não existe: ", caminho, call. = FALSE)
  }

  arquivos <- list.files(caminho, pattern = paste0("\\.", extensao, "$"),
                         ignore.case = TRUE)
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo .", extensao, " encontrado em: ", caminho, call. = FALSE)
  }

  return(normalizePath(caminho, winslash = "/"))
}


# ------------------------------------------------------------------------------
# selecionar_arquivo()
#   - Se 'caminho' estiver vazio (""), abre a janela de seleção.
#   - Confere se o arquivo existe e se a extensão é uma das permitidas.
#   - Para shapefile (.shp), avisa se faltar algum arquivo complementar.
#   Retorna o caminho completo do arquivo.
# ------------------------------------------------------------------------------
selecionar_arquivo <- function(caminho = "", titulo = "Selecione um arquivo",
                               extensoes) {

  if (is.null(caminho) || caminho == "") {
    caminho <- abrir_janela_arquivo(titulo, extensoes)
  }

  if (!file.exists(caminho)) {
    stop("O arquivo não existe: ", caminho, call. = FALSE)
  }

  extensao <- tolower(tools::file_ext(caminho))
  if (!extensao %in% extensoes) {
    stop("Extensão '.", extensao, "' não permitida. Use: ",
         paste0(".", extensoes, collapse = ", "), call. = FALSE)
  }

  # Um shapefile é formado por vários arquivos com o mesmo nome
  if (extensao == "shp") {
    base     <- tools::file_path_sans_ext(caminho)
    faltando <- c("shx", "dbf")[!file.exists(paste0(base, ".", c("shx", "dbf")))]
    if (length(faltando) > 0) {
      warning("Shapefile incompleto. Faltando: ",
              paste0(".", faltando, collapse = ", "), call. = FALSE)
    }
  }

  return(normalizePath(caminho, winslash = "/"))
}


# ------------------------------------------------------------------------------
# selecionar_cenarios()
#   - Se usar_cenarios = FALSE, retorna vazio (sem cenários).
#   - Se os caminhos foram informados, confere cada um.
#   - Se estiver vazio, abre a janela quantas vezes o usuário quiser.
#   Retorna um vetor com os caminhos; os NOMES do vetor são os nomes dos
#   cenários (se não forem informados, usa o nome da pasta).
# ------------------------------------------------------------------------------
selecionar_cenarios <- function(usar_cenarios = FALSE, caminhos_cenarios = c()) {

  if (!usar_cenarios) return(character(0))

  if (length(caminhos_cenarios) == 0) {
    # Nenhum caminho informado: seleção pela janela
    caminhos_cenarios <- character(0)
    repeat {
      numero <- length(caminhos_cenarios) + 1
      pasta  <- selecionar_pasta("", paste0("Selecione a pasta do CENÁRIO ", numero, " (.tif)"))
      caminhos_cenarios <- c(caminhos_cenarios, pasta)

      resposta <- ler_entrada("Deseja adicionar outro cenário? (s/n): ",
                              contexto = paste0("Cenários já selecionados: ", length(caminhos_cenarios)),
                              titulo = "Adicionar outro cenário?")
      if (tolower(trimws(resposta)) != "s") break
    }
  } else {
    # Caminhos informados no main: confere um por um
    nomes_informados <- names(caminhos_cenarios)
    for (i in seq_along(caminhos_cenarios)) {
      caminhos_cenarios[i] <- selecionar_pasta(caminhos_cenarios[i],
                                               paste0("Selecione a pasta do CENÁRIO ", i))
    }
    names(caminhos_cenarios) <- nomes_informados
  }

  # Cenários sem nome recebem o nome da pasta
  nomes <- names(caminhos_cenarios)
  if (is.null(nomes)) nomes <- rep("", length(caminhos_cenarios))
  sem_nome <- nomes == ""
  nomes[sem_nome] <- basename(caminhos_cenarios[sem_nome])
  names(caminhos_cenarios) <- nomes

  return(caminhos_cenarios)
}


# ------------------------------------------------------------------------------
# definir_formato_registros()
#   Confere o separador de colunas e o separador decimal do arquivo de
#   registros (.csv ou .txt) e devolve os dois em uma lista:
#       formato$sep  -> separador de colunas
#       formato$dec  -> separador decimal
#   Esses valores serão usados depois, na leitura do arquivo.
# ------------------------------------------------------------------------------
definir_formato_registros <- function(separador_colunas = ",",
                                      separador_decimal = ".") {

  if (!separador_colunas %in% c(",", ";", "\t")) {
    stop("'separador_colunas' deve ser \",\", \";\" ou \"\\t\" (tabulação).",
         call. = FALSE)
  }
  if (!separador_decimal %in% c(".", ",")) {
    stop("'separador_decimal' deve ser \".\" ou \",\".", call. = FALSE)
  }
  if (separador_colunas == separador_decimal) {
    stop("O separador de colunas e o decimal não podem ser iguais (",
         separador_colunas, "). Se as colunas são separadas por vírgula, ",
         "o decimal precisa ser ponto.", call. = FALSE)
  }

  return(list(sep = separador_colunas, dec = separador_decimal))
}


# ------------------------------------------------------------------------------
# montar_entradas()
#   Seleciona e confere todas as entradas e as reúne em uma única lista
#   ("objeto" entradas), que é passada para as próximas etapas:
#       $var_preditoras, $mascara, $registros, $formato_registros, $cenarios
# ------------------------------------------------------------------------------
montar_entradas <- function(caminho_var_preditoras, caminho_mascara, caminho_registros,
                            separador_colunas, separador_decimal,
                            usar_cenarios, caminhos_cenarios) {

  list(
    var_preditoras = selecionar_pasta(
      caminho  = caminho_var_preditoras,
      titulo   = "Selecione a pasta com as variáveis PREDITORAS (.tif)",
      extensao = "tif"
    ),
    mascara = selecionar_arquivo(
      caminho   = caminho_mascara,
      titulo    = "Selecione a máscara da área de estudo (.shp ou .tif)",
      extensoes = c("shp", "tif")
    ),
    registros = selecionar_arquivo(
      caminho   = caminho_registros,
      titulo    = "Selecione o arquivo de registros (.csv ou .txt)",
      extensoes = c("csv", "txt")
    ),
    formato_registros = definir_formato_registros(
      separador_colunas = separador_colunas,
      separador_decimal = separador_decimal
    ),
    cenarios = selecionar_cenarios(
      usar_cenarios     = usar_cenarios,
      caminhos_cenarios = caminhos_cenarios
    )
  )
}


# ------------------------------------------------------------------------------
# resumir_entradas()
#   Mostra no console um resumo do que foi selecionado.
# ------------------------------------------------------------------------------
resumir_entradas <- function(entradas) {
  n_tif <- length(list.files(entradas$var_preditoras, pattern = "\\.tif$", ignore.case = TRUE))

  cat("\n================ ENTRADAS SELECIONADAS ================\n")
  cat("Variáveis preditoras :", entradas$var_preditoras, "(", n_tif, "arquivos .tif )\n")
  cat("Máscara          :", entradas$mascara, "\n")
  cat("Registros        :", entradas$registros, "\n")

  nome_sep <- c("," = "vírgula", ";" = "ponto e vírgula", "\t" = "tabulação")
  nome_dec <- c("." = "ponto", "," = "vírgula")
  cat("  - colunas separadas por:", nome_sep[[entradas$formato_registros$sep]], "\n")
  cat("  - decimal               :", nome_dec[[entradas$formato_registros$dec]], "\n")

  if (length(entradas$cenarios) == 0) {
    cat("Cenários         : nenhum\n")
  } else {
    cat("Cenários         :\n")
    for (nome in names(entradas$cenarios)) {
      cat("   -", nome, ":", entradas$cenarios[[nome]], "\n")
    }
  }
  cat("=======================================================\n\n")
}


# ------------------------------------------------------------------------------
# definir_colunas_registros()
#   Define quais colunas do arquivo de registros são longitude, latitude e
#   espécie. Para cada coluna do main:
#     - preenchida (nome do cabeçalho)  -> usa esse nome;
#     - vazia ("")                      -> mostra as colunas do cabeçalho (com um
#                                          exemplo de valor) e pergunta qual é.
#   Para 'coluna_especie': "" pergunta (com a opção "nenhuma, uma espécie só");
#   NA significa direto que o arquivo tem uma espécie só (não pergunta).
#   Devolve list(longitude, latitude, especie); especie = "" se não houver.
# ------------------------------------------------------------------------------
definir_colunas_registros <- function(caminho_registros, formato,
                                      coluna_longitude = "", coluna_latitude = "",
                                      coluna_especie = "") {

  vazio <- function(x) is.null(x) || length(x) == 0 || (length(x) == 1 && !is.na(x) && x == "")
  sem_especie <- length(coluna_especie) == 1 && is.na(coluna_especie)

  pede_lon <- vazio(coluna_longitude)
  pede_lat <- vazio(coluna_latitude)
  pede_esp <- vazio(coluna_especie)

  if (!(pede_lon || pede_lat || pede_esp)) {
    return(list(longitude = coluna_longitude, latitude = coluna_latitude,
                especie = if (sem_especie) "" else coluna_especie))
  }
  if (!interactive()) {
    stop("Não é possível perguntar as colunas fora de uma sessão interativa. ",
         "Preencha 'coluna_longitude', 'coluna_latitude' e 'coluna_especie' no main.",
         call. = FALSE)
  }

  tabela  <- ler_registros(caminho_registros, formato, nrows = 200)
  nomes   <- names(tabela)
  exemplos <- vapply(tabela, function(x) {
    v <- x[!is.na(x) & x != ""]
    if (length(v) > 0) as.character(v[1]) else ""
  }, character(1))
  opcoes <- paste0(nomes, "   (ex.: ", exemplos, ")")

  cat("\n================ COLUNAS DO ARQUIVO DE REGISTROS ================\n")
  cat("Arquivo:", caminho_registros, "\n")

  escolher <- function(rotulo, numerica = FALSE, permitir_nenhuma = FALSE) {
    ops <- if (permitir_nenhuma) c(opcoes, "Nenhuma (o arquivo tem uma espécie só)") else opcoes
    repeat {
      cat("\n", rotulo, "\n", sep = "")
      n <- perguntar_opcoes(ops, "Número da coluna: ", rotulo)[1]
      if (permitir_nenhuma && n == length(ops)) return("")
      if (numerica && all(is.na(suppressWarnings(as.numeric(tabela[[n]]))))) {
        message("A coluna '", nomes[n], "' não parece ter números. Escolha outra ",
                "(confira também o separador decimal no main).")
        next
      }
      return(nomes[n])
    }
  }

  longitude <- if (pede_lon) escolher("Qual é a coluna de LONGITUDE?", numerica = TRUE) else coluna_longitude
  latitude  <- if (pede_lat) escolher("Qual é a coluna de LATITUDE?", numerica = TRUE) else coluna_latitude
  especie   <- if (sem_especie) "" else if (pede_esp) {
    escolher("Qual é a coluna da ESPÉCIE?", permitir_nenhuma = TRUE)
  } else coluna_especie

  cat("\nColunas escolhidas -> longitude:", longitude, "| latitude:", latitude,
      "| espécie:", if (especie == "") "(nenhuma)" else especie, "\n")
  cat("Para não perguntar de novo, preencha 'coluna_longitude', 'coluna_latitude' e 'coluna_especie' no main.\n")
  cat("=================================================================\n")

  list(longitude = longitude, latitude = latitude, especie = especie)
}


# ------------------------------------------------------------------------------
# criar_pastas_saida()
#   Cria, dentro da pasta do projeto, as subpastas onde os arquivos serão salvos:
#     mask/                  -> máscara usada no corte
#     variaveis_cortadas/    -> variáveis (preditoras e cenários) já cortadas
#     variaveis_escalonadas/ -> as mesmas variáveis em z-score (rasters)
#     verificacao/           -> relatórios da verificação (CSV)
#     correlacao/            -> matriz de correlação e amostra das variáveis (CSV)
#     pontos/                -> registros limpos, background (CSV) e mapa de esforço amostral
#     multicolinearidade/    -> resultados dos testes e variáveis finais
#     modelos/               -> dados, avaliação, modelos, mapas, consenso, JSDM e extrapolação
#   Devolve uma lista com os caminhos.
# ------------------------------------------------------------------------------
criar_pastas_saida_nomes <- function() {
  list(mask = "mask", variaveis_cortadas = "variaveis_cortadas",
       variaveis_escalonadas = "variaveis_escalonadas", verificacao = "verificacao",
       correlacao = "correlacao", pontos = "pontos", multicolinearidade = "multicolinearidade",
       modelos = "modelos")
}

criar_pastas_saida <- function(pasta_projeto) {

  pastas <- list(
    mask                  = file.path(pasta_projeto, "mask"),
    variaveis_cortadas    = file.path(pasta_projeto, "variaveis_cortadas"),
    variaveis_escalonadas = file.path(pasta_projeto, "variaveis_escalonadas"),
    verificacao           = file.path(pasta_projeto, "verificacao"),
    correlacao            = file.path(pasta_projeto, "correlacao"),
    pontos                = file.path(pasta_projeto, "pontos"),
    multicolinearidade    = file.path(pasta_projeto, "multicolinearidade"),
    modelos               = file.path(pasta_projeto, "modelos")
  )

  for (pasta in pastas) {
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  }

  return(pastas)
}
