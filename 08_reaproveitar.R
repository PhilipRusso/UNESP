# ==============================================================================
#  FUNÇÕES - Rodar o main por partes
# ------------------------------------------------------------------------------
#  Cada função "obter_*" devolve um objeto de que uma seção precisa e procura
#  ele nesta ordem:
#     1) já existe na sessão do R?            -> usa o que existe;
#     2) foi salvo em uma execução anterior?  -> carrega o arquivo salvo;
#     3) não existe em lugar nenhum?          -> avisa e GERA o objeto com a
#                                                máscara, as variáveis e os
#                                                parâmetros definidos na seção 2.
#  Assim, cada seção do main pode ser rodada sozinha.
#
#  Atenção: se você mudar um parâmetro, rode de novo as seções afetadas, pois
#  objetos que já existem na sessão (ou arquivos já salvos) não são atualizados
#  automaticamente.
# ==============================================================================


# ------------------------------------------------------------------------------
# objeto_existe()
#   TRUE se o objeto 'nome' existe no 'ambiente' (por padrão, a sessão do R) e
#   não é vazio (NULL).
# ------------------------------------------------------------------------------
objeto_existe <- function(nome, ambiente = globalenv()) {
  exists(nome, envir = ambiente, inherits = FALSE) &&
    !is.null(get(nome, envir = ambiente, inherits = FALSE))
}


# ------------------------------------------------------------------------------
# carregar_parametros_main()
#   Para rodar uma seção sozinha, sem ter rodado a seção 2 nesta sessão:
#     1) se 'pasta_projeto' não existe na sessão, abre a janela para escolher a
#        pasta do projeto (a que tem o main.R e a pasta 'funcoes');
#     2) lê os parâmetros da seção 2 do main.R dessa pasta (se o main.R não
#        estiver lá, abre a janela para escolher o arquivo);
#     3) cria na sessão só os parâmetros que ainda NÃO existem (o que você já
#        definiu na sessão não é alterado);
#     4) se algum caminho do main (variáveis, máscara, registros, cenários) não
#        existir neste computador, ele é esvaziado: a janela de seleção abre
#        quando esse caminho for necessário.
# ------------------------------------------------------------------------------
carregar_parametros_main <- function(ambiente = globalenv()) {

  if (!exists("pasta_projeto", envir = ambiente, inherits = FALSE)) {
    pasta <- selecionar_pasta(
      "", "Selecione a pasta do projeto (a que contém o main.R e a pasta 'funcoes')",
      extensao = "R")
    assign("pasta_projeto", pasta, envir = ambiente)
  }

  arquivo <- file.path(get("pasta_projeto", envir = ambiente), "main.R")
  if (!file.exists(arquivo)) {
    message("Não encontrei o main.R em ", dirname(arquivo), ". Selecione o arquivo.")
    arquivo <- selecionar_arquivo("", "Selecione o main.R (com os parâmetros da seção 2)", "r")
  }

  linhas <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
  # a seção 2 vai do título "# 2. ..." até o título da seção seguinte ("# N. ...")
  ini <- grep("^# 2\\. ", linhas)
  if (length(ini) == 0) {
    stop("Não encontrei a seção 2 (parâmetros) no main.R: ", arquivo, call. = FALSE)
  }
  titulos <- grep("^# [0-9]+\\. ", linhas)
  fim <- titulos[titulos > ini[1]]
  fim <- if (length(fim) == 0) length(linhas) + 1L else fim[1]
  if (fim - ini[1] < 2) {
    stop("A seção 2 (parâmetros) do main.R está vazia: ", arquivo, call. = FALSE)
  }

  expressoes <- parse(text = linhas[(ini[1] + 1):(fim - 1)], keep.source = FALSE)
  amb <- new.env(parent = globalenv())
  for (e in expressoes) {
    # só as atribuições (parâmetros): chamadas como registrar_parametros_pendentes()
    # ou iniciar_etapa() não devem rodar aqui
    if (!(is.call(e) && as.character(e[[1]])[1] %in% c("<-", "="))) next
    tryCatch(eval(e, amb), error = function(err) {
      message("Aviso: não consegui ler um parâmetro do main.R (", conditionMessage(err), ")")
    })
  }

  novos <- character(0)
  for (nome in ls(amb)) {
    if (!exists(nome, envir = ambiente, inherits = FALSE)) {
      assign(nome, get(nome, envir = amb), envir = ambiente)
      novos <- c(novos, nome)
    }
  }

  # caminhos que não existem neste computador: esvazia, para abrir a janela depois
  for (nome in intersect(c("caminho_var_preditoras", "caminho_mascara", "caminho_registros"), novos)) {
    valor <- get(nome, envir = ambiente)
    if (length(valor) == 1 && !is.na(valor) && valor != "" && !parametro_prompt(valor) &&
        !file.exists(valor)) {
      message("Caminho de '", nome, "' não encontrado (", valor, "). A janela de seleção abrirá quando ele for necessário.")
      assign(nome, "prompt", envir = ambiente)
    }
  }
  if ("caminhos_cenarios" %in% novos) {
    cen <- get("caminhos_cenarios", envir = ambiente)
    if (length(cen) > 0 && any(!dir.exists(cen))) {
      message("Alguma pasta de cenário do main não foi encontrada. A janela de seleção abrirá quando for necessário.")
      assign("caminhos_cenarios", c(), envir = ambiente)
    }
  }

  # parâmetros com "prompt" viram perguntas feitas só no primeiro uso (valor inválido: aviso)
  registrar_parametros_pendentes(ambiente, novos)

  if (length(novos) > 0) {
    message("Parâmetros da seção 2 carregados de ", arquivo, " (", length(novos), " parâmetros que não estavam na sessão).")
  }
  invisible(novos)
}


# ------------------------------------------------------------------------------
# garantir_parametros()
#   Chamada no começo de cada seção do main: se os parâmetros da seção 2 não
#   estão na sessão, carrega do main.R (ver carregar_parametros_main()).
# ------------------------------------------------------------------------------
garantir_parametros <- function(ambiente = globalenv()) {
  sentinelas <- c("semente", "preditores_modelos", "caminho_var_preditoras", "pasta_projeto")
  if (all(vapply(sentinelas, exists, logical(1), envir = ambiente, inherits = FALSE))) {
    return(invisible(FALSE))
  }
  carregar_parametros_main(ambiente)
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# ler_parametro()
#   Lê um parâmetro da seção 2 (ou 'pasta_projeto'). Se não existir na sessão,
#   carrega os parâmetros do main.R (e abre a janela da pasta do projeto, se
#   preciso). Só para com erro se o parâmetro não existir nem no main.R.
# ------------------------------------------------------------------------------
ler_parametro <- function(nome, ambiente = globalenv()) {
  if (!exists(nome, envir = ambiente, inherits = FALSE)) {
    carregar_parametros_main(ambiente)
  }
  if (!exists(nome, envir = ambiente, inherits = FALSE)) {
    stop("O parâmetro '", nome, "' não foi encontrado na sessão nem na seção 2 do main.R.",
         call. = FALSE)
  }
  get(nome, envir = ambiente, inherits = FALSE)
}


# ------------------------------------------------------------------------------
# arredondar_sdm()
#   Arredonda tabelas e números que são SALVOS ou mostrados no console, de acordo
#   com o parâmetro 'casas_decimais' da seção 2:
#     NULL (padrão) -> NÃO arredonda (só elimina o "ruído" de ponto flutuante,
#                      mantendo 12 algarismos significativos);
#     número        -> arredonda para essa quantidade de casas decimais.
#   Aceita número, vetor, matriz ou data.frame numérico.
# ------------------------------------------------------------------------------
arredondar_sdm <- function(x, casas = NULL) {

  if (is.null(casas)) {
    casas <- tryCatch({
      if (exists("casas_decimais", envir = globalenv(), inherits = FALSE)) {
        get("casas_decimais", envir = globalenv(), inherits = FALSE)
      } else ler_parametro("casas_decimais")
    }, error = function(e) NULL)
  }
  if (is.null(casas) || length(casas) != 1 || !is.numeric(casas) || is.na(casas)) {
    return(signif(x, 12))
  }
  round(x, casas)
}


# ------------------------------------------------------------------------------
# obter_entradas()
#   Devolve a lista 'entradas' (seção 4). Se não existir na sessão, monta com
#   os caminhos e parâmetros da seção 2.
# ------------------------------------------------------------------------------
obter_entradas <- function(ambiente = globalenv()) {

  if (objeto_existe("entradas", ambiente)) {
    return(get("entradas", envir = ambiente))
  }

  message("Aviso: 'entradas' não encontrado na sessão. ",
          "Será montado com os caminhos e parâmetros definidos na seção 2.")

  entradas <- montar_entradas(
    caminho_var_preditoras = ler_parametro("caminho_var_preditoras", ambiente),
    caminho_mascara    = ler_parametro("caminho_mascara", ambiente),
    caminho_registros  = ler_parametro("caminho_registros", ambiente),
    separador_colunas  = ler_parametro("separador_colunas", ambiente),
    separador_decimal  = ler_parametro("separador_decimal", ambiente),
    usar_cenarios      = ler_parametro("usar_cenarios", ambiente),
    caminhos_cenarios  = ler_parametro("caminhos_cenarios", ambiente)
  )
  resumir_entradas(entradas)

  return(entradas)
}


# ------------------------------------------------------------------------------
# obter_colunas_registros()
#   Colunas de longitude, latitude e espécie do arquivo de registros (seção 7):
#   da sessão ('colunas_registros') ou definidas agora com os parâmetros do main.
#   Se algum estiver vazio no main, pergunta qual coluna é (definir_colunas_registros()).
#   Devolve list(longitude, latitude, especie).
# ------------------------------------------------------------------------------
obter_colunas_registros <- function(ambiente = globalenv()) {

  if (objeto_existe("colunas_registros", ambiente)) {
    return(get("colunas_registros", envir = ambiente))
  }
  entradas <- obter_entradas(ambiente)
  colunas  <- definir_colunas_registros(
    caminho_registros = entradas$registros,
    formato           = entradas$formato_registros,
    coluna_longitude  = ler_parametro("coluna_longitude", ambiente),
    coluna_latitude   = ler_parametro("coluna_latitude", ambiente),
    coluna_especie    = ler_parametro("coluna_especie", ambiente))
  assign("colunas_registros", colunas, envir = ambiente)
  colunas
}


# ------------------------------------------------------------------------------
# obter_pastas_saida()
#   Devolve a lista com as pastas de saída (seção 3). Se não existir na
#   sessão, confere/cria as pastas dentro da pasta do projeto.
# ------------------------------------------------------------------------------
obter_pastas_saida <- function(ambiente = globalenv()) {

  if (objeto_existe("pastas_saida", ambiente)) {
    existentes <- get("pastas_saida", envir = ambiente)
    # 'pastas_saida' criado por uma versão antiga pode não ter todas as pastas
    if (all(names(criar_pastas_saida_nomes()) %in% names(existentes))) return(existentes)
    message("Aviso: 'pastas_saida' está desatualizado (faltam pastas novas). Recriando.")
  }

  pasta_projeto <- ler_parametro("pasta_projeto", ambiente)
  message("Aviso: 'pastas_saida' não encontrado na sessão. ",
          "Conferindo/criando as pastas em: ", pasta_projeto)

  return(criar_pastas_saida(pasta_projeto))
}


# ------------------------------------------------------------------------------
# obter_mascara_corte()
#   Devolve a máscara usada no corte (seções 4 e 5):
#     1) a que está na sessão;
#     2) senão, a salva em mask/ (a mais recente, .shp ou .tif);
#     3) senão, gera a máscara com os parâmetros da seção 2 (feições,
#        projeção) e salva em mask/.
# ------------------------------------------------------------------------------
obter_mascara_corte <- function(ambiente = globalenv()) {

  if (objeto_existe("mascara_corte", ambiente)) {
    return(get("mascara_corte", envir = ambiente))
  }

  pastas <- obter_pastas_saida(ambiente)

  candidatas <- file.path(pastas$mask, c("mascara_corte.shp", "mascara_corte.tif"))
  candidatas <- candidatas[file.exists(candidatas)]

  if (length(candidatas) > 0) {
    salva <- candidatas[which.max(file.mtime(candidatas))]
    message("Aviso: 'mascara_corte' não encontrada na sessão. ",
            "Carregando a máscara salva em: ", salva,
            "\n  (se você mudou a máscara ou o filtro de feições, rode as seções 4 e 5 de novo)")
    if (tolower(tools::file_ext(salva)) == "shp") return(terra::vect(salva))
    return(terra::rast(salva))
  }

  message("Aviso: não há máscara na sessão nem salva em ", pastas$mask, ". ",
          "Ela será gerada com a máscara e os parâmetros definidos na seção 2.")

  entradas <- obter_entradas(ambiente)
  mascara  <- preparar_mascara(
    caminho_mascara    = entradas$mascara,
    cortar_por_feicoes = ler_parametro("cortar_por_feicoes", ambiente),
    filtro_feicoes     = ler_parametro("filtro_feicoes", ambiente)
  )
  mascara <- ajustar_crs_mascara(mascara, entradas$var_preditoras)
  salvar_mascara(mascara, pastas$mask)

  return(mascara)
}


# ------------------------------------------------------------------------------
# obter_variaveis_cortadas()
#   Devolve a lista com os arquivos das variáveis cortadas (seção 6):
#       variaveis_cortadas[["preditoras"]], variaveis_cortadas[["<cenário>"]], ...
#     1) a que está na sessão;
#     2) senão, procura os .tif já cortados em variaveis_cortadas/<conjunto>/.
#        Uma pasta só vale se tiver TODOS os arquivos da pasta original;
#     3) o que faltar (ou estiver incompleto) é cortado agora, com a máscara e
#        as variáveis definidas na seção 2.
# ------------------------------------------------------------------------------
obter_variaveis_cortadas <- function(ambiente = globalenv()) {

  if (objeto_existe("variaveis_cortadas", ambiente)) {
    if (length(.sdm_renomeio$mapa) == 0) {
      carregar_renomeio(obter_pastas_saida(ambiente)$verificacao)
    }
    return(get("variaveis_cortadas", envir = ambiente))
  }

  entradas <- obter_entradas(ambiente)
  pastas   <- obter_pastas_saida(ambiente)
  carregar_renomeio(pastas$verificacao)   # nomes de variáveis definidos na seção 12

  # pasta de origem de cada conjunto: preditoras + cenários definidos no main
  origem <- c(list(preditoras = entradas$var_preditoras), as.list(entradas$cenarios))

  resultado <- list()
  pendentes <- character(0)

  for (conjunto in names(origem)) {
    esperados <- list.files(origem[[conjunto]], pattern = "\\.tif$", ignore.case = TRUE)
    salvos    <- list.files(file.path(pastas$variaveis_cortadas, conjunto),
                            pattern = "\\.tif$", ignore.case = TRUE, full.names = TRUE)
    # rasters excluídos na seção 7 (geometria diferente) não voltam à análise
    excluidos <- ler_rasters_excluidos(pastas$verificacao)
    esperados <- setdiff(esperados, excluidos)
    salvos    <- salvos[!basename(salvos) %in% excluidos]

    if (length(salvos) > 0 && setequal(basename(salvos), esperados)) {
      resultado[[conjunto]] <- salvos
    } else {
      pendentes <- c(pendentes, conjunto)
    }
  }

  carregados <- setdiff(names(origem), pendentes)
  if (length(carregados) > 0) {
    message("Aviso: 'variaveis_cortadas' não encontrado na sessão. ",
            "Usando os arquivos já cortados e salvos de: ",
            paste(carregados, collapse = ", "))
  }

  if (length(pendentes) > 0) {
    message("Aviso: sem arquivos cortados completos para: ",
            paste(pendentes, collapse = ", "),
            ". Serão cortados agora com a máscara e as variáveis definidas na seção 2.")

    mascara <- obter_mascara_corte(ambiente)
    for (conjunto in pendentes) {
      message("Cortando: ", conjunto)
      resultado[[conjunto]] <- cortar_pasta(
        pasta_entrada = origem[[conjunto]],
        mascara       = mascara,
        pasta_saida   = file.path(pastas$variaveis_cortadas, conjunto)
      )
    }
  }

  return(resultado[names(origem)])
}


# ------------------------------------------------------------------------------
# obter_registros_limpos()
#   Devolve os registros limpos (seção 7):
#     1) os que estão na sessão;
#     2) senão, os salvos em pontos/registros_limpos.csv;
#     3) senão, faz a limpeza agora com os parâmetros da seção 2.
# ------------------------------------------------------------------------------
obter_registros_limpos <- function(ambiente = globalenv()) {

  if (objeto_existe("registros_limpos", ambiente)) {
    return(get("registros_limpos", envir = ambiente))
  }

  pastas  <- obter_pastas_saida(ambiente)
  arquivo <- file.path(pastas$pontos, "registros_limpos.csv")

  if (file.exists(arquivo)) {
    message("Aviso: 'registros_limpos' não encontrado na sessão. ",
            "Carregando os registros salvos em: ", arquivo,
            "\n  (se você mudou os registros ou a limpeza, rode a seção 7 de novo)")
    return(list(registros = utils::read.csv(arquivo, fileEncoding = "UTF-8"),
                passos = NULL))
  }

  message("Aviso: não há registros limpos na sessão nem salvos em ", pastas$pontos,
          ". A limpeza será feita agora com os parâmetros definidos na seção 2.")

  return(preparar_registros(
    entradas           = obter_entradas(ambiente),
    mascara            = obter_mascara_corte(ambiente),
    arquivos_variaveis = obter_variaveis_cortadas(ambiente)[["preditoras"]],
    coluna_longitude   = obter_colunas_registros(ambiente)$longitude,
    coluna_latitude    = obter_colunas_registros(ambiente)$latitude,
    crs_registros      = ler_parametro("crs_registros", ambiente),
    remover_duplicados = ler_parametro("remover_duplicados", ambiente),
    um_por_celula      = ler_parametro("um_registro_por_celula", ambiente),
    coluna_especie     = obter_colunas_registros(ambiente)$especie
  ))
}


# ------------------------------------------------------------------------------
# obter_metodos_escolhidos()
#   Devolve os métodos de multicolinearidade (seção 11): da sessão ou, se não
#   existir, a partir do parâmetro 'metodos_multicolinearidade' (pergunta se
#   estiver vazio).
# ------------------------------------------------------------------------------
obter_metodos_escolhidos <- function(ambiente = globalenv()) {

  if (objeto_existe("metodos_escolhidos", ambiente)) {
    return(get("metodos_escolhidos", envir = ambiente))
  }
  message("Aviso: 'metodos_escolhidos' não encontrado na sessão. ",
          "Será obtido a partir de 'metodos_multicolinearidade' (seção 2).")
  return(escolher_metodos_multicolinearidade(ler_parametro("metodos_multicolinearidade", ambiente)))
}


# ------------------------------------------------------------------------------
# obter_dados_testes()
#   Devolve os dados em z-score dos testes (seção 10): da sessão ou gerados
#   agora com as variáveis cortadas e os parâmetros da seção 2.
# ------------------------------------------------------------------------------
obter_dados_testes <- function(ambiente = globalenv()) {

  if (objeto_existe("dados_testes", ambiente)) {
    return(get("dados_testes", envir = ambiente))
  }
  message("Aviso: 'dados_testes' não encontrado na sessão. ",
          "Serão gerados com as variáveis cortadas e os parâmetros da seção 2.")
  return(preparar_dados_testes(obter_variaveis_cortadas(ambiente),
                               ler_parametro("n_amostra_testes", ambiente),
                               ler_parametro("semente", ambiente)))
}


# ------------------------------------------------------------------------------
# obter_pontos()
#   Devolve a tabela presença/background (seção 9): da sessão, do arquivo
#   salvo em pontos/ ou gerada agora (limpeza + background).
# ------------------------------------------------------------------------------
obter_pontos <- function(ambiente = globalenv()) {

  if (objeto_existe("pontos", ambiente)) {
    return(get("pontos", envir = ambiente))
  }

  pastas  <- obter_pastas_saida(ambiente)
  arquivo <- file.path(pastas$pontos, "pontos_presenca_background.csv")

  if (file.exists(arquivo)) {
    message("Aviso: 'pontos' não encontrado na sessão. Carregando: ", arquivo,
            "\n  (se mudou registros ou background, rode as seções 7 e 9 de novo)")
    return(utils::read.csv(arquivo, fileEncoding = "UTF-8"))
  }

  message("Aviso: não há pontos salvos em ", pastas$pontos,
          ". Registros e background serão gerados agora com os parâmetros da seção 2.")
  registros <- obter_registros_limpos(ambiente)
  arquivos  <- obter_variaveis_cortadas(ambiente)[["preditoras"]]
  esforco   <- preparar_esforco(
    vies               = ler_parametro("vies_background", ambiente),
    registros          = registros$registros,
    arquivos_variaveis = arquivos,
    raio_kernel_km     = ler_parametro("raio_kernel_km", ambiente),
    caminho_esforco    = ler_parametro("caminho_esforco", ambiente),
    formato            = obter_entradas(ambiente)$formato_registros,
    coluna_longitude   = obter_colunas_registros(ambiente)$longitude,
    coluna_latitude    = obter_colunas_registros(ambiente)$latitude,
    crs_registros      = ler_parametro("crs_registros", ambiente),
    pasta_saida        = pastas$pontos,
    entradas           = obter_entradas(ambiente),
    mascara            = obter_mascara_corte(ambiente))
  background <- gerar_background(
    registros          = registros$registros,
    arquivos_variaveis = arquivos,
    distancia_km       = ler_parametro("distancia_buffer_km", ambiente),
    fator              = ler_parametro("fator_background", ambiente),
    minimo             = ler_parametro("minimo_background", ambiente),
    esforco            = esforco,
    semente            = ler_parametro("semente", ambiente),
    raio_pseudoausencia_km = ler_parametro("raio_pseudoausencia_km", ambiente))
  return(salvar_pontos(registros$registros, background, pastas$pontos))
}


# ------------------------------------------------------------------------------
# obter_resultados_sem_resposta() / obter_resultados_com_resposta()
#   Devolvem os resultados da seção 11 (da sessão ou gerados agora).
# ------------------------------------------------------------------------------
obter_resultados_sem_resposta <- function(ambiente = globalenv()) {

  if (objeto_existe("resultados_sem_resposta", ambiente)) {
    return(get("resultados_sem_resposta", envir = ambiente))
  }
  message("Aviso: 'resultados_sem_resposta' não encontrado na sessão. Os testes serão rodados agora.")
  pastas <- obter_pastas_saida(ambiente)
  rodar_testes_sem_resposta(
    dados_testes   = obter_dados_testes(ambiente),
    metodos        = obter_metodos_escolhidos(ambiente),
    pasta_saida    = file.path(pastas$multicolinearidade, "sem_resposta"),
    limite_vif     = ler_parametro("limite_vif", ambiente),
    limiar_filtro  = ler_parametro("limiar_filtro_correlacao", ambiente),
    limiar_cluster = ler_parametro("limiar_cluster", ambiente),
    variancia_pca  = ler_parametro("variancia_pca", ambiente),
    n_eixos_pca    = ler_parametro("n_eixos_pca", ambiente))
}

obter_resultados_com_resposta <- function(ambiente = globalenv()) {

  if (objeto_existe("resultados_com_resposta", ambiente)) {
    return(get("resultados_com_resposta", envir = ambiente))
  }
  message("Aviso: 'resultados_com_resposta' não encontrado na sessão. Os testes serão rodados agora.")
  pastas  <- obter_pastas_saida(ambiente)
  dados_resposta <- preparar_dados_resposta(
    pontos           = obter_pontos(ambiente),
    arquivos_preditoras  = obter_variaveis_cortadas(ambiente)[["preditoras"]],
    minimo_presencas = ler_parametro("minimo_presencas_testes", ambiente))
  rodar_testes_com_resposta(
    dados_resposta   = dados_resposta,
    metodos          = obter_metodos_escolhidos(ambiente),
    pasta_saida      = file.path(pastas$multicolinearidade, "com_resposta"),
    alfa_elastic_net = ler_parametro("alfa_elastic_net", ambiente),
    semente          = ler_parametro("semente", ambiente))
}


# ------------------------------------------------------------------------------
# obter_pca_eixos()
#   Devolve o resultado da seção 12 (PCA como transformação): da sessão ou
#   gerado agora com as variáveis cortadas e os parâmetros da seção 2.
# ------------------------------------------------------------------------------
obter_pca_eixos <- function(ambiente = globalenv()) {

  if (objeto_existe("pca_eixos", ambiente)) {
    return(get("pca_eixos", envir = ambiente))
  }
  message("Aviso: 'pca_eixos' não encontrado na sessão. Será gerado agora.")
  pastas <- obter_pastas_saida(ambiente)
  gerar_pca_eixos(
    dados_testes       = obter_dados_testes(ambiente),
    variaveis_cortadas = obter_variaveis_cortadas(ambiente),
    pasta_saida        = file.path(pastas$multicolinearidade, "pca_eixos"),
    variancia_alvo     = ler_parametro("variancia_pca", ambiente),
    n_eixos            = ler_parametro("n_eixos_pca", ambiente))
}


# ==============================================================================
#  Modelos de distribuição (seções 15 a 24)
# ==============================================================================

# ------------------------------------------------------------------------------
# obter_variaveis_finais()
#   Variáveis finais da seção 13: da sessão, do arquivo salvo ou, se não
#   houver, do parâmetro 'variaveis_mantidas'. Devolve vazio se não achar.
# ------------------------------------------------------------------------------
obter_variaveis_finais <- function(ambiente = globalenv()) {

  if (objeto_existe("variaveis_finais", ambiente)) {
    return(get("variaveis_finais", envir = ambiente))
  }
  arquivo <- file.path(obter_pastas_saida(ambiente)$multicolinearidade, "variaveis_finais.csv")
  if (file.exists(arquivo)) {
    message("Aviso: 'variaveis_finais' não encontrado na sessão. Usando: ", arquivo)
    return(utils::read.csv(arquivo, fileEncoding = "UTF-8")$variavel)
  }
  if (exists("variaveis_mantidas", envir = ambiente, inherits = FALSE)) {
    return(get("variaveis_mantidas", envir = ambiente))
  }
  character(0)
}


# ------------------------------------------------------------------------------
# obter_variaveis_escalonadas()
#   Rasters escalonados da seção 10: da sessão, dos arquivos já salvos ou,
#   se não houver, gerados agora.
# ------------------------------------------------------------------------------
obter_variaveis_escalonadas <- function(ambiente = globalenv()) {

  if (objeto_existe("variaveis_escalonadas", ambiente)) {
    return(get("variaveis_escalonadas", envir = ambiente))
  }
  pastas <- obter_pastas_saida(ambiente)
  conjuntos <- list.dirs(pastas$variaveis_escalonadas, full.names = FALSE, recursive = FALSE)
  if ("preditoras" %in% conjuntos) {
    return(sapply(conjuntos, function(cj) list.files(file.path(pastas$variaveis_escalonadas, cj),
                                                     pattern = "\\.tif$", full.names = TRUE),
                  simplify = FALSE))
  }
  message("Aviso: não há variáveis escalonadas salvas. Serão geradas agora (seção 10).")
  salvar_variaveis_escalonadas(
    variaveis_cortadas = obter_variaveis_cortadas(ambiente),
    parametros         = obter_dados_testes(ambiente)[["preditoras"]]$parametros,
    pasta_saida        = pastas$variaveis_escalonadas)
}


# ------------------------------------------------------------------------------
# garantir_rasters_preditores()
#   Confere se os rasters dos preditores existem (variáveis escalonadas ou
#   eixos da PCA) e, se não existirem, gera com as seções 10 ou 12.
# ------------------------------------------------------------------------------
garantir_rasters_preditores <- function(tipo, ambiente = globalenv()) {
  if (tipo == "variaveis") {
    invisible(obter_variaveis_escalonadas(ambiente))
  } else {
    pasta_pca <- file.path(obter_pastas_saida(ambiente)$multicolinearidade, "pca_eixos")
    if (length(list.dirs(pasta_pca, recursive = FALSE)) == 0) invisible(obter_pca_eixos(ambiente))
  }
}


# ------------------------------------------------------------------------------
# obter_modelos_escolhidos() / obter_preditores() / obter_dados_modelos()
#   Objetos das seções 14 e 15: da sessão ou gerados com os parâmetros da
#   seção 2.
# ------------------------------------------------------------------------------
obter_modelos_escolhidos <- function(ambiente = globalenv()) {
  if (objeto_existe("modelos_escolhidos", ambiente)) {
    return(get("modelos_escolhidos", envir = ambiente))
  }
  message("Aviso: 'modelos_escolhidos' não encontrado na sessão. Usando 'modelos' (seção 2).")
  escolher_modelos(ler_parametro("modelos", ambiente))
}

obter_preditores <- function(ambiente = globalenv()) {
  if (objeto_existe("preditores", ambiente)) {
    return(get("preditores", envir = ambiente))
  }
  # usa a rodada da seção 14; se ela não foi feita, pergunta agora (variáveis,
  # eixos da PCA ou os dois) e define os preditores
  obter_rodada_ativa(ambiente)$preditores
}

obter_dados_modelos <- function(ambiente = globalenv()) {
  if (objeto_existe("dados_modelos", ambiente)) {
    return(get("dados_modelos", envir = ambiente))
  }
  message("Aviso: 'dados_modelos' não encontrado na sessão. Serão preparados agora.")
  preparar_dados_modelos(
    pontos           = obter_pontos(ambiente),
    preditores       = obter_preditores(ambiente),
    pasta_saida      = file.path(obter_pastas_saida(ambiente)$modelos, "dados"),
    minimo_presencas = ler_parametro("minimo_presencas_modelos", ambiente),
    tipo_validacao   = ler_parametro("tipo_validacao", ambiente),
    n_folds          = ler_parametro("n_folds", ambiente),
    semente          = ler_parametro("semente", ambiente),
    particao_teste   = ler_parametro("particao_teste", ambiente),
    particao_metodo  = ler_parametro("particao_metodo", ambiente))
}


# ------------------------------------------------------------------------------
# obter_ajuste_maxent()
#   Classes e regmult do Maxent escolhidos para cada espécie (seção 16).
#   Devolve vazio (NULL) se o Maxent não está em 'modelos' ou se
#   'testar_ajustes_maxent' = FALSE. Senão: da sessão, do arquivo salvo em
#   modelos/avaliacao/ ou, se não houver, faz o ajuste agora (pode demorar).
# ------------------------------------------------------------------------------
obter_ajuste_maxent <- function(ambiente = globalenv()) {
  if (!isTRUE(ler_parametro("testar_ajustes_maxent", ambiente)) ||
      !"maxent" %in% obter_modelos_escolhidos(ambiente) ||
      pulado(ler_parametro("tipo_validacao", ambiente))) {
    return(NULL)
  }
  if (objeto_existe("ajuste_maxent", ambiente)) {
    return(get("ajuste_maxent", envir = ambiente))
  }
  pasta   <- file.path(obter_pastas_saida(ambiente)$modelos, "avaliacao")
  arquivo <- file.path(pasta, "ajuste_maxent.csv")
  if (file.exists(arquivo)) {
    message("Aviso: 'ajuste_maxent' não encontrado na sessão. Usando: ", arquivo)
    return(utils::read.csv(arquivo, fileEncoding = "UTF-8", stringsAsFactors = FALSE))
  }
  message("Aviso: não há ajuste do Maxent salvo. As combinações serão testadas agora.")
  selecionar_ajuste_maxent(
    dados_modelos = obter_dados_modelos(ambiente),
    ajustes       = ler_parametro("ajustes_modelos", ambiente),
    validacoes    = ler_parametro("tipo_validacao", ambiente),
    classes       = ler_parametro("maxent_classes_testadas", ambiente),
    regmult       = ler_parametro("maxent_regmult_testados", ambiente),
    pasta_saida   = pasta,
    semente       = ler_parametro("semente", ambiente))
}


# ------------------------------------------------------------------------------
# obter_avaliacao() / obter_modelos_finais() / obter_mapas()
#   Objetos das seções 16 e 17: da sessão, dos arquivos salvos ou, se não
#   houver, gerados agora (pode demorar).
# ------------------------------------------------------------------------------
obter_avaliacao <- function(ambiente = globalenv()) {
  if (pulado(ler_parametro("tipo_validacao", ambiente))) return(NULL)   # NA = sem validação
  if (objeto_existe("avaliacao", ambiente)) {
    return(get("avaliacao", envir = ambiente))
  }
  pasta <- file.path(obter_pastas_saida(ambiente)$modelos, "avaliacao")
  arquivo <- file.path(pasta, "avaliacao.rds")
  if (file.exists(arquivo)) {
    message("Aviso: 'avaliacao' não encontrado na sessão. Usando: ", arquivo)
    return(readRDS(arquivo))
  }
  message("Aviso: não há avaliação salva. A validação cruzada será feita agora.")
  avaliar_modelos(obter_dados_modelos(ambiente), obter_modelos_escolhidos(ambiente),
                  ler_parametro("ajustes_modelos", ambiente),
                  ler_parametro("tipo_validacao", ambiente), pasta,
                  ler_parametro("semente", ambiente),
                  ajuste_maxent = obter_ajuste_maxent(ambiente),
                  preditores = obter_preditores(ambiente))
}

obter_modelos_finais <- function(ambiente = globalenv()) {
  if (objeto_existe("modelos_finais", ambiente)) {
    return(get("modelos_finais", envir = ambiente))
  }
  pasta <- file.path(obter_pastas_saida(ambiente)$modelos, "objetos")
  arquivo <- file.path(pasta, "modelos_finais.rds")
  if (file.exists(arquivo)) {
    message("Aviso: 'modelos_finais' não encontrado na sessão. Usando: ", arquivo)
    return(readRDS(arquivo))
  }
  message("Aviso: não há modelos finais salvos. Serão ajustados agora.")
  ajustar_modelos_finais(obter_dados_modelos(ambiente), obter_modelos_escolhidos(ambiente),
                         ler_parametro("ajustes_modelos", ambiente), pasta,
                         ler_parametro("semente", ambiente),
                         ajuste_maxent = obter_ajuste_maxent(ambiente),
                         somente_treino = !isTRUE(ler_parametro("reajustar_modelo_final", ambiente)))
}

obter_mapas <- function(ambiente = globalenv()) {
  if (objeto_existe("mapas", ambiente)) {
    return(get("mapas", envir = ambiente))
  }
  pasta <- file.path(obter_pastas_saida(ambiente)$modelos, "mapas")
  arquivo <- file.path(pasta, "mapas.csv")
  if (file.exists(arquivo)) {
    message("Aviso: 'mapas' não encontrado na sessão. Usando: ", arquivo)
    return(utils::read.csv(arquivo, fileEncoding = "UTF-8"))
  }
  message("Aviso: não há mapas salvos. Serão gerados agora.")
  gerar_mapas(obter_modelos_finais(ambiente), obter_preditores(ambiente),
              obter_limiares(ambiente)$tabela, pasta,
              ler_parametro("salvar_binarios", ambiente))
}


# ------------------------------------------------------------------------------
# obter_limiares() / obter_consenso() / obter_jsdm()
#   Objetos das seções 17 a 19: da sessão, dos arquivos salvos ou (limiares e
#   consenso) gerados agora. O JSDM não é rodado aqui: se não existir, volta
#   vazio (NULL).
# ------------------------------------------------------------------------------
obter_limiares <- function(ambiente = globalenv()) {
  if (pulado(ler_parametro("metodos_limiar", ambiente))) return(NULL)   # NA = sem limiares
  if (objeto_existe("limiares", ambiente)) {
    return(get("limiares", envir = ambiente))
  }
  pasta <- file.path(obter_pastas_saida(ambiente)$modelos, "limiares")
  arquivo <- file.path(pasta, "limiares.rds")
  if (file.exists(arquivo)) {
    message("Aviso: 'limiares' não encontrado na sessão. Usando: ", arquivo)
    return(readRDS(arquivo))
  }
  message("Aviso: não há limiares salvos. Serão calculados agora.")
  calcular_limiares(obter_modelos_finais(ambiente), obter_dados_modelos(ambiente),
                    obter_avaliacao(ambiente), ler_parametro("metodos_limiar", ambiente),
                    ler_parametro("limiar_fixo", ambiente), pasta,
                    preditores = obter_preditores(ambiente))
}

obter_consenso <- function(ambiente = globalenv()) {
  # NA em 'metodos_consenso' (ou sem validação, que dá os pesos e o filtro) = sem consenso
  if (pulado(ler_parametro("metodos_consenso", ambiente)) ||
      pulado(ler_parametro("tipo_validacao", ambiente))) return(NULL)
  if (objeto_existe("consenso", ambiente)) {
    return(get("consenso", envir = ambiente))
  }
  pasta <- file.path(obter_pastas_saida(ambiente)$modelos, "consenso")
  arquivo <- file.path(pasta, "consenso.rds")
  if (file.exists(arquivo)) {
    message("Aviso: 'consenso' não encontrado na sessão. Usando: ", arquivo)
    return(readRDS(arquivo))
  }
  message("Aviso: não há consenso salvo. Será gerado agora.")
  gerar_consenso(obter_mapas(ambiente), obter_avaliacao(ambiente), obter_limiares(ambiente),
                 ler_parametro("metodos_consenso", ambiente),
                 ler_parametro("limiar_tss_consenso", ambiente),
                 ler_parametro("metodos_limiar", ambiente), ler_parametro("limiar_fixo", ambiente),
                 ler_parametro("limiar_votacao", ambiente), pasta,
                 ler_parametro("salvar_binarios", ambiente),
                 preditores = obter_preditores(ambiente))
}

obter_jsdm <- function(ambiente = globalenv()) {
  if (objeto_existe("jsdm", ambiente)) {
    return(get("jsdm", envir = ambiente))
  }
  arquivo <- file.path(obter_pastas_saida(ambiente)$modelos, "jsdm", "jsdm_resultado.rds")
  if (file.exists(arquivo)) {
    message("Aviso: 'jsdm' não encontrado na sessão. Usando: ", arquivo)
    return(readRDS(arquivo))
  }
  NULL
}
