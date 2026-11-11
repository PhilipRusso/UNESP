# ==============================================================================
#  FUNÇÕES - Log da execução
# ------------------------------------------------------------------------------
#  Registra o que foi feito numa execução do script, para você ter controle do
#  que rodou e poder REPETIR a análise (igual ou mudando um parâmetro):
#    - hora de início, de término e duração do script e de cada etapa (seção);
#    - todos os parâmetros da seção 2 com os valores USADOS (inclusive os que
#      estavam vazios e foram respondidos durante a execução);
#    - as entradas (caminhos), as colunas dos registros, os modelos, os métodos de
#      multicolinearidade, as variáveis finais e as rodadas de preditores;
#    - o número de registros e de pontos (presenças e background) por espécie;
#    - os arquivos gerados em cada etapa;
#    - versão do R e dos pacotes.
#
#  Cada etapa do main chama iniciar_etapa(); o fim do main chama finalizar_log().
#  O log é gravado a cada etapa e depois de cada comando que você roda, então
#  existe mesmo que você rode só uma parte ou que ocorra um erro.
#
#  Arquivos (em <pasta do projeto>/logs/, com <id> = data e hora do início):
#    log_<id>.txt             -> o log para ler;
#    log_<id>_etapas.csv      -> início, fim, duração e situação de cada etapa;
#    log_<id>_arquivos.csv    -> os arquivos gerados, etapa por etapa;
#    log_<id>_parametros.R    -> os parâmetros usados, em código R;
#    main_<id>.R              -> cópia do main com a seção 2 trocada pelos valores
#                                usados: rodando-a você repete a análise; mudando um
#                                valor nela, repete com aquele parâmetro alterado.
#  Se só uma parte do script rodou, os arquivos ficam em logs/temporarios/
#  (log temporário da parte executada). Se todas as seções rodaram e o main chegou
#  ao fim, ficam em logs/.
# ==============================================================================


# ---- estado do log (objeto oculto na sessão: sobrevive ao rm(list = ls())) -----

log_estado <- function(criar = TRUE) {
  e <- get0(".sdm_log", envir = globalenv(), inherits = FALSE)
  if (is.null(e) && criar) {
    e <- new.env(parent = emptyenv())
    e$ativo     <- FALSE
    e$finalizado <- FALSE
    e$pendentes <- list()
    e$respostas <- list()
    assign(".sdm_log", e, envir = globalenv())
  }
  e
}

# a hora de início de uma execução completa é marcada no topo do main
iniciar_log <- function(inicio = Sys.time(), zerar_parametros = TRUE) {
  e <- log_estado()
  e$ativo      <- TRUE
  e$finalizado <- FALSE
  e$id         <- format(inicio, "%Y%m%d_%H%M%S")
  e$inicio     <- inicio
  e$fim        <- NULL
  e$etapas     <- list()
  e$atual      <- 0L
  e$arquivos   <- NULL
  e$n_cb       <- 0L
  e$ultima_atividade <- inicio
  e$ultimo_flush     <- .POSIXct(0)
  e$ultima_listagem  <- .POSIXct(0)
  e$destino    <- NULL
  if (isTRUE(zerar_parametros)) {
    e$pendentes <- list()
    e$respostas <- list()
  }
  instalar_callback_log()
  invisible(e)
}

# parâmetros vazios que viraram pergunta (registro feito em registrar_parametros_pendentes)
log_registrar_pendente <- function(nome, valor) {
  e <- log_estado()
  e$pendentes[nome] <- list(valor)
  invisible(NULL)
}

# resposta dada a uma dessas perguntas (feita em perguntar_parametro)
log_registrar_resposta <- function(nome, valor) {
  e <- log_estado()
  e$respostas[nome] <- list(valor)
  invisible(NULL)
}


# ---- callback: roda depois de cada comando de nível superior -------------------

instalar_callback_log <- function() {
  suppressWarnings(try(removeTaskCallback("sdm_log"), silent = TRUE))
  addTaskCallback(function(expr, value, ok, visible) {
    try(log_apos_comando(ok), silent = TRUE)
    TRUE
  }, name = "sdm_log")
  invisible(NULL)
}

log_apos_comando <- function(ok) {
  e <- log_estado(criar = FALSE)
  if (is.null(e) || !isTRUE(e$ativo) || isTRUE(e$finalizado)) return(invisible(NULL))
  agora <- Sys.time()
  e$n_cb <- e$n_cb + 1L
  e$ultima_atividade <- agora
  if (!isTRUE(ok) && e$atual > 0L) {
    et <- e$etapas[[e$atual]]
    et$status   <- "erro"
    et$mensagem <- trimws(gsub("\n", " ", geterrmessage()))
    e$etapas[[e$atual]] <- et
  }
  if (!isTRUE(ok) || as.numeric(difftime(agora, e$ultimo_flush, units = "secs")) >= 5) {
    escrever_log(e)
  }
  invisible(NULL)
}


# ---- etapas ---------------------------------------------------------------------

# nome da rodada de preditores em uso (vazio fora das rodadas)
nome_rodada_log <- function() {
  r <- get0("rodada_modelos", envir = globalenv(), inherits = FALSE)
  if (is.list(r) && !is.null(r$nome)) as.character(r$nome) else ""
}

# Marca o começo de uma etapa (seção do main) e fecha a anterior.
#   'nome'    : título da seção (ex.: "10. BACKGROUND")
#   'detalhe' : informação extra (ex.: o nome da rodada de preditores)
iniciar_etapa <- function(nome, detalhe = "", inicio = Sys.time()) {
  e <- log_estado()
  if (!isTRUE(e$ativo) || isTRUE(e$finalizado)) iniciar_log(inicio, zerar_parametros = FALSE)
  e <- log_estado()
  fechar_etapa(e)
  e$etapas[[length(e$etapas) + 1L]] <- list(nome = nome, detalhe = detalhe, inicio = inicio,
                                           fim = NULL, status = "em andamento",
                                           mensagem = "", n_cb = e$n_cb)
  e$atual <- length(e$etapas)
  escrever_log(e)
  invisible(NULL)
}

# momento em que a etapa terminou: o fim do último comando executado nela; se a
# etapa e a seguinte rodaram no MESMO comando (loop das rodadas), o momento atual
fim_da_etapa <- function(e, et) {
  if (e$n_cb > et$n_cb) max(et$inicio, e$ultima_atividade) else Sys.time()
}

fechar_etapa <- function(e) {
  if (e$atual == 0L) return(invisible(NULL))
  et <- e$etapas[[e$atual]]
  if (identical(et$status, "em andamento")) et$status <- "ok"
  et$fim <- fim_da_etapa(e, et)
  e$etapas[[e$atual]] <- et
  registrar_arquivos_etapa(e, e$atual)
  e$atual <- 0L
  invisible(NULL)
}

# Fecha o log (hora de término e duração). Chamada no fim do main.
finalizar_log <- function() {
  e <- log_estado(criar = FALSE)
  if (is.null(e) || !isTRUE(e$ativo) || isTRUE(e$finalizado)) return(invisible(NULL))
  fechar_etapa(e)
  e$fim <- Sys.time()
  e$finalizado <- TRUE
  suppressWarnings(try(removeTaskCallback("sdm_log"), silent = TRUE))
  escrever_log(e, final = TRUE)
  if (!is.null(e$destino)) message(">> Log salvo em: ", e$destino)
  invisible(NULL)
}


# ---- arquivos gerados -----------------------------------------------------------

pasta_base_log <- function() {
  p <- get0("pasta_projeto", envir = globalenv(), inherits = FALSE)
  if (is.null(p) || length(p) != 1 || is.na(p) || !nzchar(p) || !dir.exists(p)) return(NULL)
  p
}

# arquivos da pasta do projeto alterados desde 'desde' (sem a pasta logs/)
listar_arquivos_novos <- function(base, desde) {
  arq <- list.files(base, recursive = TRUE, full.names = TRUE)
  if (length(arq) == 0) return(NULL)
  rel <- substring(arq, nchar(base) + 2L)
  manter <- !grepl("^logs/", rel)
  arq <- arq[manter]
  rel <- rel[manter]
  if (length(arq) == 0) return(NULL)
  info <- file.info(arq)
  ok <- !is.na(info$mtime) & info$mtime >= (desde - 1) & !info$isdir
  if (!any(ok)) return(NULL)
  data.frame(arquivo = rel[ok], tamanho_kb = round(info$size[ok] / 1024, 1),
             modificado = info$mtime[ok], stringsAsFactors = FALSE)
}

# guarda os arquivos que pertencem à etapa 'i' (alterados entre o início e o fim dela)
registrar_arquivos_etapa <- function(e, i) {
  base <- pasta_base_log()
  if (is.null(base)) return(invisible(NULL))
  et  <- e$etapas[[i]]
  fim <- if (is.null(et$fim)) Sys.time() else et$fim
  todos <- listar_arquivos_novos(base, et$inicio)
  e$ultima_listagem <- Sys.time()

  if (!is.null(e$arquivos)) e$arquivos <- e$arquivos[e$arquivos$etapa_idx != i, , drop = FALSE]
  if (is.null(todos)) return(invisible(NULL))
  meus <- todos[todos$modificado <= (fim + 1), , drop = FALSE]
  if (nrow(meus) == 0) return(invisible(NULL))
  novos <- data.frame(etapa_idx = i, etapa = et$nome, detalhe = et$detalhe, meus,
                      stringsAsFactors = FALSE)
  e$arquivos <- if (is.null(e$arquivos)) novos else rbind(e$arquivos, novos)
  invisible(NULL)
}


# ---- formatação -------------------------------------------------------------------

formatar_hora <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  format(x, "%Y-%m-%d %H:%M:%S")
}

formatar_duracao <- function(segundos) {
  if (is.null(segundos) || length(segundos) == 0 || is.na(segundos)) return("-")
  s <- max(0, round(segundos))
  sprintf("%02d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
}

duracao_etapa <- function(et, e) {
  fim <- et$fim
  if (is.null(fim)) fim <- max(et$inicio, e$ultima_atividade)
  as.numeric(difftime(fim, et$inicio, units = "secs"))
}

# valor de um parâmetro como código R
valor_como_codigo <- function(valor) {
  paste(deparse(valor, width.cutoff = 100L,
                control = c("keepNA", "keepInteger", "niceNames", "showAttributes")),
        collapse = "\n")
}

# valor resumido (uma linha) para o log de leitura
valor_resumido <- function(valor, limite = 300L) {
  txt <- gsub("\\s+", " ", valor_como_codigo(valor))
  if (nchar(txt) > limite) paste0(substr(txt, 1, limite), " ...") else txt
}


# ---- parâmetros da seção 2 -------------------------------------------------------

# linhas do main.R e posição da seção 2 (da linha do título até a linha antes do título seguinte)
localizar_secao_parametros <- function(linhas) {
  ini <- grep("^# 2\\. ", linhas)
  if (length(ini) == 0) return(NULL)
  ini <- ini[1]
  titulos <- grep("^# [0-9]+\\. ", linhas)
  fim <- titulos[titulos > ini]
  fim <- if (length(fim) == 0) length(linhas) + 1L else fim[1]
  c(ini = ini, fim = fim)
}

# nomes dos parâmetros atribuídos na seção 2, na ordem em que aparecem
nomes_parametros_main <- function(linhas, pos) {
  if (is.null(pos) || pos[["fim"]] - pos[["ini"]] < 2) return(character(0))
  expressoes <- tryCatch(parse(text = linhas[(pos[["ini"]] + 1L):(pos[["fim"]] - 1L)], keep.source = FALSE),
                         error = function(err) NULL)
  if (is.null(expressoes)) return(character(0))
  nomes <- vapply(expressoes, function(x) {
    if (is.call(x) && as.character(x[[1]])[1] %in% c("<-", "=") && is.name(x[[2]])) {
      as.character(x[[2]]) } else NA_character_
  }, character(1))
  unique(nomes[!is.na(nomes)])
}

parametro_vazio_simples <- function(valor) {
  length(valor) == 0 || (length(valor) == 1 && (is.na(valor) || identical(valor, "") ||
                                                  identical(tolower(valor), "prompt")))
}

# escolhas feitas durante a execução que preenchem parâmetros deixados vazios no main
decisoes_da_sessao <- function() {
  g <- globalenv()
  obter <- function(nome) get0(nome, envir = g, inherits = FALSE)
  d <- list()

  entradas <- obter("entradas")
  if (is.list(entradas)) {
    if (is.character(entradas$var_preditoras)) d$caminho_var_preditoras <- entradas$var_preditoras
    if (is.character(entradas$mascara))    d$caminho_mascara    <- entradas$mascara
    if (is.character(entradas$registros))  d$caminho_registros  <- entradas$registros
    if (is.character(entradas$cenarios) && length(entradas$cenarios) > 0) {
      d$caminhos_cenarios <- entradas$cenarios
    }
  }
  colunas <- obter("colunas_registros")
  if (is.list(colunas)) {
    if (!is.null(colunas$longitude)) d$coluna_longitude <- colunas$longitude
    if (!is.null(colunas$latitude))  d$coluna_latitude  <- colunas$latitude
    if (!is.null(colunas$especie))   d$coluna_especie   <- colunas$especie
  }
  metodos <- obter("metodos_escolhidos")
  if (is.character(metodos) && length(metodos) > 0) d$metodos_multicolinearidade <- metodos
  modelos <- obter("modelos_escolhidos")
  if (is.character(modelos) && length(modelos) > 0) d$modelos <- modelos

  rodadas <- obter("rodadas_modelos")
  if (is.list(rodadas) && length(rodadas) > 0) {
    tipos <- unique(vapply(rodadas, function(r) r$tipo, character(1)))
    d$preditores_modelos <- if (length(tipos) > 1) "ambos" else tipos
    for (r in rodadas) {
      if (identical(r$tipo, "variaveis")) d$variaveis_modelos <- r$preditores$nomes
    }
    rodadas_pca <- Filter(function(r) identical(r$tipo, "pca"), rodadas)
    if (length(rodadas_pca) == 1) {
      d$pca_modelos       <- rodadas_pca[[1]]$pca
      d$eixos_pca_modelos <- rodadas_pca[[1]]$preditores$nomes
    }
  }

  if (!is.null(obter("resposta_independentes"))) {
    d$dados_independentes <- isTRUE(obter("resposta_independentes"))
    arq <- obter("arquivo_independente")
    if (is.character(arq) && length(arq) == 1) d$caminho_independente <- arq
  }
  d
}

# valor que o parâmetro teve de fato na execução: lista(valor, origem)
valor_usado <- function(nome, e, decisoes) {
  if (nome %in% names(e$respostas)) {
    return(list(valor = e$respostas[[nome]], origem = "respondido durante a execução"))
  }
  g <- globalenv()
  if (nome %in% names(e$pendentes)) {
    original <- e$pendentes[[nome]]
    if (!is.null(decisoes[[nome]]) && parametro_vazio_simples(original)) {
      return(list(valor = decisoes[[nome]], origem = "vazio no main; escolhido durante a execução"))
    }
    return(list(valor = original, origem = "vazio no main; não foi perguntado nesta execução"))
  }
  if (exists(nome, envir = g, inherits = FALSE)) {
    valor <- get(nome, envir = g, inherits = FALSE)
    if (!is.null(decisoes[[nome]]) && parametro_vazio_simples(valor)) {
      return(list(valor = decisoes[[nome]], origem = "vazio no main; escolhido durante a execução"))
    }
    return(list(valor = valor, origem = ""))
  }
  if (!is.null(decisoes[[nome]])) {
    return(list(valor = decisoes[[nome]], origem = "escolhido durante a execução"))
  }
  NULL
}


# ---- gravação do log ----------------------------------------------------------------

escrever_log <- function(e, final = FALSE) {
  tryCatch(escrever_log_interno(e, final), error = function(err) {
    message("Aviso: não consegui atualizar o log (", conditionMessage(err), ").")
    invisible(NULL)
  })
}

escrever_log_interno <- function(e, final = FALSE) {

  base <- pasta_base_log()
  if (is.null(base)) return(invisible(NULL))
  agora <- Sys.time()
  e$ultimo_flush <- agora

  # arquivos da etapa em andamento (com intervalo mínimo, porque listar a pasta custa)
  if (e$atual > 0L && as.numeric(difftime(agora, e$ultima_listagem, units = "secs")) >= 30) {
    registrar_arquivos_etapa(e, e$atual)
  }

  # seções do main.R: o log só é "completo" se todas rodaram
  arquivo_main <- file.path(base, "main.R")
  linhas_main  <- if (file.exists(arquivo_main)) readLines(arquivo_main, encoding = "UTF-8", warn = FALSE) else character(0)
  titulos      <- grep("^\\s*#\\s+[0-9]+\\.\\s.*-{3,}\\s*$", linhas_main, value = TRUE)
  esperadas    <- unique(as.integer(sub("^\\s*#\\s+([0-9]+)\\..*$", "\\1", titulos)))
  executadas   <- unique(suppressWarnings(as.integer(sub("^([0-9]+)\\..*$", "\\1",
                    vapply(e$etapas, function(et) et$nome, character(1))))))
  executadas   <- executadas[!is.na(executadas)]
  faltam       <- setdiff(esperadas, executadas)
  completo     <- isTRUE(e$finalizado) && length(esperadas) > 0 && length(faltam) == 0 &&
    !any(vapply(e$etapas, function(et) identical(et$status, "erro"), logical(1)))

  destino <- if (completo) file.path(base, "logs") else file.path(base, "logs", "temporarios")
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(e$destino) && !identical(e$destino, destino)) {
    antigos <- list.files(e$destino, pattern = paste0("(^|_)", e$id, "(_|\\.)"), full.names = TRUE)
    unlink(antigos)
  }
  e$destino <- destino
  prefixo <- file.path(destino, paste0("log_", e$id))

  fim_total <- if (!is.null(e$fim)) e$fim else max(e$ultima_atividade, e$inicio)
  duracao   <- as.numeric(difftime(fim_total, e$inicio, units = "secs"))

  # ---------- parâmetros ----------
  pos    <- localizar_secao_parametros(linhas_main)
  nomes  <- nomes_parametros_main(linhas_main, pos)
  decisoes <- decisoes_da_sessao()
  usados <- list()
  for (nome in nomes) {
    u <- valor_usado(nome, e, decisoes)
    if (!is.null(u)) usados[[nome]] <- u
  }

  # ---------- tabela de etapas ----------
  tab_etapas <- if (length(e$etapas) > 0) {
    do.call(rbind, lapply(seq_along(e$etapas), function(i) {
      et <- e$etapas[[i]]
      n_arq <- if (is.null(e$arquivos)) 0L else sum(e$arquivos$etapa_idx == i)
      data.frame(etapa = et$nome, detalhe = et$detalhe,
                 inicio = formatar_hora(et$inicio),
                 fim = formatar_hora(if (is.null(et$fim)) max(et$inicio, e$ultima_atividade) else et$fim),
                 duracao_s = round(duracao_etapa(et, e), 1),
                 duracao = formatar_duracao(duracao_etapa(et, e)),
                 situacao = if (identical(et$status, "em andamento") && !is.null(e$fim)) "interrompida" else et$status,
                 arquivos_gerados = n_arq, mensagem = et$mensagem, stringsAsFactors = FALSE)
    }))
  } else NULL

  # ---------- texto ----------
  g <- globalenv()
  obter <- function(nome) get0(nome, envir = g, inherits = FALSE)
  L <- character(0)
  add <- function(...) L <<- c(L, paste0(...))
  linha <- strrep("=", 78)

  add(linha); add("LOG DA EXECUÇÃO - SDM"); add(linha)
  add("Situação        : ", if (completo) "COMPLETO (todas as seções do main rodaram)" else
        paste0("PARCIAL / TEMPORÁRIO", if (!isTRUE(e$finalizado)) " (execução em andamento ou interrompida)" else ""))
  if (!completo && length(esperadas) > 0) {
    add("Seções rodadas  : ", paste(sort(executadas), collapse = ", "))
    if (length(faltam) > 0) add("Seções não rodadas: ", paste(sort(faltam), collapse = ", "))
  }
  add("Início          : ", formatar_hora(e$inicio))
  add("Término         : ", formatar_hora(fim_total), if (!isTRUE(e$finalizado)) "  (até o último comando executado)" else "")
  add("Duração total   : ", formatar_duracao(duracao))
  add("")
  add("Pasta do projeto: ", base)
  add("R               : ", R.version.string, " | ", R.version$platform)
  add("Sistema         : ", Sys.info()[["sysname"]], " ", Sys.info()[["release"]])

  pacotes_usados <- obter("pacotes")
  if (is.character(pacotes_usados) && length(pacotes_usados) > 0) {
    versoes <- vapply(pacotes_usados, function(p) {
      tryCatch(as.character(utils::packageVersion(p)), error = function(err) "não instalado")
    }, character(1))
    add("Pacotes         : ", paste0(pacotes_usados, " ", versoes, collapse = ", "))
  }

  add(""); add(linha); add("ETAPAS (tempo de cada seção)"); add(linha)
  if (!is.null(tab_etapas)) {
    for (i in seq_len(nrow(tab_etapas))) {
      t <- tab_etapas[i, ]
      add(formatC(t$etapa, width = -48), " ", t$duracao, "  ", substring(t$inicio, 12), " -> ",
          substring(t$fim, 12), "  [", t$situacao, "]",
          if (nzchar(t$detalhe)) paste0("  (", t$detalhe, ")") else "",
          if (t$arquivos_gerados > 0) paste0("  ", t$arquivos_gerados, " arquivo(s)") else "",
          if (nzchar(t$mensagem)) paste0("\n    ERRO: ", t$mensagem) else "")
    }
  } else add("(nenhuma etapa registrada)")

  add(""); add(linha); add("PARÂMETROS USADOS (seção 2)"); add(linha)
  add("Valores que valeram nesta execução. Os marcados ficaram vazios no main.")
  for (nome in names(usados)) {
    u <- usados[[nome]]
    add(formatC(nome, width = -26), " = ", valor_resumido(u$valor),
        if (nzchar(u$origem)) paste0("   <- ", u$origem) else "")
  }

  add(""); add(linha); add("ENTRADAS E ESCOLHAS"); add(linha)
  entradas <- obter("entradas")
  if (is.list(entradas)) {
    add("Variáveis preditoras : ", paste(entradas$var_preditoras, collapse = "; "))
    add("Máscara          : ", paste(entradas$mascara, collapse = "; "))
    add("Registros        : ", paste(entradas$registros, collapse = "; "))
    if (is.list(entradas$formato_registros)) {
      add("Formato registros: separador de colunas '", entradas$formato_registros$sep,
          "', decimal '", entradas$formato_registros$dec, "'")
    }
    if (length(entradas$cenarios) > 0) {
      add("Cenários         : ", paste0(names(entradas$cenarios), " = ", entradas$cenarios, collapse = "; "))
    } else add("Cenários         : nenhum")
  } else add("(entradas ainda não montadas nesta sessão)")
  colunas <- obter("colunas_registros")
  if (is.list(colunas)) {
    add("Colunas dos registros: longitude = ", colunas$longitude, " | latitude = ", colunas$latitude,
        " | espécie = ", if (is.null(colunas$especie) || is.na(colunas$especie)) "(espécie única)" else colunas$especie)
  }
  metodos <- obter("metodos_escolhidos")
  if (!is.null(metodos)) {
    add("Métodos de multicolinearidade: ", if (length(metodos) == 0) "nenhum" else paste(metodos, collapse = ", "))
  }
  vf <- obter("variaveis_finais")
  if (!is.null(vf) && length(vf) > 0) add("Variáveis finais (", length(vf), "): ", paste(vf, collapse = ", "))
  me <- obter("modelos_escolhidos")
  if (!is.null(me)) add("Modelos: ", paste(me, collapse = ", "))
  rodadas <- obter("rodadas_modelos")
  if (is.list(rodadas) && length(rodadas) > 0) {
    add("Rodadas de preditores:")
    for (r in rodadas) {
      add("  - ", r$nome, " (", r$tipo, if (!is.null(r$pca) && !is.na(r$pca)) paste0(", ", r$pca) else "",
          "): ", length(r$preditores$nomes), " preditores: ", paste(r$preditores$nomes, collapse = ", "))
      add("      conjuntos: ", paste(r$preditores$conjuntos, collapse = ", "), " | saída: ", r$pasta_modelos)
    }
  }

  add(""); add(linha); add("DADOS"); add(linha)
  reg <- obter("registros_limpos")
  if (is.list(reg) && is.data.frame(reg$registros)) {
    r <- reg$registros
    if ("especie" %in% names(r)) {
      tab <- table(r$especie)
      add("Registros limpos: ", nrow(r), " (", length(tab), " espécie(s))")
      for (i in seq_along(tab)) add("  ", names(tab)[i], ": ", tab[[i]])
    } else add("Registros limpos: ", nrow(r))
  } else add("(registros limpos ainda não gerados nesta sessão)")
  pts <- obter("pontos")
  if (is.data.frame(pts) && all(c("presenca") %in% names(pts))) {
    esp <- if ("especie" %in% names(pts)) pts$especie else rep("(única)", nrow(pts))
    tab <- table(esp, pts$presenca)
    add("Pontos (presença / background) por espécie:")
    for (i in seq_len(nrow(tab))) {
      add("  ", rownames(tab)[i], ": ", if ("1" %in% colnames(tab)) tab[i, "1"] else 0, " / ",
          if ("0" %in% colnames(tab)) tab[i, "0"] else 0)
    }
  }

  add(""); add(linha); add("SAÍDAS POR ETAPA (pastas com arquivos criados ou alterados)"); add(linha)
  if (!is.null(e$arquivos) && nrow(e$arquivos) > 0) {
    for (i in sort(unique(e$arquivos$etapa_idx))) {
      sub <- e$arquivos[e$arquivos$etapa_idx == i, , drop = FALSE]
      pastas <- dirname(sub$arquivo)
      resumo <- sort(table(pastas), decreasing = TRUE)
      add(sub$etapa[1], if (nzchar(sub$detalhe[1])) paste0(" (", sub$detalhe[1], ")") else "",
          ": ", nrow(sub), " arquivo(s)")
      mostrar <- utils::head(resumo, 8)
      for (k in seq_along(mostrar)) add("    ", names(mostrar)[k], "  (", mostrar[[k]], ")")
      if (length(resumo) > 8) add("    ... e mais ", length(resumo) - 8, " pasta(s)")
    }
    add("A lista completa está em: log_", e$id, "_arquivos.csv")
  } else add("(nenhum arquivo registrado)")

  add(""); add(linha); add("COMO REPETIR ESTA ANÁLISE"); add(linha)
  add("1) Abra main_", e$id, ".R (nesta pasta): é o main com a seção 2 trocada pelos valores acima.")
  add("2) Copie-o para a pasta do projeto, se quiser rodá-lo de lá, e rode as seções.")
  add("3) Para repetir mudando um parâmetro, altere o valor dele na seção 2 desse arquivo.")
  add("Os valores marcados como 'escolhido durante a execução' já vêm preenchidos, então ")
  add("o script não pergunta de novo. Apague o valor (NA, \"\" ou c()) para ser perguntado.")

  # ---------- arquivos ----------
  gravar_texto <- function(arquivo, linhas) {
    con <- file(arquivo, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(enc2utf8(linhas), con)
  }
  gravar_texto(paste0(prefixo, ".txt"), L)

  if (!is.null(tab_etapas)) {
    utils::write.csv(tab_etapas, paste0(prefixo, "_etapas.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(e$arquivos) && nrow(e$arquivos) > 0) {
    arq <- e$arquivos
    arq$modificado <- format(arq$modificado, "%Y-%m-%d %H:%M:%S")
    utils::write.csv(arq[, c("etapa", "detalhe", "arquivo", "tamanho_kb", "modificado")],
                     paste0(prefixo, "_arquivos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

  # parâmetros como código R e main reproduzível
  atribuicoes <- character(0)
  for (nome in names(usados)) {
    u <- usados[[nome]]
    atribuicoes <- c(atribuicoes,
                     paste0(nome, " <- ", valor_como_codigo(u$valor),
                            if (nzchar(u$origem)) paste0("   # ", u$origem) else ""))
  }
  cabecalho <- c(paste0("# Parâmetros usados na execução de ", formatar_hora(e$inicio), " (log_", e$id, ")"),
                 "# Gerado automaticamente pelo log. Para repetir a análise, use main_<id>.R.", "")
  gravar_texto(paste0(prefixo, "_parametros.R"), c(cabecalho, atribuicoes))

  if (!is.null(pos) && length(linhas_main) > 0) {
    gancho <- grep("^iniciar_etapa\\(", linhas_main[(pos[["ini"]] + 1L):min(pos[["ini"]] + 3L, length(linhas_main))], value = TRUE)
    secao <- c(linhas_main[pos[["ini"]]], gancho,
               paste0("# (Seção 2 gerada pelo log log_", e$id, ": valores USADOS na execução de ",
                      formatar_hora(e$inicio), ")"),
               "# Altere um valor para repetir a análise mudando aquele parâmetro.",
               "", atribuicoes, "",
               "# Parâmetros deixados vazios (NA, \"\" ou c()): pergunta o valor no primeiro uso",
               "registrar_parametros_pendentes()", "")
    novo_main <- c(linhas_main[seq_len(pos[["ini"]] - 1L)], secao,
                   if (pos[["fim"]] <= length(linhas_main)) linhas_main[pos[["fim"]]:length(linhas_main)])
    gravar_texto(file.path(destino, paste0("main_", e$id, ".R")), novo_main)
  }

  invisible(prefixo)
}
