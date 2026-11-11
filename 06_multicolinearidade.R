# ==============================================================================
#  FUNÇÕES - Escolha dos métodos para tratar a multicolinearidade
# ------------------------------------------------------------------------------
#  Esta etapa REGISTRA quais métodos o usuário quer usar.
#  Os métodos são executados nos arquivos 10 (sem resposta) e 11 (com resposta).
# ==============================================================================


# ------------------------------------------------------------------------------
# listar_metodos_multicolinearidade()
#   Catálogo dos métodos disponíveis. 'precisa_resposta' indica se o método
#   precisa de uma variável resposta (presença/ausência ou background),
#   que será criada em uma etapa posterior.
# ------------------------------------------------------------------------------
listar_metodos_multicolinearidade <- function() {
  data.frame(
    codigo = c("vif", "filtro_correlacao", "cluster", "pca",
               "lasso", "ridge", "elastic_net", "pcr", "plsr", "cppls", "boruta"),
    nome = c("VIF (fator de inflação da variância)",
             "Filtro por correlação entre pares",
             "Análise de cluster das variáveis",
             "PCA (análise de componentes principais)",
             "Lasso",
             "Ridge",
             "Elastic net",
             "PCR (regressão por componentes principais)",
             "PLSR (regressão por mínimos quadrados parciais)",
             "CPPLS (PLS com componentes canônicos)",
             "Boruta (importância por random forest)"),
    precisa_resposta = c(FALSE, FALSE, FALSE, FALSE,
                         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# escolher_metodos_multicolinearidade()
#   - Se 'metodos' foi informado no main, confere se os códigos existem.
#   - Se estiver vazio, mostra a lista e pergunta no console quais usar.
#   Devolve um vetor com os códigos escolhidos (vazio = nenhum método).
# ------------------------------------------------------------------------------
escolher_metodos_multicolinearidade <- function(metodos = c()) {

  catalogo <- listar_metodos_multicolinearidade()

  if (length(metodos) == 0) {

    if (!interactive()) {
      stop("Não é possível perguntar fora de uma sessão interativa. ",
           "Preencha 'metodos_multicolinearidade' no main.", call. = FALSE)
    }

    cat("\nMétodos para tratar a multicolinearidade:\n")
    opcoes <- paste0(catalogo$nome,
                     ifelse(catalogo$precisa_resposta, "  (*)", ""))
    opcoes <- c(opcoes, "Nenhum método (manter todas as variáveis)", "all (todos os métodos)")
    cat("(*) precisa de variável resposta (presença/background)\n")

    escolhidos <- perguntar_opcoes(opcoes, "Número(s) do(s) método(s): ",
                                   "Métodos para tratar a multicolinearidade\n(*) precisa de variável resposta (presença/background)")

    if ((nrow(catalogo) + 2) %in% escolhidos) {
      metodos <- catalogo$codigo
    } else if ((nrow(catalogo) + 1) %in% escolhidos) {
      if (length(escolhidos) > 1) message("'Nenhum método' foi marcado: os demais serão ignorados.")
      metodos <- character(0)
    } else {
      metodos <- catalogo$codigo[escolhidos]
    }

  } else {
    metodos <- tolower(trimws(metodos))
    if ("all" %in% metodos) metodos <- catalogo$codigo
    invalidos <- setdiff(metodos, catalogo$codigo)
    if (length(invalidos) > 0) {
      stop("Método(s) inválido(s): ", paste(invalidos, collapse = ", "),
           "\nOpções: ", paste(catalogo$codigo, collapse = ", "), call. = FALSE)
    }
  }

  # ---- resumo ----
  cat("\n================ MÉTODOS ESCOLHIDOS ================\n")
  if (length(metodos) == 0) {
    cat("Nenhum: todas as variáveis serão mantidas.\n")
  } else {
    for (codigo in metodos) {
      linha <- catalogo[catalogo$codigo == codigo, ]
      cat("-", linha$nome, if (linha$precisa_resposta) "(precisa de resposta)", "\n")
    }
    if (any(catalogo$precisa_resposta[catalogo$codigo %in% metodos])) {
      cat("\nOs métodos marcados com 'precisa de resposta' só poderão ser\n",
          "executados depois que os registros e o background forem preparados.\n", sep = "")
    }
  }
  cat("====================================================\n\n")

  return(metodos)
}
