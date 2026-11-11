# ==============================================================================
#  FUNÇÕES - Consolidação e escolha final das variáveis
# ------------------------------------------------------------------------------
#  Junta o que cada método sugeriu em uma única tabela e deixa o usuário
#  decidir quais variáveis ficam (pergunta no console ou pelo parâmetro
#  'variaveis_mantidas' do main).
# ==============================================================================


# ------------------------------------------------------------------------------
# consolidar_sugestoes()
#   Monta a tabela: uma linha por variável, uma coluna por método.
#     - métodos sem resposta (rodam por conjunto): 1 = sugerida, 0 = não
#       (PCA: nota de importância de 0 a 1, sem voto);
#       a tabela é feita nas PREDITORAS.
#     - métodos com resposta (rodam por espécie): fração das espécies em que
#       a variável foi mantida (0 a 1). Métodos que só ranqueiam (ridge, PCR)
#       ficam vazios (NA) e não entram na votação.
#     - 'em_todos_cenarios': TRUE se a variável existe em todos os cenários
#       (se FALSE, ela não pode ser projetada em algum cenário).
#   'votos' = soma das colunas dos métodos que selecionam variáveis.
# ------------------------------------------------------------------------------
consolidar_sugestoes <- function(variaveis, resultados_sem, resultados_com, dados_testes = NULL) {

  tabela <- data.frame(variavel = variaveis, stringsAsFactors = FALSE)
  colunas_voto <- character(0)

  # ---- métodos sem resposta ----
  conjuntos <- names(resultados_sem)
  metodos_sem <- unique(unlist(lapply(resultados_sem, names)))
  for (m in metodos_sem) {
    base <- if ("preditoras" %in% conjuntos) "preditoras" else conjuntos[1]
    r <- resultados_sem[[base]][[m]]
    if (isFALSE(r$seleciona_variaveis)) {
      # só ranqueia (PCA): nota de 0 a 1, sem voto
      tabela[[m]] <- r$importancia$importancia[match(variaveis, r$importancia$variavel)]
    } else {
      tabela[[m]] <- as.numeric(variaveis %in% r$mantidas)
      colunas_voto <- c(colunas_voto, m)
    }
  }

  # ---- métodos com resposta ----
  especies <- names(resultados_com)
  metodos_com <- unique(unlist(lapply(resultados_com, names)))
  for (m in metodos_com) {
    com_metodo <- Filter(function(r) !is.null(r[[m]]), resultados_com)
    if (length(com_metodo) == 0) next
    seleciona <- com_metodo[[1]][[m]]$seleciona_variaveis
    fracao <- sapply(variaveis, function(v)
      mean(sapply(com_metodo, function(r) v %in% r[[m]]$mantidas)))
    # métodos que só ranqueiam (ridge, PCR) não selecionam: coluna vazia (NA)
    tabela[[m]] <- if (isTRUE(seleciona)) arredondar_sdm(as.numeric(fracao)) else NA_real_
    if (isTRUE(seleciona)) colunas_voto <- c(colunas_voto, m)
  }

  tabela$votos <- if (length(colunas_voto) > 0)
    arredondar_sdm(rowSums(tabela[, colunas_voto, drop = FALSE])) else 0

  # ---- a variável existe em todos os cenários? (só assim pode ser projetada) ----
  cenarios <- setdiff(names(dados_testes), "preditoras")
  if (length(cenarios) > 0) {
    tabela$em_todos_cenarios <- sapply(variaveis, function(v)
      all(sapply(cenarios, function(cn) v %in% colnames(dados_testes[[cn]]$dados))))
  }

  tabela[order(-tabela$votos, tabela$variavel), ]
}


# ------------------------------------------------------------------------------
# escolher_variaveis_finais()
#   - Se 'variaveis_mantidas' foi preenchido no main, confere os nomes e usa.
#   - Se estiver vazio, mostra a tabela e pergunta quais variáveis ficam.
#   Salva em pasta_saida:  sugestoes_consolidadas.csv  e  variaveis_finais.csv
#   Devolve o vetor de nomes das variáveis finais.
# ------------------------------------------------------------------------------
escolher_variaveis_finais <- function(tabela, variaveis_mantidas = c(), pasta_saida) {

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tabela, file.path(pasta_saida, "sugestoes_consolidadas.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")

  cat("\n================ SUGESTÕES DOS MÉTODOS ================\n")
  cat("(sem resposta: 1 = sugerida | com resposta: fração das espécies)\n\n")
  print(tabela, row.names = FALSE)
  cat("=======================================================\n")

  if (length(variaveis_mantidas) == 0) {

    if (!interactive()) {
      stop("Não é possível perguntar fora de uma sessão interativa. ",
           "Preencha 'variaveis_mantidas' no main.", call. = FALSE)
    }

    cat("\nEscolha as variáveis que ficam para a modelagem:\n")
    escolhidos <- perguntar_opcoes(
      c(paste0(tabela$variavel, "  (votos: ", round(tabela$votos, 2), ")"), "all (todas as variáveis)"),
      "Número(s) da(s) variável(is) (ex.: 1,2,5-8): ",
      "Quais variáveis ficam para a modelagem?")
    finais <- if ((nrow(tabela) + 1) %in% escolhidos) tabela$variavel else tabela$variavel[escolhidos]

  } else {
    finais   <- trimws(variaveis_mantidas)
    if ("all" %in% tolower(finais)) finais <- tabela$variavel
    invalidas <- setdiff(finais, tabela$variavel)
    if (length(invalidas) > 0) {
      stop("Variável(is) não encontrada(s): ", paste(invalidas, collapse = ", "),
           "\nDisponíveis: ", paste(tabela$variavel, collapse = ", "), call. = FALSE)
    }
  }

  utils::write.csv(data.frame(variavel = finais), file.path(pasta_saida, "variaveis_finais.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")

  cat("\nVariáveis finais (", length(finais), "):\n", sep = "")
  cat(paste(finais, collapse = ", "), "\n\n")
  cat("Para repetir esta escolha sem perguntas, copie no main:\n")
  cat("variaveis_mantidas <- c(", paste0('"', finais, '"', collapse = ", "), ")\n\n", sep = "")

  return(finais)
}


# ------------------------------------------------------------------------------
# checar_conjunto_final()
#   Confere se as variáveis finais ainda têm multicolinearidade forte, em cada
#   conjunto (preditoras e cenários), usando a amostra da área da seção 10:
#     - VIF de cada variável calculado só com as variáveis finais;
#     - maior |r| de cada variável com as outras variáveis finais, e com qual.
#   Marca "ATENÇÃO" quando o VIF passa de 'limite_vif' ou o |r| passa de
#   'limiar_r'. Variáveis finais que não existem em um cenário são avisadas
#   (elas não poderão ser projetadas nele).
#   Salva checagem_conjunto_final.csv em 'pasta_saida' e devolve a tabela.
# ------------------------------------------------------------------------------
checar_conjunto_final <- function(variaveis, dados_testes, pasta_saida,
                                  limite_vif = 10, limiar_r = 0.7) {

  resultado <- list()

  cat("\n================ CHECAGEM DO CONJUNTO FINAL ================\n")
  cat("Marca ATENÇÃO quando VIF >", limite_vif, "ou |r| >", limiar_r, "\n")

  for (conjunto in names(dados_testes)) {
    dados     <- dados_testes[[conjunto]]$dados
    presentes <- intersect(variaveis, colnames(dados))
    faltam    <- setdiff(variaveis, presentes)

    cat("\n--", conjunto, "--\n")
    if (length(faltam) > 0) {
      cat("Não existem neste conjunto:", paste(faltam, collapse = ", "), "\n")
    }
    if (length(presentes) < 2) {
      cat("Menos de 2 variáveis finais neste conjunto: não há o que verificar.\n")
      next
    }

    sub <- dados[, presentes, drop = FALSE]
    vif <- calcular_vif(sub)
    R   <- abs(stats::cor(sub))
    diag(R) <- 0

    tabela <- data.frame(
      conjunto = conjunto,
      variavel = presentes,
      vif      = arredondar_sdm(as.numeric(vif[presentes])),
      maior_r  = arredondar_sdm(apply(R, 1, max)),
      com      = colnames(R)[apply(R, 1, which.max)],
      stringsAsFactors = FALSE
    )
    tabela$situacao <- ifelse(tabela$vif > limite_vif | tabela$maior_r > limiar_r,
                              "ATENÇÃO", "ok")

    print(tabela[, -1], row.names = FALSE)
    i <- which.max(tabela$maior_r)
    cat("VIF máximo:", max(tabela$vif), "| maior |r|:", tabela$maior_r[i],
        paste0("(", tabela$variavel[i], " x ", tabela$com[i], ")"), "\n")

    resultado[[conjunto]] <- tabela
  }

  resultado <- do.call(rbind, resultado)
  rownames(resultado) <- NULL

  if (!is.null(resultado)) {
    utils::write.csv(resultado, file.path(pasta_saida, "checagem_conjunto_final.csv"),
                     row.names = FALSE, fileEncoding = "UTF-8")
    if (any(resultado$situacao == "ATENÇÃO")) {
      cat("\nHá variáveis com ATENÇÃO: considere tirar uma de cada par muito correlacionado,\n",
          "ajustar 'variaveis_mantidas' na seção 2 e rodar esta seção de novo.\n", sep = "")
    } else {
      cat("\nNenhuma variável passou dos limites.\n")
    }
  }
  cat("============================================================\n")

  return(invisible(resultado))
}
