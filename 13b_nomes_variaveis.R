# ==============================================================================
#  FUNÇÕES - Variáveis com NOMES diferentes entre preditoras e cenários
# ------------------------------------------------------------------------------
#  A PCA "comum" usa só as variáveis que existem, com o MESMO nome, nas
#  preditoras e em todos os cenários. O nome vem do nome do arquivo
#  (ver nome_curto_variavel). Se os arquivos dos cenários tiverem outro padrão de
#  nome (ex.: "bio01_ssp585_2071-2100" x "bio01_1981-2010"), a PCA comum fica sem
#  variáveis. Quando isso acontece, o script avisa e deixa escolher:
#    "renomear" : propõe qual variável do cenário corresponde a cada preditora
#                 (nome igual, nome contido no outro ou, por último, mesma ordem),
#                 mostra a tabela e deixa aceitar, retirar um texto dos nomes do
#                 cenário (ex.: "_ssp585") ou desistir. Os ARQUIVOS NÃO são
#                 renomeados: a correspondência fica em
#                 verificacao/renomeio_variaveis.csv e vale para todas as etapas
#                 seguintes (inclusive em outra sessão).
#    "pular"    : a PCA comum não é feita (só a PCA das preditoras) e as análises
#                 que dependem dela não são feitas.
#  A escolha vem do parâmetro 'nomes_variaveis_diferentes' (seção 2 do main):
#  "renomear", "pular" ou "prompt" (o script pergunta).
# ==============================================================================

# correspondência arquivo -> nome da variável (guardada na sessão)
.sdm_renomeio <- new.env()
.sdm_renomeio$mapa <- character(0)


# ------------------------------------------------------------------------------
# carregar_renomeio() / salvar_renomeio()
#   Lê/grava verificacao/renomeio_variaveis.csv (colunas: conjunto, arquivo,
#   nome_variavel). 'arquivo' é o nome do .tif sem a extensão.
# ------------------------------------------------------------------------------
carregar_renomeio <- function(pasta_verificacao) {

  caminho <- file.path(pasta_verificacao, "renomeio_variaveis.csv")
  if (!file.exists(caminho)) return(invisible(FALSE))

  tabela <- tryCatch(utils::read.csv(caminho, stringsAsFactors = FALSE,
                                     fileEncoding = "UTF-8"),
                     error = function(e) NULL)
  if (is.null(tabela) || !all(c("arquivo", "nome_variavel") %in% names(tabela))) {
    return(invisible(FALSE))
  }
  .sdm_renomeio$mapa <- setNames(as.character(tabela$nome_variavel),
                                 as.character(tabela$arquivo))
  invisible(TRUE)
}

salvar_renomeio <- function(novos, pasta_verificacao) {
  # 'novos': data.frame(conjunto, arquivo, nome_variavel)
  dir.create(pasta_verificacao, recursive = TRUE, showWarnings = FALSE)
  caminho <- file.path(pasta_verificacao, "renomeio_variaveis.csv")

  anteriores <- if (file.exists(caminho)) {
    tryCatch(utils::read.csv(caminho, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
             error = function(e) NULL)
  } else NULL
  if (!is.null(anteriores) && all(c("conjunto", "arquivo", "nome_variavel") %in% names(anteriores))) {
    anteriores <- anteriores[!(paste(anteriores$conjunto, anteriores$arquivo) %in%
                                 paste(novos$conjunto, novos$arquivo)), , drop = FALSE]
    novos <- rbind(anteriores[, c("conjunto", "arquivo", "nome_variavel")], novos)
  }
  utils::write.csv(novos, caminho, row.names = FALSE, fileEncoding = "UTF-8")
  .sdm_renomeio$mapa <- setNames(as.character(novos$nome_variavel), as.character(novos$arquivo))
  invisible(caminho)
}


# ------------------------------------------------------------------------------
# chave_natural()
#   Chave para ordenar nomes com números "do jeito humano" (bio2 antes de bio10).
# ------------------------------------------------------------------------------
chave_natural <- function(x) {
  vapply(tolower(x), function(s) {
    m <- gregexpr("[0-9]+", s)
    regmatches(s, m) <- lapply(regmatches(s, m),
                               function(d) formatC(as.numeric(d), width = 12, flag = "0", format = "d"))
    s
  }, character(1), USE.NAMES = FALSE)
}


# ------------------------------------------------------------------------------
# nome_contido()
#   TRUE se o nome 'curto' aparece dentro de 'longo' sem ficar "colado" em outro
#   número (bio1 NÃO aparece em bio10, mas aparece em bio1_ssp585).
# ------------------------------------------------------------------------------
nome_contido <- function(curto, longo) {
  curto <- tolower(curto); longo <- tolower(longo)
  esc   <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", curto)
  antes  <- if (grepl("^[0-9]", curto)) "(?<![0-9])" else ""
  depois <- if (grepl("[0-9]$", curto)) "(?![0-9])" else ""
  grepl(paste0(antes, esc, depois), longo, perl = TRUE)
}


# ------------------------------------------------------------------------------
# parear_nomes()
#   Propõe qual variável do cenário ('cen') corresponde a cada preditora ('pred'):
#     1) nome igual; 2) um nome contido no outro (só se houver UMA candidata);
#     3) o que sobrar, na mesma ordem, se sobrarem quantidades iguais.
#   Devolve data.frame(cenario, preditora, regra); preditora = NA se sem par.
# ------------------------------------------------------------------------------
parear_nomes <- function(pred, cen) {

  tab <- data.frame(cenario = cen, preditora = NA_character_, regra = "sem par",
                    stringsAsFactors = FALSE)
  usa <- function(i, p, regra) { tab$preditora[i] <<- p; tab$regra[i] <<- regra }

  # 1) iguais
  for (p in pred) {
    i <- which(tab$cenario == p & is.na(tab$preditora))
    if (length(i) == 1) usa(i, p, "nome igual")
  }

  # 2) um contém o outro (repete até não achar mais nenhum par)
  repeat {
    achou <- FALSE
    livres_p <- setdiff(pred, tab$preditora)
    for (p in livres_p) {
      livres_c <- which(is.na(tab$preditora))
      cand <- livres_c[vapply(tab$cenario[livres_c], function(s) {
        nome_contido(p, s) || nome_contido(s, p)
      }, logical(1))]
      if (length(cand) == 1) { usa(cand, p, "nome contido"); achou <- TRUE }
    }
    if (!achou) break
  }

  # 3) mesma ordem
  livres_p <- setdiff(pred, tab$preditora)
  livres_c <- which(is.na(tab$preditora))
  if (length(livres_p) > 0 && length(livres_p) == length(livres_c)) {
    livres_p <- livres_p[order(chave_natural(livres_p))]
    livres_c <- livres_c[order(chave_natural(tab$cenario[livres_c]))]
    for (k in seq_along(livres_p)) usa(livres_c[k], livres_p[k], "mesma ordem (confira!)")
  }

  return(tab)
}


# ------------------------------------------------------------------------------
# pareamento_interativo()
#   Mostra a proposta de correspondência de UM cenário e pergunta:
#   aceitar / retirar um texto dos nomes do cenário / desistir (pular a PCA comum).
#   Devolve data.frame(cenario, preditora, regra) aceito, ou NULL se desistiu.
#   'cenario' são os nomes ORIGINAIS (antes de retirar textos).
# ------------------------------------------------------------------------------
pareamento_interativo <- function(pred, cen, nome_cenario) {

  retirar <- character(0)
  repeat {
    ajustado <- cen
    for (t in retirar) ajustado <- gsub(t, "", ajustado, fixed = TRUE)
    if (anyDuplicated(ajustado) > 0) {
      message("Aviso: depois de retirar o texto, há nomes repetidos no cenário. ",
              "Os repetidos não serão pareados; use outro texto.")
      ajustado[duplicated(ajustado) | duplicated(ajustado, fromLast = TRUE)] <- NA_character_
      ajustado <- ifelse(is.na(ajustado), paste0("<repetido ", seq_along(ajustado), ">"), ajustado)
    }

    tab <- parear_nomes(pred, ajustado)
    tab$cenario <- cen

    cat("\n---- Cenário '", nome_cenario, "': correspondência proposta ----\n", sep = "")
    if (length(retirar) > 0) cat("Texto(s) retirado(s) dos nomes:", paste0("\"", retirar, "\"", collapse = ", "), "\n")
    exibir <- data.frame(variavel_do_cenario = tab$cenario,
                         vira_a_preditora = ifelse(is.na(tab$preditora), "-", tab$preditora),
                         como = tab$regra, stringsAsFactors = FALSE)
    print(exibir, row.names = FALSE)
    sem_par_p <- setdiff(pred, tab$preditora)
    if (length(sem_par_p) > 0) {
      cat("Preditoras sem correspondente neste cenário:", paste(sem_par_p, collapse = ", "), "\n")
    }
    n_pares <- sum(!is.na(tab$preditora))

    r <- perguntar_opcoes(
      c("Aceitar esta correspondência",
        "Retirar um texto dos nomes do cenário (ex.: _ssp585) e refazer a proposta",
        "Desistir: pular a PCA comum"),
      "Número da opção: ", paste0("Cenário '", nome_cenario, "': como parear os nomes das variáveis?"))[1]

    if (r == 1) {
      if (n_pares < 2) {
        message("Só ", n_pares, " variável(is) pareada(s): a PCA precisa de pelo menos 2. ",
                "Tente retirar um texto dos nomes ou desista.")
        next
      }
      return(tab)
    }
    if (r == 2) {
      txt <- ler_entrada("Texto a retirar dos nomes do cenário (ENTER = nenhum): ",
                         contexto = "Texto que aparece nos nomes do cenário e não nas preditoras (ex.: _ssp585)",
                         titulo = "Texto a retirar dos nomes")
      txt <- trimws(txt)
      if (nzchar(txt)) retirar <- c(retirar, txt)
      next
    }
    return(NULL)
  }
}


# ------------------------------------------------------------------------------
# resolver_nomes_variaveis()
#   Chamada pela PCA quando faltam variáveis comuns. 'vars' = lista com os nomes
#   das variáveis de cada conjunto. Só age nos cenários que, ao mesmo tempo, não
#   têm variáveis das preditoras E têm variáveis que as preditoras não têm
#   (sinal de nomes diferentes). Devolve list(acao, vars):
#     acao = "seguir" (nada a resolver), "renomeado" ou "pular".
# ------------------------------------------------------------------------------
resolver_nomes_variaveis <- function(vars, variaveis_cortadas, pasta_verificacao) {

  preditoras <- vars[["preditoras"]]
  cenarios   <- setdiff(names(vars), "preditoras")
  suspeitos  <- cenarios[vapply(cenarios, function(cn) {
    length(setdiff(preditoras, vars[[cn]])) > 0 && length(setdiff(vars[[cn]], preditoras)) > 0
  }, logical(1))]

  if (length(suspeitos) == 0) return(list(acao = "seguir", vars = vars))

  cat("\n======= VARIÁVEIS COM NOMES DIFERENTES ENTRE PREDITORAS E CENÁRIOS =======\n")
  for (cn in suspeitos) {
    cat("Cenário '", cn, "':\n", sep = "")
    cat("  só nas preditoras:", paste(head(setdiff(preditoras, vars[[cn]]), 15), collapse = ", "), "\n")
    cat("  só no cenário    :", paste(head(setdiff(vars[[cn]], preditoras), 15), collapse = ", "), "\n")
  }
  cat("Sem nomes iguais, a PCA comum (a usada para projetar nos cenários) não é possível.\n")
  cat("==========================================================================\n\n")

  escolha <- ler_parametro("nomes_variaveis_diferentes")

  if (escolha == "pular") {
    return(list(acao = "pular", vars = vars))
  }

  if (!interactive()) {
    stop("Há variáveis com nomes diferentes entre preditoras e cenários e a correspondência ",
         "precisa ser conferida por você. Rode em sessão interativa ou use ",
         "'nomes_variaveis_diferentes' = \"pular\".", call. = FALSE)
  }

  novos <- NULL
  for (cn in suspeitos) {
    tab <- pareamento_interativo(preditoras, vars[[cn]], cn)
    if (is.null(tab)) return(list(acao = "pular", vars = vars))

    arquivos <- variaveis_cortadas[[cn]]
    brutos   <- nome_curto_bruto(arquivos)
    for (i in which(!is.na(tab$preditora) & tab$cenario != tab$preditora)) {
      k <- which(brutos == tab$cenario[i])
      if (length(k) == 0) next
      novos <- rbind(novos, data.frame(
        conjunto = cn,
        arquivo = tools::file_path_sans_ext(basename(arquivos[k[1]])),
        nome_variavel = tab$preditora[i], stringsAsFactors = FALSE))
    }
    novo_nome <- ifelse(is.na(tab$preditora), tab$cenario, tab$preditora)
    vars[[cn]] <- novo_nome[match(vars[[cn]], tab$cenario)]
  }

  if (!is.null(novos)) {
    salvar_renomeio(novos, pasta_verificacao)
    message("Correspondência salva em: ", file.path(pasta_verificacao, "renomeio_variaveis.csv"))
  }
  return(list(acao = "renomeado", vars = vars))
}


# ------------------------------------------------------------------------------
# aplicar_renomeio_vars()
#   Traduz os nomes de 'vars' (lista por conjunto) usando a correspondência já
#   salva (renomeio_variaveis.csv), para não perguntar de novo.
# ------------------------------------------------------------------------------
aplicar_renomeio_vars <- function(vars, variaveis_cortadas) {

  if (length(.sdm_renomeio$mapa) == 0) return(vars)
  for (cn in setdiff(names(vars), "preditoras")) {
    arquivos <- variaveis_cortadas[[cn]]
    if (is.null(arquivos)) next
    brutos  <- nome_curto_bruto(arquivos)
    mapeado <- nome_curto_variavel(arquivos)
    pos     <- match(vars[[cn]], brutos)
    vars[[cn]] <- ifelse(is.na(pos), vars[[cn]], mapeado[pos])
  }
  return(vars)
}
