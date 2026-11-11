# ==============================================================================
#  FUNÇÕES - Conferência e carregamento de pacotes
# ==============================================================================


# ------------------------------------------------------------------------------
# verificar_pacotes()
#   Recebe um vetor com nomes de pacotes e, para cada um:
#     - se NÃO estiver instalado, instala do CRAN;
#     - em seguida, carrega o pacote com library().
# ------------------------------------------------------------------------------
verificar_pacotes <- function(pacotes) {

  faltando <- pacotes[!sapply(pacotes, requireNamespace, quietly = TRUE)]

  if (length(faltando) > 0) {
    message("Instalando pacotes que faltam: ", paste(faltando, collapse = ", "))
    install.packages(faltando, repos = "https://cloud.r-project.org")
  }

  for (pacote in pacotes) {
    suppressPackageStartupMessages(library(pacote, character.only = TRUE))
  }

  message("Pacotes carregados: ", paste(pacotes, collapse = ", "))
  invisible(pacotes)
}
