# ==============================================================================
#  FUNÇÕES - PCA como transformação: eixos (componentes) como novas variáveis
# ------------------------------------------------------------------------------
#  Em vez de escolher variáveis, a PCA cria eixos independentes entre si.
#  Regras desta etapa:
#   - a PCA é ajustada SÓ nas variáveis PREDITORAS (amostra em z-score);
#   - os cenários são projetados nos MESMOS eixos (mesma média, desvio e
#     cargas das preditoras), para que presente e futuro fiquem comparáveis;
#   - se preditoras e cenários tiverem variáveis diferentes, é feita uma segunda
#     PCA só com as variáveis COMUNS a todos os conjuntos. As demais ficam de
#     fora. Essa PCA "comum" é a que serve para projetar nos cenários.
#
#  Resultado (pasta multicolinearidade/pca_eixos/<nome_da_pca>/):
#       <conjunto>/PC1.tif, PC2.tif, ...  -> rasters dos eixos
#       pca_variancia.csv, pca_cargas.csv, pca_importancia.csv
#       pca_modelo.rds                    -> variáveis, média, desvio e cargas
# ==============================================================================


# ------------------------------------------------------------------------------
# ajustar_pca_eixos()
#   Ajusta a PCA com as 'variaveis' escolhidas, usando a amostra das PREDITORAS.
#   Eixos: n_eixos > variancia_alvo > regra automática (ver regra_eixos_pca).
# ------------------------------------------------------------------------------
ajustar_pca_eixos <- function(dados_testes, variaveis, variancia_alvo = NULL, n_eixos = NULL) {

  dados <- dados_testes[["preditoras"]]$dados[, variaveis, drop = FALSE]
  pca   <- calcular_pca(dados, variancia_alvo, n_eixos)
  par   <- dados_testes[["preditoras"]]$parametros

  list(
    variaveis = variaveis,
    n_eixos   = pca$n_componentes,
    regra     = pca$regra,
    modelo    = pca$modelo,
    tabela    = pca$tabela,
    cargas    = pca$cargas,
    importancia = pca$importancia,
    media     = par$media[variaveis],
    desvio    = par$desvio[variaveis]
  )
}


# ------------------------------------------------------------------------------
# projetar_eixos_raster()
#   Calcula os rasters dos eixos para UM conjunto (preditoras ou cenário):
#       z_j  = (x_j - media_j) / desvio_j          (média e desvio das PREDITORAS)
#       PCk  = soma_j  ((z_j - centro_j) / escala_j) * carga_jk
#   'centro' e 'escala' são os da PCA ajustada nas preditoras (quase 0 e 1, porque
#   as preditoras já estão em z-score). NA continua NA.
#   Todos os rasters do conjunto precisam ter a mesma grade.
# ------------------------------------------------------------------------------
projetar_eixos_raster <- function(pca_eixos, arquivos, pasta_saida) {

  nomes <- nome_curto_variavel(basename(arquivos))
  faltam <- setdiff(pca_eixos$variaveis, nomes)
  if (length(faltam) > 0) {
    stop("Faltam no conjunto as variáveis: ", paste(faltam, collapse = ", "), call. = FALSE)
  }

  arquivos <- arquivos[match(pca_eixos$variaveis, nomes)]
  camadas  <- lapply(arquivos, terra::rast)
  names(camadas) <- pca_eixos$variaveis

  for (v in pca_eixos$variaveis[-1]) {
    ok <- terra::compareGeom(camadas[[1]], camadas[[v]], stopOnError = FALSE)
    if (!isTRUE(ok)) {
      stop("A variável '", v, "' não tem a mesma grade (extensão/resolução) das demais. ",
           "Não é possível combinar as camadas em eixos. Veja o relatório da seção 7.",
           call. = FALSE)
    }
  }

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  vars    <- pca_eixos$variaveis
  rotacao <- pca_eixos$modelo$rotation
  centro  <- pca_eixos$modelo$center[vars]
  escala  <- pca_eixos$modelo$scale
  escala  <- if (isFALSE(escala) || is.null(escala)) setNames(rep(1, length(vars)), vars) else escala[vars]
  media   <- pca_eixos$media[vars]
  desvio  <- pca_eixos$desvio[vars]
  gerados <- character(0)

  for (k in seq_len(pca_eixos$n_eixos)) {
    carga <- rotacao[vars, k]
    soma  <- NULL
    for (v in vars) {
      termo <- camadas[[v]] * (carga[[v]] / (desvio[[v]] * escala[[v]]))
      soma  <- if (is.null(soma)) termo else soma + termo
    }
    constante <- sum(carga * (media / desvio + centro) / escala)
    eixo <- soma - constante
    names(eixo) <- paste0("PC", k)

    arquivo <- file.path(pasta_saida, paste0("PC", k, ".tif"))
    terra::writeRaster(eixo, arquivo, overwrite = TRUE, datatype = "FLT4S",
                       gdal = c("COMPRESS=LZW"))
    gerados <- c(gerados, arquivo)
  }

  return(gerados)
}


# ------------------------------------------------------------------------------
# salvar_pca_eixos()
#   Salva variância, cargas, importância e o modelo (RDS) de uma PCA.
# ------------------------------------------------------------------------------
salvar_pca_eixos <- function(pca_eixos, pasta) {

  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  gravar <- function(tab, nome) utils::write.csv(tab, file.path(pasta, nome),
                                                 row.names = FALSE, fileEncoding = "UTF-8")
  gravar(pca_eixos$tabela, "pca_variancia.csv")
  gravar(data.frame(variavel = rownames(pca_eixos$cargas), pca_eixos$cargas), "pca_cargas.csv")
  gravar(pca_eixos$importancia, "pca_importancia.csv")

  saveRDS(list(variaveis = pca_eixos$variaveis, n_eixos = pca_eixos$n_eixos,
               media = pca_eixos$media, desvio = pca_eixos$desvio,
               centro = pca_eixos$modelo$center,
               escala = pca_eixos$modelo$scale,
               rotacao = pca_eixos$modelo$rotation[, seq_len(pca_eixos$n_eixos), drop = FALSE]),
          file.path(pasta, "pca_modelo.rds"))
}


# ------------------------------------------------------------------------------
# gerar_pca_eixos()
#   Faz tudo:
#     - variáveis iguais em todos os conjuntos -> uma PCA ("pca_todas_variaveis")
#       aplicada às preditoras e a todos os cenários;
#     - variáveis diferentes -> duas PCAs:
#         "pca_preditoras_completa": todas as variáveis das preditoras (só preditoras);
#         "pca_comum": só as variáveis comuns a TODOS os conjuntos, aplicada às
#                      preditoras e a cada cenário (use esta nos modelos de projeção).
#   Devolve uma lista com as PCAs e os arquivos gerados.
# ------------------------------------------------------------------------------
gerar_pca_eixos <- function(dados_testes, variaveis_cortadas, pasta_saida,
                            variancia_alvo = NULL, n_eixos = NULL,
                            pasta_verificacao = obter_pastas_saida()$verificacao) {

  conjuntos <- names(dados_testes)
  vars      <- lapply(dados_testes, function(x) colnames(x$dados))
  vars      <- aplicar_renomeio_vars(vars, variaveis_cortadas)   # correspondência já salva
  preditoras    <- vars[["preditoras"]]
  comuns    <- Reduce(intersect, vars)
  iguais    <- all(sapply(vars, function(v) setequal(v, preditoras)))

  # nomes diferentes entre preditoras e cenários: renomear (parear) ou pular a PCA comum
  pular_comum <- FALSE
  if (!iguais) {
    res <- resolver_nomes_variaveis(vars, variaveis_cortadas, pasta_verificacao)
    vars        <- res$vars
    pular_comum <- identical(res$acao, "pular")
    comuns      <- Reduce(intersect, vars)
    iguais      <- all(sapply(vars, function(v) setequal(v, preditoras)))
  }

  cat("\n================ PCA: EIXOS COMO NOVAS VARIÁVEIS ================\n")

  if (iguais) {
    cat("Todos os conjuntos têm as mesmas", length(preditoras), "variáveis: uma PCA para todos.\n")
    planos <- list(pca_todas_variaveis = list(variaveis = preditoras, conjuntos = conjuntos))
  } else {
    cat("Preditoras e cenários têm variáveis DIFERENTES.\n")
    for (cn in setdiff(conjuntos, "preditoras")) {
      fora <- setdiff(preditoras, vars[[cn]])
      cat("  -", cn, "não tem", length(fora), "variável(is) das preditoras\n")
    }
    cat("Variáveis comuns a todos os conjuntos:", length(comuns), "\n")
    cat("Ficam de fora da PCA 'comum':", paste(setdiff(preditoras, comuns), collapse = ", "), "\n")
    if (pular_comum) {
      planos <- list(
        pca_preditoras_completa = list(variaveis = preditoras, conjuntos = "preditoras")
      )
      aviso <- paste0("A PCA COMUM (preditoras + cenários) NÃO FOI FEITA: as variáveis têm ",
                      "nomes diferentes e você escolheu pular. As análises que dependem dela ",
                      "(modelos com eixos da PCA projetados nos cenários) não serão feitas. ",
                      "Foi feita só a PCA das preditoras ('pca_preditoras_completa'), sem projeção.")
      cat("\n*** AVISO:", aviso, "***\n")
      warning(aviso, call. = FALSE, immediate. = TRUE)
    } else {
      if (length(comuns) < 2) {
        stop("Há menos de 2 variáveis comuns a todos os conjuntos: não dá para fazer a PCA.",
             call. = FALSE)
      }
      planos <- list(
        pca_preditoras_completa = list(variaveis = preditoras, conjuntos = "preditoras"),
        pca_comum           = list(variaveis = comuns, conjuntos = conjuntos)
      )
    }
  }

  resultado <- list()
  for (nome in names(planos)) {
    plano <- planos[[nome]]
    pca   <- ajustar_pca_eixos(dados_testes, plano$variaveis, variancia_alvo, n_eixos)
    pasta <- file.path(pasta_saida, nome)
    salvar_pca_eixos(pca, pasta)

    cat("\n--", nome, "--\n")
    cat("Variáveis:", length(plano$variaveis), "| Eixos:", pca$n_eixos, "\n")
    cat("Regra:", pca$regra$motivo, "\n")
    cat("Variância acumulada nos eixos retidos:",
        round(pca$tabela$acumulada[pca$n_eixos] * 100, 1), "%\n")

    arquivos <- list()
    for (cn in plano$conjuntos) {
      if (cn == "preditoras") cat("Gerando os rasters dos eixos nas preditoras (variáveis dos modelos)\n")
      else cat("Projetando a PCA no cenário:", cn, "\n")
      arquivos[[cn]] <- projetar_eixos_raster(pca, variaveis_cortadas[[cn]],
                                              file.path(pasta, cn))
    }
    resultado[[nome]] <- list(pca = pca, arquivos = arquivos, pasta = pasta)
    cat("Salvo em:", pasta, "\n")
  }
  cat("==================================================================\n")

  return(resultado)
}
