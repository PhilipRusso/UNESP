# ==============================================================================
#  FUNÇÕES - Parâmetros que podem ficar vazios e são perguntados só quando
#            alguma função precisa deles pela primeira vez
# ------------------------------------------------------------------------------
#  REGRA GERAL: o script só pergunta ao usuário quando o parâmetro, na seção 2
#  do main, vale "prompt" (texto). Qualquer outro valor é usado como está; se não
#  for aceito pelo parâmetro, o script PARA com um aviso listando o que corrigir
#  (nunca pergunta no lugar).
#
#  Como funciona: no fim da seção 2, os parâmetros com "prompt" viram "promessas"
#  (delayedAssign): o R só executa a pergunta no primeiro uso da variável, guarda
#  a resposta na sessão e não pergunta de novo. Para não ser perguntado em outra
#  sessão, preencha o valor no main.
#
#  EXCEÇÃO: em 'tipo_validacao', 'metodos_limiar', 'metodos_consenso' e
#  'metodo_correlacao', NA significa PULAR o que depende deles (sem correlação;
#  sem validação cruzada, avaliação, consenso e JSDM; sem limiares e mapas binários;
#  sem consenso).
#
#  Têm o seu próprio jeito de perguntar (janela, lista no console), mas também só
#  perguntam com "prompt" (o "prompt" vira o valor vazio que a função entende):
#  caminhos (janela), colunas dos registros, filtro_feicoes, modelos,
#  metodos_multicolinearidade, variaveis_mantidas, preditores_modelos, modelos,
#  eixos_pca_modelos, dados_independentes e caminho_independente.
#  Não perguntam: variaveis_modelos (c() = variáveis finais), modelos_importancia
#  (c() = todos), pca_modelos ("" = automático), caminho_esforco e caminhos_cenarios.
#  'ajustes_modelos' (lista) mantém os valores do main.
# ==============================================================================


# ------------------------------------------------------------------------------
# especificacoes_parametros()
#   Descreve, para cada parâmetro: o tipo da pergunta, o texto e os limites.
#     tipo: "logico", "opcao" (uma), "multi" (várias), "numero", "numero_lista",
#           "texto", "texto_lista"
#     vazio_pergunta: TRUE se "" também conta como vazio (texto/opção)
#     aceita_nulo: no "numero" e no "texto", ENTER sem digitar vira NULL (ex.: todas as células)
# ------------------------------------------------------------------------------
especificacoes_parametros <- function() {

  lim <- c("max_tss", "max_kappa", "sens_espec", "mtp", "p10", "min_roc", "max_acuracia",
           "media_pres", "fixo")
  lim_rot <- c("max_tss: maior TSS", "max_kappa: maior Kappa",
               "sens_espec: sensibilidade = especificidade",
               "mtp: menor adequabilidade entre as presenças",
               "p10: 10% das presenças ficam fora",
               "min_roc: menor distância ao canto (0,1) da curva ROC",
               "max_acuracia: maior proporção de acertos",
               "media_pres: média da adequabilidade nas presenças",
               "fixo: o valor de 'limiar_fixo'")

  list(
    usar_cenarios = list(tipo = "logico",
      pergunta = "Usar cenários (variáveis projetadas, ex.: futuro ou passado)?"),
    separador_colunas = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Separador de colunas do arquivo de registros",
      opcoes = c(",", ";", "\t"), rotulos = c("vírgula (,)", "ponto e vírgula (;)", "tabulação")),
    separador_decimal = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Separador decimal do arquivo de registros",
      opcoes = c(".", ","), rotulos = c("ponto (ex.: -49.06)", "vírgula (ex.: -49,06)")),
    crs_registros = list(tipo = "texto", vazio_pergunta = TRUE, padrao = "EPSG:4326",
      pergunta = "Sistema de coordenadas dos registros (WGS84 = EPSG:4326)"),
    remover_duplicados = list(tipo = "logico",
      pergunta = "Remover registros da MESMA espécie com a mesma longitude e latitude?"),
    um_registro_por_celula = list(tipo = "logico",
      pergunta = "Manter só 1 registro por célula do raster (por espécie)?"),
    distancia_buffer_km = list(tipo = "numero", min = 0, padrao = 100,
      pergunta = "Raio (km) em volta dos registros onde o background é sorteado"),
    raio_pseudoausencia_km = list(tipo = "numero", min = 0, aceita_nulo = TRUE,
      pergunta = "Raio (km) em volta das presenças onde NÃO se sorteia background (ENTER = sem raio; precisa ser menor que o raio do buffer)"),
    fator_background = list(tipo = "numero", min = 0, padrao = 20,
      pergunta = "Fator do background: nº de pontos = fator x nº de registros"),
    minimo_background = list(tipo = "numero", inteiro = TRUE, min = 0, padrao = 1000,
      pergunta = "Mínimo de pontos de background por espécie"),
    vies_background = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Viés de coleta no background",
      opcoes = c("nenhum", "registros", "arquivo"),
      rotulos = c("nenhum: todas as células do buffer têm a mesma chance",
                  "registros: chance proporcional ao esforço dos registros de todas as espécies",
                  "arquivo: idem, com os pontos de coleta de outro arquivo")),
    nomes_variaveis_diferentes = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Variáveis das preditoras e dos cenários têm NOMES diferentes. O que fazer?",
      opcoes = c("renomear", "pular"),
      rotulos = c("renomear: o script propõe a correspondência entre os nomes (você confere e aceita) e faz a PCA comum",
                  "pular: não faz a PCA comum (e as análises que dependem dela)")),
    rasters_diferentes = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Há rasters com grade diferente (tamanho de célula/extensão). O que fazer?",
      opcoes = c("reamostrar", "excluir"),
      rotulos = c("reamostrar: os de célula maior são divididos na grade do raster de MENOR célula (sem cálculo de novos valores)",
                  "excluir: tira da análise os rasters que fogem da geometria mais comum")),
    raio_kernel_km = list(tipo = "numero", min = 0, padrao = 20,
      pergunta = "Raio do kernel do esforço de coleta (km; 0 = só as células com coleta)"),
    cortar_por_feicoes = list(tipo = "logico",
      pergunta = "Cortar só por algumas feições da máscara (em vez da máscara inteira)?"),
    semente = list(tipo = "numero", inteiro = TRUE, padrao = 123,
      pergunta = "Semente aleatória (mantém os sorteios iguais a cada execução)"),
    metodo_correlacao = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Método da correlação entre as variáveis",
      opcoes = c("pearson", "spearman"), rotulos = c("Pearson", "Spearman")),
    n_amostra_correlacao = list(tipo = "numero", inteiro = TRUE, min = 30, aceita_nulo = TRUE,
      pergunta = "Nº de células sorteadas para a correlação (ENTER = usar TODAS as células)"),
    casas_decimais = list(tipo = "numero", inteiro = TRUE, min = 1, max = 15, aceita_nulo = TRUE, padrao = 6,
      pergunta = "Casas decimais das tabelas salvas e mostradas (digite \"todas\" para NÃO arredondar)"),
    n_amostra_testes = list(tipo = "numero", inteiro = TRUE, min = 100, aceita_nulo = TRUE, padrao = 10000,
      pergunta = "Nº de células sorteadas para os testes de multicolinearidade (digite \"todas\" para usar TODAS as células)"),
    limite_vif = list(tipo = "numero", min = 1, padrao = 10,
      pergunta = "Limite do VIF (variáveis acima disso são removidas, uma a uma)"),
    limiar_filtro_correlacao = list(tipo = "numero", min = 0, max = 1, padrao = 0.7,
      pergunta = "Limiar de |r| do filtro por correlação (pares acima perdem uma variável)"),
    limiar_cluster = list(tipo = "numero", min = 0, max = 1, padrao = 0.7,
      pergunta = "Limiar de |r| do cluster (variáveis acima ficam no mesmo grupo)"),
    variancia_pca = list(tipo = "numero", min = 0, max = 1, aceita_nulo = TRUE,
      pergunta = "PCA: variância acumulada que os eixos devem somar, de 0 a 1 (ENTER = regra automática 1 - 1/p)"),
    n_eixos_pca = list(tipo = "numero", inteiro = TRUE, min = 1, aceita_nulo = TRUE,
      pergunta = "PCA: nº fixo de eixos (ENTER = usar a regra da variância)"),
    alfa_elastic_net = list(tipo = "numero", min = 0, max = 1, padrao = 0.5,
      pergunta = "Alfa do elastic net (0 = ridge, 1 = lasso)"),
    minimo_presencas_testes = list(tipo = "numero", inteiro = TRUE, min = 1, padrao = 10,
      pergunta = "Mínimo de presenças por espécie nos testes com resposta"),
    minimo_presencas_modelos = list(tipo = "numero", inteiro = TRUE, min = 1, padrao = 10,
      pergunta = "Mínimo de presenças por espécie para modelar"),
    tipo_validacao = list(tipo = "multi", extras = "aleatoria",
      pergunta = "Validação cruzada (o PRIMEIRO que digitar é o principal)",
      opcoes = c("blocos", "kfold", "jackknife", "teste"),
      rotulos = c("blocos: 4 blocos espaciais com o mesmo nº de presenças",
                  "kfold: k grupos sorteados", "jackknife: uma presença de fora por vez",
                  "teste: separa uma parte dos pontos só para testar (precisa de particao_teste)")),
    particao_teste = list(tipo = "numero", min = 0.05, max = 0.95, aceita_nulo = TRUE,
      pergunta = "Fração dos pontos separada para TESTE no hold-out, ex.: 0.3 (ENTER = não usar)"),
    particao_metodo = list(tipo = "opcao", padrao = "aleatoria",
      pergunta = "Como separar o teste",
      opcoes = c("aleatoria", "blocos"),
      rotulos = c("aleatoria: sorteio estratificado (presenças e background)",
                  "blocos: faixa espacial com a fração das presenças")),
    reajustar_modelo_final = list(tipo = "logico", padrao = TRUE,
      pergunta = "Reajustar os modelos finais com TODOS os pontos após o hold-out? (não = só treino)"),
    n_folds = list(tipo = "numero", inteiro = TRUE, min = 2, padrao = 5,
      pergunta = "Nº de grupos do k-fold"),
    metodos_limiar = list(tipo = "multi",
      pergunta = "Limiares para transformar adequabilidade em presença/ausência",
      opcoes = lim, rotulos = lim_rot),
    limiar_fixo = list(tipo = "numero", min = 0, max = 1, padrao = 0.5,
      pergunta = "Valor do limiar \"fixo\""),
    salvar_binarios = list(tipo = "logico",
      pergunta = "Salvar um mapa de presença/ausência para cada limiar?"),
    metodos_consenso = list(tipo = "multi",
      pergunta = "Métodos do consenso entre os modelos",
      opcoes = c("media_ponderada", "media_ponderada_kappa", "media_ponderada_auc",
                 "media_ponderada_acuracia", "media", "mediana", "votacao"),
      rotulos = c("média ponderada (peso = TSS)", "média ponderada (peso = Kappa)",
                  "média ponderada (peso = AUC)", "média ponderada (peso = proporção de acertos)",
                  "média simples", "mediana", "votação")),
    limiar_tss_consenso = list(tipo = "numero", min = -1, max = 1, padrao = 0.5,
      pergunta = "TSS médio mínimo para o modelo entrar no consenso"),
    limiar_votacao = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Limiar dos mapas binários que votam no consenso \"votacao\"",
      opcoes = lim, rotulos = lim_rot),
    testar_ajustes_maxent = list(tipo = "logico",
      pergunta = "Testar combinações de features e regmult do Maxent (demora mais)?"),
    maxent_classes_testadas = list(tipo = "texto_lista", validos = c("l", "q", "h", "p", "t"),
      padrao = "l lq h lqh lqph",
      pergunta = "Features do Maxent a testar (l linear, q quadrática, h dobradiça, p produto, t limiar)"),
    maxent_regmult_testados = list(tipo = "numero_lista", min = 0, padrao = "0.5 1 2 3 4",
      pergunta = "Multiplicadores da regularização do Maxent a testar"),
    metodos_importancia = list(tipo = "multi",
      pergunta = "Métodos da importância das variáveis",
      opcoes = c("permutacao", "jackknife", "regressao"),
      rotulos = c("permutação: embaralha cada preditor e vê a queda do AUC (rápido)",
                  "jackknife: refaz o modelo sem cada preditor e só com ele (lento)",
                  "regressão: GLM padronizado, queda de deviance e sinal do efeito")),
    calcular_moran = list(tipo = "logico", padrao = TRUE,
      pergunta = "Testar a autocorrelação espacial (I de Moran) dos dados e dos resíduos dos modelos?"),
    moran_alvos = list(tipo = "multi",
      pergunta = "O que testar na autocorrelação espacial",
      opcoes = c("residuos", "presenca"),
      rotulos = c("residuos: resíduos (observado - previsto) de cada modelo, com mapa e correlograma",
                  "presenca: os próprios dados de presença/background")),
    moran_classes = list(tipo = "numero", inteiro = TRUE, min = 3, padrao = 15,
      pergunta = "Nº de classes de distância do correlograma"),
    moran_distancia_max_km = list(tipo = "numero", min = 0, aceita_nulo = TRUE,
      pergunta = "Distância máxima do correlograma e do I global, em km (ENTER = metade da maior distância)"),
    moran_n_max = list(tipo = "numero", inteiro = TRUE, min = 100, aceita_nulo = TRUE, padrao = 2000,
      pergunta = "Nº máximo de pontos por espécie no I de Moran (digite \"todos\" para usar todos)"),
    moran_pontos = list(tipo = "opcao", padrao = "todos",
      pergunta = "Quais pontos usar no I de Moran",
      opcoes = c("todos", "presencas"),
      rotulos = c("todos: presenças e background", "presencas: só as presenças")),
    moran_suavizacao_km = list(tipo = "numero", min = 0, aceita_nulo = TRUE,
      pergunta = "Largura (km) da suavização do mapa de resíduos (ENTER = automática, 3% da diagonal da área)"),
    importancia_variaveis_originais = list(tipo = "logico", padrao = TRUE,
      pergunta = "Modelos com eixos da PCA: medir também a importância das VARIÁVEIS originais?"),
    n_permutacoes = list(tipo = "numero", inteiro = TRUE, min = 1, padrao = 10,
      pergunta = "Repetições do embaralhamento na importância por permutação"),
    figuras_tipos = list(tipo = "multi",
      pergunta = "Figuras a montar",
      opcoes = c("modelos", "binarios", "consenso", "cenarios", "variaveis", "importancia",
                 "curvas_resposta", "extrapolacao"),
      rotulos = c("modelos: painel com o mapa contínuo de cada algoritmo e do consenso",
                  "binarios: painel de presença/ausência por limiar",
                  "consenso: consenso contínuo e seus mapas binários",
                  "cenarios: consenso de cada conjunto lado a lado",
                  "variaveis: mapa de cada preditor usado (variáveis ou eixos da PCA)",
                  "importancia: barras da importância das variáveis",
                  "curvas_resposta: resposta de cada algoritmo a cada preditor",
                  "extrapolacao: MESS, área em extrapolação e variável mais diferente")),
    figuras_layout = list(tipo = "multi",
      pergunta = "Montar as figuras como painel (todos os mapas num quadro) ou cada mapa em uma figura separada?",
      opcoes = c("painel", "separadas"),
      rotulos = c("painel: todos os mapas juntos num quadro",
                  "separadas: uma figura para cada mapa")),
    formato_figuras = list(tipo = "multi",
      pergunta = "Formato das figuras",
      opcoes = c("png", "pdf"), rotulos = c("PNG (imagem)", "PDF (vetorial)")),
    resolucao_figuras = list(tipo = "numero", inteiro = TRUE, min = 72, padrao = 300,
      pergunta = "Resolução dos PNG (pontos por polegada)"),
    largura_painel_cm = list(tipo = "numero", min = 4, padrao = 10,
      pergunta = "Largura de cada quadro nos painéis de mapas (cm)"),
    paleta_figuras = list(tipo = "texto", vazio_pergunta = TRUE, padrao = "YlGnBu",
      pergunta = "Paleta do mapa contínuo (qualquer uma de hcl.colors(), ex.: YlGnBu, viridis, Greens)"),
    figuras_pontos = list(tipo = "logico",
      pergunta = "Desenhar os pontos de presença sobre os mapas?"),
    figura_correlacao_mapa = list(tipo = "logico", padrao = TRUE,
      pergunta = "Salvar o mapa de calor da correlação entre as variáveis?"),
    figuras_escala = list(tipo = "logico", padrao = TRUE,
      pergunta = "Desenhar a barra de escala nos mapas?"),
    figuras_norte = list(tipo = "logico", padrao = TRUE,
      pergunta = "Desenhar a seta do norte nos mapas?"),
    figuras_escala_posicao = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Canto do mapa onde fica a barra de escala",
      opcoes = posicoes_elementos_mapa,
      rotulos = c("inferior direito", "inferior esquerdo", "superior direito", "superior esquerdo")),
    figuras_norte_posicao = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Canto do mapa onde fica a seta do norte",
      opcoes = posicoes_elementos_mapa,
      rotulos = c("inferior direito", "inferior esquerdo", "superior direito", "superior esquerdo")),
    figuras_elementos_cores = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Cores da escala e do norte (útil quando eles ficam sobre áreas escuras do mapa)",
      opcoes = c("normais", "invertidas"),
      rotulos = c("normais: preto (e branco na barra)", "invertidas: branco sobre fundo escuro")),
    figuras_margem_mapa = list(tipo = "numero", min = 0, max = 0.3, padrao = 0.03,
      pergunta = "Folga entre o mapa e as bordas do quadro (fração da largura e da altura; 0 = sem folga)"),
    figuras_mascara = list(tipo = "logico", padrao = TRUE,
      pergunta = "Desenhar o contorno da máscara de estudo sobre os mapas?"),
    figuras_mascara_cor = list(tipo = "texto", vazio_pergunta = TRUE, padrao = "black",
      pergunta = "Cor do contorno da máscara (nome de cor do R ou código hexadecimal, ex.: black, #333333)"),
    figuras_mascara_preenchimento = list(tipo = "texto", aceita_nulo = TRUE,
      pergunta = "Cor das células sem adequabilidade (valor 0) dentro da máscara (nome de cor do R ou hexadecimal; ENTER = cor mínima da escala de cores)"),
    figuras_mascara_espessura = list(tipo = "numero", min = 0.1, max = 10, padrao = 0.8,
      pergunta = "Espessura da linha do contorno da máscara"),
    figuras_elementos_painel = list(tipo = "opcao", vazio_pergunta = TRUE,
      pergunta = "Nos painéis, onde ficam a legenda, a escala e o norte?",
      opcoes = c("todos", "ultimo"),
      rotulos = c("todos: em todos os mapas do painel", "ultimo: só no último mapa do painel")),
    figuras_elementos_fundo = list(tipo = "logico", padrao = FALSE,
      pergunta = "Colocar um fundo semitransparente atrás da escala e do norte (ajuda quando eles sobrepõem o mapa)?")
  )
}


# parâmetros em que NA significa "pular" (e não "perguntar")
parametros_pulaveis <- c("tipo_validacao", "metodos_limiar", "metodos_consenso",
                         "metodo_correlacao")

# TRUE se o valor é um único NA (= pular o passo que depende do parâmetro)
pulado <- function(valor) {
  length(valor) == 1 && !is.null(valor) && is.na(valor)
}

# TRUE se o valor é o texto "prompt" (= pedir ao usuário)
parametro_prompt <- function(valor) {
  is.character(valor) && length(valor) == 1 && !is.na(valor) &&
    tolower(trimws(valor)) == "prompt"
}

# ------------------------------------------------------------------------------
# validar_parametro()
#   Confere se o valor do main é aceito pelo parâmetro. Devolve NULL se estiver
#   certo ou o texto do problema.
# ------------------------------------------------------------------------------
validar_parametro <- function(nome, valor, esp) {

  if (nome %in% parametros_pulaveis && pulado(valor)) return(NULL)   # NA = pular

  intervalo <- function(v) {
    if (!is.numeric(v) || length(v) == 0 || anyNA(v)) return("use número(s)")
    if (!is.null(esp$min) && any(v < esp$min)) return(paste0("o valor mínimo é ", esp$min))
    if (!is.null(esp$max) && any(v > esp$max)) return(paste0("o valor máximo é ", esp$max))
    if (isTRUE(esp$inteiro) && any(v != round(v))) return("use número inteiro")
    NULL
  }
  opcoes <- c(esp$opcoes, esp$extras)
  lista_opcoes <- function() paste0("\"", esp$opcoes, "\"", collapse = ", ")

  switch(esp$tipo,
    logico = if (!(is.logical(valor) && length(valor) == 1 && !is.na(valor))) {
      "use TRUE ou FALSE"
    },
    opcao = if (!(is.character(valor) && length(valor) == 1 && !is.na(valor) &&
                  tolower(trimws(valor)) %in% tolower(opcoes))) {
      paste0("use um destes: ", lista_opcoes())
    },
    multi = if (!(is.character(valor) && length(valor) >= 1 && !anyNA(valor) &&
                  all(tolower(trimws(valor)) %in% c(tolower(opcoes), "all")))) {
      paste0("use \"all\" (todas) ou um ou mais destes (em c(...)): ", lista_opcoes())
    },
    numero = if (is.null(valor)) {
      if (!isTRUE(esp$aceita_nulo)) "NULL não é aceito aqui; use um número"
    } else if (length(valor) != 1) {
      "use um único número"
    } else intervalo(valor),
    numero_lista = if (!is.numeric(valor)) "use números em c(...)" else intervalo(valor),
    texto_lista = if (!(is.character(valor) && length(valor) >= 1 && !anyNA(valor) &&
                        all(vapply(strsplit(tolower(valor), ""),
                                   function(x) length(x) > 0 && all(x %in% esp$validos),
                                   logical(1))))) {
      paste0("use texto(s) só com as letras: ", paste(esp$validos, collapse = ", "))
    },
    texto = if (is.null(valor)) {
      if (!isTRUE(esp$aceita_nulo)) "NULL não é aceito aqui; use um texto"
    } else if (!(is.character(valor) && length(valor) == 1 && !is.na(valor) &&
                 nzchar(trimws(valor)))) {
      "use um texto não vazio"
    },
    NULL)
}


# ------------------------------------------------------------------------------
# Parâmetros com jeito próprio de perguntar (janela, lista no console etc.).
#   "prompt" no main vira o valor vazio que cada função já entende como "perguntar".
#   Vazio SEM "prompt" é um valor não aceito (obrigatorio = TRUE) e gera aviso.
# ------------------------------------------------------------------------------
parametros_prompt_proprios <- list(
  caminho_registros          = list(vazio = "",           obrigatorio = TRUE),
  caminho_var_preditoras         = list(vazio = "",           obrigatorio = TRUE),
  caminho_mascara            = list(vazio = "",           obrigatorio = TRUE),
  caminho_independente       = list(vazio = "",           obrigatorio = FALSE),
  caminho_esforco            = list(vazio = "",           obrigatorio = FALSE),
  figuras_especies           = list(vazio = character(0), obrigatorio = TRUE),
  figuras_conjuntos          = list(vazio = character(0), obrigatorio = TRUE),
  figuras_mapas              = list(vazio = character(0), obrigatorio = TRUE),
  figuras_limiares           = list(vazio = character(0), obrigatorio = TRUE),
  coluna_longitude           = list(vazio = "",           obrigatorio = TRUE),
  coluna_latitude            = list(vazio = "",           obrigatorio = TRUE),
  coluna_especie             = list(vazio = "",           obrigatorio = TRUE),  # NA = sem coluna
  metodos_multicolinearidade = list(vazio = character(0), obrigatorio = TRUE),
  variaveis_mantidas         = list(vazio = character(0), obrigatorio = TRUE),
  modelos                    = list(vazio = character(0), obrigatorio = TRUE),
  preditores_modelos         = list(vazio = "",           obrigatorio = TRUE),
  eixos_pca_modelos          = list(vazio = character(0), obrigatorio = FALSE),
  dados_independentes        = list(vazio = NULL,         obrigatorio = TRUE),
  filtro_feicoes             = list(vazio = list(),       obrigatorio = FALSE)
)

# nomes que o "prompt" já converteu (para não acusar o vazio resultante)
.sdm_prompt_convertidos <- new.env(parent = emptyenv())
.sdm_prompt_convertidos$nomes <- character(0)

vazio_legado <- function(valor, modelo) {
  if (is.null(modelo)) return(is.null(valor))
  if (identical(modelo, "")) return(is.character(valor) && length(valor) == 1 && !is.na(valor) && trimws(valor) == "")
  length(valor) == 0
}


# ------------------------------------------------------------------------------
# registrar_parametros_pendentes()
#   Roda no fim da seção 2 (e ao carregar parâmetros do main):
#   1) valor "prompt" -> o script pergunta: os parâmetros da lista acima viram
#      o vazio que a função entende como pergunta; os demais viram "promessas"
#      (delayedAssign) e a pergunta só acontece no primeiro uso da variável;
#   2) valor não aceito -> NÃO pergunta: para com um aviso listando o que corrigir.
#   NA só é aceito (= pular o passo) em 'tipo_validacao', 'metodos_limiar',
#   'metodos_consenso' e 'metodo_correlacao'.
# ------------------------------------------------------------------------------
registrar_parametros_pendentes <- function(ambiente = globalenv(), nomes = NULL) {

  especificacoes <- especificacoes_parametros()
  completo <- is.null(nomes)
  if (completo) .sdm_prompt_convertidos$nomes <- character(0)
  if (completo) nomes <- unique(c(names(especificacoes), names(parametros_prompt_proprios)))
  pendentes <- character(0)
  pulados   <- character(0)
  erros     <- character(0)

  for (nome in nomes) {
    if (!exists(nome, envir = ambiente, inherits = FALSE)) next
    valor <- get(nome, envir = ambiente, inherits = FALSE)

    # parâmetros com pergunta própria
    proprio <- parametros_prompt_proprios[[nome]]
    if (!is.null(proprio) || nome %in% names(parametros_prompt_proprios)) {
      if (parametro_prompt(valor)) {
        log_registrar_pendente(nome, valor)
        assign(nome, proprio$vazio, envir = ambiente)
        .sdm_prompt_convertidos$nomes <- union(.sdm_prompt_convertidos$nomes, nome)
        pendentes <- c(pendentes, nome)
      } else if (isTRUE(proprio$obrigatorio) && vazio_legado(valor, proprio$vazio) &&
                 !nome %in% .sdm_prompt_convertidos$nomes &&
                 !(nome == "coluna_especie" && pulado(valor))) {
        erros <- c(erros, paste0("'", nome, "' está vazio. Preencha o valor ou use \"prompt\" ",
                                 "para o script perguntar."))
      }
      next
    }

    esp <- especificacoes[[nome]]
    if (is.null(esp)) next

    if (nome %in% parametros_pulaveis && pulado(valor)) {
      pulados <- c(pulados, nome)
      next
    }
    if (parametro_prompt(valor)) {
      log_registrar_pendente(nome, valor)   # para o log da execução
      do.call(delayedAssign, list(nome, call("perguntar_parametro", nome),
                                  eval.env = globalenv(), assign.env = ambiente))
      pendentes <- c(pendentes, nome)
      next
    }
    problema <- validar_parametro(nome, valor, esp)
    if (is.null(problema) && identical(esp$tipo, "multi") && is.character(valor) &&
        "all" %in% tolower(trimws(valor))) {
      assign(nome, esp$opcoes, envir = ambiente)   # "all" = todas as opções
    }
    if (!is.null(problema)) {
      texto <- if (is.null(valor)) "NULL" else paste(deparse(valor), collapse = "")
      erros <- c(erros, paste0("'", nome, "' = ", substr(texto, 1, 60), ": ", problema,
                               ". (Ou use \"prompt\" para o script perguntar.)"))
    }
  }

  if (length(erros) > 0) {
    stop("Corrija estes parâmetros na seção 2 do main.R:\n  - ",
         paste(erros, collapse = "\n  - "), call. = FALSE)
  }
  if (length(pulados) > 0) {
    message("Parâmetros em NA (os passos que dependem deles serão PULADOS): ",
            paste(pulados, collapse = ", "))
  }
  if (length(pendentes) > 0) {
    message("Parâmetros com \"prompt\" (serão perguntados quando forem usados): ",
            paste(pendentes, collapse = ", "))
  }
  invisible(pendentes)
}


# ------------------------------------------------------------------------------
# perguntar_parametro()
#   Faz a pergunta de um parâmetro no console e devolve o valor já no formato
#   certo. Repete até a resposta ser válida. Fora de sessão interativa, para
#   com uma mensagem pedindo para preencher o parâmetro no main.
# ------------------------------------------------------------------------------
perguntar_parametro <- function(nome) {

  esp <- especificacoes_parametros()[[nome]]
  if (is.null(esp)) stop("Parâmetro desconhecido: ", nome, call. = FALSE)
  if (!interactive()) {
    stop("O parâmetro '", nome, "' está vazio no main e não dá para perguntar fora de uma ",
         "sessão interativa. Preencha '", nome, "' na seção 2.", call. = FALSE)
  }

  cat("\n---------------- PARÂMETRO:", nome, "----------------\n")
  cat(esp$pergunta, "\n", sep = "")

  # lê uma resposta de texto; ENTER devolve o padrão (se houver)
  ler <- function(prompt, padrao = NULL) {
    sufixo <- if (!is.null(padrao)) paste0(" [ENTER = ", padrao, "]") else ""
    r <- trimws(ler_entrada(paste0(prompt, sufixo, ": "), contexto = esp$pergunta,
                              titulo = esp$pergunta))
    if (r == "" && !is.null(padrao)) r <- as.character(padrao)
    r
  }
  numeros <- function(texto) {
    partes <- unlist(strsplit(trimws(texto), "[;[:space:]]+|,\\s+"))
    partes <- partes[partes != ""]
    suppressWarnings(as.numeric(sub(",", ".", partes)))
  }
  confere <- function(v) {
    if (length(v) == 0 || any(is.na(v))) return("Digite número(s) válido(s).")
    if (!is.null(esp$min) && any(v < esp$min)) return(paste0("O valor mínimo é ", esp$min, "."))
    if (!is.null(esp$max) && any(v > esp$max)) return(paste0("O valor máximo é ", esp$max, "."))
    if (isTRUE(esp$inteiro) && any(v != round(v))) return("Use um número inteiro.")
    NULL
  }

  valor <- switch(esp$tipo,
    logico = perguntar_opcoes(c("Sim", "Não"), "Número da opção: ", esp$pergunta)[1] == 1,
    opcao  = esp$opcoes[perguntar_opcoes(esp$rotulos, "Número da opção: ", esp$pergunta)[1]],
    multi  = {
      n <- length(esp$opcoes)
      r <- perguntar_opcoes(c(esp$rotulos, "all: todas as opções"),
                            "Número(s) da(s) opção(ões) (ex.: 1,3): ", esp$pergunta)
      if ((n + 1) %in% r) esp$opcoes else esp$opcoes[r]
    },
    numero = {
      repeat {
        padrao <- esp$padrao
        r <- ler("Valor", padrao)
        if (r == "" && isTRUE(esp$aceita_nulo)) { v <- NULL; break }
        if (isTRUE(esp$aceita_nulo) && tolower(r) %in% c("todas", "todos", "all", "null")) { v <- NULL; break }
        v <- numeros(r)
        erro <- if (length(v) != 1) "Digite um único número." else confere(v)
        if (is.null(erro)) break
        message(erro)
      }
      v
    },
    numero_lista = {
      repeat {
        v <- numeros(ler("Valores (separados por espaço)", esp$padrao))
        erro <- confere(v)
        if (is.null(erro)) break
        message(erro)
      }
      v
    },
    texto_lista = {
      repeat {
        v <- unlist(strsplit(tolower(ler("Valores (separados por espaço)", esp$padrao)), "[,;[:space:]]+"))
        v <- v[v != ""]
        invalidos <- v[!vapply(strsplit(v, ""), function(x) all(x %in% esp$validos), logical(1))]
        if (length(v) > 0 && length(invalidos) == 0) break
        message("Use só combinações das letras: ", paste(esp$validos, collapse = ", "), ".")
      }
      v
    },
    texto = {
      if (isTRUE(esp$aceita_nulo)) {
        v <- ler("Valor (ENTER = NULL)", esp$padrao)
        if (v == "" || tolower(v) %in% c("null", "nenhum", "nenhuma")) v <- NULL
      } else {
        repeat {
          v <- ler("Valor", esp$padrao)
          if (v != "") break
          message("Digite um valor.")
        }
      }
      v
    }
  )

  log_registrar_resposta(nome, valor)   # para o log da execução

  texto_valor <- if (is.null(valor)) "NULL (automático)" else paste(deparse(valor), collapse = "")
  message("Parâmetro '", nome, "' = ", texto_valor,
          " (guardado nesta sessão). Para não perguntar de novo, preencha no main: ",
          nome, " <- ", if (is.null(valor)) "NULL" else paste(deparse(valor), collapse = ""))
  valor
}
