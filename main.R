# MAIN - Modelagem de Distribuição de Espécies (SDM)----------------------------
#    - Definição dos parâmetros para configurar o script;
#    - Chamada das funções, que ficam guardadas na pasta "funcoes/".
#
#  O script pode ser rodado inteiro ou por partes.
#  RODANDO SÓ PARTES: as seções 0, 1 e 2 (pasta do projeto, pacotes e
#  parâmetros) precisam ser rodadas sempre que você abrir o R. As demais podem
#  ser rodadas separadamente: se algo de que a seção precisa não existir na
#  sessão, o script pode três comportamentos: 1) usar o que já foi salvo em 
#  execuções anteriores, se houver dados de execuções anteriores na pasta do projeto
#  2) gera com os parâmetros da seção 2 e avisa você, 3) solicita rodar uma seção
#  necessária para gravar os parâmetros.
#  Se você mudar um parâmetro, rode de novo as seções afetadas.
#  Se você mudar algo nos scripts da pasta funções, rode novamente 
#  'Carrega (source) todas as funções da pasta "funcoes/"' na seção 0.
#  (A linha rm(list = ls()) abaixo limpa a sessão, incluindo as variáveis salvas
#  no ambiente: rode só para recomeçar.)
#  Qualquer parâmetro designado com "prompt" será solicitado inserir os valores 
#  quando o código rodar.

# 0. Configuração do ambiente---------------------------------------------------

.sdm_inicio <- Sys.time()   # hora de início, para o log (objeto oculto: o rm abaixo não apaga)
rm(list = ls())   # limpa o ambiente para começar do zero

# Informe aqui a pasta onde estão este main.R com a subpasta "funcoes/".
# Exemplo (Windows): "C:/Users/SDM"
# "prompt" abre uma janela para você selecionar a pasta.
pasta_projeto <- "prompt"

if (identical(pasta_projeto, "")) {
  stop("'pasta_projeto' está vazio. Informe o caminho da pasta ou use \"prompt\" ",
       "para abrir a janela de seleção.")
}
if (identical(tolower(trimws(pasta_projeto)), "prompt")) {
  titulo <- "Selecione a pasta do projeto"
  message(">> ", titulo)

  if (.Platform$OS.type == "windows") {
    pasta_projeto <- utils::choose.dir(caption = titulo)
  } else {
    pasta_projeto <- tcltk::tk_choose.dir(caption = titulo)
  }

  if (length(pasta_projeto) == 0 || is.na(pasta_projeto) || pasta_projeto == "") {
    stop("Nenhuma pasta foi selecionada.")
  }
}
if (!dir.exists(pasta_projeto)) {
  stop("A pasta do projeto não existe: ", pasta_projeto)
}

pasta_projeto <- normalizePath(pasta_projeto, winslash = "/")

pasta_funcoes <- file.path(pasta_projeto, "funcoes")
if (!dir.exists(pasta_funcoes)) {
  stop("Não encontrei a pasta 'funcoes' em: ", pasta_projeto,
       "\nConfira o caminho de 'pasta_projeto': a pasta 'funcoes' deve estar dentro dela.")
}

# Carrega (source) todas as funções da pasta "funcoes/"
for (arquivo in list.files(pasta_funcoes, pattern = "\\.R$", full.names = TRUE)) {
  source(arquivo, encoding = "UTF-8")
}

# Log da execução: registra parâmetros, entradas, modelos, saídas e o tempo de cada
# etapa (seção). Fica em <pasta do projeto>/logs/
# Cada seção abaixo começa com iniciar_etapa("<título da seção>"): se você mudar o
# número ou o nome de uma seção, mude também o texto dessa linha.
iniciar_log(inicio = .sdm_inicio)
iniciar_etapa("0. Configuração do ambiente", inicio = .sdm_inicio)


# 1. PACOTES--------------------------------------------------------------------
iniciar_etapa("1. PACOTES")
# 
# Lista de pacotes usados neste script. Os que não estiverem instalados
# serão instalados automaticamente (é preciso estar conectado à internet).
#   terra        : leitura e manipulação de rasters (.tif) e shapefiles (.shp)
#   glmnet       : lasso, ridge e elastic net e modelo GLMNET
#   pls          : PCR, PLSR e CPPLS
#   Boruta       : seleção de variáveis com random forest
#   mgcv         : modelo GAM
#   earth        : modelo MARS
#   rpart        : modelo CTA (árvore de classificação)
#   randomForest : modelo RF
#   gbm          : modelo BRT
#   maxnet       : modelo Maxent
#   e1071        : modelo SVM
#   nnet         : modelo ANN (rede neural)
#   gllvm        : modelo JSDM
#   rstudioapi   : janelas de pergunta no RStudio (evita que o console consuma as
#                  linhas seguintes do script como resposta)

pacotes <- c(
  "terra", "glmnet", "pls", "Boruta", "mgcv", "earth", "rpart", "randomForest",
  "gbm", "maxnet", "e1071", "nnet", "gllvm", "rstudioapi"
)

verificar_pacotes(pacotes)


# 2. PARÂMETROS DE ENTRADA -----------------------------------------------------
iniciar_etapa("2. PARÂMETROS DE ENTRADA")
# Para cada parâmetro:
#  - escreva o valor              -> ele será usado diretamente;
#  - escreva "prompt"             -> o script PERGUNTA o valor quando for usá-lo
#  - valor não aceito pelo parâmetro -> o script PARA com um aviso do que corrigir
#  - "all" (para escolha de opções: métodos, modelos, figuras...) -> usa TODAS as opções;
#  - NA só vale em 'tipo_validacao', 'metodos_limiar', 'metodos_consenso' e
#    'metodo_correlacao' e significa PULAR o que depende deles.


#---------------- Caminhos

# Registros de ocorrência da espécie (.csv ou .txt)
caminho_registros <- "prompt"

# Pasta com as variáveis ambientais PREDITORAS (arquivos .tif)
caminho_var_preditoras <- "prompt"   

# Máscara da área de estudo (.shp ou .tif)
caminho_mascara <- "prompt"

# Cenários (variáveis PROJETADAS, ex.: futuro ou passado)
usar_cenarios <- TRUE

caminhos_cenarios <- c(
  #"scenario1" = "",
  #"scenario3" = ""
)

# Validação independente (dados que NÃO entraram no ajuste dos modelos)
#   dados_independentes : NULL  = o script pergunta se você tem esses dados
#   caminho_independente: arquivo .csv/.txt com 3 colunas (espécie, longitude, latitude)
dados_independentes  <- FALSE              #default: FALSE
caminho_independente <- ""


#---------------- Configuração e preparação de arquivos

# Formato e definição de campos do arquivo de registros
separador_colunas <- ","                   #valores: "," ";" "\t"
separador_decimal <- "."                   #valores: "." ","

coluna_longitude <- "prompt"
coluna_latitude  <- "prompt"
coluna_especie   <- "prompt"
crs_registros    <- "EPSG:4326" # sistema de coordenadas dos registros (WGS84 = "EPSG:4326").

# Arredondamento de valores
#   casas_decimais : nº de casas decimais das tabelas e resultados mostrados no
#                    console. Textos e títulos de figuras continuam com poucas casas.
casas_decimais <- NULL                     #default: NULL (sem arredondar, -12 algarismos)

# Semente aleatória: mantém os 'sorteios' iguais a cada execução
semente <- 123

# Limpeza dos registros
#   Sempre são descartados: coordenadas inválidas, registros fora da área de
#   estudo e registros em células sem valor (NA) em alguma variável..
remover_duplicados     <- FALSE   # para cada espécie remove registros com a mesma coordenada
um_registro_por_celula <- TRUE   # para cada espécie mantém 1 registro por célula do raster

# Corte da área de estudo (válido apenas para .shp)
#   FALSE -> usa a máscara inteira
#
#   filtro_feicoes: lista no formato  coluna = valores
#     Exemplo:  list(NM_UF = c("SP", "MG"), bioma = "Cerrado")
#     - vários valores na MESMA coluna: a feição precisa ter UM deles
#     - colunas DIFERENTES: a feição precisa atender a TODAS
cortar_por_feicoes <- TRUE

filtro_feicoes <- list(
  # features = c("SP", "MG")
)


#---------------- Definir Background/pseudoausências

# Background (pontos de fundo)
#   distancia_buffer_km    : raio (km) em volta dos registros onde o background é sorteado
#   raio_pseudoausencia_km : raio (km) em volta de cada presença onde o
#                            background NÃO é sorteado. Precisa ser MENOR que
#                            'distancia_buffer_km'.
#   fator_background       : nº de pontos de background = fator x nº de registros
#   minimo_background      : piso do nº de pontos de background de cada espécie.
distancia_buffer_km    <-  1000           #default: 100
raio_pseudoausencia_km <-  10             #default: NULL (sem raio)
fator_background       <-  30             #default: 100
minimo_background      <-  1000           #default: 1000

# Viés de coleta no background
#   vies_background : "nenhum"    -> todas as células do buffer têm a mesma chance
#                     "registros" -> chance proporcional ao esforço de coleta,
#                                    estimado pelos registros ORIGINAIS de TODAS as
#                                    espécies do arquivo 
#                     "arquivo"   -> idem, com os pontos de coleta de outro arquivo,
#                                    no MESMO formato dos registros 
#   caminho_esforco : arquivo de pontos de coleta (só para "arquivo")
#   raio_kernel_km  : quanto o esforço de cada coleta se espalha em volta dela
#                     (desvio do kernel gaussiano, em km). 0 = só as células com
#                     coleta recebem background
vies_background <- "registros"              #default: "registros"
caminho_esforco <- ""                       #default: ""
raio_kernel_km  <- 50                       


#---------------- Correlação e multicolinearidade

# Correlação entre as variáveis
#   n_amostra_correlacao : NULL  = usa TODAS as células da área de estudo
#                          número = sorteia esse nº de células (mais rápido)
metodo_correlacao    <- "prompt"            #valores: "pearson", "spearman", "prompt", NA (pular a correlação)
n_amostra_correlacao <- NULL                #default: NULL

#   figura_correlacao_mapa : TRUE = salva o mapa de valores da correlação 
figura_correlacao_mapa <- TRUE

# Selecionar os métodos para tratar a multicolinearidade
#   Sem resposta: "vif", "filtro_correlacao", "cluster", "pca"
#   Com resposta: "lasso", "ridge", "elastic_net", "pcr", "plsr", "cppls", "boruta"
metodos_multicolinearidade <- "prompt"

# Parâmetros dos testes de multicolinearidade
#   n_amostra_testes         : nº de células sorteadas da área de estudo (métodos sem resposta).
#                              NULL = usa TODAS as células válidas
#   limite_vif               : variáveis com VIF acima disso são removidas, uma a uma
#   limiar_filtro_correlacao : pares com |r| acima disso perdem uma das variáveis
#   limiar_cluster           : variáveis com |r| acima disso ficam no mesmo grupo
#   variancia_pca            : PCA - variância acumulada que os eixos devem somar (0 a 1).
#                              NULL = regra automática: com p variáveis, soma 1 - 1/p
#                              (ex.: 20 variáveis -> 95%; 8 variáveis -> 87,5%)
#   n_eixos_pca              : PCA - nº de eixos fixo. NULL = usa a regra acima.
#                              Se preencher, tem prioridade sobre 'variancia_pca'.
#   alfa_elastic_net         : 0 = ridge, 1 = lasso; entre os dois = elastic net
#   minimo_presencas_testes  : espécies com menos presenças ficam fora dos testes com resposta
#   variaveis_mantidas       : definir as variáveis finais que serão usadas nos modelos
n_amostra_testes         <- 10000           #default: 10000 (NULL = todas as células)
limite_vif               <- 10              #default: 10
limiar_filtro_correlacao <- 0.7             #default: 0.7
limiar_cluster           <- 0.7             #default: 0.7
variancia_pca            <- NULL            #default: NULL
n_eixos_pca              <- NULL            #default: NULL
alfa_elastic_net         <- 0.5             #default: 0.5
minimo_presencas_testes  <- 10              #default: 10
variaveis_mantidas       <- "prompt"


#---------------- Definições e ajuste dos modelos

# Modelos de distribuição
#   modelos : só presença           : "bioclim", "domain", "mahalanobis"
#             presença e background : "glm", "glmnet", "gam", "mars", "cta", "rf",
#                                     "brt", "maxent", "svm", "ann", "esm"
#             todas as espécies juntas (fora do consenso): "jsdm"
#   preditores_modelos : com o que os modelos serão feitos:
#                        "variaveis" -> variáveis escolhidas (escalonadas)
#                        "pca"       -> eixos da PCA (seções 11 e 13 são PULADAS)
#                        "ambos"     -> os dois, em rodadas SEPARADAS 
#   variaveis_modelos  : se "variaveis", quais. c() = as variáveis finais
#   pca_modelos        : se "pca", qual PCA: "pca_comum","pca_preditoras_completa", "pca_todas_variaveis".
#                        "" = automático: pca_todas_variaveis; se preditoras e cenários têm
#                        variáveis diferentes, roda DUAS rodadas separadas:
#                          pca_preditoras_completa -> só as preditoras, todas as variáveis (sem projeção)
#                          pca_comum           -> preditoras e cenários, variáveis comuns (com projeção)
#   eixos_pca_modelos  : se "pca", quais eixos usar (ex.: c(1, 2, 3) ou c("PC1", "PC2")).
#                        NULL     = regra automática do teste de multicolinearidade: os
#                                   primeiros eixos que somam 1 - 1/p da variância (p = nº de
#                                   variáveis da PCA); "all" = todos os eixos gerados;
#                                   "prompt" = mostra os eixos e pergunta
#   minimo_presencas_modelos : espécies com menos presenças não são modeladas
#   tipo_validacao     : esquema(s) de validação cruzada.
#                        "blocos"    -> 4 blocos espaciais com o mesmo nº de presenças
#                        "kfold"     -> k grupos sorteados (k = n_folds)
#                        "jackknife" -> deixa uma presença de fora por vez
#                        "teste"     -> hold-out: separa uma parte dos pontos só para testar
#                                       (usa 'particao_teste'); modelos ajustados só com o
#                                       treino e métricas (AUC, TSS, Kappa, Boyce, omissão)
#                                       calculadas no teste; limiares e consenso saem do teste
#                                       quando "teste" é o primeiro da lista
#   n_folds            : nº de grupos no "kfold"
#   particao_teste     : fração dos pontos para teste no hold-out (ex.: 0.3). NULL = não usa
#   particao_metodo    : "aleatoria" (sorteio estratificado) ou "blocos" (faixa espacial com a
#                        fração das presenças, no eixo de maior extensão)
#   reajustar_modelo_final : TRUE = após a avaliação, os modelos finais (mapas) são reajustados
#                        com todos os pontos; FALSE = usam só as linhas de treino do hold-out
#   metodos_limiar     : limiares para transformar adequabilidade em presença/ausência
#                        "max_tss"      -> maior TSS (sensibilidade + especificidade)
#                        "max_kappa"    -> maior Kappa
#                        "sens_espec"   -> sensibilidade igual à especificidade
#                        "mtp"          -> menor adequabilidade entre as presenças
#                        "p10"          -> 10% das presenças ficam fora do mapa
#                        "min_roc"      -> menor distância ao canto (0, 1) da curva ROC
#                        "max_acuracia" -> maior proporção de acertos
#                        "media_pres"   -> média da adequabilidade nas presenças (mapa bem restrito)
#                        "fixo"         -> o valor de 'limiar_fixo'
#   limiar_fixo        : valor usado no limiar "fixo"
#   salvar_binarios    : TRUE = salva um mapa de presença/ausência para cada limiar
#   metodos_consenso   : "media_ponderada" (peso = TSS), "media_ponderada_kappa" (peso = Kappa),
#                        "media_ponderada_auc" (peso = 2 x AUC - 1),
#                        "media_ponderada_acuracia" (peso = proporção de acertos, com
#                        presenças e background pesando igual = (TSS + 1) / 2),
#                        "media", "mediana", "votacao"
#   limiar_tss_consenso: só entram no consenso os modelos com TSS médio >= este valor
#   limiar_votacao     : limiar dos mapas binários que votam no consenso "votacao"
#
#   NA = pular execução em 'tipo_validacao', 'metodos_limiar', 'metodos_consenso'
#     tipo_validacao  = NA : sem validação cruzada, sem avaliação do Maxent, sem consenso
#                            (precisa do TSS) e sem JSDM; os limiares usam só as previsões
#                            do treino e o jackknife de variáveis é pulado
#     metodos_limiar  = NA : sem limiares e sem mapas binários (só mapas contínuos); o
#                            consenso "votacao" também sai
#     metodos_consenso = NA: seção 18 (consenso) pulada
modelos                  <- "prompt"
preditores_modelos       <- "prompt"
variaveis_modelos        <- c()
pca_modelos              <- ""
eixos_pca_modelos        <- "prompt"              #default: "NULL"
minimo_presencas_modelos <- 10
tipo_validacao           <- "kfold"
n_folds                  <- 5
particao_teste           <- NULL
particao_metodo          <- "aleatoria"
reajustar_modelo_final   <- TRUE
metodos_limiar           <- "prompt"
limiar_fixo              <- 0.5
salvar_binarios          <- TRUE
metodos_consenso         <- "prompt"
limiar_tss_consenso      <- 0.5
limiar_votacao           <- "max_tss"

# Ajustes dos algoritmos (valores usuais; mude só se souber o efeito)
# Qualquer item apagado desta lista volta ao valor usual. NA = padrão do pacote.
ajustes_modelos <- list(
  # GLM e ESM
  glm_termos       = "quadratico", # "quadratico", "linear"
  # GLMNET
  glmnet_alfa      = 1,            # 1 = lasso, 0 = ridge, entre 0 e 1 = elastic net
  glmnet_lambda    = "lambda.min", # "lambda.min" (menor erro) ou "lambda.1se" (modelo mais simples)
  glmnet_folds     = NA,           # grupos da validação interna (NA = automático, de 3 a 5)
  glmnet_padronizar = TRUE,        # padroniza os preditores antes de ajustar
  # GAM
  gam_k            = 4,            # flexibilidade das curvas (nº de bases)
  gam_metodo       = "REML",       # suavização: "REML", "ML" ou "GCV.Cp"
  gam_select       = FALSE,        # TRUE = penaliza termos a ponto de poder removê-los do modelo
  gam_gamma        = 1,            # maior que 1 = curvas mais suaves
  # MARS
  mars_grau        = 1,            # grau das interações (1 = sem interações)
  mars_nk          = NA,           # máximo de termos antes da poda (NA = padrão do pacote)
  mars_penalidade  = NA,           # custo por termo na poda (NA = padrão: 2, ou 3 com interações)
  mars_thresh      = 0.001,        # para de adicionar termos se o ganho for menor que isto
  # CTA (árvore de classificação)
  cta_cp           = 0.01,         # parâmetro de complexidade (menor = árvore maior)
  cta_minsplit     = 10,           # mínimo de observações para dividir um nó
  cta_maxdepth     = 30,           # profundidade máxima da árvore (máximo permitido: 30)
  cta_minbucket    = NA,           # mínimo de observações num nó final (NA = cta_minsplit / 3)
  # RF
  rf_arvores       = 500,          # nº de árvores
  rf_mtry          = NA,           # variáveis sorteadas por divisão (NA = raiz do nº de preditores)
  rf_tamanho_no    = 1,            # tamanho mínimo dos nós finais
  rf_balancear     = TRUE,         # TRUE = cada árvore sorteia o mesmo nº de presenças e background
  rf_reposicao     = TRUE,         # sorteio das amostras com reposição
  # BRT
  brt_taxa         = 0.01,         # taxa de aprendizado
  brt_complexidade = 3,            # profundidade das interações
  brt_max_arvores  = 5000,         # máximo de árvores (o nº ideal sai da validação interna)
  brt_bag          = 0.75,         # fração dos dados sorteada a cada árvore
  brt_min_obs      = 5,            # mínimo de observações por nó final
  brt_cv_folds     = 5,            # grupos da validação interna que escolhe o nº de árvores
  brt_distribuicao = "bernoulli",  # função de perda: "bernoulli" ou "adaboost"
  # Maxent
  maxent_classes   = "default",    # features ("l", "lq", "lqh"...); "default" = conforme nº de presenças
  maxent_regmult   = 1,            # multiplicador da regularização (beta multiplier)
  maxent_maxit     = 10000,        # máximo de iterações do ajuste
  maxent_beta_lqp  = 1,            # regularização das features linear, quadrática e produto (fator; 1 = padrão)
  maxent_beta_hinge = 1,           # regularização das features dobradiça (fator; 1 = padrão)
  maxent_beta_threshold = 1,       # regularização das features limiar (fator; 1 = padrão)
  maxent_beta_categorical = 1,     # regularização das variáveis categóricas (fator; 1 = padrão)
  # SVM
  svm_custo        = 1,            # custo (C)
  svm_kernel       = "radial",     # "radial", "linear", "polynomial" ou "sigmoid"
  svm_gamma        = NA,           # alcance do kernel (NA = 1 / nº de preditores)
  svm_pesos_classe = TRUE,         # TRUE = equilibra os pesos de presença e background
  svm_tolerancia   = 0.001,        # tolerância do critério de parada do ajuste
  # ANN
  ann_neuronios    = 5,            # nós na camada oculta
  ann_decaimento   = 0.1,          # regularização (decay)
  ann_maxit        = 1000,         # máximo de iterações do treino
  ann_erro         = "ce",         # função de erro: "ce" (entropia cruzada) ou "sse" (soma dos quadrados)
  ann_repeticoes   = 1,            # nº de redes treinadas com pesos iniciais diferentes (fica a melhor)
  ann_abstol       = 1e-4,         # para o treino se o erro cair abaixo disto
  ann_reltol       = 1e-8,         # para o treino se o erro deixar de melhorar por este fator
  ann_rang         = 0.5,          # amplitude dos pesos iniciais (sorteados entre -rang e +rang)
  ann_skip         = FALSE,        # TRUE = liga as entradas direto à saída, além da camada oculta
  # ESM (modelos de pares de variáveis)
  esm_folds        = 3,            # grupos da validação que pesa cada par
  esm_limite_d     = 0,            # só entram pares com D acima deste valor (0 = melhor que o acaso)
  # JSDM
  jsdm_latentes    = 2             # nº de variáveis latentes
)

# Ajuste do Maxent (só roda se "maxent" estiver em 'modelos')
#   testar_ajustes_maxent : TRUE  -> testa as combinações abaixo com a MESMA validação
#                                    cruzada principal ('tipo_validacao') e usa, para
#                                    cada espécie, a melhor delas
#                           FALSE -> usa maxent_classes e maxent_regmult de 'ajustes_modelos'
#   maxent_classes_testadas : features: "l" linear, "q" quadrática, "h" dobradiça,
#                             "p" produto, "t" limiar
#   maxent_regmult_testados : multiplicadores da regularização (maior = curvas mais suaves)
#   Critério: menor omissão P10 média; empate -> maior AUC média (como no ENMeval).
#   Cada combinação repete a validação cruzada (5 classes x 5 regmult = 25 vezes),
#   então esta etapa demora mais com muitas espécies ou muitos pontos.
testar_ajustes_maxent   <- TRUE
maxent_classes_testadas <- c("l", "lq", "h", "lqh", "lqph")
maxent_regmult_testados <- c(0.5, 2, 4)


#---------------- Teste para os modelos finais

# Importância das variáveis
#   metodos_importancia : medir quanto cada preditor pesa nos modelos
#                         "permutacao" -> embaralha um preditor por vez e vê quanto o AUC cai
#                         "jackknife"  -> refaz o modelo SEM cada preditor e SÓ com ele,
#                         "regressao"  -> GLM com preditores padronizados: queda de deviance ao retirar
#                          cada preditor, coeficiente padronizado e sinal do efeito
#   modelos_importancia : algoritmos avaliados (ex.: c("glm", "rf", "maxent")).
#   n_permutacoes       : repetições do embaralhamento na "permutacao"
metodos_importancia <- "prompt"
modelos_importancia <- c()
n_permutacoes       <- 10

#   importancia_variaveis_originais : TRUE = quando os modelos usam eixos da PCA, mede também
#                         quais VARIÁVEIS originais melhor respondem a variação no modelo
#                         Sem efeito nos modelos que usam variáveis.
importancia_variaveis_originais <- TRUE


# Autocorrelação espacial (I de Moran)
#   calcular_moran        : TRUE = testa a autocorrelação espacial
#   moran_alvos           : "residuos" -> resíduos (observado - previsto) de cada modelo final:
#                                         I de Moran global, correlograma e mapa dos resíduos
#                           "presenca" -> os próprios dados (presença = 1, background = 0)
#                           "all"      -> roda os dois métodos
#   moran_classes         : nº de classes de distância do correlograma
#   moran_distancia_max_km: alcance do correlograma e do I global (km). NULL = metade da maior
#                           distância entre os pontos
#   moran_n_max           : máximo de pontos por espécie (a matriz de distâncias é n x n).
#                           NULL = todos (pode faltar memória)
#   moran_pontos          : "todos" (presenças + background) ou "presencas"
#                           (no background o resíduo depende do sorteio dos pontos)
#   moran_suavizacao_km   : largura da suavização do mapa de resíduos (km). NULL = 3% da diagonal
#   As figuras (mapa dos resíduos + correlograma) usam 'formato_figuras' e 'resolucao_figuras'.
calcular_moran         <- TRUE
moran_alvos            <- "prompt"
moran_classes          <- 15
moran_distancia_max_km <- NULL
moran_n_max            <- 2000
moran_pontos           <- "todos"
moran_suavizacao_km    <- NULL

#---------------- Figuras finais

#   figuras_tipos : quais figuras montar:
#                   "modelos"         -> painel por espécie e conjunto: mapa contínuo de cada
#                                        algoritmo e do consenso
#                   "binarios"        -> painel por espécie, conjunto e limiar: presença/ausência
#                                        de cada algoritmo e do consenso
#                   "consenso"        -> consenso contínuo ao lado dos seus mapas binários
#                   "cenarios"        -> consenso de cada conjunto lado a lado (preditoras x cenários)
#                   "variaveis"       -> mapa de cada preditor usado nos modelos
#                   "importancia"     -> gráficos de barras da seção 22
#                   "curvas_resposta" -> curva de resposta de cada algoritmo a cada preditor
#                   "extrapolacao"    -> por espécie e conjunto: MESS, área em extrapolação (MESS < 0)
#                                        e variável mais diferente (usa os rasters da seção 21)
#   formato_figuras   : "png", "pdf"
#   figuras_layout    : "painel"    -> todos os mapas juntos num quadro
#                       "separadas" -> uma figura para cada mapa
#                       c("painel", "separadas") ou "all" -> os dois
#   Mapas específicos (em vez de todos). Cada um vale "all" (todos), um ou mais valores
#   (ex.: figuras_especies <- c("Especie a")) ou "prompt" (mostra a lista e pergunta):
#   figuras_especies  : espécies
#   figuras_conjuntos : conjuntos ("preditoras" e os cenários)
#   figuras_mapas     : algoritmos (ex.: "rf", "maxent") e/ou "consenso_<método>"
#                       (ex.: "consenso_media_ponderada")
#   figuras_limiares  : limiares dos mapas binários (ex.: "max_tss")
#   resolucao_figuras : pontos por polegada dos PNG (300 = qualidade de publicação)
#   largura_painel_cm : largura de cada quadro nos painéis (cm); a altura segue a
#                       proporção do mapa
#   paleta_figuras    : paleta do mapa contínuo, grDevices::hcl.colors()
#                       (ex.: "YlGnBu", "viridis", "Greens");
#   figuras_pontos    : TRUE = desenha os pontos de presença sobre os mapas
#   figuras_escala    : TRUE = desenha a barra de escala nos mapas (em km se as coordenadas
#                       forem geográficas)
#   figuras_norte     : TRUE = desenha a seta do norte nos mapas
#   figuras_escala_posicao: canto do mapa de cada um: "inferior_direito",
#                           "inferior_esquerdo", "superior_direito" ou "superior_esquerdo". 
#   figuras_norte_posicao : Idem ao anterior. 
#   figuras_elementos_cores : "normais" (preto) ou "invertidas" (branco)
#   figuras_margem_mapa : folga entre o mapa e as bordas do quadro, como fração da largura e da
#                         altura do mapa (0.03 = 3%; 0 = sem folga)
#   figuras_mascara   : TRUE = desenha o contorno da máscara de estudo (a usada no corte) sobre
#                       todos os mapas
#   figuras_mascara_preenchimento : cor das células sem adequabilidade (valor 0) dentro da máscara, nos
#                       mapas contínuos e binários: nome de cor do R ou hexadecimal (ex.: "lightgray").
#                       NULL = não pinta
#   figuras_mascara_cor       : cor da linha do contorno da máscara (nome de cor do R ou hexadecimal)
#   figuras_mascara_espessura : espessura da linha do contorno da máscara
#   figuras_elementos_painel  : Desenha elementos gráficos no painel: "todos" = legenda, escala e
#                       norte em todos os mapas do painel; "ultimo" = só no último mapa
#   figuras_elementos_fundo : TRUE = coloca um fundo semitransparente atrás da escala e do norte

figuras_tipos     <- c("modelos", "binarios", "consenso", "cenarios", "variaveis", "importancia", "curvas_resposta", "extrapolacao")
formato_figuras   <- "png"
figuras_layout    <- "painel"
figuras_especies  <- "all"
figuras_conjuntos <- "all"
figuras_mapas     <- "all"
figuras_limiares  <- "all"
resolucao_figuras <- 300
largura_painel_cm <- 10
paleta_figuras    <- "YlGnBu"
figuras_pontos    <- FALSE
figuras_escala    <- TRUE
figuras_norte     <- TRUE
figuras_escala_posicao  <- "inferior_direito"
figuras_norte_posicao   <- "inferior_direito"
figuras_elementos_cores <- "normais"
figuras_elementos_fundo <- FALSE
figuras_elementos_painel <- "todos"
figuras_margem_mapa       <- 0.03
figuras_mascara           <- TRUE
figuras_mascara_cor       <- "darkgray"
figuras_mascara_espessura <- 0.5
figuras_mascara_preenchimento <- NULL


#---------------- Resolver conflitos

# Rasters com grade diferente (tamanho de célula, extensão, linhas/colunas)
#   Se houverem rasters com geometria diferente, o script vai solicitar uma ação:
#   rasters_diferentes : "reamostrar" -> os de célula maior são divididos na grade do
#                                        raster de MENOR célula (vizinho mais próximo;
#                                        os originais ficam em "originais_antes_da_reamostragem")
#                        "excluir"    -> tira da análise os rasters que fogem da geometria
#                                        mais comum (lista em verificacao/rasters_excluidos.csv)
rasters_diferentes <- "prompt"           #default: "prompt"

# Variáveis com nomes diferentes entre preditoras e cenários
#   Válido apenas para rodadas com projeção em diferentes cenários. Se houver conflito 
#   de nomes nas variáveis preditoras e cenários, o script vai pedir pela correspondência:
#   nomes_variaveis_diferentes : "renomear" -> o script propõe qual variável do cenário
#                                              corresponde a cada preditora; você confere e
#                                              aceita (os arquivos NÃO são renomeados; fica em
#                                              verificacao/renomeio_variaveis.csv)
#                                "pular"    -> não faz a PCA comum, avisa, e as análises que
#                                              dependem dela não são feitas
nomes_variaveis_diferentes <- "prompt"   #default: "prompt"



# Parâmetros com "prompt": pergunta o valor no primeiro uso; valor inválido: aviso e para
registrar_parametros_pendentes()

# 3. PASTAS DE SAÍDA------------------------------------------------------------
iniciar_etapa("3. PASTAS DE SAÍDA")
# Cria, dentro da pasta do projeto, as subpastas onde os arquivos serão salvos:
#   mask/                   -> máscara usada no corte
#   variaveis_cortadas/     -> variáveis preditoras e cenários já cortados
#   variaveis_escalonadas/  -> as mesmas variáveis em z-score (rasters)
#   verificacao/            -> relatórios da verificação (CSV)
#   correlacao/             -> matriz de correlação e amostra das variáveis (CSV)
#   pontos/                 -> registros limpos, background (CSV) e mapa de esforço amostral
#   multicolinearidade/     -> resultados dos testes e variáveis finais
#   modelos/                -> dados, avaliação, modelos, mapas, consenso, JSDM e extrapolação

garantir_parametros()
pastas_saida <- criar_pastas_saida(pasta_projeto)

# 4. MÁSCARA--------------------------------------------------------------------
iniciar_etapa("4. MÁSCARA")
# Mostra as feições da máscara (se for .shp) para escolher, na etapa de corte,
# se será usada o shapefile inteiro ou apenas algumas feições.

garantir_parametros()
entradas <- obter_entradas()

tabela_mascara <- mostrar_tabela_mascara(entradas$mascara)

# Prepara a máscara que será usada no corte (inteira ou só as feições escolhidas)
mascara_corte <- preparar_mascara(
  caminho_mascara    = entradas$mascara,
  cortar_por_feicoes = cortar_por_feicoes,
  filtro_feicoes     = filtro_feicoes
)

# 5. PROJEÇÃO E SALVAMENTO DA MÁSCARA-------------------------------------------
iniciar_etapa("5. PROJEÇÃO E SALVAMENTO DA MÁSCARA")
# Verifica e padroniza a projeção da máscara e dos rasters

garantir_parametros()   
entradas      <- obter_entradas()
pastas_saida  <- obter_pastas_saida()
mascara_corte <- obter_mascara_corte()

mascara_corte <- ajustar_crs_mascara(mascara_corte, entradas$var_preditoras)

caminho_mascara_salva <- salvar_mascara(mascara_corte, pastas_saida$mask)


# 6. CORTE DAS VARIÁVEIS--------------------------------------------------------
iniciar_etapa("6. CORTE DAS VARIÁVEIS")
# Corta as variáveis preditoras e as de todos os cenários pela máscara.

garantir_parametros()   
entradas      <- obter_entradas()
pastas_saida  <- obter_pastas_saida()
mascara_corte <- obter_mascara_corte()

variaveis_cortadas <- cortar_todas_variaveis(
  entradas = entradas,
  mascara  = mascara_corte,
  pasta_saida = pastas_saida$variaveis_cortadas
)


# 7. VERIFICAÇÃO DE ARQUIVOS----------------------------------------------------
iniciar_etapa("7. VERIFICAÇÃO DE ARQUIVOS")

garantir_parametros()  
entradas           <- obter_entradas()
pastas_saida       <- obter_pastas_saida()
mascara_corte      <- obter_mascara_corte()
variaveis_cortadas <- obter_variaveis_cortadas()

# Verifica se os rasters cortados abrem sem erro, têm a mesma projeção, extensão, 
# nº de linhas/colunas, tamanho de célula, e se cobrem toda a área da máscara
verificacao_rasters <- verificar_rasters(
  variaveis_cortadas = variaveis_cortadas,
  mascara            = mascara_corte,
  pasta_saida        = pastas_saida$verificacao
)

# Se os rasters não tiverem todos a mesma grade, avisa quais são e pergunta se
# reamostra (para a menor célula) ou exclui da análise (ver 'rasters_diferentes').
# Sem isso, as etapas seguintes param com "[rast] extents do not match".
variaveis_cortadas <- tratar_rasters_diferentes(
  variaveis_cortadas = variaveis_cortadas,
  mascara            = mascara_corte,
  pasta_verificacao  = pastas_saida$verificacao
)
if (isTRUE(attr(variaveis_cortadas, "tratado"))) {
  attr(variaveis_cortadas, "tratado") <- NULL
  # houve reamostragem/exclusão: refaz a verificação com os rasters já tratados
  verificacao_rasters <- verificar_rasters(
    variaveis_cortadas = variaveis_cortadas,
    mascara            = mascara_corte,
    pasta_saida        = pastas_saida$verificacao
  )
}

colunas_registros <- obter_colunas_registros()

# Verifica se as coordenadas dos registros são válidas e caem dentro da área de estudo
verificacao_registros <- verificar_registros(
  entradas         = entradas,
  mascara          = mascara_corte,
  coluna_longitude = colunas_registros$longitude,
  coluna_latitude  = colunas_registros$latitude,
  crs_registros    = crs_registros,
  pasta_saida      = pastas_saida$verificacao
)

# Descarta registros inválidos, fora da área de estudo e em células sem valor.
# Duplicados só são removidos se ativar nos parâmetros.

registros_limpos <- preparar_registros(
  entradas           = entradas,
  mascara            = mascara_corte,
  arquivos_variaveis = variaveis_cortadas[["preditoras"]],
  coluna_longitude   = colunas_registros$longitude,
  coluna_latitude    = colunas_registros$latitude,
  crs_registros      = crs_registros,
  remover_duplicados = remover_duplicados,
  um_por_celula      = um_registro_por_celula,
  coluna_especie     = colunas_registros$especie
)

# 8. CORRELAÇÃO ENTRE AS VARIÁVEIS----------------------------------------------
iniciar_etapa("8. CORRELAÇÃO ENTRE AS VARIÁVEIS")
# Calcula a matriz de correlação (todos os pares de variáveis) para cada conjunto:

garantir_parametros() 
pastas_saida       <- obter_pastas_saida()
variaveis_cortadas <- obter_variaveis_cortadas()

if (pulado(metodo_correlacao)) {
  correlacoes <- NULL
  message("'metodo_correlacao' = NA: correlação pulada.")
} else {
  correlacoes <- calcular_correlacao_todos(
    variaveis_cortadas = variaveis_cortadas,
    n_amostra          = n_amostra_correlacao,
    metodo             = metodo_correlacao,
    semente            = semente,
    pasta_saida        = pastas_saida$correlacao,
    figura             = ler_parametro("figura_correlacao_mapa"),
    limiar_forte       = limiar_filtro_correlacao,
    formatos           = ler_parametro("formato_figuras"),
    resolucao          = resolucao_figuras
  )
}

# 9. BACKGROUND----------------------------------------------------------------
iniciar_etapa("9. BACKGROUND")
# Sorteia o background/pseudoausências dentro de um buffer em volta dos registros,
# SEPARADAMENTE para cada espécie
# Cada espécie recebe fator_background x registros pontos, com o piso de
# 'minimo_background'. Se 'vies_background' for diferente de "nenhum", é feito
# o mapa do esforço de coleta e o sorteio segue esse mapa

garantir_parametros()   
entradas           <- obter_entradas()
pastas_saida       <- obter_pastas_saida()
variaveis_cortadas <- obter_variaveis_cortadas()
mascara_corte      <- obter_mascara_corte()
registros_limpos   <- obter_registros_limpos()
colunas_registros  <- obter_colunas_registros()

esforco <- preparar_esforco(
  vies               = vies_background,
  registros          = registros_limpos$registros,
  arquivos_variaveis = variaveis_cortadas[["preditoras"]],
  raio_kernel_km     = raio_kernel_km,
  caminho_esforco    = caminho_esforco,
  formato            = entradas$formato_registros,
  coluna_longitude   = colunas_registros$longitude,
  coluna_latitude    = colunas_registros$latitude,
  crs_registros      = crs_registros,
  pasta_saida        = pastas_saida$pontos,
  entradas           = entradas,
  mascara            = mascara_corte
)

background <- gerar_background(
  registros          = registros_limpos$registros,
  arquivos_variaveis = variaveis_cortadas[["preditoras"]],
  distancia_km       = distancia_buffer_km,
  fator              = fator_background,
  minimo             = minimo_background,
  esforco            = esforco,
  semente            = semente,
  raio_pseudoausencia_km = raio_pseudoausencia_km
)

pontos <- salvar_pontos(registros_limpos$registros, background, pastas_saida$pontos)

# 10. ESCALONAMENTO DE VARIÁVEIS------------------------------------------------
iniciar_etapa("10. ESCALONAMENTO DE VARIÁVEIS")
# Padroniza as variáveis em z-score. Os cenários usam a média e o desvio das
# PREDITORAS, para continuarem comparáveis.

garantir_parametros()   
pastas_saida       <- obter_pastas_saida()
variaveis_cortadas <- obter_variaveis_cortadas()

dados_testes <- preparar_dados_testes(
  variaveis_cortadas = variaveis_cortadas,
  n_amostra          = n_amostra_testes,
  semente            = semente
)

variaveis_escalonadas <- salvar_variaveis_escalonadas(
  variaveis_cortadas = variaveis_cortadas,
  parametros         = dados_testes[["preditoras"]]$parametros,
  pasta_saida        = pastas_saida$variaveis_escalonadas
)

# 11. ANÁLISE DE MULTICOLINEARIDADE---------------------------------------------
iniciar_etapa("11. ANÁLISE DE MULTICOLINEARIDADE")

if (so_pca(ler_parametro("preditores_modelos"))) {
  message("preditores_modelos = \"pca\": testes de multicolinearidade pulados (a PCA da seção 12 não depende deles).")
} else {
  garantir_parametros()   
  pastas_saida       <- obter_pastas_saida()
  dados_testes       <- obter_dados_testes()
  pontos             <- obter_pontos()
  variaveis_cortadas <- obter_variaveis_cortadas()

  # Registra quais métodos você quer usar. Eles são executados na seção 11.
  metodos_escolhidos <- escolher_metodos_multicolinearidade(metodos_multicolinearidade)

  # Testes sem variável resposta (VIF, filtro por correlação, cluster, PCA)
  resultados_sem_resposta <- rodar_testes_sem_resposta(
    dados_testes   = dados_testes,
    metodos        = metodos_escolhidos,
    pasta_saida    = file.path(pastas_saida$multicolinearidade, "sem_resposta"),
    limite_vif     = limite_vif,
    limiar_filtro  = limiar_filtro_correlacao,
    limiar_cluster = limiar_cluster,
    variancia_pca  = variancia_pca,
    n_eixos_pca    = n_eixos_pca
  )

  # Testes com variável resposta (lasso, ridge, elastic net, PCR, PLSR, CPPLS, Boruta)
  dados_resposta <- preparar_dados_resposta(
    pontos           = pontos,
    arquivos_preditoras  = variaveis_cortadas[["preditoras"]],
    minimo_presencas = minimo_presencas_testes
  )

  resultados_com_resposta <- rodar_testes_com_resposta(
    dados_resposta   = dados_resposta,
    metodos          = metodos_escolhidos,
    pasta_saida      = file.path(pastas_saida$multicolinearidade, "com_resposta"),
    alfa_elastic_net = alfa_elastic_net,
    semente          = semente
  )
}


# 12. PCA-----------------------------------------------------------------------
iniciar_etapa("12. PCA")
# Alternativa à escolha de variáveis: usa os eixos da PCA como novas variáveis.
# A PCA é ajustada nas PREDITORAS e os cenários são projetados nos mesmos eixos.
# Se preditoras e cenários tiverem variáveis diferentes, é feita uma segunda PCA só
# com as variáveis comuns a todos os conjuntos (a usada nos modelos de projeção).

garantir_parametros()   
pastas_saida       <- obter_pastas_saida()
variaveis_cortadas <- obter_variaveis_cortadas()
dados_testes       <- obter_dados_testes()

pca_eixos <- gerar_pca_eixos(
  dados_testes       = dados_testes,
  variaveis_cortadas = variaveis_cortadas,
  pasta_saida        = file.path(pastas_saida$multicolinearidade, "pca_eixos"),
  variancia_alvo     = variancia_pca,
  n_eixos            = n_eixos_pca
)

# 13. ESCOLHA FINAL DAS VARIÁVEIS-----------------------------------------------
iniciar_etapa("13. ESCOLHA FINAL DAS VARIÁVEIS")
# Sumariza as sugestões de todos os métodos em uma tabela para decisão de quais
# variáveis serão usadas nos modelos
# Retorna o VIF e o maior |r| das variáveis escolhidas, nas preditoras e em
# cada cenário. Se aparecer ATENÇÃO, ajuste 'variaveis_mantidas' e rode de novo.

if (so_pca(ler_parametro("preditores_modelos"))) {
  message("preditores_modelos = \"pca\": escolha final das variáveis pulada (os modelos usam os eixos da PCA).")
} else {
  garantir_parametros() 
  pastas_saida            <- obter_pastas_saida()
  dados_testes            <- obter_dados_testes()
  resultados_sem_resposta <- obter_resultados_sem_resposta()
  resultados_com_resposta <- obter_resultados_com_resposta()

  tabela_sugestoes <- consolidar_sugestoes(
    variaveis      = colnames(dados_testes[["preditoras"]]$dados),
    resultados_sem = resultados_sem_resposta,
    resultados_com = resultados_com_resposta,
    dados_testes   = dados_testes
  )

  variaveis_finais <- escolher_variaveis_finais(
    tabela             = tabela_sugestoes,
    variaveis_mantidas = variaveis_mantidas,
    pasta_saida        = pastas_saida$multicolinearidade
  )

  checagem_final <- checar_conjunto_final(
    variaveis    = variaveis_finais,
    dados_testes = dados_testes,
    pasta_saida  = pastas_saida$multicolinearidade,
    limite_vif   = limite_vif,
    limiar_r     = limiar_filtro_correlacao
  )
}


# 14. MODELOS E PREDITORES------------------------------------------------------
iniciar_etapa("14. MODELOS E PREDITORES")
# Define os algoritmos e os preditores para modelar.

garantir_parametros() 
pastas_saida       <- obter_pastas_saida()
modelos_escolhidos <- escolher_modelos(modelos)

rodadas_modelos <- montar_rodadas_modelos(
  tipo              = preditores_modelos,
  variaveis         = variaveis_modelos,
  pca               = pca_modelos,
  eixos             = eixos_pca_modelos,
  pasta_escalonadas = pastas_saida$variaveis_escalonadas,
  pasta_pca         = file.path(pastas_saida$multicolinearidade, "pca_eixos"),
  pasta_modelos     = file.path(pasta_projeto, "modelos"),
  variaveis_finais  = obter_variaveis_finais()
)

# SEÇÕES 16 A 24: RODADAS DOS MODELOS-------------------------------------------
# As próximas seções executam UMA VEZ para cada rodada de 'rodadas_modelos':
# uma com as variáveis, outra com os eixos da PCA, ou só delas, conforme a escolha.
# Cada rodada começa limpando da sessão os objetos da anterior e salva na sua
# pasta.

resposta_independentes <- NULL
arquivo_independente   <- NULL

# Quais rodadas executar ('rodadas_executar'):
#   "all"            -> todas
#   nomes ou números -> ex.: "variaveis", "pca_preditoras_completa", "pca_comum"


# Para trabalhar numa seção isolada coloque o nome em rodadas_executar e ativar_rodada
# ativar_rodada("pca")  ou  ativar_rodada(2)  ou  ativar_rodada(rodadas_modelos[["pca"]])

rodadas_executar <- "prompt"


for (rodada in selecionar_rodadas(rodadas_modelos, rodadas_executar)) {

  ativar_rodada(rodada)                    #default: rodada


  # 15. DADOS DOS MODELOS-------------------------------------------------------
  iniciar_etapa("15. DADOS DOS MODELOS", nome_rodada_log())
  # Lê os preditores nos pontos de presença e background de cada espécie, calcula
  # os pesos (presenças e background com o mesmo total) e define os grupos de cada
  # esquema de validação de 'tipo_validacao'.

  garantir_parametros()
  pastas_saida <- obter_pastas_saida()
  pontos       <- obter_pontos()
  preditores   <- obter_preditores()

  dados_modelos <- preparar_dados_modelos(
    pontos           = pontos,
    preditores       = preditores,
    pasta_saida      = file.path(pastas_saida$modelos, "dados"),
    minimo_presencas = minimo_presencas_modelos,
    tipo_validacao   = tipo_validacao,
    n_folds          = n_folds,
    semente          = semente,
    particao_teste   = particao_teste,
    particao_metodo  = particao_metodo
  )


  # 16. AVALIAÇÃO DOS MODELOS (validação cruzada)-------------------------------
  iniciar_etapa("16. AVALIAÇÃO DOS MODELOS (validação cruzada)", nome_rodada_log())
  # Cada grupo (ou presença, no jackknife) fica de fora uma vez: o modelo é
  # ajustado sem ele e testado nele. Métricas: AUC, TSS, Kappa, Boyce e taxa de
  # omissão (limiares MTP e P10 do treino). 
  # Antes, se o Maxent foi escolhido e testar_ajustes_maxent = TRUE, testa as
  # combinações de features e regmult com a validação principal e escolhe a
  # melhor para cada espécie

  garantir_parametros()   
  pastas_saida       <- obter_pastas_saida()
  modelos_escolhidos <- obter_modelos_escolhidos()
  dados_modelos      <- obter_dados_modelos()

  if (!pulado(tipo_validacao) && "maxent" %in% modelos_escolhidos && testar_ajustes_maxent) {
    ajuste_maxent <- selecionar_ajuste_maxent(
      dados_modelos = dados_modelos,
      ajustes       = ajustes_modelos,
      validacoes    = tipo_validacao,
      classes       = maxent_classes_testadas,
      regmult       = maxent_regmult_testados,
      pasta_saida   = file.path(pastas_saida$modelos, "avaliacao"),
      semente       = semente
    )
  } else {
    ajuste_maxent <- NULL
  }

  avaliacao <- if (pulado(tipo_validacao)) {
    message("'tipo_validacao' = NA: validação cruzada pulada.")
    NULL
  } else avaliar_modelos(
    dados_modelos = dados_modelos,
    modelos       = modelos_escolhidos,
    ajustes       = ajustes_modelos,
    validacoes    = tipo_validacao,
    pasta_saida   = file.path(pastas_saida$modelos, "avaliacao"),
    semente       = semente,
    ajuste_maxent = ajuste_maxent,
    preditores    = preditores
  )


  # 17. MODELOS FINAIS, LIMIARES E MAPAS----------------------------------------
  iniciar_etapa("17. MODELOS FINAIS, LIMIARES E MAPAS", nome_rodada_log())
  # Ajusta cada algoritmo com todos os pontos da espécie, calcula os limiares de
  # 'metodos_limiar' e projeta nas preditoras e nos cenários: mapa contínuo (0 a 1) e
  # um mapa de presença/ausência para cada limiar.

  garantir_parametros()   
  pastas_saida       <- obter_pastas_saida()
  modelos_escolhidos <- obter_modelos_escolhidos()
  preditores         <- obter_preditores()
  dados_modelos      <- obter_dados_modelos()
  avaliacao          <- obter_avaliacao()
  ajuste_maxent      <- obter_ajuste_maxent()

  modelos_finais <- ajustar_modelos_finais(
    dados_modelos = dados_modelos,
    modelos       = modelos_escolhidos,
    ajustes       = ajustes_modelos,
    pasta_saida   = file.path(pastas_saida$modelos, "objetos"),
    semente       = semente,
    ajuste_maxent = ajuste_maxent,
    somente_treino = !reajustar_modelo_final
  )

  limiares <- if (pulado(metodos_limiar)) {
    message("'metodos_limiar' = NA: limiares e mapas binários pulados (só os mapas contínuos).")
    NULL
  } else calcular_limiares(
    modelos_finais = modelos_finais,
    dados_modelos  = dados_modelos,
    avaliacao      = avaliacao,
    metodos        = metodos_limiar,
    limiar_fixo    = limiar_fixo,
    pasta_saida    = file.path(pastas_saida$modelos, "limiares"),
    preditores     = preditores
  )

  mapas <- gerar_mapas(
    modelos_finais  = modelos_finais,
    preditores      = preditores,
    limiares        = limiares$tabela,
    pasta_saida     = file.path(pastas_saida$modelos, "mapas"),
    salvar_binarios = salvar_binarios
  )

  # 18. CONSENSO----------------------------------------------------------------
  iniciar_etapa("18. CONSENSO", nome_rodada_log())
  # Sumariza os mapas dos algoritmos de cada espécie. Entram os modelos com TSS
  # médio >= 'limiar_tss_consenso'. Também gera os mapas de presença/ausência do
  # consenso, com limiares calculados para o próprio consenso 

  garantir_parametros()   
  pastas_saida <- obter_pastas_saida()
  avaliacao    <- obter_avaliacao()
  limiares     <- obter_limiares()
  mapas        <- obter_mapas()

  consenso <- if (pulado(metodos_consenso) || pulado(tipo_validacao)) {
    message("Consenso pulado ('metodos_consenso' = NA ou sem validação, que dá o TSS).")
    NULL
  } else gerar_consenso(
    mapas           = mapas,
    avaliacao       = avaliacao,
    limiares        = limiares,
    metodos         = metodos_consenso,
    limiar_tss      = limiar_tss_consenso,
    metodos_limiar  = metodos_limiar,
    limiar_fixo     = limiar_fixo,
    limiar_votacao  = limiar_votacao,
    pasta_saida     = file.path(pastas_saida$modelos, "consenso"),
    salvar_binarios = salvar_binarios,
    preditores      = preditores
  )

  # 19. JSDM (todas as espécies juntas, fora do consenso)-----------------------
  iniciar_etapa("19. JSDM (todas as espécies juntas, fora do consenso)", nome_rodada_log())
  # Só roda se "jsdm" estiver em 'modelos'. Os sítios são as células com presença
  # de qualquer espécie ou com background; onde a espécie não tem registro conta
  # como ausência (suposta).

  garantir_parametros()   # parâmetros da seção 2 (carrega do main.R se faltarem)
  pastas_saida       <- obter_pastas_saida()
  modelos_escolhidos <- obter_modelos_escolhidos()
  pontos             <- obter_pontos()
  preditores         <- obter_preditores()

  if ("jsdm" %in% modelos_escolhidos && pulado(tipo_validacao)) {
    message("'tipo_validacao' = NA: o JSDM (que usa validação cruzada) foi pulado.")
  } else if ("jsdm" %in% modelos_escolhidos) {
    jsdm <- rodar_jsdm(
      pontos          = pontos,
      preditores      = preditores,
      ajustes         = ajustes_modelos,
      pasta_saida     = file.path(pastas_saida$modelos, "jsdm"),
      tipo_validacao  = tipo_validacao,
      n_folds         = n_folds,
      salvar_binarios = salvar_binarios,
      metodos_limiar  = metodos_limiar,
      limiar_fixo     = limiar_fixo,
      semente         = semente
    )
  } else {
    message("O JSDM não está em 'modelos': seção pulada.")
  }


  # 20. VALIDAÇÃO INDEPENDENTE--------------------------------------------------
  iniciar_etapa("20. VALIDAÇÃO INDEPENDENTE", nome_rodada_log())
  # Avalia os modelos finais, o consenso e o JSDM com dados que NÃO entraram no
  # ajuste.

  garantir_parametros()   
  pastas_saida       <- obter_pastas_saida()
  
  if (is.null(resposta_independentes)) {
    resposta_independentes <- perguntar_dados_independentes(dados_independentes)
  }
  usar_independentes <- resposta_independentes

  if (usar_independentes) {
    preditores     <- obter_preditores()
    dados_modelos  <- obter_dados_modelos()
    modelos_finais <- obter_modelos_finais()
    limiares       <- obter_limiares()
    consenso       <- obter_consenso()
    jsdm           <- obter_jsdm()

    if (is.null(arquivo_independente)) {
      arquivo_independente <- selecionar_arquivo(caminho_independente,
                                                 "Selecione os dados independentes (.csv ou .txt)",
                                                 c("csv", "txt"))
    }

    independentes <- ler_dados_independentes(
      caminho          = arquivo_independente,
      crs_registros    = crs_registros,
      preditores       = preditores
    )

    validacao_independente <- avaliar_independente(
      independentes  = independentes,
      dados_modelos  = dados_modelos,
      modelos_finais = modelos_finais,
      limiares       = limiares,
      preditores     = preditores,
      consenso       = consenso,
      jsdm           = jsdm,
      pasta_saida    = file.path(pastas_saida$modelos, "validacao_independente")
    )
  } else {
    message("Validação independente não realizada.")
  }

  # 21. EXTRAPOLAÇÃO (MESS)-----------------------------------------------------
  iniciar_etapa("21. EXTRAPOLAÇÃO (MESS)", nome_rodada_log())
  # Mostra onde os mapas (preditoras e cenários) usam valores dos preditores FORA da
  # faixa dos pontos de ajuste de cada espécie (presenças + background). Nesses
  # lugares o modelo está extrapolando e os mapas devem ser lidos com cuidado.
  # Para cada espécie e conjunto: MESS (negativo = extrapolação), mapa binário da
  # extrapolação e o preditor responsável. 
  
  garantir_parametros()   
  pastas_saida  <- obter_pastas_saida()
  preditores    <- obter_preditores()
  dados_modelos <- obter_dados_modelos()

  extrapolacao <- calcular_extrapolacao(
    dados_modelos = dados_modelos,
    preditores    = preditores,
    pasta_saida   = file.path(pastas_saida$modelos, "extrapolacao")
  )

  # 22. IMPORTÂNCIA DAS VARIÁVEIS-----------------------------------------------
  iniciar_etapa("22. IMPORTÂNCIA DAS VARIÁVEIS", nome_rodada_log())
  # Avalia o quanto cada preditor pesa nos modelos. Com eixos da PCA, a importância
  # é dos eixos e, se 'importancia_variaveis_originais' = TRUE, também das variáveis originais.

  garantir_parametros()   
  pastas_saida       <- obter_pastas_saida()
  modelos_escolhidos <- obter_modelos_escolhidos()
  dados_modelos      <- obter_dados_modelos()
  modelos_finais     <- obter_modelos_finais()
  preditores         <- obter_preditores()
  ajuste_maxent      <- obter_ajuste_maxent()

  importancia <- calcular_importancia(
    dados_modelos  = dados_modelos,
    modelos_finais = modelos_finais,
    modelos        = modelos_importancia,
    metodos        = metodos_importancia,
    ajustes        = ajustes_modelos,
    validacoes     = tipo_validacao,
    n_folds        = n_folds,
    n_permutacoes  = n_permutacoes,
    pasta_saida    = file.path(pastas_saida$modelos, "importancia"),
    semente        = semente,
    ajuste_maxent  = ajuste_maxent,
    preditores     = preditores,
    pca_info       = argumentos_importancia_pca()
  )

  # 23. AUTOCORRELAÇÃO ESPACIAL (I DE MORAN)-----------------------------------
  iniciar_etapa("23. AUTOCORRELAÇÃO ESPACIAL (I DE MORAN)", nome_rodada_log())
  # I de Moran dos dados e dos resíduos dos modelos finais (observado - previsto), com o
  # correlograma e o mapa dos resíduos.

  garantir_parametros()
  if (!isTRUE(calcular_moran)) {
    message("'calcular_moran' = FALSE: autocorrelação espacial pulada.")
    autocorrelacao <- NULL
  } else {
    pastas_saida   <- obter_pastas_saida()
    preditores     <- obter_preditores()
    dados_modelos  <- obter_dados_modelos()
    modelos_finais <- obter_modelos_finais()

    autocorrelacao <- calcular_autocorrelacao(
      dados_modelos    = dados_modelos,
      modelos_finais   = modelos_finais,
      preditores       = preditores,
      alvos            = moran_alvos,
      n_classes        = moran_classes,
      distancia_max_km = moran_distancia_max_km,
      n_max            = moran_n_max,
      pontos           = moran_pontos,
      suavizacao_km    = moran_suavizacao_km,
      pasta_saida      = file.path(pastas_saida$modelos, "autocorrelacao"),
      figuras          = TRUE,
      formatos         = ler_parametro("formato_figuras"),
      resolucao        = resolucao_figuras,
      semente          = semente
    )
  }

  # 24. FIGURAS-----------------------------------------------------------------
  iniciar_etapa("24. FIGURAS", nome_rodada_log())
  # Monta figuras prontas (PNG e/ou PDF) dos mapas, do consenso, da importância
  # das variáveis e das curvas de resposta

  garantir_parametros()   
  pastas_saida   <- obter_pastas_saida()
  preditores     <- obter_preditores()
  dados_modelos  <- obter_dados_modelos()
  modelos_finais <- obter_modelos_finais()
  mapas          <- obter_mapas()
  consenso       <- obter_consenso()
  tipos_figuras  <- normalizar_tipos_figuras(figuras_tipos)
  importancia_fig <- if ("importancia" %in% tipos_figuras) obter_importancia() else NULL

  figuras <- gerar_figuras(
    mapas          = mapas,
    consenso       = consenso,
    importancia    = importancia_fig,
    modelos_finais = modelos_finais,
    dados_modelos  = dados_modelos,
    tipos          = tipos_figuras,
    formatos       = formato_figuras,
    pasta_saida    = file.path(pastas_saida$modelos, "figuras"),
    resolucao      = resolucao_figuras,
    largura_painel = largura_painel_cm,
    paleta         = paleta_figuras,
    usar_pontos    = figuras_pontos,
    preditores     = preditores,
    layout         = ler_parametro("figuras_layout"),
    escala         = ler_parametro("figuras_escala"),
    norte          = ler_parametro("figuras_norte"),
    posicao_escala  = ler_parametro("figuras_escala_posicao"),
    posicao_norte   = ler_parametro("figuras_norte_posicao"),
    cores_elementos = if (isTRUE(ler_parametro("figuras_escala")) || isTRUE(ler_parametro("figuras_norte")))
                        ler_parametro("figuras_elementos_cores") else "normais",
    mascara           = if (isTRUE(ler_parametro("figuras_mascara"))) {
                          # sessão -> mask/ -> gerada com os parâmetros da seção 2 (pergunta se faltar algo)
                          tryCatch(obter_mascara_corte(), error = function(e) {
                            message("Aviso: máscara não obtida (", conditionMessage(e),
                                    "). As figuras serão feitas sem o contorno.")
                            NULL
                          })
                        } else NULL,
    margem_mapa       = ler_parametro("figuras_margem_mapa"),
    mascara_cor       = ler_parametro("figuras_mascara_cor"),
    mascara_espessura = ler_parametro("figuras_mascara_espessura"),
    mascara_preenchimento = ler_parametro("figuras_mascara_preenchimento"),
    elementos_painel = if (any(c("painel", "all") %in% ler_parametro("figuras_layout")))
                         ler_parametro("figuras_elementos_painel") else "todos",
    fundo_elementos = if (isTRUE(ler_parametro("figuras_escala")) || isTRUE(ler_parametro("figuras_norte")))
                        ler_parametro("figuras_elementos_fundo") else FALSE,
    sel_especies   = ler_parametro("figuras_especies"),
    sel_conjuntos  = ler_parametro("figuras_conjuntos"),
    sel_mapas      = ler_parametro("figuras_mapas"),
    sel_limiares   = ler_parametro("figuras_limiares")
  )

}

# Fecha o log: hora de término e duração (ver logs/ na pasta do projeto)
finalizar_log()
