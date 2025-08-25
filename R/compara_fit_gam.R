#-----------------------------------------------------------------------
# SCRIPT DE BENCHMARK FINAL E DEFINITIVO PARA GAM vs GAM PARALELO
#-----------------------------------------------------------------------

# 1. Carregar pacotes
# (Certifique-se de que todos estão instalados: install.packages(c("bench", "sdmtools", ...)))
library(bench)
library(sdmtools)
library(dplyr)
library(future)
library(furrr)
library(mgcv) # GAMs precisam deste pacote

# 2. Carregar suas funções de GAM
# IMPORTANTE: Salve as funções que gerei para você em arquivos com estes nomes
source("fit_gam.R") # A função original, serial
source("fit_gam_parallel.R") # A função otimizada, paralela

# 3. Preparar dados para um teste REALMENTE pesado
# Esta parte é idêntica, pois a partição dos dados é independente do modelo.
message("Preparando um conjunto de dados pesado (300 modelos)...")
data("abies")
set.seed(123)
abies_truly_heavy <- part_random(
  data = abies,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 3, replicates = 10) # 300 partições
)

# 4. TESTE DE SANIDADE: Medir o tempo da tarefa serial sozinha
message("Executando teste de sanidade na versão serial do GAM... Aguarde.")
tempo_serial_gam <- system.time({
  fit_gam(
    data = abies_truly_heavy,
    response = "pr_ab",
    # Usando os mesmos preditores para uma comparação justa
    predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
    predictors_f = c("landform"),
    # O argumento 'poly' foi removido. A complexidade do GAM é controlada por 'k' (padrão k=-1).
    partition = ".part",
    thr = c("max_sens_spec")
  )
})
message(paste("\n--- Tempo do teste de sanidade serial (GAM):", round(tempo_serial_gam['elapsed'], 2), "segundos ---\n"))

# Se o tempo acima for significativo (ex: > 30 segundos), o benchmark a seguir será muito revelador.

# 5. Executar o benchmark final e definitivo
# --- CONFIGURAÇÃO DE NÚCLEOS ---
# Vamos usar metade dos núcleos disponíveis para um teste realista.
# Altere este valor conforme sua necessidade.
N_CORES <- 1#future::availableCores() %/% 2 
if (N_CORES == 0) N_CORES <- 1 # Garante pelo menos 1 núcleo

message(paste("Iniciando o benchmark final para GAM usando", N_CORES, "núcleo(s)... \nIsso pode levar vários minutos."))

comparison_final_gam <- bench::mark(
  `Serial (1 núcleo)` = {
    fit_gam(
      data = abies_truly_heavy,
      response = "pr_ab",
      predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
      predictors_f = c("landform"),
      partition = ".part",
      thr = c("max_sens_spec")
    )
  },
  `Paralelo` = {
    fit_gam_parallel(
      data = abies_truly_heavy,
      response = "pr_ab",
      predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
      predictors_f = c("landform"),
      partition = ".part",
      thr = c("max_sens_spec"),
      n_cores = N_CORES # <-- Usando a variável de núcleos aqui
    )
  },
  check = FALSE,      # Não verifica se os resultados são idênticos (podem ter pequenas diferenças)
  min_iterations = 2, # Roda cada expressão pelo menos 2 vezes
  time_unit = "s"     # Exibe o tempo em segundos
)

# 6. Visualizar os resultados
message("\n--- Resultados Finais do Benchmark para GAM ---")
print(comparison_final_gam)

# O summary relativo mostra o "speedup": quantas vezes a versão paralela foi mais rápida.
message(paste0("\n--- Resumo Relativo (Speedup) em ", N_CORES, " núcleo(s) ---"))
summary(comparison_final_gam, relative = TRUE)

# O plot oferece uma visualização clara da distribuição dos tempos de execução.
plot(comparison_final_gam)
