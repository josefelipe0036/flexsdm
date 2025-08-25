#-----------------------------------------------------------------------
# SCRIPT DE BENCHMARK FINAL E DEFINITIVO
#-----------------------------------------------------------------------

# 1. Carregar pacotes
library(bench)
library(sdmtools)
library(dplyr)
library(future)
library(furrr)
library(purrr)

# 2. Carregar suas funções
source("fit_glm_original.R")
source("fit_glm_parallel.R")

# 3. Preparar dados para um teste REALMENTE pesado
message("Preparando um conjunto de dados pesado (500 modelos complexos)...")
data("abies")
set.seed(123)
abies_truly_heavy <- part_random(
  data = abies,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 30, replicates = 10) # 500 partições
)

# 4. TESTE DE SANIDADE: Medir o tempo da tarefa serial sozinha
message("Executando teste de sanidade na versão serial... Aguarde.")
tempo_serial <- system.time({
  fit_glm(
    data = abies_truly_heavy,
    response = "pr_ab",
    # AUMENTANDO A COMPLEXIDADE: Todos os preditores originais + termos polinomiais
    predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
    predictors_f = c("landform"),
    poly = 2, # <-- ISSO DEIXARÁ CADA MODELO MAIS LENTO
    partition = ".part",
    thr = c("max_sens_spec")
  )
})
message(paste("\n--- Tempo do teste de sanidade serial:", round(tempo_serial['elapsed'], 2), "segundos ---\n"))

# Se o tempo acima for maior que 20-30 segundos, o benchmark a seguir fará sentido.

# 5. Executar o benchmark final e definitivo
message("Iniciando o benchmark final... Por favor, aguarde, isso vai levar vários minutos.")
N_CORES <- 1

comparison_final <- bench::mark(
  `Serial (1 núcleo)` = {
    fit_glm(
      data = abies_truly_heavy,
      response = "pr_ab",
      predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
      predictors_f = c("landform"),
      poly = 2,
      partition = ".part",
      thr = c("max_sens_spec")
    )
  },
  `Paralelo` = {
    fit_glm_parallel(
      data = abies_truly_heavy,
      response = "pr_ab",
      predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
      predictors_f = c("landform"),
      poly = 2,
      partition = ".part",
      thr = c("max_sens_spec"),
      n_cores = 1
    )
  },
  check = FALSE,
  min_iterations = 2,
  time_unit = "s"
)

# 6. Visualizar os resultados
message("\n--- Resultados Finais do Benchmark Definitivo ---")
print(comparison_final)

message(paste0("\n--- Resumo Relativo (Speedup) em ", N_CORES, " núcleos ---"))
summary(comparison_final, relative = TRUE)

plot(comparison_final)
