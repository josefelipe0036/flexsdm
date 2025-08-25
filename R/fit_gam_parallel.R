#' Fit and validate Generalized Additive Models in parallel
#'
#' @param data data.frame. Database with response (0,1) and predictors values.
#' @param response character. Column name with species absence-presence data (0,1).
#' @param predictors character. Vector with the column names of quantitative
#' predictor variables (i.e. continuous variables).
#' @param predictors_f character. Vector with the column names of qualitative
#' predictor variables (i.e. factors).
#' @param partition character. Column name with training and validation partition groups.
#' @param thr character. Vector of threshold types (e.g., 'max_sens_spec').
#' @param fit_formula formula. A formula object (e.g., pr_ab ~ s(aet) + landform).
#' @param k integer. The dimension of the basis for smooth terms. Default is -1 (uses mgcv's default).
#' @param n_cores integer. The number of CPU cores to use for parallel processing. Default is 1.
#'
#' @return A list containing the final model, performance metrics, and data for ensembles.
#'
#' @importFrom dplyr bind_rows group_by summarise across tibble relocate where
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession sequential
#' @importFrom mgcv gam predict.gam s
#' @importFrom stats complete.cases formula na.exclude sd
#'
fit_gam_parallel <- function(data,
                             response,
                             predictors,
                             predictors_f = NULL,
                             partition,
                             thr = NULL,
                             fit_formula = NULL,
                             k = -1,
                             n_cores = 1) {
  # --- 1. Pré-processamento e Validação ---
  variables <- c(predictors, predictors_f)
  
  data <- as.data.frame(data)
  if (!is.null(predictors_f)) {
    data[predictors_f] <- lapply(data[predictors_f], factor)
  }
  
  # Remove NAs de forma eficiente
  complete_vec <- stats::complete.cases(data[, c(response, variables)])
  if (sum(!complete_vec) > 0) {
    message(sum(!complete_vec), " rows were excluded from database due to NAs.")
    data <- data[complete_vec, , drop = FALSE]
  }
  
  # --- 2. Criação da Fórmula ---
  if (is.null(fit_formula)) {
    s_terms <- paste0("s(", predictors, ", k = ", k, ")", collapse = " + ")
    all_terms <- paste(c(s_terms, predictors_f), collapse = " + ")
    formula1 <- stats::formula(paste(response, "~", all_terms))
  } else {
    formula1 <- fit_formula
  }
  message("Formula used for model fitting:\n", deparse1(formula1), "\n")
  
  # --- 3. Execução Paralela da Validação Cruzada ---
  p_names <- grep(paste0("^", partition), names(data), value = TRUE)
  np <- length(p_names)
  
  if (n_cores > 1) {
    future::plan(future::multisession, workers = n_cores)
    message("Using ", n_cores, " cores for parallel processing.")
  } else {
    future::plan(future::sequential)
  }
  
  parallel_results <- furrr::future_map(p_names, function(p_col) {
    train_idx <- which(data[[p_col]] == 1)
    test_idx <- which(data[[p_col]] == 0)
    
    # Pula partições inválidas
    if (length(train_idx) == 0 || length(test_idx) == 0 ||
        length(unique(data[[response]][train_idx])) < 2) {
      return(NULL)
    }
    
    train_data <- data[train_idx, , drop = FALSE]
    test_data <- data[test_idx, , drop = FALSE]
    
    # Ajusta o modelo GAM
    mod <- tryCatch(
      mgcv::gam(formula1, data = train_data, family = "binomial"),
      error = function(e) NULL
    )
    
    if (is.null(mod)) return(NULL)
    
    # Predição e avaliação
    pr_ab_test <- test_data[[response]]
    preds <- mgcv::predict.gam(mod, newdata = test_data, type = "response")
    
    pred_test <- data.frame(
      pr_ab = pr_ab_test,
      pred = preds,
      rnames = rownames(test_data)
    )
    
    eval <- sdm_eval(
      p = preds[pr_ab_test == 1],
      a = preds[pr_ab_test == 0],
      thr = thr
    )
    
    list(
      eval_results = dplyr::tibble(model = "gam", eval),
      pred_results = pred_test
    )
  }, .options = furrr_options(seed = TRUE))
  
  # Limpa o plano de paralelismo
  future::plan(future::sequential)
  
  # --- 4. Agregação dos Resultados ---
  valid_results <- parallel_results[!sapply(parallel_results, is.null)]
  
  if (length(valid_results) == 0) {
    warning("No valid partitions were successfully processed. Returning NULL.")
    return(NULL)
  }
  
  eval_partial <- dplyr::bind_rows(lapply(valid_results, `[[`, "eval_results"), .id = "replica")
  
  # Sumarização robusta das métricas de validação cruzada
  eval_final_cv <- eval_partial %>%
    dplyr::group_by(model, threshold) %>%
    dplyr::summarise(
      dplyr::across(where(is.numeric), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
  
  pred_test_ens <- dplyr::bind_rows(lapply(valid_results, `[[`, "pred_results"), .id = "replica")
  
  # --- 5. Modelo Final e Retorno ---
  message("Fitting final model on all data...")
  mod_final <- mgcv::gam(formula1, data = data, family = "binomial")
  
  # Performance do modelo final
  pred_final <- mgcv::predict.gam(mod_final, newdata = data, type = "response")
  performance_final <- sdm_eval(
    p = pred_final[data[[response]] == 1],
    a = pred_final[data[[response]] == 0],
    thr = thr
  )
  
  list(
    model = mod_final,
    predictors = dplyr::tibble(c = predictors, f = predictors_f),
    performance_cv = eval_final_cv,
    performance_final = performance_final,
    data_ens = pred_test_ens
  )
}