fit_glm_parallel <- function(data,
                                  response,
                                  predictors,
                                  predictors_f = NULL,
                                  partition,
                                  thr = NULL,
                                  fit_formula = NULL,
                                  poly = 2,
                                  n_cores = 1) {
  
  variables <- c(predictors, predictors_f)
  
  data <- as.data.frame(data)
  if (!is.null(predictors_f)) {
    data[predictors_f] <- lapply(data[predictors_f], factor)
  }
  
  # --- remove NAs ---
  complete_vec <- stats::complete.cases(data[, c(response, variables)])
  if (sum(!complete_vec) > 0) {
    message(sum(!complete_vec), " rows removed due to NA.")
    data <- data[complete_vec, , drop = FALSE]
  }
  
  # --- cria fórmula ---
  if (is.null(fit_formula)) {
    # <<< CORREÇÃO 1: Forma robusta de criar termos polinomiais >>>
    # A abordagem anterior não generalizava bem para múltiplos preditores.
    # Usar poly() é a maneira correta e mais segura no R.
    if (poly >= 2 && length(predictors) > 0) {
      poly_terms <- paste0("poly(", predictors, ", ", poly, ", raw = TRUE)", collapse = " + ")
      predictor_terms <- paste(c(predictors, predictors_f), collapse = " + ")
      # A fórmula original com poly() é mais complexa, então uma alternativa
      # simples e funcional é criar os termos manualmente de forma correta.
      poly_manual <- unlist(lapply(2:poly, function(p) paste0("I(", predictors, "^", p, ")")))
      
      all_vars <- c(predictors, predictors_f, poly_manual)
      formula1 <- stats::formula(paste(response, "~", paste(all_vars, collapse = " + ")))
    } else {
      formula1 <- stats::formula(paste(response, "~", paste(variables, collapse = " + ")))
    }
  } else {
    formula1 <- fit_formula
  }
  message("Formula used:\n", deparse1(formula1))
  
  # --- matriz completa (pré-processamento para eficiência) ---
  mf_full <- model.frame(formula1, data = data)
  y_full  <- model.response(mf_full, "numeric")
  X_full  <- model.matrix(formula1, data = mf_full)
  
  # --- partições ---
  p_names <- grep(paste0("^", partition), names(data), value = TRUE)
  np <- length(p_names)
  
  # --- paralelismo ---
  if (n_cores > 1) {
    future::plan(future::multisession, workers = n_cores)
    message(n_cores, " cores in use.")
  } else {
    future::plan(future::sequential)
  }
  
  parallel_results <- furrr::future_map(1:np, function(h) {
    part_vec <- data[[p_names[h]]]
    train_idx <- which(part_vec == 1)
    test_idx  <- which(part_vec == 0)
    
    # verifica treino válido
    y_train <- y_full[train_idx]
    if (length(unique(y_train)) < 2) return(NULL)
    
    X_train <- X_full[train_idx, , drop = FALSE]
    
    # GLM rápido
    mod <- glm.fit(x = X_train, y = y_train, family = binomial(),
                   control = glm.control(maxit = 50, epsilon = 1e-6))
    
    # <<< CORREÇÃO 2: Lidar com coeficientes NA (essencial!) >>>
    # Se houver multicolinearidade, glm.fit pode retornar NA para alguns coeficientes.
    # Isso quebraria a multiplicação da matriz. Substituir NAs por 0 é uma abordagem segura.
    mod$coefficients[is.na(mod$coefficients)] <- 0
    
    # verifica teste não vazio
    if (length(test_idx) == 0) return(NULL)
    X_test <- X_full[test_idx, , drop = FALSE]
    
    eta <- X_test %*% mod$coefficients
    preds <- stats::binomial()$linkinv(eta)
    preds <- pmin(pmax(preds, 1e-6), 1 - 1e-6) # Evita valores exatamente 0 ou 1
    
    pr_ab <- y_full[test_idx]
    pred_test <- data.frame(
      pr_ab = pr_ab,
      pred  = preds,
      rnames = rownames(data)[test_idx]
    )
    
    # Supondo que sdm_eval existe e funciona como esperado
    eval <- sdm_eval(
      p = preds[pr_ab == 1],
      a = preds[pr_ab == 0],
      thr = thr
    )
    
    list(
      eval_results = tibble::tibble(model = "glm", eval),
      pred_results = pred_test
    )
  }, .options = furrr_options(seed = TRUE))
  
  # Sempre limpe o plano futuro
  future::plan(future::sequential)
  
  # filtra partições inválidas
  valid_results <- !sapply(parallel_results, is.null)
  if(!any(valid_results)) {
    warning("No valid partitions were successfully processed.")
    return(NULL)
  }
  parallel_results <- parallel_results[valid_results]
  
  eval_partial <- bind_rows(lapply(parallel_results, `[[`, "eval_results"), .id = "replica")
  
  # <<< CORREÇÃO 3: Sumarização mais robusta com dplyr >>>
  # Usar where(is.numeric) é mais seguro do que nomear as colunas (ex: TPR:IMAE),
  # pois não depende da ordem ou existência de colunas específicas.
  eval_final_cv <- eval_partial %>%
    dplyr::group_by(model, threshold) %>%
    dplyr::summarise(
      dplyr::across(where(is.numeric), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
  
  pred_test_ens <- bind_rows(lapply(parallel_results, `[[`, "pred_results"), .id = "replica")
  
  # --- modelo final completo ---
  mod_final <- glm(formula1, data = data, family = binomial())
  pred_final <- predict(mod_final, newdata = data, type = "response")
  
  # Avaliação do modelo final
  performance_final <- sdm_eval(
    p = pred_final[y_full == 1],
    a = pred_final[y_full == 0],
    thr = thr
  )
  
  # <<< CORREÇÃO 4: Estrutura de retorno mais clara >>>
  # Retornar as métricas de CV e do modelo final separadamente é mais limpo
  # e evita confusão.
  list(
    model = mod_final,
    predictors = variables,
    performance_cv = eval_final_cv,
    performance_final = performance_final,
    data_ens = pred_test_ens
  )
}
