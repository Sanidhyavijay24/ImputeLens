build_experiment_grid <- function(cfg) {
  expand.grid(
    dataset = names(cfg$datasets),
    mechanism = cfg$missing$mechanisms,
    missing_rate = cfg$missing$rates,
    seed = cfg$missing$seeds,
    imputer = cfg$imputers,
    stringsAsFactors = FALSE
  )
}

run_single_experiment <- function(cfg, dataset_name, mechanism, missing_rate, seed, imputer) {
  ds_cfg <- cfg$datasets[[dataset_name]]
  df <- load_dataset(ds_cfg, cfg$data_dir)
  split <- train_test_split(df, target = ds_cfg$target, task = ds_cfg$task, seed = seed)
  corrupted <- apply_missingness(split$train, ds_cfg$target, rate = missing_rate, mechanism = mechanism, seed = seed)

  imputed_train <- impute_dataset(
    corrupted$data,
    method = imputer,
    target_col = ds_cfg$target,
    seed = seed,
    deep_cfg = cfg$dl_imputer
  )
  imp_metrics <- imputation_error(imputed_train, split$train, corrupted$mask, target_col = ds_cfg$target)
  shift_metrics <- distribution_shift_metrics(imputed_train, split$train, corrupted$mask, target_col = ds_cfg$target)
  model_metrics <- train_and_evaluate_xgb(
    train_df = imputed_train,
    test_df = split$test,
    target_col = ds_cfg$target,
    task = ds_cfg$task,
    model_cfg = cfg$model,
    seed = seed
  )

  data.frame(
    dataset = dataset_name,
    task = ds_cfg$task,
    mechanism = mechanism,
    missing_rate = missing_rate,
    seed = seed,
    imputer = imputer,
    imputation_rmse = imp_metrics$rmse,
    imputation_mae = imp_metrics$mae,
    distribution_ks = shift_metrics$ks,
    distribution_wasserstein = shift_metrics$wasserstein,
    compared_numeric_features = shift_metrics$n_features,
    auc = model_metrics$auc,
    f1 = model_metrics$f1,
    rmse_model = model_metrics$rmse_model,
    r2 = model_metrics$r2,
    status = "ok",
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
}

run_grid_experiments <- function(cfg, grid) {
  results <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {
    row <- grid[i, ]
    res <- tryCatch(
      run_single_experiment(
        cfg = cfg,
        dataset_name = row$dataset,
        mechanism = row$mechanism,
        missing_rate = row$missing_rate,
        seed = row$seed,
        imputer = row$imputer
      ),
      error = function(e) {
        ds_cfg <- cfg$datasets[[row$dataset]]
        data.frame(
          dataset = row$dataset,
          task = ds_cfg$task,
          mechanism = row$mechanism,
          missing_rate = row$missing_rate,
          seed = row$seed,
          imputer = row$imputer,
          imputation_rmse = NA_real_,
          imputation_mae = NA_real_,
          distribution_ks = NA_real_,
          distribution_wasserstein = NA_real_,
          compared_numeric_features = NA_integer_,
          auc = NA_real_,
          f1 = NA_real_,
          rmse_model = NA_real_,
          r2 = NA_real_,
          status = "error",
          error_message = as.character(e$message),
          stringsAsFactors = FALSE
        )
      }
    )
    results[[i]] <- res
  }

  do.call(rbind, results)
}

write_results <- function(results_df, output_path) {
  ensure_parent_dir(output_path)
  write.csv(results_df, output_path, row.names = FALSE)
  output_path
}

run_all_experiments <- function(project_root = ".") {
  cfg <- get_experiment_config(project_root)
  grid <- build_experiment_grid(cfg)
  results <- run_grid_experiments(cfg, grid)
  write_results(results, cfg$output_path)
}