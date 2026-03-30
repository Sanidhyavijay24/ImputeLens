train_and_evaluate_xgb <- function(train_df, test_df, target_col, task, model_cfg, seed = 42) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is not installed")
  }

  train_x <- train_df[setdiff(names(train_df), target_col)]
  test_x <- test_df[setdiff(names(test_df), target_col)]

  # Combine train and test to ensure identical dummy columns (same factor levels)
  combined_x <- rbind(train_x, test_x)
  combined_matrix <- model.matrix(~ . - 1, data = combined_x)

  x_train <- combined_matrix[seq_len(nrow(train_x)), , drop = FALSE]
  x_test <- combined_matrix[(nrow(train_x) + 1):nrow(combined_matrix), , drop = FALSE]

  set.seed(seed)

  if (task == "classification") {
    y_train_factor <- as.factor(train_df[[target_col]])
    y_test_factor <- as.factor(test_df[[target_col]])

    if (length(levels(y_train_factor)) != 2) {
      stop("Classification target must have exactly 2 classes")
    }

    positive_class <- levels(y_train_factor)[2]
    y_train <- as.integer(y_train_factor == positive_class)
    y_test <- as.integer(y_test_factor == positive_class)

    dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
    model <- xgboost::xgb.train(
      params = list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = model_cfg$eta,
        max_depth = model_cfg$max_depth,
        subsample = model_cfg$subsample,
        colsample_bytree = model_cfg$colsample_bytree
      ),
      data = dtrain,
      nrounds = model_cfg$nrounds,
      verbose = 0
    )

    prob <- predict(model, x_test)
    pred <- as.integer(prob >= 0.5)

    list(
      auc = binary_auc(y_test, prob),
      f1 = f1_binary(y_test, pred),
      rmse_model = NA_real_,
      r2 = NA_real_
    )
  } else {
    y_train <- as.numeric(train_df[[target_col]])
    y_test <- as.numeric(test_df[[target_col]])

    dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
    model <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        eta = model_cfg$eta,
        max_depth = model_cfg$max_depth,
        subsample = model_cfg$subsample,
        colsample_bytree = model_cfg$colsample_bytree
      ),
      data = dtrain,
      nrounds = model_cfg$nrounds,
      verbose = 0
    )

    pred <- predict(model, x_test)
    ss_res <- sum((y_test - pred)^2)
    ss_tot <- sum((y_test - mean(y_test))^2)

    list(
      auc = NA_real_,
      f1 = NA_real_,
      rmse_model = sqrt(mean((y_test - pred)^2)),
      r2 = if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot
    )
  }
}