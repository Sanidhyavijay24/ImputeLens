impute_mean_mode <- function(df, target_col) {
  out <- df
  feature_cols <- setdiff(names(out), target_col)

  for (col in feature_cols) {
    if (is.numeric(out[[col]])) {
      fill_value <- mean(out[[col]], na.rm = TRUE)
      if (!is.finite(fill_value)) {
        fill_value <- 0
      }
      out[[col]][is.na(out[[col]])] <- fill_value
    } else {
      fill_value <- mode_value(out[[col]])
      out[[col]][is.na(out[[col]])] <- fill_value
    }
  }

  out
}

impute_torch_vae <- function(df, target_col, seed = 42, deep_cfg = NULL) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("Package 'torch' is not installed")
  }
  if (!torch::torch_is_installed()) {
    stop("Torch backend is missing. Run torch::install_torch() in R, then rerun experiments.")
  }

  feature_cols <- setdiff(names(df), target_col)
  numeric_cols <- feature_cols[vapply(df[feature_cols], is.numeric, logical(1))]

  # Keep categorical behavior stable with mean/mode fills.
  out <- impute_mean_mode(df, target_col)

  if (length(numeric_cols) == 0) {
    return(out)
  }

  x_raw <- as.matrix(df[numeric_cols])
  missing_mask <- is.na(x_raw)

  # If there are almost no missing values, deep training is unnecessary.
  if (sum(missing_mask) == 0) {
    return(out)
  }

  col_means <- colMeans(x_raw, na.rm = TRUE)
  col_means[!is.finite(col_means)] <- 0

  x_filled <- x_raw
  for (j in seq_len(ncol(x_filled))) {
    x_filled[is.na(x_filled[, j]), j] <- col_means[j]
  }

  col_sds <- apply(x_filled, 2, stats::sd)
  col_sds[!is.finite(col_sds) | col_sds == 0] <- 1

  x_scaled <- sweep(x_filled, 2, col_means, FUN = "-")
  x_scaled <- sweep(x_scaled, 2, col_sds, FUN = "/")

  epochs <- if (!is.null(deep_cfg$epochs)) deep_cfg$epochs else 120
  latent_dim <- if (!is.null(deep_cfg$latent_dim)) deep_cfg$latent_dim else 8
  hidden_dim <- if (!is.null(deep_cfg$hidden_dim)) deep_cfg$hidden_dim else 32
  lr <- if (!is.null(deep_cfg$lr)) deep_cfg$lr else 0.001
  beta <- if (!is.null(deep_cfg$beta)) deep_cfg$beta else 0.001

  torch::torch_manual_seed(seed)
  x_tensor <- torch::torch_tensor(x_scaled, dtype = torch::torch_float())
  observed_mask <- torch::torch_tensor((!missing_mask) * 1, dtype = torch::torch_float())

  vae_module <- torch::nn_module(
    "TabularVAE",
    initialize = function(input_dim, hidden_dim, latent_dim) {
      self$fc1 <- torch::nn_linear(input_dim, hidden_dim)
      self$mu <- torch::nn_linear(hidden_dim, latent_dim)
      self$logvar <- torch::nn_linear(hidden_dim, latent_dim)
      self$fc3 <- torch::nn_linear(latent_dim, hidden_dim)
      self$fc4 <- torch::nn_linear(hidden_dim, input_dim)
    },
    forward = function(x) {
      h <- torch::nnf_relu(self$fc1(x))
      mu <- self$mu(h)
      logvar <- self$logvar(h)
      std <- torch::torch_exp(0.5 * logvar)
      eps <- torch::torch_randn_like(std)
      z <- mu + eps * std
      recon <- self$fc4(torch::nnf_relu(self$fc3(z)))
      list(recon = recon, mu = mu, logvar = logvar)
    }
  )

  model <- vae_module(input_dim = ncol(x_scaled), hidden_dim = hidden_dim, latent_dim = latent_dim)
  optimizer <- torch::optim_adam(model$parameters, lr = lr)

  for (epoch in seq_len(epochs)) {
    optimizer$zero_grad()
    out_pass <- model(x_tensor)
    recon <- out_pass$recon

    # Reconstruction loss only on observed entries.
    rec_loss <- (((recon - x_tensor)^2) * observed_mask)$sum() / observed_mask$sum()$clamp(min = 1)
    kl_loss <- -0.5 * (1 + out_pass$logvar - out_pass$mu$pow(2) - out_pass$logvar$exp())$mean()
    loss <- rec_loss + beta * kl_loss

    loss$backward()
    optimizer$step()
  }

  recon_scaled <- torch::with_no_grad({
    model(x_tensor)$recon
  })

  recon_np <- as.matrix(torch::as_array(recon_scaled))
  recon_unscaled <- sweep(recon_np, 2, col_sds, FUN = "*")
  recon_unscaled <- sweep(recon_unscaled, 2, col_means, FUN = "+")

  x_completed <- x_raw
  x_completed[missing_mask] <- recon_unscaled[missing_mask]
  out[numeric_cols] <- as.data.frame(x_completed)
  out
}

impute_dataset <- function(df, method, target_col, seed = 42, deep_cfg = NULL) {
  set.seed(seed)
  feature_cols <- setdiff(names(df), target_col)

  if (method == "mean_mode") {
    return(impute_mean_mode(df, target_col))
  }

  if (method == "mice") {
    if (!requireNamespace("mice", quietly = TRUE)) {
      stop("Package 'mice' is not installed")
    }
    completed <- mice::complete(
      mice::mice(df[feature_cols], m = 1, maxit = 5, printFlag = FALSE, seed = seed),
      1
    )
    completed[[target_col]] <- df[[target_col]]
    return(completed[names(df)])
  }

  if (method == "missForest") {
    if (!requireNamespace("missForest", quietly = TRUE)) {
      stop("Package 'missForest' is not installed")
    }
    completed <- missForest::missForest(df[feature_cols], verbose = FALSE)$ximp
    completed[[target_col]] <- df[[target_col]]
    return(completed[names(df)])
  }

  if (method == "torch_vae") {
    return(impute_torch_vae(df, target_col, seed = seed, deep_cfg = deep_cfg))
  }

  stop(paste("Unknown imputer:", method))
}

imputation_error <- function(imputed_df, original_df, mask_matrix, target_col) {
  numeric_cols <- setdiff(safe_numeric_cols(original_df), target_col)
  if (length(numeric_cols) == 0) {
    return(list(rmse = NA_real_, mae = NA_real_))
  }

  actual <- c()
  predicted <- c()
  for (col in numeric_cols) {
    idx <- which(mask_matrix[, col])
    if (length(idx) > 0) {
      actual <- c(actual, original_df[[col]][idx])
      predicted <- c(predicted, imputed_df[[col]][idx])
    }
  }

  if (length(actual) == 0) {
    return(list(rmse = NA_real_, mae = NA_real_))
  }

  list(
    rmse = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    mae = mean(abs(actual - predicted), na.rm = TRUE)
  )
}