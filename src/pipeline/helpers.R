ensure_parent_dir <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_numeric_cols <- function(df) {
  names(df)[vapply(df, is.numeric, logical(1))]
}

safe_non_numeric_cols <- function(df) {
  names(df)[!vapply(df, is.numeric, logical(1))]
}

binary_auc <- function(actual01, score) {
  ord <- order(score)
  ranks <- rank(score, ties.method = "average")
  pos <- actual01 == 1
  n_pos <- sum(pos)
  n_neg <- sum(!pos)
  if (n_pos == 0 || n_neg == 0) {
    return(NA_real_)
  }
  (sum(ranks[pos]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

f1_binary <- function(actual01, pred01) {
  tp <- sum(actual01 == 1 & pred01 == 1)
  fp <- sum(actual01 == 0 & pred01 == 1)
  fn <- sum(actual01 == 1 & pred01 == 0)
  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  if ((precision + recall) == 0) {
    return(0)
  }
  2 * precision * recall / (precision + recall)
}

wasserstein_1d <- function(x, y, n_grid = 100) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) {
    return(NA_real_)
  }

  probs <- seq(0, 1, length.out = n_grid)
  qx <- stats::quantile(x, probs = probs, names = FALSE, type = 8)
  qy <- stats::quantile(y, probs = probs, names = FALSE, type = 8)
  mean(abs(qx - qy))
}

distribution_shift_metrics <- function(imputed_df, original_df, mask_matrix, target_col) {
  numeric_cols <- setdiff(safe_numeric_cols(original_df), target_col)
  if (length(numeric_cols) == 0) {
    return(list(ks = NA_real_, wasserstein = NA_real_, n_features = 0L))
  }

  ks_vals <- c()
  wass_vals <- c()
  used_features <- 0L

  for (col in numeric_cols) {
    idx <- which(mask_matrix[, col])
    if (length(idx) < 2) {
      next
    }

    original_vals <- original_df[[col]][idx]
    imputed_vals <- imputed_df[[col]][idx]

    if (sum(is.finite(original_vals)) < 2 || sum(is.finite(imputed_vals)) < 2) {
      next
    }

    ks_stat <- suppressWarnings(as.numeric(stats::ks.test(original_vals, imputed_vals)$statistic))
    wass_stat <- wasserstein_1d(original_vals, imputed_vals)

    ks_vals <- c(ks_vals, ks_stat)
    wass_vals <- c(wass_vals, wass_stat)
    used_features <- used_features + 1L
  }

  if (used_features == 0L) {
    return(list(ks = NA_real_, wasserstein = NA_real_, n_features = 0L))
  }

  list(
    ks = mean(ks_vals, na.rm = TRUE),
    wasserstein = mean(wass_vals, na.rm = TRUE),
    n_features = used_features
  )
}