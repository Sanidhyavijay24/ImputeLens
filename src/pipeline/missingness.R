apply_missingness <- function(df, target_col, rate, mechanism, seed = 42) {
  set.seed(seed)
  out <- df

  feature_cols <- setdiff(names(df), target_col)
  mask <- matrix(FALSE, nrow = nrow(df), ncol = ncol(df), dimnames = list(NULL, names(df)))

  if (length(feature_cols) == 0 || rate <= 0) {
    return(list(data = out, mask = mask))
  }

  numeric_features <- feature_cols[vapply(out[feature_cols], is.numeric, logical(1))]
  candidate_cols <- if (length(numeric_features) > 0) numeric_features else feature_cols

  if (mechanism == "MCAR") {
    total_cells <- nrow(out) * length(candidate_cols)
    n_mask <- max(1, floor(total_cells * rate))
    rows <- sample(seq_len(nrow(out)), size = n_mask, replace = TRUE)
    cols <- sample(candidate_cols, size = n_mask, replace = TRUE)

    for (i in seq_len(n_mask)) {
      r <- rows[i]
      c <- cols[i]
      if (!is.na(out[r, c])) {
        out[r, c] <- NA
        mask[r, c] <- TRUE
      }
    }
  } else {
    driver_col <- if (length(numeric_features) > 0) sample(numeric_features, 1) else sample(candidate_cols, 1)
    driver <- out[[driver_col]]

    if (!is.numeric(driver)) {
      driver <- as.numeric(as.factor(driver))
    }

    z <- as.numeric(scale(driver))
    z[is.na(z)] <- 0
    p <- plogis(z)
    p <- p / max(p)

    mar_cols <- sample(candidate_cols, size = max(1, floor(length(candidate_cols) / 2)))
    for (col in mar_cols) {
      draw <- runif(nrow(out))
      set_na <- draw < (p * rate)
      set_na[is.na(set_na)] <- FALSE
      valid <- set_na & !is.na(out[[col]])
      out[[col]][valid] <- NA
      mask[valid, col] <- TRUE
    }
  }

  list(data = out, mask = mask)
}