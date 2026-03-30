metric_direction <- function(metric_name) {
  lower_better <- c("imputation_rmse", "imputation_mae", "distribution_ks", "distribution_wasserstein", "rmse_model")
  if (metric_name %in% lower_better) {
    return("lower_better")
  }
  "higher_better"
}

complete_block_matrix <- function(df, metric_name) {
  work <- df[, c("imputer", "mechanism", "missing_rate", "seed", metric_name)]
  names(work)[5] <- "metric_value"
  work <- work[is.finite(work$metric_value), ]
  if (nrow(work) == 0) {
    return(NULL)
  }

  work$block <- paste(work$mechanism, work$missing_rate, work$seed, sep = "__")
  wide <- reshape(
    work[, c("block", "imputer", "metric_value")],
    idvar = "block",
    timevar = "imputer",
    direction = "wide"
  )

  metric_cols <- grep("^metric_value\\.", names(wide), value = TRUE)
  if (length(metric_cols) < 2) {
    return(NULL)
  }

  wide <- wide[stats::complete.cases(wide[, metric_cols, drop = FALSE]), , drop = FALSE]
  if (nrow(wide) < 2) {
    return(NULL)
  }

  mat <- as.matrix(wide[, metric_cols, drop = FALSE])
  colnames(mat) <- sub("^metric_value\\.", "", metric_cols)
  mat
}

paired_wilcoxon_table <- function(mat) {
  imputers <- colnames(mat)
  if (length(imputers) < 2) {
    return(data.frame())
  }

  out <- list()
  idx <- 1L
  for (i in seq_len(length(imputers) - 1)) {
    for (j in (i + 1):length(imputers)) {
      x <- mat[, i]
      y <- mat[, j]
      pval <- tryCatch(
        stats::wilcox.test(x, y, paired = TRUE, exact = FALSE)$p.value,
        error = function(e) NA_real_
      )
      out[[idx]] <- data.frame(
        comparison = paste(imputers[i], "vs", imputers[j]),
        raw_p_value = pval,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  tab <- do.call(rbind, out)
  tab$adjusted_p_holm <- stats::p.adjust(tab$raw_p_value, method = "holm")
  tab
}

compute_statistical_report <- function(results_df) {
  if (nrow(results_df) == 0) {
    return(data.frame())
  }

  ok <- results_df[results_df$status == "ok", , drop = FALSE]
  metrics <- c("imputation_rmse", "imputation_mae", "distribution_ks", "distribution_wasserstein", "auc", "f1", "rmse_model", "r2")
  metrics <- metrics[metrics %in% names(ok)]

  rows <- list()
  row_id <- 1L

  for (dataset_name in unique(ok$dataset)) {
    ds <- ok[ok$dataset == dataset_name, , drop = FALSE]

    for (metric_name in metrics) {
      mat <- complete_block_matrix(ds, metric_name)
      if (is.null(mat) || ncol(mat) < 2 || nrow(mat) < 2) {
        next
      }

      friedman_p <- tryCatch(
        stats::friedman.test(mat)$p.value,
        error = function(e) NA_real_
      )

      avg_perf <- colMeans(mat, na.rm = TRUE)
      rank_order <- order(avg_perf, decreasing = metric_direction(metric_name) == "higher_better")
      ranking <- paste(sprintf("%d:%s", seq_along(rank_order), names(avg_perf)[rank_order]), collapse = " | ")

      rows[[row_id]] <- data.frame(
        dataset = dataset_name,
        metric = metric_name,
        test_type = "friedman_global",
        comparison = "all_imputers",
        statistic = NA_real_,
        p_value = friedman_p,
        adjusted_p = NA_real_,
        n_blocks = nrow(mat),
        n_imputers = ncol(mat),
        ranking = ranking,
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1L

      pairwise <- paired_wilcoxon_table(mat)
      if (nrow(pairwise) > 0) {
        for (k in seq_len(nrow(pairwise))) {
          rows[[row_id]] <- data.frame(
            dataset = dataset_name,
            metric = metric_name,
            test_type = "wilcoxon_paired",
            comparison = pairwise$comparison[k],
            statistic = NA_real_,
            p_value = pairwise$raw_p_value[k],
            adjusted_p = pairwise$adjusted_p_holm[k],
            n_blocks = nrow(mat),
            n_imputers = ncol(mat),
            ranking = NA_character_,
            stringsAsFactors = FALSE
          )
          row_id <- row_id + 1L
        }
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame())
  }

  do.call(rbind, rows)
}

write_statistical_report <- function(stats_df, output_path) {
  ensure_parent_dir(output_path)
  write.csv(stats_df, output_path, row.names = FALSE)
  output_path
}