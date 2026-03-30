load_dataset <- function(dataset_cfg, data_dir) {
  file_path <- file.path(data_dir, dataset_cfg$file)
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  # Convert character columns to factors for compatibility (e.g., missForest, model.matrix)
  char_cols <- vapply(df, is.character, logical(1))
  df[char_cols] <- lapply(df[char_cols], as.factor)

  if (!is.null(dataset_cfg$transform_target)) {
    df <- dataset_cfg$transform_target(df)
  }

  target <- dataset_cfg$target
  if (dataset_cfg$task == "classification") {
    df[[target]] <- as.factor(df[[target]])
  } else {
    df[[target]] <- as.numeric(df[[target]])
  }

  df
}

train_test_split <- function(df, target, task, seed = 42, test_frac = 0.2) {
  set.seed(seed)

  if (task == "classification") {
    idx_test <- unlist(lapply(split(seq_len(nrow(df)), df[[target]]), function(ids) {
      n_take <- max(1, floor(length(ids) * test_frac))
      sample(ids, size = n_take)
    }))
  } else {
    n_test <- max(1, floor(nrow(df) * test_frac))
    idx_test <- sample(seq_len(nrow(df)), size = n_test)
  }

  list(
    train = df[-idx_test, , drop = FALSE],
    test = df[idx_test, , drop = FALSE]
  )
}