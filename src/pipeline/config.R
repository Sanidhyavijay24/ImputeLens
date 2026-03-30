get_experiment_config <- function(project_root = ".") {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = FALSE)

  list(
    project_root = project_root,
    data_dir = file.path(project_root, "data", "raw"),
    output_path = file.path(project_root, "experiments", "results", "benchmark_results.csv"),
    missing = list(
      mechanisms = c("MCAR", "MAR"),
      rates = c(0.1, 0.3, 0.5),
      seeds = c(11, 22, 33)
    ),
    imputers = c("mean_mode", "mice", "missForest", "torch_vae"),
    dl_imputer = list(
      epochs = 120,
      latent_dim = 8,
      hidden_dim = 32,
      lr = 0.001,
      beta = 0.001
    ),
    datasets = list(
      bank = list(
        file = "bank.csv",
        target = "deposit",
        task = "classification"
      ),
      housing = list(
        file = "housing.csv",
        target = "median_house_value",
        task = "regression"
      ),
      wine = list(
        file = "winequality-red.csv",
        target = "quality",
        task = "classification",
        transform_target = function(df) {
          df$quality <- ifelse(df$quality >= 6, "good", "bad")
          df
        }
      )
    ),
    model = list(
      nrounds = 120,
      max_depth = 6,
      eta = 0.08,
      subsample = 0.9,
      colsample_bytree = 0.9
    )
  )
}