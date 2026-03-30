library(targets)

source(file.path("src", "pipeline", "config.R"))
source(file.path("src", "pipeline", "helpers.R"))
source(file.path("src", "pipeline", "data_loader.R"))
source(file.path("src", "pipeline", "missingness.R"))
source(file.path("src", "pipeline", "imputation.R"))
source(file.path("src", "pipeline", "modeling.R"))
source(file.path("src", "pipeline", "experiment.R"))
source(file.path("src", "pipeline", "stats_analysis.R"))

tar_option_set(packages = c("xgboost", "mice", "missForest"))

list(
  tar_target(config, get_experiment_config(".")),
  tar_target(grid, build_experiment_grid(config)),
  tar_target(results_df, run_grid_experiments(config, grid)),
  tar_target(results_path, write_results(results_df, config$output_path), format = "file"),
  tar_target(stats_df, compute_statistical_report(results_df)),
  tar_target(
    stats_path,
    write_statistical_report(stats_df, file.path(config$project_root, "experiments", "results", "statistical_tests.csv")),
    format = "file"
  )
)