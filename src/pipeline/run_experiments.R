source(file.path("src", "pipeline", "config.R"))
source(file.path("src", "pipeline", "helpers.R"))
source(file.path("src", "pipeline", "data_loader.R"))
source(file.path("src", "pipeline", "missingness.R"))
source(file.path("src", "pipeline", "imputation.R"))
source(file.path("src", "pipeline", "modeling.R"))
source(file.path("src", "pipeline", "experiment.R"))
source(file.path("src", "pipeline", "stats_analysis.R"))

cfg <- get_experiment_config(".")
grid <- build_experiment_grid(cfg)
results <- run_grid_experiments(cfg, grid)

results_path <- write_results(results, cfg$output_path)
stats_path <- write_statistical_report(
	compute_statistical_report(results),
	file.path(cfg$project_root, "experiments", "results", "statistical_tests.csv")
)

message("Results saved to: ", results_path)
message("Statistical report saved to: ", stats_path)