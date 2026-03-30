# Core data processing & Imputation
install.packages(c("tidyverse", "mice", "missForest", "data.table"))

# Machine Learning & Modeling
install.packages(c("xgboost", "tidymodels", "DALEX"))

# Deep Learning (VAE Impuation)
install.packages("torch")
# One-time backend install needed by torch models (Lantern/libtorch)
if (requireNamespace("torch", quietly = TRUE)) {
	torch::install_torch()
}

# Dashboard & UI
install.packages(c("shiny", "bslib", "shinydashboard", "plotly", "cowplot"))

# Statistical Analysis
install.packages(c("rstatix", "PMCMRplus"))

# Workflow & Orchestration
install.packages("targets")

# Initialize renv if needed
# install.packages("renv")
# renv::init()
