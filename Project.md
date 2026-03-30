# ImputeLens  
**Data-Centric Missing-Data Robustness Benchmark in R.**

## 1. Overview
This project focuses on the principles of **Data-Centric AI**, where the goal is to improve data quality and study its impact on model robustness rather than focusing solely on model architecture. We investigate how different imputation methods affect predictive performance under varying missing data scenarios.

---

## 2. Motivation
The AI community is shifting from model-centric improvements (e.g., tuning architectures) to **data-centric approaches**, where improved preprocessing, imputation, and feature handling yield significant performance gains. This project aligns with that trend by evaluating:
- How missingness affects model accuracy
- Whether DL-based imputation outperforms traditional techniques
- Whether downstream performance is statistically different across methods

---

## 3. Objectives
- Benchmark multiple imputation strategies under controlled missingness.
- Evaluate downstream ML model performance after imputation.
- Assess statistical and distributional shifts caused by each imputer.
- Quantify robustness of ML models trained on imputed data.

---

## 4. Experimental Design

### 4.1 Datasets
Three real-world, initially complete tabular datasets:
1. **Wine Quality Dataset** (Regression / Classification)
2. **California Housing Prices** (Regression)
3. **Bank Marketing Dataset** (Binary Classification)

All datasets are available via Kaggle or UCI repositories and are of moderate size (5K–45K records).

---

### 4.2 Missing Data Simulation
- **Mechanisms**: MCAR (Missing Completely at Random), MAR (Missing At Random)
- **Missing Rates**: 10%, 30%, 50%
- **Seeds**: Exactly 3 fixed seeds for reproducibility
- **Scope**: Missingness introduced only in `train` (and optionally `val`) sets; test sets remain untouched

---

### 4.3 Imputation Methods
- Mean / Median Imputation
- Iterative Imputer (`mice`)
- Tree-based Imputer (`missForest`)
- VAE-based Deep Learning Imputer (using R `torch` natively; optionally GAIN if time allows)

Each method is used to impute training data before downstream model training.

---

### 4.4 Downstream Evaluation
Model: **XGBoost** (via R `xgboost` or `tidymodels`)
- Classification: AUC, F1-score
- Regression: RMSE, R²
- Metrics are computed on original test set (clean data)

---

## 5. Evaluation Criteria

### 5.1 Imputation Metrics
- RMSE / MAE (on artificially masked entries)
- Distributional comparisons:
  - **KS-test**
  - **Wasserstein Distance**
  - **Density plots** (original vs imputed)

### 5.2 Downstream Metrics
- Predictive performance after training on imputed data
- Mean ± Std across 3 seeds

### 5.3 Statistical Testing
- Friedman Test for ranking imputers (blocked by scenario seed/mechanism/rate)
- Paired Wilcoxon post-hoc tests with Holm correction for pairwise comparison
- Export statistical report to `experiments/results/statistical_tests.csv`

---

## 6. Implementation Plan

### Phase 1: R-Driven Pipeline
- `targets` or standard R scripts to orchestrate:
  - Data corruption
  - Imputation (mice, missForest, torch)
  - Downstream model training
  - Logging results to CSV / RDS / Feather

### Phase 2: Shiny Dashboard
- Interactive Shiny web application for:
  - Dynamic plotting of metric degradation across missing rates
  - Distributional overlays (original vs. imputed)
  - Statistical significance testing tables (scientific p-value display)
  - Overall rankings and pairwise significance matrix
  - Error analysis view for failed runs

---

## 7. Deliverables
- Fully reproducible R pipeline
- Clean `experiments/results/` directory with logs for all runs
- Interactive Shiny Dashboard for analysis
- Statistical test report CSV (`friedman` + paired `wilcoxon`)
- GitHub-ready repo with README and LICENSE
- 4–6 slide summary deck for presentation
- Resume bullet:  
  > Built an R-based data-centric AI benchmarking pipeline and interactive Shiny dashboard evaluating statistical and deep learning–based imputation strategies, quantifying their impact on downstream model robustness under varying missingness conditions.

---

## 8. Folder Structure

```
project/
├── data/
│   ├── raw/
│   └── processed/
├── experiments/
│   ├── configs/
│   └── results/
├── src/
│   ├── pipeline/
│   └── app/
├── notebooks/
├── slides/
├── install.R
├── README.md
└── Project.md
```

---

## 9. Non-Goals
- No full-scale AutoML system
- No MNAR mechanism unless as exploratory stretch
- No UI or deployment outside the Shiny dashboard (To be done later)
- No large language models

---

## 10. Tools and Technologies
- Runtime: R (`renv` for dependency management)
- R Libraries: `tidyverse`, `mice`, `missForest`, `xgboost`, `torch`, `shiny`, `bslib`, `rstatix`, `DALEX`, `PMCMRplus`, `cowplot`, `targets`
- Optional: MLflow or wandb (via R interfaces) for logging

---

## 11. Execution Steps
1. Install dependencies and torch backend:
  - `Rscript install.R`
2. Run the full benchmark pipeline:
  - `Rscript src/pipeline/run_experiments.R`
3. Expected outputs:
  - `experiments/results/benchmark_results.csv`
  - `experiments/results/statistical_tests.csv`
4. Launch dashboard:
  - `Rscript -e "shiny::runApp('src/app')"`
5. Interpretation notes:
  - For metrics where lower is better (`rmse`/distribution distances), smaller values indicate better imputers.
  - p-values shown as `0` in fixed decimal are interpreted as very small values; dashboard uses scientific notation to avoid ambiguity.
