# ImputeLens

**Data-Centric Missing-Data Robustness Benchmark in R.**

## 1) What this project is about (in simple words)
In this project, we studied what happens when data has missing values.

Instead of only tuning machine learning models, we focused on improving the data first. This is called a data-centric approach.

We compared different ways to fill missing values (imputation), and then checked how those choices affect final model performance.

## 2) Why this matters
Real-world datasets are often incomplete.

If we fill missing values badly, model accuracy drops.
If we fill them well, models become more reliable and robust.

So the main question was:
"Which imputation method gives the most reliable downstream results under different missing-data conditions?"

## 3) What data we used
We used 3 tabular datasets:
- Bank Marketing (classification)
- California Housing (regression)
- Wine Quality (classification)

## 4) What we did step by step
1. Split each dataset into train/test.
2. Artificially created missing values in the train set using:
   - MCAR (completely random)
   - MAR (depends on observed data)
3. Tried multiple missingness rates: 10%, 30%, 50%.
4. Repeated with 3 seeds for reproducibility.
5. Imputed missing values using 4 methods:
   - mean_mode
   - mice
   - missForest
   - torch_vae
6. Trained XGBoost on imputed training data.
7. Evaluated model performance on clean test data.
8. Calculated distribution shift metrics and statistical significance tests.

## 5) How many experiments were run
- Total successful runs: 216
- Failed runs: 0
- Imputers compared: mean_mode, mice, missForest, torch_vae
- Datasets covered: bank, housing, wine

## 6) What metrics we tracked
Imputation quality metrics:
- imputation_rmse (lower is better)
- imputation_mae (lower is better)
- distribution_ks (lower is better)
- distribution_wasserstein (lower is better)

Downstream model metrics:
- Classification: auc, f1 (higher is better)
- Regression: rmse_model (lower is better), r2 (higher is better)

## 7) Main findings (easy summary)
1. missForest was strongest overall for predictive performance and imputation error.
   - Best AUC for bank and wine.
   - Best imputation RMSE/MAE on all datasets.
   - Best F1 for wine.

2. mice was strongest for preserving distributions in many cases.
   - Best (lowest) KS distance for all datasets.
   - Best Wasserstein for bank and wine.

3. For housing regression, mice gave the best downstream model performance.
   - Best rmse_model (lowest)
   - Best r2 (highest)

4. For bank classification F1, mean_mode was slightly best in average score.

5. Many differences were statistically significant.
   - Friedman global tests run: 18
   - Significant at p < 0.05: 15 out of 18

6. Not every classification metric difference was significant.
   - Non-significant global tests were: bank F1, wine AUC, and wine F1.

## 8) Important interpretation notes
- p-values shown as 0.00 in dashboards usually mean "very small", not literal zero.
- Use scientific notation for proper interpretation (for example 1.96e-11).
- r2 values can be negative in hard settings; that indicates poor fit relative to a naive baseline.

## 9) Final conclusion
This project shows that data cleaning strategy (imputation) can strongly change model outcomes.

In our results:
- missForest is generally a strong default choice.
- mice is very good when preserving original data distribution is important.
- torch_vae works and is competitive, but in this benchmark it did not consistently beat missForest/mice.

So, choosing the right imputer is a key part of building robust ML systems.

## 10) What we built for presentation
- Reproducible R pipeline for full experiments
- Statistical test export (Friedman + paired Wilcoxon)
- Interactive Shiny dashboard with:
  - dataset drilldown
  - overall rankings
  - significance matrix
  - error analysis

This gives both technical rigor and an easy way to explain results visually.

## 11) Simple glossary of important terms
- Missing value: A data cell with no recorded value (empty/NA).
- Imputation: Filling missing values with estimated values.
- mean_mode imputer: Fills numeric columns with mean and categorical columns with mode (most frequent value).
- mice imputer: "Multiple Imputation by Chained Equations"; uses iterative predictive models to fill missing values.
- missForest imputer: Random-forest-based iterative imputer; often strong for mixed tabular data.
- torch_vae imputer: Deep-learning imputer based on a VAE (Variational Autoencoder) using R torch.
- MCAR: Missing Completely At Random. Missingness has no pattern and is independent of values.
- MAR: Missing At Random. Missingness depends on observed variables.
- Seed: Fixed random number starting point; used for reproducibility.
- AUC: Area Under ROC Curve (classification metric). Higher is better.
- F1 score: Harmonic mean of precision and recall. Higher is better.
- RMSE: Root Mean Squared Error. Lower is better.
- MAE: Mean Absolute Error. Lower is better.
- R2 (R-squared): Regression fit quality. Higher is better (can be negative in difficult settings).
- KS statistic (distribution_ks): Measures difference between two distributions. Lower means closer distributions.
- Wasserstein distance (distribution_wasserstein / WS): Distance between two distributions based on "movement cost." Lower means closer distributions.
- p-value: Probability of seeing results at least this extreme if there is actually no real difference.
- Friedman test: Non-parametric global test to compare multiple methods across repeated blocks.
- Paired Wilcoxon test: Non-parametric pairwise comparison between two methods on matched runs.
- Holm correction: Adjustment method for multiple pairwise tests to reduce false positives.
- Statistical significance (p < 0.05): Common threshold suggesting differences are unlikely due to random chance.
