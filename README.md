# JACSIS two-wave AI-divide analysis

R analysis script for:

> Nakagomi, A., Inagaki, S., & Tabuchi, T. (2026). Prospective predictors of generative AI initiation, engagement, and problematic use among Japanese adults: a two-wave study.
> *[Journal]*. DOI: [will be added on acceptance].

This repository contains the analysis code and variable codebook used to generate every result, table, and figure reported in the manuscript and its Supplement. The analysis examines baseline (2024) predictors of generative AI initiation, use intensity, purposes of use, and problematic use measured at follow-up (2025), using two consecutive waves of the Japan COVID-19 and Society Internet Survey (JACSIS).

## Repository contents

| File | Description |
|---|---|
| `analysis.R` | Main analysis script. Constructs all variables, fits the seven regression models (one initiation model on the at-risk sample and six within-user models), evaluates the hypotheses, validates the outcome measures, runs the sensitivity analyses, and produces Table 1, Figure 2, and the material of Supplementary Tables S1–S13. |
| `CODEBOOK.md` | Mapping between JACSIS survey items and the analytic variables used in the script. |
| `LICENSE` | MIT License. |
| `README.md` | This file. |

## Data availability

The JACSIS data are not publicly available due to ethical restrictions on participant privacy. De-identified data may be shared upon reasonable request, subject to approval by the JACSIS steering committee and relevant ethics review boards. See the manuscript Data Availability section for details.

Two data files, supplied by approved researchers, are expected (paths are set at the top of `analysis.R`):

| File | Content |
|---|---|
| `data/df.csv` | The two-wave panel: respondents who completed both the 2024 and the 2025 wave. Columns are named `<item>_2024` and `<item>_2025` as documented in `CODEBOOK.md`, and include the baseline completion timestamp. |
| `data/df_2024_all.csv` | All valid respondents of the 2024 wave, with the same `<item>_2024` columns plus `panel_matched` (1 = also completed the 2025 wave, 0 = lost to follow-up). Used only for the attrition analysis (Supplementary Table S12); that section is skipped, with a message, if the file is absent. |

## Requirements

- R version 4.5.3 (or later)
- R packages: `dplyr`, `tidyr`, `ggplot2`, `sandwich`, `lmtest`, `psych`, `GPArotation`, `car`, `MASS`, `quantreg`, `lavaan`, `semTools`

The script will automatically install any missing packages from CRAN.

## Usage

1. Place the data files at `data/df.csv` and `data/df_2024_all.csv` (column names matching `CODEBOOK.md`).
2. From the repository root, run:
   ```
   Rscript analysis.R
   ```
3. Outputs are written to the `output/` directory. On the full data the run takes a few minutes; most of the time is spent on the 1,000-replication bootstrap, the 1,000-replication parallel analysis, and the measurement-invariance models.

## Outputs

File names carry the number of the manuscript table, figure, or supplementary table they feed. `output/00_output_index.csv` lists every file with a one-line description.

| File | Description |
|---|---|
| `00_sample_sizes.csv` | Sample derivation counts (Figure 1). |
| `00_reliability_alpha.csv` | Cronbach's alpha for every multi-item scale. |
| `Table1_descriptives.csv` | Baseline characteristics of the at-risk sample, overall and by initiation status (Table 1). |
| `Figure2_forest.pdf` / `.png` / `.tiff` | Forest plot of the seven regression models (Figure 2; vector PDF, 300-dpi PNG, 600-dpi TIFF). |
| `S01_*.csv` | Polychoric exploratory factor analysis of the nine frequency items, parallel analysis, item distributions (Supplementary Table S1). |
| `S02_*.csv` | Confirmatory factor analysis of the three-factor purpose structure in the holdout sample of pre-2025 initiators (S2). |
| `S03_*.csv` | Problematic ChatGPT Use Scale: one-factor CFA, categorical omega, measurement invariance across sex and age, convergent validity, item distributions (S3). |
| `S04_main_regression_long.csv` / `_wide.csv` / `S04_R2_by_outcome.csv` | All coefficients of the seven main models (long format, presentation format, and model R²) (S4). |
| `S05_directional_expectations.csv` | Evaluation of the 20 directional expectations of H1–H3 (S5). |
| `S06_*.csv` | Criteria for the distinctness of problematic use from intensity (H4): HTMT with bootstrap confidence interval, one- versus two-factor CFA, and the summary table (S6). |
| `S07_*.csv` | Direct tests of coefficient differences between outcomes for all 10 pairs of within-user outcomes, profile similarity, and the five-outcome heterogeneity test (S7). |
| `S08_*.csv` | Residual and unadjusted correlations among the five within-user outcomes (S8). |
| `S09_*.csv` | Distributional sensitivity analyses: outcome distributions, proportional-odds ordinal regression, log-transformed and quantile (τ = .90) models of problematic use, concordance with the OLS models (S9). |
| `S10_*.csv` | Timing sensitivity analyses: baseline completion month, exclusion of baselines completed in January 2025, initiation half-year as a covariate (S10). |
| `S11_*.csv` | Selection-weighted within-user models (inverse probability of adoption) (S11). |
| `S12_*.csv` | Attrition analysis: retention model, standardized mean differences, attrition-weighted models (S12). |
| `S13_GVIF.csv` | Generalized variance-inflation factors (S13). |
| `sessionInfo.txt` | Full R session information for reproducibility. |

## Reproducibility

- Random seed: `set.seed(20260905)`, re-set immediately before the HTMT bootstrap and before the parallel analysis.
- Standard errors are heteroscedasticity-consistent (HC3) in every regression model; standard errors of the stacked cross-outcome models are clustered by respondent.
- The full computational environment (R version, package versions, locale) is captured in `output/sessionInfo.txt` after script execution.

## Citation

If you use or adapt this code, please cite:

> Nakagomi, A., Inagaki, S., & Tabuchi, T. (2026). Prospective predictors of generative AI initiation, engagement, and problematic use among Japanese adults: a two-wave study. *[Journal]*. DOI: [will be added on acceptance].

## License

The code in this repository is released under the MIT License. See `LICENSE` for details.

## Contact

Atsushi Nakagomi — anakagomi0211@chiba-u.jp
Center for Preventive Medicine, Chiba University
ORCID: [0000-0002-3908-696X](https://orcid.org/0000-0002-3908-696X)
