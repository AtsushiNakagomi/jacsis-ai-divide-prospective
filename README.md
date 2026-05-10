# JACSIS prospective AI-divide analysis

R analysis scripts for:

> Nakagomi, A., Inagaki, S., & Tabuchi, T. (2026). [Title: Prospective predictors of generative AI initiation, intensity, purposes, and problematic use among Japanese adults: A two-wave panel studye]. 
> *[Journal]*. DOI: [will be added on acceptance].

This repository contains the analysis code and variable codebook used to generate the results, tables, and figures reported in the manuscript. The analysis examines prospective predictors of generative AI initiation, intensity, purposes, and problematic use among Japanese adults, using two consecutive waves of the Japan COVID-19 and Society Internet Survey (JACSIS).

## Repository contents

| File | Description |
|---|---|
| `analysis.R` | Main analysis script. Constructs all variables, fits the seven regression models (one initiation + six within-user), and produces all manuscript tables and the forest plot. |
| `CODEBOOK.md` | Mapping between JACSIS survey items and analytic variables used in the script. |
| `LICENSE` | MIT License. |
| `README.md` | This file. |

## Data availability

The JACSIS data are not publicly available due to ethical restrictions on participant privacy. De-identified data may be shared upon reasonable request, subject to approval by the JACSIS steering committee and relevant ethics review boards. See the manuscript Data Availability section for details.

The data file expected by `analysis.R` at `data/df.csv` must be supplied by approved researchers. Column names should match those documented in `CODEBOOK.md`.

## Requirements

- R version 4.5.3 (or later)
- R packages: `dplyr`, `tidyr`, `ggplot2`, `sandwich`, `lmtest`, `psych`, `car`

The script will automatically install any missing packages from CRAN.

## Usage

1. Place the panel CSV at `data/df.csv` (column names matching `CODEBOOK.md`).
2. From the repository root, run:
   ```
   Rscript analysis.R
   ```
3. Outputs are written to the `output/` directory.

## Outputs

After successful execution, `output/` will contain:

| File | Description |
|---|---|
| `Table1_descriptives.csv` | Descriptive statistics, stratified by 2025 AI-use status (Manuscript Table 1). |
| `RegressionResults_long.csv` | All regression coefficients (long format). |
| `RegressionResults_wide.csv` | Predictor × outcome coefficient matrix (Supplementary Table 2). |
| `Table_alpha.csv` | Cronbach's alpha for each multi-item scale. |
| `Table_VIF.csv` | Generalised variance inflation factors. |
| `Table_EFA_loadings.csv` | Factor loadings for the AI-purpose EFA (Supplementary Table 1). |
| `Table_EFA_fit.csv` | EFA fit statistics. |
| `Table_residual_correlations.csv` | 5 × 5 residual correlations across within-user outcomes. |
| `Table_R2_per_outcome.csv` | Model R² for each within-user regression. |
| `Figure2_forest.png` | Forest plot of regression coefficients (Manuscript Figure 2). |
| `sessionInfo.txt` | Full R session information for reproducibility. |

## Reproducibility

- Random seed: `set.seed(20260427)` is set at the top of the script.
- The full computational environment (R version, package versions, locale) is captured in `output/sessionInfo.txt` after script execution.

## Citation

If you use or adapt this code, please cite:

> Nakagomi, A., Inagaki, S., & Tabuchi, T. (2026). [Title]. *[Journal]*. DOI: [will be added on acceptance].

## License

The code in this repository is released under the MIT License. See `LICENSE` for details.

## Contact

Atsushi Nakagomi — anakagomi0211@chiba-u.jp
Center for Preventive Medicine, Chiba University
ORCID: [0000-0002-3908-696X](https://orcid.org/0000-0002-3908-696X)
