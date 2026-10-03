# Codebook

JACSIS variable mapping for the analysis script (`analysis.R`).

This codebook documents how the JACSIS 2024 baseline and 2025 follow-up survey
items are mapped onto the analytic variables used in the manuscript. It is
intended for researchers granted access to the JACSIS data who wish to
reproduce or extend the analysis.

All 49 predictors are drawn from the **2024 baseline wave**. Outcome variables
are derived from the **2025 follow-up wave**, where the generative-AI module
was introduced. Two 2025-wave scales (K6 and UCLA-3) are used only to assess
the concurrent convergent validity of the problematic-use score.

---

## 1. Data files

| File | Rows | Required columns |
|---|---|---|
| `data/df.csv` | Respondents who completed both waves | `<item>_2024` and `<item>_2025` columns listed below; `回答完了日時_2024` (baseline completion timestamp, UTC, ISO 8601 `YYYY-MM-DDThh:mm:ssZ`; the column name can be changed with `BASELINE_TS_COL` at the top of the script) |
| `data/df_2024_all.csv` | All valid respondents of the 2024 wave | The `<item>_2024` columns listed in Sections 3–5; `panel_matched` (1 = also completed the 2025 wave, 0 = lost to follow-up). Un-suffixed 2024 column names are accepted. |

All JACSIS items are mandatory, so the analytic samples have complete data on
every predictor and outcome; the script uses complete cases throughout.

---

## 2. Outcomes (2025 wave)

| Construct | Item(s) | Variable in script | Coding |
|---|---|---|---|
| Generative-AI status | Q37S1 | `Q37S1_2025` | 6-category self-report of first use and current use (wording in Supplementary Methods 1) |
| AI initiation (binary) | derived from Q37S1 | `AI_initiated` | 1 if `Q37S1_2025 ∈ {5, 6}`; 0 if `Q37S1_2025 == 1` |
| AI-use frequency (9 items, 5-point) | Q37S3.1–Q37S3.9 | `Q37S3.x_2025_ord` | 1 = never … 5 = daily |
| Overall AI-use intensity | mean of the 9 Q37S3 items | `AI_use_9` | item mean (1–5) |
| Purpose: productivity/creative | mean of Q37S3.{1, 2, 4, 5} | `AI_purpose_productivity` | document drafting, translation/summarization, image/video generation, learning |
| Purpose: daily-life/information | mean of Q37S3.{3, 6, 7} | `AI_purpose_dailyinfo` | information search, daily-life planning, health advice |
| Purpose: social/emotional | mean of Q37S3.{8, 9} | `AI_purpose_socialemo` | conversation, emotional support |
| Problematic AI use (PCUS, 11 items, 7-point) | Q38.1–Q38.11 | `AI_addiction_mean` | item mean (1–7) |
| Initiation half-year | derived from Q37S1 | `init_late2025` | 0 = January–June 2025 (option 5); 1 = July–December 2025 (option 6); used in the timing sensitivity analysis |
| K6 distress, 2025 | Q65.1–Q65.6 (2025) | `K6_sum_2025` | same coding as the 2024 K6; convergent validity only |
| UCLA-3 loneliness, 2025 | Q66.1–Q66.3 (2025) | `UCLA3_sum_2025` | same coding as the 2024 UCLA-3; convergent validity only |

`Q37S1_2025` categories used by the script:

| Code | Meaning | Used as |
|---|---|---|
| 1 | Never used | Non-event in the initiation model; not in the within-user sample |
| 2 | Used in the past but not currently | Excluded from every analysis |
| 3, 4 | Current user who first used AI before 2025 | Not in any regression model; holdout sample for measurement validation (Supplementary Tables S2 and S3) |
| 5, 6 | Current user who first used AI in 2025 | Event in the initiation model; within-user sample |

The purpose composites are standardized (mean 0, SD 1) before estimation, as is
the problematic-use score. The item assignment of the three composites was
confirmed by the polychoric exploratory factor analysis of Supplementary
Table S1 and the holdout confirmatory factor analysis of Supplementary Table S2.

---

## 3. Personal predictors (2024 wave)

| Construct | 2024 item | Variable in script | Coding |
|---|---|---|---|
| Sex (binary) | `SEX_2024` | `Sex_female` | 1 = female, 0 = male |
| Age (10 bands, ref = 65+) | `AGE_2024` | `age_lt25` … `age_60_64` | <25 / 25–29 / 30–34 / 35–39 / 40–44 / 45–49 / 50–54 / 55–59 / 60–64 / 65+ |
| Self-rated physical health (Flourishing item) | `Q76.3_2024` | `PhysicalHealth_z` | recode 1–11 → 0–10; standardized |
| K6 distress (6 items, 5-point) | `Q65.1`–`Q65.6_2024` | `K6_ge13` | recode 1–5 → 4–0; sum 0–24; binary indicator ≥ 13 |
| UCLA-3 loneliness (3 items, 4-point) | `Q66.1`–`Q66.3_2024` | `UCLA3_z` | recode 1–4 → 3–0; sum 0–9; standardized |
| Adverse childhood experiences (10-item CDC–Kaiser proxy) | `Q77.1`–`Q77.9`, `Q77.13_2024` | `ACE_1`, `ACE_2`, `ACE_3`, `ACE_4plus` | items coded 1 = yes / 2 = no; sum 0–10; categorised as 0 (ref) / 1 / 2 / 3 / 4+. Q77.9 ("felt loved by a parent") is protective and is reverse-scored |
| Big Five (TIPI-J, 7-point) | `Q79.1`–`Q79.10_2024` | `BigFive_E_z`, `BigFive_A_z`, `BigFive_C_z`, `BigFive_ES_z`, `BigFive_O_z` | 5 dimensions × 2 items each; items 2, 4, 6, 8, 10 reverse-scored; dimension = mean of its two items; standardized |

---

## 4. Positional predictors (2024 wave)

| Construct | 2024 item | Variable in script | Coding |
|---|---|---|---|
| Education (3 levels, ref = high school or less) | `Q21.1_2024` | `edu_UnivColl`, `edu_Grad` | university or college (codes 4–8) / graduate school (9) / high school or less (all other codes, ref) |
| Living arrangement (3 levels, ref = with spouse/partner) | `Q1.1_2024` (household size) + `Q3.1_2024` (spouse/partner in household) | `living_Alone`, `living_WithOthers` | with spouse or partner (ref) / alone / with others (non-spouse) |
| Employment status (ref = regular employee) | `Q5.1_2024` | `emp_Executive`, `emp_SelfEmployed`, `emp_NonRegular`, `emp_Student` | regular employee (codes 5–6, ref) / executive (1) / self-employed (2–4) / non-regular (7–11) / student (12–13). "Not working" (codes 14–16) is represented by the occupation variable `occ_NotWorking` |
| Occupation (ref = office/clerical) | `Q7_2024` | `occ_Professional`, `occ_SalesService`, `occ_Manual`, `occ_Other`, `occ_NotWorking` | office/clerical (code 2, ref) / professional or technical (1) / sales and service (3–4) / manual or physical (5–9) / other (10) / not working (Q7 not asked) |

---

## 5. Resource-based predictors (2024 wave)

| Construct | Item | Variable in script | Coding |
|---|---|---|---|
| Annual household income (fixed bands, ref = < 3 million JPY) | `Q80.1_2024` | `inc_mid`, `inc_uppermid`, `inc_high`, `Income_unknown` | 18 income bands: < 3 million JPY (codes 1–5, ref) / 3 to < 6 million (6–8) / 6 to < 10 million (9–12) / ≥ 10 million (13–18); "don't know" (19), "prefer not to answer" (20), or missing → `Income_unknown`. Band midpoints (million JPY) are used only for the descriptive mean in Table 1 |
| LSNS-6 family network low | `Q17.1`–`Q17.3_2024` | `LSNS_family_low` | recode 1–6 → 0–5; sum 0–15; binary indicator < 6 |
| LSNS-6 friend network low | `Q17.4`–`Q17.6_2024` | `LSNS_friends_low` | same coding |
| Smartphone screen time (5 levels, ref = 0–<1 h/day) | `Q28.13_2024` | `sp_1to2h`, `sp_3to4h`, `sp_5plush`, `sp_Unknown` | see screen-time crosswalk below |
| PC/tablet screen time (5 levels, ref = 0–<1 h/day) | `Q28.14_2024` | `pc_1to2h`, `pc_3to4h`, `pc_5plush`, `pc_Unknown` | same coding |

---

## 6. Screen-time crosswalk

The 2024 wave uses 12 categories (Q28.13 / Q28.14); these are collapsed to a
5-level ladder used in the analysis.

| 2024 raw category | Meaning | Mapped to |
|---|---|---|
| 1 | None (0 h) | 0–<1 h |
| 2 | <30 min/day | 0–<1 h |
| 3 | ~30 min/day | 0–<1 h |
| 4 | 1 h/day | 1–2 h |
| 5 | 2 h/day | 1–2 h |
| 6 | 3 h/day | 3–4 h |
| 7 | 4–5 h/day | 3–4 h |
| 8 | 6–7 h/day | 5+ h |
| 9 | 8–9 h/day | 5+ h |
| 10 | 10–11 h/day | 5+ h |
| 11 | ≥12 h/day | 5+ h |
| 12 | Don't know | Unknown |

---

## 7. Derived timing variables

| Variable in script | Source | Coding |
|---|---|---|
| `baseline_date_jst` | `回答完了日時_2024` | Baseline completion timestamp converted from UTC to Japan Standard Time (UTC+9) and truncated to the calendar date |
| `baseline_jan2025` | `baseline_date_jst` | 1 if the baseline was completed on or after 1 January 2025 (JST); these respondents are excluded in the timing sensitivity analysis of Supplementary Table S10 |
| `age3` | `AGE_2024` | <35 / 35–64 / ≥65; grouping for the measurement-invariance analysis of Supplementary Table S3 |

---

## 8. Standardization and estimation conventions

- Continuous predictors are z-standardized on the analytic file (mean 0, SD 1).
- Categorical predictors are entered as 0/1 dummies with the reference category indicated above.
- All five within-user outcomes (intensity, three purpose composites, PCUS) are
  standardized to mean 0, SD 1 prior to estimation.
- AI initiation is a 0/1 outcome entered in modified Poisson regression (log
  link) on the at-risk sample; coefficients are reported as risk ratios.
- Every regression model uses heteroscedasticity-consistent (HC3) standard
  errors; the Benjamini–Hochberg false-discovery-rate correction is applied
  within each model over the 49 baseline predictors. The intensity covariate in
  the intensity-adjusted problematic-use model, and the initiation half-year
  covariate in the timing sensitivity analysis, are reported without
  false-discovery-rate adjustment.
- In the cross-outcome comparisons (Supplementary Tables S6 and S7), the two
  standardized outcomes are stacked (two rows per respondent) and regressed on
  the 49 predictors, an outcome indicator, and their products, with standard
  errors clustered by respondent.
