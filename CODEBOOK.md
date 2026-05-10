# Codebook

JACSIS variable mapping for the analysis script (`analysis.R`).

This codebook documents how the JACSIS 2024 baseline and 2025 follow-up survey
items are mapped onto the analytic variables used in the manuscript. It is
intended for researchers granted access to the JACSIS data who wish to
reproduce or extend the analysis.

All predictors are drawn from the **2024 baseline wave**, with the single
exception of eHealth literacy (eHEALS-8), which was fielded only in the 2025
wave; the manuscript discusses this caveat explicitly. Outcome variables are
derived from the **2025 follow-up wave**, where the AI-use module was
introduced.

---

## 1. Outcomes (2025 wave)

| Construct | Item(s) | Variable in script | Coding |
|---|---|---|---|
| AI use status / initiation timing | Q37S1 | `Q37S1_2025` | 6-cat self-reported start time |
| AI initiation (binary) | derived from Q37S1 | `AI_initiated` | 1 if `Q37S1_2025 ∈ {5, 6}`; 0 if `Q37S1_2025 == 1` |
| AI usage frequency (9 items, 5-pt) | Q37S3.1–Q37S3.9 | `Q37S3.x_2025` | 1 = never … 5 = daily |
| Overall AI-use intensity | mean of 9 Q37S3 items | `AI_use_9` | item mean |
| Purpose: Productivity/Creative | mean of Q37S3.{1, 2, 4, 5} | `AI_purpose_productivity` | document drafting, translation/summarization, image/video generation, learning |
| Purpose: Daily-life/Information | mean of Q37S3.{3, 6, 7} | `AI_purpose_dailyinfo` | information search, daily-life planning, health advice |
| Purpose: Social/Emotional | mean of Q37S3.{8, 9} | `AI_purpose_socialemo` | conversation, emotional support |
| Problematic AI use (PCUS, 11 items, 7-pt) | Q38.1–Q38.11 | `AI_addiction_mean` | item mean |

`Q37S1_2025` categories used by the script:

| Code | Meaning | Used as |
|---|---|---|
| 1 | Never used | Non-event in initiation model; excluded from within-user models |
| 2 | Used in past, but quit | Excluded entirely (heterogeneous group) |
| 3, 4 | Already using before 2025 | Excluded from initiation at-risk frame |
| 5, 6 | Initiated AI use in 2025 | Event in initiation model; within-user sample |

---

## 2. Personal predictors

| Construct | 2024 item | Variable in script | Coding |
|---|---|---|---|
| Sex (binary) | `SEX_2024` | `Sex_female` | 1 = female, 0 = male |
| Age (10 bins, ref = 65+) | `AGE_2024` | `age_lt25` … `age_60_64` | <25 / 25–29 / 30–34 / 35–39 / 40–44 / 45–49 / 50–54 / 55–59 / 60–64 / 65+ |
| Self-rated physical health (Flourishing item) | `Q76.3_2024` | `PhysicalHealth_z` | recode 1–11 → 0–10; standardised |
| K6 distress (6 items, 5-pt) | `Q65.1`–`Q65.6_2024` | `K6_ge13` | recode 1–5 → 0–4; sum 0–24; binary indicator ≥ 13 |
| UCLA-3 loneliness (3 items, 4-pt) | `Q66.1`–`Q66.3_2024` | `UCLA3_z` | recode 1–4 → 0–3; sum 0–9; standardised |
| ACE (10-item CDC–Kaiser proxy) | `Q77.1`–`Q77.9`, `Q77.13_2024` | `ACE_1`, `ACE_2`, `ACE_3`, `ACE_4plus` | sum 0–10; categorised as 0 (ref) / 1 / 2 / 3 / 4+. Q77.9 is reverse-scored ("felt loved by parent" → protective) |
| Big Five (TIPI-10, 7-pt) | `Q79.1`–`Q79.10_2024` | `BigFive_E_z`, `BigFive_A_z`, `BigFive_C_z`, `BigFive_ES_z`, `BigFive_O_z` | 5 dimensions × 2 items each; items 2, 4, 6, 8, 10 reverse-scored; standardised. Japanese TIPI-J (Oshio et al., 2014) |

---

## 3. Positional predictors

| Construct | 2024 item | Variable in script | Coding |
|---|---|---|---|
| Education (3 levels, ref = high school or less) | `Q21.1_2024` | `edu_UnivColl`, `edu_Grad` | high school or less / university or college / graduate |
| Living arrangement (3 levels, ref = with spouse/partner) | `Q1.1_2024` (HH size) + `Q3.1_2024` (spouse in HH) | `living_Alone`, `living_WithOthers` | with spouse-partner (ref) / alone / with others (non-spouse) |
| Employment status (5 levels, ref = regular employee) | `Q5.1_2024` | `emp_Executive`, `emp_SelfEmployed`, `emp_NonRegular`, `emp_Student` | regular employee (ref) / executive / self-employed / non-regular / student. "Not working" is captured by occupation (Q7 = NA → `occ_NotWorking`) |
| Occupation (6 levels, ref = office/clerical) | `Q7_2024` | `occ_Professional`, `occ_SalesService`, `occ_Manual`, `occ_Other`, `occ_NotWorking` | office/clerical (ref) / professional/technical / sales & service / manual/physical / other / not working |

---

## 4. Resource-based predictors

| Construct | Item | Variable in script | Coding |
|---|---|---|---|
| Household income (quartiles, ref = Q1) | `Q80.1_2024` | `inc_Q2`, `inc_Q3`, `inc_Q4`, `Income_unknown` | 18 bands recoded to band-midpoints (millions JPY); quartiles via `dplyr::ntile()`. Quartile cutoffs are computed on the at-risk sample for the initiation regression and re-computed within the within-user sample for the within-user regressions. "Don't know" / "No answer" → separate `Income_unknown` dummy |
| LSNS-6 family network low | `Q17.1`–`Q17.3_2024` | `LSNS_family_low` | recode 1–6 → 0–5; sum 0–15; binary indicator < 6 |
| LSNS-6 friend network low | `Q17.4`–`Q17.6_2024` | `LSNS_friends_low` | same coding |
| eHealth literacy (eHEALS-8) | `Q33.1`–`Q33.8_2025` | `eHEALS_z` | 5-pt; sum 8–40; standardised. **Same-wave 2025 predictor** (not asked in 2024); limitation acknowledged in Methods |
| Smartphone screen time (5 levels, ref = 0–<1 h/day) | `Q28.13_2024` | `sp_1to2h`, `sp_3to4h`, `sp_5plush`, `sp_Unknown` | see screen-time crosswalk below |
| PC/tablet screen time (5 levels, ref = 0–<1 h/day) | `Q28.14_2024` | `pc_1to2h`, `pc_3to4h`, `pc_5plush`, `pc_Unknown` | same coding |

---

## 5. Screen-time crosswalk

The 2024 wave uses 12 frequency bins (Q28.13 / Q28.14); these are collapsed
to a 5-level ladder used in analysis.

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

## 6. Standardisation conventions

- Continuous predictors are z-standardised on the analytic sample (mean 0, SD 1).
- Categorical predictors are entered as 0/1 dummies with the reference category indicated above.
- All five within-user outcomes (intensity, three purpose composites, PCUS) are
  standardised to mean 0, SD 1 prior to estimation, so cross-outcome R²
  differences reflect differential predictor loadings rather than scale
  differences.
- The AI initiation outcome is a 0/1 binary entered as the dependent variable
  in modified Poisson regression (log link, HC0 robust SEs); coefficients are
  reported on both the log–risk-ratio and risk-ratio scales.
