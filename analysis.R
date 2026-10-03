# =============================================================================
# Two-wave (JACSIS 2024 -> 2025) study of the prospective predictors of
# generative-AI initiation, engagement, and problematic use.
#
# Analysis script for:
#   Nakagomi, A., Inagaki, S., & Tabuchi, T. (2026).
#   Prospective predictors of generative AI initiation, engagement, and
#   problematic use among Japanese adults: a two-wave study.
#   [Journal]. DOI: [will be added on acceptance].
#
# Repository: https://github.com/[username]/[repo]
# Author:     Atsushi Nakagomi (anakagomi0211@chiba-u.jp)
# Updated:    2026-09-07
# License:    MIT (see LICENSE file)
#
# DATA AVAILABILITY
# -----------------
# JACSIS data are not publicly available due to ethical restrictions on
# participant privacy. De-identified data may be shared upon reasonable
# request, subject to approval by the JACSIS steering committee and
# relevant ethics review boards. See the manuscript Data Availability
# section for details. Two data files, supplied by approved researchers,
# are expected (paths are set in the Configuration block):
#   data/df.csv           the two-wave panel (respondents who completed
#                         both the 2024 and the 2025 wave); columns named
#                         <item>_2024 / <item>_2025 as documented in
#                         CODEBOOK.md.
#   data/df_2024_all.csv  all valid respondents of the 2024 wave, with the
#                         same <item>_2024 columns plus panel_matched
#                         (1 = also completed the 2025 wave). Used only for
#                         the attrition analysis (Section 15); that section
#                         is skipped, with a message, if the file is absent.
#
# REPRODUCIBILITY
# ---------------
# - R version 4.5.3
# - Required packages: dplyr, tidyr, ggplot2, sandwich, lmtest, psych,
#   GPArotation, car, MASS, quantreg, lavaan, semTools
# - Random seed: set.seed(20260905) (bootstrap and parallel analysis)
# - sessionInfo() is saved to output/sessionInfo.txt at the end of the run.
#
# USAGE
# -----
# 1. Place the data files at the paths defined in the Configuration block.
# 2. Run: Rscript analysis.R
# 3. Outputs are written to the directory defined in OUT_DIR. File names
#    carry the number of the manuscript table, figure, or supplementary
#    table they feed (Table1_, Figure2_, S01_ ... S13_); an index with a
#    one-line description of every file is written to 00_output_index.csv.
#
# SECTIONS
#   1  Configuration, packages, helpers
#   2  Data loading
#   3  Outcome construction (2025 wave)
#   4  Baseline predictor construction (2024 wave)
#   5  Predictor list and labels (49 baseline predictors)
#   6  Analytic samples and reliability
#   7  Model-fitting engine and comparison helpers
#   8  Main models (Supplementary Table S4), outcome correlations (S8),
#      variance-inflation factors (S13)
#   9  Table 1
#  10  Figure 2 (forest plot)
#  11  Directional expectations H1-H3 (S5)
#  12  Direct tests of coefficient differences between outcomes (S7)
#  13  Distinctness of problematic use from intensity, H4 (S6)
#  14  Measurement validation: purpose composites (S1, S2), PCUS (S3)
#  15  Sensitivity analyses: distributional (S9), timing (S10),
#      selection weighting (S11), attrition (S12)
#  16  Output index and session information
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Configuration, packages, helpers
# -----------------------------------------------------------------------------
DATA_PATH      <- "data/df.csv"           # two-wave panel (not distributed)
DATA_2024_PATH <- "data/df_2024_all.csv"  # all valid 2024 respondents (not distributed)
OUT_DIR        <- "output"
BASELINE_TS_COL <- "回答完了日時_2024"   # baseline completion timestamp (UTC, ISO 8601)
N_BOOT     <- 1000   # bootstrap replications for the HTMT confidence interval
N_PARALLEL <- 1000   # random-data replications for parallel analysis

set.seed(20260905)

required <- c("dplyr", "tidyr", "ggplot2", "sandwich", "lmtest", "psych",
              "GPArotation", "car", "MASS", "quantreg", "lavaan", "semTools")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
out_index <- list()
save_csv <- function(x, name, note = "", row.names = FALSE) {
  write.csv(x, file.path(OUT_DIR, name), row.names = row.names)
  out_index[[length(out_index) + 1L]] <<- data.frame(file = name, note = note,
                                                     stringsAsFactors = FALSE)
  invisible(name)
}
note_file <- function(name, note)
  out_index[[length(out_index) + 1L]] <<- data.frame(file = name, note = note,
                                                     stringsAsFactors = FALSE)

g   <- function(x) suppressWarnings(as.numeric(as.character(x)))   # numeric coercion
zs  <- function(x) as.numeric(scale(x))                              # z-standardise
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)

# -----------------------------------------------------------------------------
# 2. Data loading
# -----------------------------------------------------------------------------
if (!file.exists(DATA_PATH)) stop("Data file not found: ", DATA_PATH)
df_raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE, check.names = FALSE,
                   fileEncoding = "UTF-8")
cat("Loaded ", nrow(df_raw), " rows by ", ncol(df_raw), " columns from ",
    DATA_PATH, "\n", sep = "")

# =============================================================================
# 3. Outcome construction (2025 wave)
# =============================================================================
# Q37S1_2025 (generative-AI status; full wording in Supplementary Methods 1):
#   1 = never used; 2 = used in the past but not currently;
#   3, 4 = current user who first used AI before 2025;
#   5 = current user who first used AI in January-June 2025;
#   6 = current user who first used AI in July 2025 or later.
# Option 2 (past users who quit) is excluded from every analysis. Options
# 3-4 (pre-2025 initiators) enter no regression model; they are retained
# only as an independent holdout sample for validating the outcome
# measures.
item_purpose <- paste0("Q37S3.", 1:9, "_2025_ord")    # nine frequency items
item_pcus    <- paste0("Q38.", 1:11, "_2025_ord")     # eleven PCUS items
purpose_item_labels <- c("Document drafting", "Translation/summarization",
                         "Information search", "Image/video generation",
                         "Learning", "Daily-life planning", "Health advice",
                         "Conversation", "Emotional support")

construct_outcomes_2025 <- function(df) {
  q37s1 <- g(df$Q37S1_2025)
  df$Q37S1             <- q37s1
  df$AI_initiated      <- as.integer(q37s1 %in% c(5, 6))
  df$initiation_sample <- as.integer(q37s1 %in% c(1, 5, 6))   # at-risk sample
  df$user_sample       <- as.integer(q37s1 %in% c(5, 6))      # within-user sample
  df$preexisting_user  <- as.integer(q37s1 %in% c(3, 4))      # holdout
  df$init_late2025     <- ifelse(q37s1 %in% c(5, 6), as.integer(q37s1 == 6), NA_integer_)

  # Purpose/frequency items Q37S3.1-9 (5-point) and PCUS items Q38.1-11 (7-point)
  for (k in 1:9)  df[[paste0("Q37S3.", k, "_2025_ord")]] <- g(df[[paste0("Q37S3.", k, "_2025")]])
  for (k in 1:11) df[[paste0("Q38.", k, "_2025_ord")]]   <- g(df[[paste0("Q38.", k, "_2025")]])
  m_int <- as.matrix(df[, item_purpose])
  df$AI_use_9                <- rowMeans(m_int)                       # intensity (all 9 items)
  df$AI_purpose_productivity <- rowMeans(m_int[, c(1, 2, 4, 5)])      # productivity/creative
  df$AI_purpose_dailyinfo    <- rowMeans(m_int[, c(3, 6, 7)])         # daily-life/information
  df$AI_purpose_socialemo    <- rowMeans(m_int[, c(8, 9)])            # social/emotional
  df$AI_addiction_mean       <- rowMeans(as.matrix(df[, item_pcus]))  # problematic AI use (PCUS)

  # 2025-wave K6 and UCLA-3 (concurrent convergent validity of the PCUS)
  if (all(paste0("Q65.", 1:6, "_2025") %in% names(df))) {
    k6m <- 5 - sapply(paste0("Q65.", 1:6, "_2025"), function(v) g(df[[v]]))
    k6m[k6m < 0 | k6m > 4] <- NA
    df$K6_sum_2025 <- rowSums(k6m)
  } else df$K6_sum_2025 <- NA_real_
  if (all(paste0("Q66.", 1:3, "_2025") %in% names(df))) {
    um <- 4 - sapply(paste0("Q66.", 1:3, "_2025"), function(v) g(df[[v]]))
    um[um < 0 | um > 3] <- NA
    df$UCLA3_sum_2025 <- rowSums(um)
  } else df$UCLA3_sum_2025 <- NA_real_
  df
}

# =============================================================================
# 4. Baseline predictor construction (2024 wave)
# =============================================================================
# construct_baseline() works on any data frame that carries the <item>_2024
# columns; it is applied to the panel file and to the all-2024 file.
construct_baseline <- function(df) {

  # Sex (1 = female)
  df$Sex_female <- ifelse(g(df$SEX_2024) == 2, 1L, 0L)

  # Age (10 bands; reference = 65+)
  age <- g(df$AGE_2024)
  age_breaks <- c(-Inf, 24, 29, 34, 39, 44, 49, 54, 59, 64, Inf)
  age_labels <- c("lt25", "25_29", "30_34", "35_39", "40_44", "45_49",
                  "50_54", "55_59", "60_64", "65plus")
  df$age_cat <- cut(age, breaks = age_breaks, labels = age_labels,
                    right = TRUE, include.lowest = TRUE)
  for (lab in age_labels[age_labels != "65plus"])
    df[[paste0("age_", lab)]] <- as.integer(df$age_cat == lab)
  df$Age_years <- age
  df$age3 <- factor(case_when(age < 35 ~ "lt35", age < 65 ~ "35_64", TRUE ~ "65plus"),
                    levels = c("lt35", "35_64", "65plus"))   # invariance grouping

  # Self-rated physical health (Q76.3; 1-11 -> 0-10; standardised)
  ph <- g(df$Q76.3_2024)
  df$PhysicalHealth_raw <- ifelse(ph %in% 1:11, ph - 1, NA_real_)
  df$PhysicalHealth_z   <- zs(df$PhysicalHealth_raw)

  # Kessler 6 (Q65.1-6; 1 = always ... 5 = never -> 4 ... 0; sum 0-24; >= 13)
  k6_mat   <- sapply(paste0("Q65.", 1:6, "_2024"), function(v) g(df[[v]]))
  k6_score <- 5 - k6_mat
  k6_score[k6_score < 0 | k6_score > 4] <- NA
  df$K6_sum  <- rowSums(k6_score)
  df$K6_ge13 <- as.integer(df$K6_sum >= 13)

  # UCLA-3 loneliness (Q66.1-3; 1 = always ... 4 = never -> 3 ... 0; sum 0-9)
  ucla_mat   <- sapply(paste0("Q66.", 1:3, "_2024"), function(v) g(df[[v]]))
  ucla_score <- 4 - ucla_mat
  ucla_score[ucla_score < 0 | ucla_score > 3] <- NA
  df$UCLA3_sum <- rowSums(ucla_score)
  df$UCLA3_z   <- zs(df$UCLA3_sum)

  # Adverse childhood experiences (10 items: Q77.1-8 and Q77.13 coded
  # 1 = yes / 2 = no; Q77.9 "felt loved by a parent" is protective and is
  # reverse-scored before summing)
  ace_pos <- sapply(paste0("Q77.", c(1:8, 13), "_2024"), function(v) {
    x <- g(df[[v]]); ifelse(x == 1, 1L, ifelse(x == 2, 0L, NA_integer_))
  })
  ace_q9  <- { x <- g(df$Q77.9_2024); ifelse(x == 1, 0L, ifelse(x == 2, 1L, NA_integer_)) }
  ace_mat <- cbind(ace_pos, ace_q9)
  df$ACE_sum <- rowSums(ace_mat)
  df$ACE_cat <- cut(df$ACE_sum, breaks = c(-Inf, 0, 1, 2, 3, Inf),
                    labels = c("0", "1", "2", "3", "4plus"), right = TRUE)
  for (lv in c("1", "2", "3", "4plus"))
    df[[paste0("ACE_", lv)]] <- as.integer(df$ACE_cat == lv)

  # Big Five (TIPI-J, Q79.1-10, 7-point; items 2, 4, 6, 8, 10 reverse-scored;
  # each dimension is the mean of two items; standardised)
  tipi_raw <- sapply(paste0("Q79.", 1:10, "_2024"), function(v) g(df[[v]]))
  tipi_raw[tipi_raw < 1 | tipi_raw > 7] <- NA
  tipi_score <- tipi_raw
  tipi_score[, c(2, 4, 6, 8, 10)] <- 8 - tipi_score[, c(2, 4, 6, 8, 10)]
  df$BigFive_E  <- rowMeans(tipi_score[, c(1, 6)])
  df$BigFive_A  <- rowMeans(tipi_score[, c(2, 7)])
  df$BigFive_C  <- rowMeans(tipi_score[, c(3, 8)])
  df$BigFive_ES <- rowMeans(tipi_score[, c(4, 9)])
  df$BigFive_O  <- rowMeans(tipi_score[, c(5, 10)])
  for (v in c("BigFive_E", "BigFive_A", "BigFive_C", "BigFive_ES", "BigFive_O"))
    df[[paste0(v, "_z")]] <- zs(df[[v]])

  # Education (Q21.1; reference = high school or less)
  edu <- g(df$Q21.1_2024)
  df$Education_cat <- factor(case_when(
    edu == 9 ~ "Graduate", edu %in% 4:8 ~ "UnivCollege", TRUE ~ "HSorLess"),
    levels = c("HSorLess", "UnivCollege", "Graduate"))
  df$edu_UnivColl <- as.integer(df$Education_cat == "UnivCollege")
  df$edu_Grad     <- as.integer(df$Education_cat == "Graduate")

  # Living arrangement (Q1.1 household size; Q3.1 spouse/partner in household)
  hh <- g(df$Q1.1_2024); spouse <- g(df$Q3.1_2024)
  df$Living_arrangement <- factor(case_when(
    hh == 1 ~ "Alone",
    hh >= 2 & !is.na(spouse) & spouse >= 1 ~ "WithSpouse",
    hh >= 2 ~ "WithOthers",
    TRUE ~ NA_character_), levels = c("WithSpouse", "Alone", "WithOthers"))
  df$living_Alone      <- as.integer(df$Living_arrangement == "Alone")
  df$living_WithOthers <- as.integer(df$Living_arrangement == "WithOthers")

  # Employment status (Q5.1; reference = regular employee)
  emp <- g(df$Q5.1_2024)
  df$Employment <- factor(case_when(
    emp %in% c(5, 6) ~ "Regular", emp == 1 ~ "Executive",
    emp %in% c(2, 3, 4) ~ "SelfEmployed", emp %in% 7:11 ~ "NonRegular",
    emp %in% c(12, 13) ~ "Student", emp %in% 14:16 ~ "NotWorking",
    TRUE ~ NA_character_),
    levels = c("Regular", "Executive", "SelfEmployed", "NonRegular", "Student", "NotWorking"))
  for (lv in c("Executive", "SelfEmployed", "NonRegular", "Student"))
    df[[paste0("emp_", lv)]] <- as.integer(df$Employment == lv)

  # Occupation (Q7; reference = office/clerical; Q7 missing = not working,
  # so that non-employed respondents are retained)
  occ <- g(df$Q7_2024)
  df$Occupation <- factor(case_when(
    occ == 2 ~ "Office", occ == 1 ~ "Professional", occ %in% c(3, 4) ~ "SalesService",
    occ %in% 5:9 ~ "Manual", occ == 10 ~ "Other", is.na(occ) ~ "NotWorking",
    TRUE ~ "Other"),
    levels = c("Office", "Professional", "SalesService", "Manual", "Other", "NotWorking"))
  for (lv in c("Professional", "SalesService", "Manual", "Other", "NotWorking"))
    df[[paste0("occ_", lv)]] <- as.integer(df$Occupation == lv)

  # Annual household income (Q80.1; 18 bands plus "don't know" (19) and
  # "prefer not to answer" (20)). Entered as fixed bands: < 3 million JPY
  # (codes 1-5; reference), 3 to < 6 million (6-8), 6 to < 10 million
  # (9-12), >= 10 million (13-18), and unknown (19-20 or missing). Band
  # midpoints are used only for the descriptive mean in Table 1.
  q80_midpoints <- c(0, 0.25, 0.75, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
                     8.5, 9.5, 11, 13, 15, 17, 19, 22, NA, NA)
  inc <- g(df$Q80.1_2024)
  df$Income_million <- ifelse(inc %in% 1:18, q80_midpoints[inc], NA_real_)
  df$Income_unknown <- as.integer(inc %in% c(19, 20) | is.na(inc))
  df$Income_band <- factor(case_when(
    inc %in% 1:5   ~ "low",
    inc %in% 6:8   ~ "mid",
    inc %in% 9:12  ~ "uppermid",
    inc %in% 13:18 ~ "high",
    TRUE           ~ "Unknown"),
    levels = c("low", "mid", "uppermid", "high", "Unknown"))
  df$inc_mid      <- as.integer(df$Income_band == "mid")
  df$inc_uppermid <- as.integer(df$Income_band == "uppermid")
  df$inc_high     <- as.integer(df$Income_band == "high")

  # Lubben Social Network Scale-6 (Q17.1-3 family, Q17.4-6 friends;
  # 1-6 -> 0-5; sum 0-15; "low" = sum < 6)
  lsns_mat   <- sapply(paste0("Q17.", 1:6, "_2024"), function(v) g(df[[v]]))
  lsns_score <- lsns_mat - 1
  lsns_score[lsns_score < 0 | lsns_score > 5] <- NA
  df$LSNS_family_sum  <- rowSums(lsns_score[, 1:3])
  df$LSNS_friends_sum <- rowSums(lsns_score[, 4:6])
  df$LSNS_family_low  <- as.integer(df$LSNS_family_sum  < 6)
  df$LSNS_friends_low <- as.integer(df$LSNS_friends_sum < 6)

  # Daily screen time (Q28.13 smartphone; Q28.14 PC/tablet; 12 categories
  # collapsed to 0-<1 h (reference), 1-2 h, 3-4 h, >= 5 h, unknown)
  recode_screen <- function(x) factor(case_when(
    x %in% 1:3 ~ "lt1h", x %in% 4:5 ~ "1to2h", x %in% 6:7 ~ "3to4h",
    x %in% 8:11 ~ "5plush", x == 12 ~ "Unknown", TRUE ~ NA_character_),
    levels = c("lt1h", "1to2h", "3to4h", "5plush", "Unknown"))
  df$Smartphone_cat <- recode_screen(g(df$Q28.13_2024))
  df$PCtab_cat      <- recode_screen(g(df$Q28.14_2024))
  for (lv in c("1to2h", "3to4h", "5plush", "Unknown")) {
    df[[paste0("sp_", lv)]] <- as.integer(df$Smartphone_cat == lv)
    df[[paste0("pc_", lv)]] <- as.integer(df$PCtab_cat == lv)
  }

  # Baseline completion date (UTC ISO 8601 timestamp -> Japan Standard Time)
  if (BASELINE_TS_COL %in% names(df)) {
    ts <- as.POSIXct(as.character(df[[BASELINE_TS_COL]]),
                     format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    df$baseline_date_jst <- as.Date(format(ts + 9 * 3600, "%Y-%m-%d"))
    df$baseline_month    <- format(df$baseline_date_jst, "%Y-%m")
    df$baseline_jan2025  <- as.integer(df$baseline_date_jst >= as.Date("2025-01-01"))
  } else {
    df$baseline_date_jst <- as.Date(NA); df$baseline_month <- NA_character_
    df$baseline_jan2025  <- NA_integer_
  }

  attr(df, "score_mats") <- list(k6 = k6_score, ucla = ucla_score, ace = ace_mat,
                                 tipi = tipi_score, lsns = lsns_score)
  df
}

# --- Apply to the panel file -------------------------------------------------
df <- construct_outcomes_2025(df_raw)
df <- df[!(df$Q37S1 %in% 2), , drop = FALSE]     # exclude past users who quit
df <- construct_baseline(df)
score_mats <- attr(df, "score_mats")

# =============================================================================
# 5. Predictor list and labels (49 baseline predictors)
# =============================================================================
predictor_groups <- list(
  Personal = c(
    "Sex_female",
    paste0("age_", c("lt25", "25_29", "30_34", "35_39", "40_44", "45_49",
                     "50_54", "55_59", "60_64")),
    "PhysicalHealth_z", "K6_ge13", "UCLA3_z",
    paste0("ACE_", c("1", "2", "3", "4plus")),
    "BigFive_E_z", "BigFive_A_z", "BigFive_C_z", "BigFive_ES_z", "BigFive_O_z"
  ),
  Positional = c(
    "edu_UnivColl", "edu_Grad",
    "living_Alone", "living_WithOthers",
    "emp_Executive", "emp_SelfEmployed", "emp_NonRegular", "emp_Student",
    "occ_Professional", "occ_SalesService", "occ_Manual", "occ_Other", "occ_NotWorking"
  ),
  Resource = c(
    "inc_mid", "inc_uppermid", "inc_high", "Income_unknown",
    "LSNS_family_low", "LSNS_friends_low",
    "sp_1to2h", "sp_3to4h", "sp_5plush", "sp_Unknown",
    "pc_1to2h", "pc_3to4h", "pc_5plush", "pc_Unknown"
  )
)
all_predictors <- unlist(predictor_groups, use.names = FALSE)
stopifnot(length(all_predictors) == 49)

pred_label <- c(
  Sex_female        = "Sex: Female (vs. Male, ref.)",
  age_lt25          = "Age: <25",
  age_25_29         = "Age: 25-29",
  age_30_34         = "Age: 30-34",
  age_35_39         = "Age: 35-39",
  age_40_44         = "Age: 40-44",
  age_45_49         = "Age: 45-49",
  age_50_54         = "Age: 50-54",
  age_55_59         = "Age: 55-59",
  age_60_64         = "Age: 60-64",
  PhysicalHealth_z  = "Physical health (per 1 SD)",
  K6_ge13           = "K6 >= 13 (severe distress)",
  UCLA3_z           = "Loneliness (UCLA-3, per 1 SD)",
  ACE_1             = "ACE: 1 event (vs. 0)",
  ACE_2             = "ACE: 2 events",
  ACE_3             = "ACE: 3 events",
  ACE_4plus         = "ACE: 4+ events",
  BigFive_E_z       = "Big Five: Extraversion (per 1 SD)",
  BigFive_A_z       = "Big Five: Agreeableness (per 1 SD)",
  BigFive_C_z       = "Big Five: Conscientiousness (per 1 SD)",
  BigFive_ES_z      = "Big Five: Emotional stability (per 1 SD)",
  BigFive_O_z       = "Big Five: Openness (per 1 SD)",
  edu_UnivColl      = "Education: University/College (vs. <=High school, ref.)",
  edu_Grad          = "Education: Graduate school",
  living_Alone      = "Living alone (vs. with spouse/partner, ref.)",
  living_WithOthers = "Living with others (non-spouse)",
  emp_Executive     = "Employment: Executive (vs. Regular employee, ref.)",
  emp_SelfEmployed  = "Employment: Self-employed",
  emp_NonRegular    = "Employment: Non-regular",
  emp_Student       = "Employment: Student",
  occ_Professional  = "Occupation: Professional/Technical (vs. Office/Clerical, ref.)",
  occ_SalesService  = "Occupation: Sales & Service",
  occ_Manual        = "Occupation: Manual/Physical",
  occ_Other         = "Occupation: Other",
  occ_NotWorking    = "Occupation: Not working (no occupation)",
  inc_mid           = "Income: 3 to <6M JPY (vs. <3M, ref.)",
  inc_uppermid      = "Income: 6 to <10M JPY",
  inc_high          = "Income: >=10M JPY",
  Income_unknown    = "Income: Unknown",
  LSNS_family_low   = "LSNS-6 family low (<6)",
  LSNS_friends_low  = "LSNS-6 friends low (<6)",
  sp_1to2h          = "Smartphone screen time: 1-2 h/day (vs. 0-<1h, ref.)",
  sp_3to4h          = "Smartphone: 3-4 h/day",
  sp_5plush         = "Smartphone: 5+ h/day",
  sp_Unknown        = "Smartphone: Unknown",
  pc_1to2h          = "PC/tablet: 1-2 h/day (vs. 0-<1h, ref.)",
  pc_3to4h          = "PC/tablet: 3-4 h/day",
  pc_5plush         = "PC/tablet: 5+ h/day",
  pc_Unknown        = "PC/tablet: Unknown",
  AI_use_9_z        = "AI-use intensity (per 1 SD; covariate)",
  init_late2025     = "Initiated Jul-Dec 2025 (vs. Jan-Jun 2025; covariate)"
)
pred_domain <- c(
  setNames(rep("Personal",   length(predictor_groups$Personal)),   predictor_groups$Personal),
  setNames(rep("Positional", length(predictor_groups$Positional)), predictor_groups$Positional),
  setNames(rep("Resource",   length(predictor_groups$Resource)),   predictor_groups$Resource)
)

outcome_levels <- c("Initiation", "Intensity", "Productivity_purpose",
                    "DailyInfo_purpose", "SocialEmo_purpose", "PCUS",
                    "PCUS_intensity_adjusted")
within_outcome_vars <- c(
  Intensity            = "AI_use_9",
  Productivity_purpose = "AI_purpose_productivity",
  DailyInfo_purpose    = "AI_purpose_dailyinfo",
  SocialEmo_purpose    = "AI_purpose_socialemo",
  PCUS                 = "AI_addiction_mean"
)
# Reader-facing names of the specifications reported in the sensitivity analyses
spec_labels <- c(
  main               = "Main analysis (49 baseline predictors, HC3)",
  prop_odds          = "Proportional-odds ordinal regression",
  log_OLS            = "Log-transformed PCUS, OLS",
  quantile_0.9       = "Quantile regression (tau = .90)",
  exclude_Jan2025    = "Baseline completed in December 2024 only",
  timing_covariate   = "Adjusted for initiation half-year",
  IPSW               = "Selection-weighted (inverse probability of adoption)",
  IPAW               = "Attrition-weighted (inverse probability of retention)")

# =============================================================================
# 6. Analytic samples and reliability
# =============================================================================
df_init <- df[df$initiation_sample == 1L, , drop = FALSE]   # at-risk sample
df_user <- df[df$user_sample == 1L, , drop = FALSE]         # new-onset users
df_pre  <- df[df$preexisting_user == 1L, , drop = FALSE]    # holdout (measurement only)

cat("\nAt-risk (initiation) sample N =", nrow(df_init),
    "; new-onset users =", sum(df_init$AI_initiated), "\n")
cat("Within-user sample n =", nrow(df_user), "\n")
cat("Pre-2025 initiators (holdout) n =", nrow(df_pre), "\n")

sample_sizes <- data.frame(
  Sample = c("Two-wave panel after excluding past users who quit (Q37S1 = 2)",
             "At-risk sample (never-users + new-onset 2025 users)",
             "Never-users at follow-up (Q37S1 = 1)",
             "New-onset 2025 users (Q37S1 = 5 or 6)",
             "  of which initiated January-June 2025 (Q37S1 = 5)",
             "  of which initiated July-December 2025 (Q37S1 = 6)",
             "Pre-2025 initiators (Q37S1 = 3 or 4; holdout for measurement validation)",
             "At-risk respondents whose baseline was completed on/after 2025-01-01 JST"),
  N = c(nrow(df), nrow(df_init), sum(df_init$AI_initiated == 0), nrow(df_user),
        sum(df_user$init_late2025 == 0, na.rm = TRUE), sum(df_user$init_late2025 == 1, na.rm = TRUE),
        nrow(df_pre), sum(df_init$baseline_jan2025 == 1, na.rm = TRUE)))
save_csv(sample_sizes, "00_sample_sizes.csv", "Sample derivation counts (Figure 1)")

alpha_safe <- function(mat, label) {
  mat <- as.matrix(mat)
  a <- tryCatch(suppressWarnings(psych::alpha(mat, na.rm = TRUE, warnings = FALSE,
                                              check.keys = FALSE)),
                error = function(e) NULL)
  data.frame(Scale = label, alpha = if (is.null(a)) NA else round(a$total$raw_alpha, 3),
             n_items = ncol(mat))
}
alpha_df <- dplyr::bind_rows(
  alpha_safe(score_mats$k6,   "K6 (6 items, 2024)"),
  alpha_safe(score_mats$ucla, "UCLA-3 (3 items, 2024)"),
  alpha_safe(score_mats$ace,  "ACE (10 items, 2024)"),
  alpha_safe(score_mats$tipi[, c(1, 6)],  "TIPI Extraversion (2 items)"),
  alpha_safe(score_mats$tipi[, c(2, 7)],  "TIPI Agreeableness (2 items)"),
  alpha_safe(score_mats$tipi[, c(3, 8)],  "TIPI Conscientiousness (2 items)"),
  alpha_safe(score_mats$tipi[, c(4, 9)],  "TIPI Emotional stability (2 items)"),
  alpha_safe(score_mats$tipi[, c(5, 10)], "TIPI Openness (2 items)"),
  alpha_safe(score_mats$lsns[, 1:3], "LSNS-6 family (3 items)"),
  alpha_safe(score_mats$lsns[, 4:6], "LSNS-6 friends (3 items)"),
  alpha_safe(df_user[, item_purpose], "AI-use intensity, 9 items (new-onset users)"),
  alpha_safe(df_user[, item_purpose[c(1, 2, 4, 5)]], "Purpose: productivity/creative (4 items)"),
  alpha_safe(df_user[, item_purpose[c(3, 6, 7)]],    "Purpose: daily-life/information (3 items)"),
  alpha_safe(df_user[, item_purpose[c(8, 9)]],       "Purpose: social/emotional (2 items)"),
  alpha_safe(df_user[, item_pcus], "PCUS, 11 items (new-onset users)"),
  alpha_safe(df_pre[, item_pcus],  "PCUS, 11 items (pre-2025 initiators)")
)
save_csv(alpha_df, "00_reliability_alpha.csv", "Cronbach's alpha for every multi-item scale")

# =============================================================================
# 7. Model-fitting engine and comparison helpers
# =============================================================================
# fit_outcome(): one regression with heteroscedasticity-consistent (HC3)
# standard errors.
#   family = "linear"       OLS on a standardised outcome (unless std = FALSE)
#            "poisson_log"  modified Poisson (log-link Poisson with robust
#                           standard errors; risk ratios). Fitted as
#                           quasipoisson so that fractional weights raise no
#                           warning; point estimates are identical to poisson().
#            "logistic"     logistic regression (odds ratios)
#   weights: optional probability weights (selection or attrition weights).
#   bh_family: the predictors whose p-values form the Benjamini-Hochberg
#            family (the 49 baseline predictors); covariates such as
#            intensity or initiation half-year are reported but not adjusted.
fit_outcome <- function(data, outcome_var, family = "linear",
                        predictors = all_predictors, extra = character(0),
                        std = TRUE, weights = NULL, vcov_type = "HC3",
                        bh_family = predictors, outcome_name = outcome_var) {
  use_preds <- c(predictors, extra)
  d <- data
  d$.y <- d[[outcome_var]]
  if (family == "linear" && std) d$.y <- zs(d$.y)
  for (v in extra) {
    if (is.numeric(d[[v]]) && length(unique(stats::na.omit(d[[v]]))) > 2)
      d[[v]] <- zs(d[[v]])
  }
  d$.w <- if (is.null(weights)) 1 else weights
  keep <- complete.cases(d[, c(".y", ".w", use_preds)])
  d <- d[keep, , drop = FALSE]
  N <- nrow(d)
  rhs <- paste(paste0("`", use_preds, "`"), collapse = " + ")
  fml <- as.formula(paste0(".y ~ ", rhs))
  m <- switch(family,
    linear      = lm(fml, data = d, weights = .w),
    poisson_log = suppressWarnings(glm(fml, data = d, weights = .w,
                                       family = quasipoisson(link = "log"))),
    logistic    = suppressWarnings(glm(fml, data = d, weights = .w,
                                       family = quasibinomial(link = "logit"))),
    stop("unknown family"))
  V <- sandwich::vcovHC(m, type = vcov_type)
  if (any(!is.finite(diag(V)))) {
    # HC3 is undefined when a leverage equals 1 (a dummy variable that
    # identifies a single observation); this does not occur at the sample
    # sizes of the study.
    warning(sprintf("%s: %s gave non-finite variances; HC1 used instead",
                    outcome_name, vcov_type))
    V <- sandwich::vcovHC(m, type = "HC1")
  }
  ct <- lmtest::coeftest(m, vcov. = V)
  ci <- lmtest::coefci(m, vcov. = V, level = 0.95)
  rn <- gsub("`", "", rownames(ct))
  pick <- match(use_preds, rn)
  out <- data.frame(
    Outcome = outcome_name, Predictor = use_preds,
    beta = unname(ct[pick, 1]), SE = unname(ct[pick, 2]),
    z_t = unname(ct[pick, 3]), p = unname(ct[pick, 4]),
    CI_lo = unname(ci[pick, 1]), CI_hi = unname(ci[pick, 2]),
    N = N, Family = family, stringsAsFactors = FALSE)
  if (family %in% c("poisson_log", "logistic")) {
    out$RR <- exp(out$beta); out$RR_lo <- exp(out$CI_lo); out$RR_hi <- exp(out$CI_hi)
  } else { out$RR <- NA_real_; out$RR_lo <- NA_real_; out$RR_hi <- NA_real_ }
  bh_idx <- which(out$Predictor %in% bh_family)
  out$p_BH <- NA_real_
  out$p_BH[bh_idx] <- p.adjust(out$p[bh_idx], method = "BH")
  out$BH_sig  <- as.integer(out$p_BH < 0.05)
  out$sig_raw <- as.integer(out$p < 0.05)
  out$R2 <- if (family == "linear") summary(m)$r.squared else
    1 - m$deviance / m$null.deviance   # deviance pseudo-R2 for GLMs
  list(table = out, model = m, vcov = V, data = d)
}

# fit_all_models(): the seven models of the paper (one initiation model on
# the at-risk sample; five within-user models; the intensity-adjusted
# problematic-use model), with optional weights.
fit_all_models <- function(d_init, d_user, predictors = all_predictors,
                           w_init = NULL, w_user = NULL, tag = "main") {
  fits <- list()
  fits$Initiation <- fit_outcome(d_init, "AI_initiated", "poisson_log", predictors,
                                 std = FALSE, weights = w_init, outcome_name = "Initiation")
  for (nm in names(within_outcome_vars))
    fits[[nm]] <- fit_outcome(d_user, within_outcome_vars[[nm]], "linear", predictors,
                              weights = w_user, outcome_name = nm)
  d_user$AI_use_9_z <- zs(d_user$AI_use_9)
  fits$PCUS_intensity_adjusted <- fit_outcome(d_user, "AI_addiction_mean", "linear",
                                              predictors, extra = "AI_use_9_z",
                                              weights = w_user,
                                              outcome_name = "PCUS_intensity_adjusted")
  tab <- dplyr::bind_rows(lapply(fits, `[[`, "table"))
  tab$Outcome <- factor(tab$Outcome, levels = outcome_levels)
  tab$Spec <- tag
  list(fits = fits, table = tab)
}

# fit_within(): the six within-user models only (used by the sensitivity
# analyses that concern the within-user sample)
fit_within <- function(d_user, predictors = all_predictors, extra = character(0),
                       weights = NULL, tag) {
  fits <- lapply(names(within_outcome_vars), function(nm)
    fit_outcome(d_user, within_outcome_vars[[nm]], "linear", predictors, extra = extra,
                weights = weights, outcome_name = nm))
  names(fits) <- names(within_outcome_vars)
  d_user$AI_use_9_z <- zs(d_user$AI_use_9)
  fits$PCUS_intensity_adjusted <- fit_outcome(d_user, "AI_addiction_mean", "linear", predictors,
                                              extra = c(extra, "AI_use_9_z"), weights = weights,
                                              outcome_name = "PCUS_intensity_adjusted")
  tab <- dplyr::bind_rows(lapply(fits, `[[`, "table"))
  tab$Outcome <- factor(tab$Outcome, levels = outcome_levels); tab$Spec <- tag
  list(fits = fits, table = tab)
}

# make_wide(): presentation table, estimate (95% CI) with the BH flag, one
# column per outcome (* = BH-adjusted p < .05; † = covariate, unadjusted p < .05)
make_wide <- function(tab, predictors = all_predictors, extra_rows = character(0)) {
  tab <- tab %>% dplyr::mutate(
    est = ifelse(Family == "poisson_log", RR, beta),
    lo  = ifelse(Family == "poisson_log", RR_lo, CI_lo),
    hi  = ifelse(Family == "poisson_log", RR_hi, CI_hi),
    cell = sprintf("%s (%s, %s)%s", fmt(est), fmt(lo), fmt(hi),
                   ifelse(BH_sig %in% 1L, "*",
                          ifelse(is.na(BH_sig) & sig_raw %in% 1L, "†", ""))))
  wide <- tab %>% dplyr::select(Predictor, Outcome, cell) %>%
    tidyr::pivot_wider(names_from = Outcome, values_from = cell)
  rows <- c(predictors, extra_rows)
  wide <- wide[match(rows, wide$Predictor), , drop = FALSE]
  wide$Label  <- unname(pred_label[wide$Predictor])
  wide$Domain <- unname(pred_domain[wide$Predictor])
  wide[, c("Domain", "Predictor", "Label", intersect(outcome_levels, names(wide)))]
}

# compare_specs(): concordance of an alternative specification with the main
# models, per outcome: sign agreement, agreement in BH significance, share
# of main-model-significant predictors retained, Spearman correlation of
# the z-statistics, mean and maximum absolute difference in estimates.
compare_specs <- function(alt, main, alt_label, predictors = all_predictors) {
  m <- main %>% dplyr::filter(Predictor %in% predictors) %>%
    dplyr::select(Outcome, Predictor, beta_main = beta, z_main = z_t, sig_main = BH_sig)
  a <- alt %>% dplyr::filter(Predictor %in% predictors) %>%
    dplyr::select(Outcome, Predictor, beta_alt = beta, z_alt = z_t, sig_alt = BH_sig,
                  N_alt = N)
  j <- dplyr::inner_join(m, a, by = c("Outcome", "Predictor"))
  j %>% dplyr::group_by(Outcome) %>% dplyr::summarise(
    Spec = alt_label,
    Specification = ifelse(alt_label %in% names(spec_labels),
                           unname(spec_labels[alt_label]), alt_label),
    N_alt = dplyr::first(N_alt), n_pred = dplyr::n(),
    sign_agreement = mean(sign(beta_main) == sign(beta_alt), na.rm = TRUE),
    BH_agreement = mean(sig_main == sig_alt, na.rm = TRUE),
    main_sig_retained = ifelse(sum(sig_main == 1, na.rm = TRUE) > 0,
                               mean(sig_alt[sig_main %in% 1L] == 1, na.rm = TRUE), NA),
    spearman_z = suppressWarnings(cor(z_main, z_alt, method = "spearman", use = "complete.obs")),
    mean_abs_diff = mean(abs(beta_main - beta_alt), na.rm = TRUE),
    max_abs_diff = max(abs(beta_main - beta_alt), na.rm = TRUE), .groups = "drop")
}

# =============================================================================
# 8. Main models (Supplementary Table S4), outcome correlations (S8),
#    variance-inflation factors (S13)
# =============================================================================
main <- fit_all_models(df_init, df_user, all_predictors, tag = "main")
main_tab <- main$table
save_csv(main_tab, "S04_main_regression_long.csv",
         "All coefficients: 7 models x 49 predictors (+ intensity covariate), HC3, BH within outcome")
save_csv(make_wide(main_tab, all_predictors, extra_rows = "AI_use_9_z"),
         "S04_main_regression_wide.csv",
         "Supplementary Table S4: estimate (95% CI); * = BH-adjusted p < .05 within outcome; † = covariate p < .05")

r2_tab <- main_tab %>% dplyr::group_by(Outcome) %>%
  dplyr::summarise(N = dplyr::first(N), R2 = dplyr::first(R2), .groups = "drop") %>%
  dplyr::mutate(R2_type = ifelse(Outcome == "Initiation", "deviance pseudo-R2", "OLS R2"))
save_csv(r2_tab, "S04_R2_by_outcome.csv", "R2 per model (descriptive context for H4)")

# Residual correlations among the five within-user outcomes after adjustment
# for the 49 predictors, and unadjusted correlations (Supplementary Table S8)
resid_mat <- sapply(names(within_outcome_vars), function(nm) {
  r <- residuals(main$fits[[nm]]$model); names(r) <- rownames(main$fits[[nm]]$data); r[rownames(df_user)] })
resid_corr <- round(cor(resid_mat, use = "pairwise.complete.obs"), 3)
save_csv(as.data.frame(resid_corr), "S08_residual_correlations.csv",
         "Pearson correlations of the residuals of the five within-user models", row.names = TRUE)
raw_corr <- round(cor(df_user[, unname(within_outcome_vars)]), 3)
dimnames(raw_corr) <- list(names(within_outcome_vars), names(within_outcome_vars))
save_csv(as.data.frame(raw_corr), "S08_unadjusted_outcome_correlations.csv",
         "Pearson correlations of the five within-user outcomes (unadjusted)", row.names = TRUE)

# Generalised variance-inflation factors (Supplementary Table S13)
gvif_table <- function(m, label) {
  ali <- is.na(coef(m))
  if (any(ali)) {   # aliased terms cannot occur at the study's sample sizes
    keep <- setdiff(gsub("`", "", names(coef(m))[!ali]), "(Intercept)")
    m <- lm(as.formula(paste0(".y ~ ", paste(paste0("`", keep, "`"), collapse = " + "))), data = m$model)
  }
  v <- car::vif(m)
  if (is.matrix(v)) data.frame(Model = label, Predictor = rownames(v), GVIF = round(v[, 1], 3))
  else data.frame(Model = label, Predictor = gsub("`", "", names(v)), GVIF = round(v, 3))
}
gvif_tab <- dplyr::bind_rows(
  gvif_table(main$fits$Intensity$model, "49 predictors (within-user models)"),
  gvif_table(main$fits$PCUS_intensity_adjusted$model, "49 predictors + intensity (intensity-adjusted model)"))
save_csv(gvif_tab, "S13_GVIF.csv", "Generalised variance-inflation factors")

# =============================================================================
# 9. Table 1: baseline characteristics of the at-risk sample
# =============================================================================
t1_subsets <- list(
  `Overall (at-risk)`    = df_init,
  `Never users`          = df_init[df_init$AI_initiated == 0L, , drop = FALSE],
  `New-onset 2025 users` = df_user)
fmt_n_pct   <- function(n, denom) if (denom == 0) "-" else sprintf("%d (%.1f)", n, 100 * n / denom)
fmt_mean_sd <- function(x) { x <- x[!is.na(x)]; if (!length(x)) "-" else sprintf("%.2f (%.2f)", mean(x), sd(x)) }
t1_rows <- list()
add_row <- function(row) t1_rows[[length(t1_rows) + 1L]] <<- row
add_row(c(list(Variable = "Sample N", Category = ""),
          lapply(t1_subsets, function(d) sprintf("%d", nrow(d)))))
add_continuous <- function(var, label) {
  add_row(c(list(Variable = label, Category = "Mean (SD)"),
            lapply(t1_subsets, function(d) fmt_mean_sd(d[[var]]))))
}
# Categorical rows: percentages are column percentages over the full column N
add_categorical <- function(var, label, cat_labels) {
  levs <- names(cat_labels)
  for (i in seq_along(levs)) {
    lvl <- levs[i]
    cells <- lapply(t1_subsets, function(d) {
      x <- as.character(d[[var]]); fmt_n_pct(sum(!is.na(x) & x == lvl), nrow(d)) })
    add_row(c(list(Variable = if (i == 1) label else "", Category = cat_labels[[lvl]]), cells))
  }
}
for (d in names(t1_subsets))
  t1_subsets[[d]]$Sex_lab <- ifelse(t1_subsets[[d]]$Sex_female == 1, "Female", "Male")
add_categorical("Sex_lab", "Sex", c(Male = "Male", Female = "Female"))
add_continuous("Age_years", "Age, years")
add_categorical("age_cat", "Age band, years",
                c(lt25 = "<25", `25_29` = "25-29", `30_34` = "30-34", `35_39` = "35-39",
                  `40_44` = "40-44", `45_49` = "45-49", `50_54` = "50-54", `55_59` = "55-59",
                  `60_64` = "60-64", `65plus` = ">=65"))
add_continuous("PhysicalHealth_raw", "Self-rated physical health (0-10)")
add_categorical("K6_ge13", "K6 >= 13 (severe distress)", c(`0` = "No", `1` = "Yes"))
add_continuous("UCLA3_sum", "UCLA-3 loneliness (0-9)")
add_categorical("ACE_cat", "ACE category", c(`0` = "0", `1` = "1", `2` = "2", `3` = "3", `4plus` = ">=4"))
add_continuous("BigFive_E",  "Extraversion (1-7)")
add_continuous("BigFive_A",  "Agreeableness (1-7)")
add_continuous("BigFive_C",  "Conscientiousness (1-7)")
add_continuous("BigFive_ES", "Emotional stability (1-7)")
add_continuous("BigFive_O",  "Openness (1-7)")
add_categorical("Education_cat", "Education",
                c(HSorLess = "High school or less", UnivCollege = "University or college",
                  Graduate = "Graduate school"))
add_categorical("Living_arrangement", "Living arrangement",
                c(WithSpouse = "With spouse or partner", Alone = "Living alone",
                  WithOthers = "With others (non-spouse)"))
add_categorical("Employment", "Employment status",
                c(Regular = "Regular employee", Executive = "Executive", SelfEmployed = "Self-employed",
                  NonRegular = "Non-regular employee", Student = "Student", NotWorking = "Not working"))
add_categorical("Occupation", "Occupation",
                c(Office = "Office or clerical", Professional = "Professional or technical",
                  SalesService = "Sales and service", Manual = "Manual or physical",
                  Other = "Other", NotWorking = "Not working"))
add_continuous("Income_million", "Household income, million JPY (band midpoint; known income only)")
add_categorical("Income_band", "Annual household income",
                c(low = "<3 million JPY", mid = "3 to <6 million JPY",
                  uppermid = "6 to <10 million JPY", high = ">=10 million JPY",
                  Unknown = "Unknown / prefer not to answer"))
add_categorical("LSNS_family_low",  "LSNS-6 family < 6",  c(`0` = "No", `1` = "Yes"))
add_categorical("LSNS_friends_low", "LSNS-6 friends < 6", c(`0` = "No", `1` = "Yes"))
add_categorical("Smartphone_cat", "Smartphone screen time",
                c(lt1h = "0 to <1 h/day", `1to2h` = "1-2 h/day", `3to4h` = "3-4 h/day",
                  `5plush` = ">=5 h/day", Unknown = "Unknown"))
add_categorical("PCtab_cat", "PC or tablet screen time",
                c(lt1h = "0 to <1 h/day", `1to2h` = "1-2 h/day", `3to4h` = "3-4 h/day",
                  `5plush` = ">=5 h/day", Unknown = "Unknown"))
for (d in names(t1_subsets))
  t1_subsets[[d]]$bl_month <- ifelse(t1_subsets[[d]]$baseline_jan2025 == 1, "Jan2025", "Dec2024")
add_categorical("bl_month", "Baseline questionnaire completed",
                c(Dec2024 = "December 2024", Jan2025 = "January 2025 or later"))
for (d in names(t1_subsets))
  t1_subsets[[d]]$init_lab <- ifelse(is.na(t1_subsets[[d]]$init_late2025), NA,
                                     ifelse(t1_subsets[[d]]$init_late2025 == 1, "late", "early"))
t1_subsets_all <- t1_subsets
t1_subsets <- t1_subsets["New-onset 2025 users"]   # outcome rows: within-user sample only
add_categorical("init_lab", "Initiation timing (new-onset users)",
                c(early = "January-June 2025 (Q37S1 = 5)", late = "July-December 2025 (Q37S1 = 6)"))
add_continuous("AI_use_9",                "AI-use intensity (1-5)")
add_continuous("AI_purpose_productivity", "Purpose: productivity/creative (1-5)")
add_continuous("AI_purpose_dailyinfo",    "Purpose: daily-life/information (1-5)")
add_continuous("AI_purpose_socialemo",    "Purpose: social/emotional (1-5)")
add_continuous("AI_addiction_mean",       "Problematic AI use (PCUS, 1-7)")
t1_subsets <- t1_subsets_all
table1 <- dplyr::bind_rows(lapply(t1_rows, function(r)
  as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)))
save_csv(table1, "Table1_descriptives.csv",
         "Table 1: n (%) over the column N for categorical rows; mean (SD) for continuous rows")

# =============================================================================
# 10. Figure 2: forest plot of the seven models
# =============================================================================
outcome_facet_labels <- c(
  Initiation              = "AI initiation\n(vs. never-users)",
  Intensity               = "AI-use intensity\n(users)",
  Productivity_purpose    = "Purpose:\nproductivity/creative",
  DailyInfo_purpose       = "Purpose:\ndaily-life/info",
  SocialEmo_purpose       = "Purpose:\nsocial/emotional",
  PCUS                    = "Problematic\nAI use",
  PCUS_intensity_adjusted = "Problematic AI use\n(intensity-adjusted)")
# The "unknown" screen-time categories are reported in the tables but
# omitted from the figure for legibility.
fig_drop     <- c("sp_Unknown", "pc_Unknown")
fig_personal <- predictor_groups$Personal
fig_position <- predictor_groups$Positional
fig_resource <- setdiff(predictor_groups$Resource, fig_drop)
header_personal <- "--- PERSONAL ---"; header_positional <- "--- POSITIONAL ---"
header_resource <- "--- RESOURCE-BASED ---"
ref_specs <- list(
  list(before = "age_lt25",         label = "Age: 65+ (ref.)",                       domain = "Personal"),
  list(before = "ACE_1",            label = "ACE: 0 events (ref.)",                  domain = "Personal"),
  list(before = "edu_UnivColl",     label = "Education: High school or less (ref.)", domain = "Positional"),
  list(before = "living_Alone",     label = "Living: with spouse/partner (ref.)",    domain = "Positional"),
  list(before = "emp_Executive",    label = "Employment: regular employee (ref.)",   domain = "Positional"),
  list(before = "occ_Professional", label = "Occupation: office/clerical (ref.)",    domain = "Positional"),
  list(before = "inc_mid",          label = "Income: <3M JPY (ref.)",                domain = "Resource"),
  list(before = "sp_1to2h",         label = "Smartphone: 0 to <1 h/day (ref.)",      domain = "Resource"),
  list(before = "pc_1to2h",         label = "PC/tablet: 0 to <1 h/day (ref.)",       domain = "Resource"))
build_y_block <- function(preds, header_label) {
  out <- header_label
  for (p in preds) {
    for (r in ref_specs) if (r$before == p) out <- c(out, r$label)
    out <- c(out, unname(pred_label[p]))
  }
  out
}
y_levels <- rev(c(build_y_block(fig_personal, header_personal),
                  build_y_block(fig_position, header_positional),
                  build_y_block(fig_resource, header_resource)))

forest_plot <- function(tab, file_stem, width = 17, height = 14, title = "") {
  fig_pred_df <- tab %>%
    dplyr::filter(Predictor %in% c(fig_personal, fig_position, fig_resource)) %>%
    dplyr::mutate(y_text = unname(pred_label[Predictor]), Domain = unname(pred_domain[Predictor]))
  fig_outcomes <- outcome_levels[outcome_levels %in% unique(as.character(tab$Outcome))]
  header_grid <- merge(data.frame(y_text = c(header_personal, header_positional, header_resource),
                                  Domain = c("Personal", "Positional", "Resource"),
                                  stringsAsFactors = FALSE),
                       data.frame(Outcome = fig_outcomes, stringsAsFactors = FALSE))
  header_grid$Predictor <- NA_character_; header_grid$beta <- NA_real_
  header_grid$CI_lo <- NA_real_; header_grid$CI_hi <- NA_real_; header_grid$BH_sig <- NA_integer_
  ref_grid <- merge(data.frame(y_text = sapply(ref_specs, `[[`, "label"),
                               Domain = sapply(ref_specs, `[[`, "domain"), stringsAsFactors = FALSE),
                    data.frame(Outcome = fig_outcomes, stringsAsFactors = FALSE))
  ref_grid$Predictor <- "REF_ROW"; ref_grid$beta <- 0
  ref_grid$CI_lo <- NA_real_; ref_grid$CI_hi <- NA_real_; ref_grid$BH_sig <- NA_integer_
  pd <- dplyr::bind_rows(fig_pred_df %>% dplyr::select(Outcome, Predictor, y_text, Domain,
                                                       beta, CI_lo, CI_hi, BH_sig),
                         header_grid, ref_grid)
  pd$OutcomeLab <- factor(outcome_facet_labels[as.character(pd$Outcome)],
                          levels = unname(outcome_facet_labels[fig_outcomes]))
  pd$y_label <- factor(pd$y_text, levels = y_levels)
  domain_colors <- c(Personal = "#fde4e1", Positional = "#dceffd", Resource = "#e3f5db")
  y_face <- ifelse(grepl("^---", levels(pd$y_label)), "bold", "plain")
  stripe_df <- data.frame(y_label = factor(y_levels[seq(2, length(y_levels), by = 2)], levels = y_levels))
  p <- ggplot(pd, aes(x = beta, y = y_label, xmin = CI_lo, xmax = CI_hi)) +
    geom_tile(aes(x = 0, y = y_label, fill = Domain), width = Inf, height = 1,
              inherit.aes = FALSE, alpha = 0.18, show.legend = FALSE, na.rm = TRUE) +
    scale_fill_manual(values = domain_colors) +
    geom_tile(data = stripe_df, aes(x = 0, y = y_label), fill = "grey85", width = Inf,
              height = 1, inherit.aes = FALSE, alpha = 0.30, show.legend = FALSE) +
    geom_tile(data = subset(pd, is.na(Predictor)), aes(x = 0, y = y_label), fill = "grey70",
              width = Inf, height = 1, inherit.aes = FALSE, alpha = 0.55, show.legend = FALSE, na.rm = TRUE) +
    geom_vline(xintercept = 0, linewidth = 0.25, colour = "grey60") +
    geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi), width = 0, na.rm = TRUE,
                  colour = "grey45", orientation = "y") +
    geom_point(data = subset(pd, Predictor == "REF_ROW"), aes(x = beta, y = y_label),
               shape = 18, colour = "grey45", size = 2.4, na.rm = TRUE, inherit.aes = FALSE) +
    geom_point(aes(colour = factor(BH_sig)), size = 1.7, na.rm = TRUE) +
    scale_colour_manual(values = c("0" = "grey55", "1" = "#1a4ea8"),
                        labels = c("BH-FDR n.s.", "BH-FDR q < .05"), name = NULL, na.translate = FALSE) +
    facet_wrap(~ OutcomeLab, nrow = 1, scales = "fixed") +
    labs(title = title,
         x = "Coefficient (log risk ratio for initiation; standardized beta otherwise) with 95% CI",
         y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.15, colour = "grey92"),
          strip.background = element_rect(fill = "grey94", colour = NA),
          strip.text = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 13, face = y_face),
          axis.title.x = element_text(size = 15),
          legend.position = "bottom", legend.text = element_text(size = 16),
          legend.key.size = unit(0.6, "cm"))
  ggsave(file.path(OUT_DIR, paste0(file_stem, ".pdf")), p, width = width, height = height, units = "in")
  ggsave(file.path(OUT_DIR, paste0(file_stem, ".png")), p, width = width, height = height, units = "in", dpi = 300)
  tryCatch(ggsave(file.path(OUT_DIR, paste0(file_stem, ".tiff")), p, width = width, height = height,
                  units = "in", dpi = 600, compression = "lzw"),
           error = function(e) cat("TIFF export failed:", conditionMessage(e), "\n"))
  invisible(p)
}
forest_plot(main_tab, "Figure2_forest")
note_file("Figure2_forest.{pdf,png,tiff}", "Figure 2 (vector PDF; 300-dpi PNG; 600-dpi TIFF)")

# =============================================================================
# 11. Directional expectations H1-H3 (Supplementary Table S5)
# =============================================================================
# Each expectation names an outcome, the coefficient(s) that represent the
# predictor, and the expected sign. Rule: supported when the coefficient
# (or at least half of the dummies representing the predictor) is
# BH-significant in the expected direction; partially supported when at
# least one but fewer than half are; not supported when none is;
# contradicted when a coefficient is BH-significant in the opposite direction.
age_dummies <- paste0("age_", c("lt25", "25_29", "30_34", "35_39", "40_44", "45_49",
                                "50_54", "55_59", "60_64"))
device_dummies <- c("sp_1to2h", "sp_3to4h", "sp_5plush", "pc_1to2h", "pc_3to4h", "pc_5plush")
expectations <- list(
  # H1: personal factors
  list(H = "H1", outcome = "Initiation", label = "Younger age (nine bands vs. >=65 years)", preds = age_dummies, sign = +1),
  list(H = "H1", outcome = "Initiation", label = "Better physical health (per SD)", preds = "PhysicalHealth_z", sign = +1),
  list(H = "H1", outcome = "Initiation", label = "Openness (per SD)", preds = "BigFive_O_z", sign = +1),
  list(H = "H1", outcome = "Intensity",  label = "Younger age", preds = age_dummies, sign = +1),
  list(H = "H1", outcome = "Intensity",  label = "Psychological distress (K6 >= 13)", preds = "K6_ge13", sign = +1),
  list(H = "H1", outcome = "Intensity",  label = "Openness (per SD)", preds = "BigFive_O_z", sign = +1),
  list(H = "H1", outcome = "PCUS",       label = "Younger age", preds = age_dummies, sign = +1),
  list(H = "H1", outcome = "PCUS",       label = "Female sex (vs. male)", preds = "Sex_female", sign = -1),
  list(H = "H1", outcome = "PCUS",       label = "Psychological distress (K6 >= 13)", preds = "K6_ge13", sign = +1),
  list(H = "H1", outcome = "PCUS",       label = "Agreeableness (per SD)", preds = "BigFive_A_z", sign = -1),
  # H2: positional factors
  list(H = "H2", outcome = "Initiation", label = "Higher education (university/college; graduate school)", preds = c("edu_UnivColl", "edu_Grad"), sign = +1),
  list(H = "H2", outcome = "Initiation", label = "Managerial or professional employment (executive; professional/technical)", preds = c("emp_Executive", "occ_Professional"), sign = +1),
  list(H = "H2", outcome = "Initiation", label = "Non-regular employment; not working", preds = c("emp_NonRegular", "occ_NotWorking"), sign = -1),
  list(H = "H2", outcome = "Productivity_purpose", label = "Higher education (university/college; graduate school)", preds = c("edu_UnivColl", "edu_Grad"), sign = +1),
  list(H = "H2", outcome = "Productivity_purpose", label = "Managerial or professional employment (executive; professional/technical)", preds = c("emp_Executive", "occ_Professional"), sign = +1),
  list(H = "H2", outcome = "Productivity_purpose", label = "Non-regular employment; not working", preds = c("emp_NonRegular", "occ_NotWorking"), sign = -1),
  # H3: resource-based factors
  list(H = "H3", outcome = "Initiation", label = "Higher household income (3 to <6; 6 to <10; >=10 million JPY vs. <3 million)", preds = c("inc_mid", "inc_uppermid", "inc_high"), sign = +1),
  list(H = "H3", outcome = "Initiation", label = "Heavier device use (smartphone and PC/tablet, three bands each vs. <1 h/day)", preds = device_dummies, sign = +1),
  list(H = "H3", outcome = "Intensity",  label = "Heavier device use (smartphone and PC/tablet)", preds = device_dummies, sign = +1),
  list(H = "H3", outcome = "SocialEmo_purpose", label = "Limited social network (family; friends)", preds = c("LSNS_family_low", "LSNS_friends_low"), sign = +1)
)
evaluate_expectation <- function(e) {
  r <- main_tab[main_tab$Outcome == e$outcome & main_tab$Predictor %in% e$preds, ]
  r <- r[match(e$preds, r$Predictor), ]
  est <- if (e$outcome == "Initiation") r$RR else r$beta
  expected_dir <- if (e$outcome == "Initiation") sign(log(est)) == e$sign else sign(est) == e$sign
  n_expected     <- sum(r$BH_sig %in% 1L & expected_dir %in% TRUE)
  n_contradicted <- sum(r$BH_sig %in% 1L & expected_dir %in% FALSE)
  k <- nrow(r)
  verdict <- if (n_contradicted > 0) "Contradicted" else
    if (n_expected >= k / 2 && n_expected > 0) "Supported" else
    if (n_expected >= 1) "Partially supported" else "Not supported"
  data.frame(Hypothesis = e$H, Outcome = e$outcome, Predictor = e$label,
             Coefficients = paste(e$preds, collapse = "; "),
             Expected_direction = ifelse(e$sign > 0, "+", "-"),
             Estimates = paste0(sprintf("%.2f%s", est, ifelse(r$BH_sig == 1, "*", "")), collapse = "; "),
             n_coefficients = k, n_significant_expected = n_expected,
             n_significant_opposite = n_contradicted, Verdict = verdict,
             stringsAsFactors = FALSE)
}
expect_tab <- dplyr::bind_rows(lapply(expectations, evaluate_expectation))
save_csv(expect_tab, "S05_directional_expectations.csv",
         "Supplementary Table S5: evaluation of the 20 directional expectations of H1-H3 (RR for initiation, beta otherwise; * = BH-adjusted p < .05)")
cat("\nDirectional expectations:", paste(names(table(expect_tab$Verdict)),
                                         table(expect_tab$Verdict), collapse = "; "), "\n")

# =============================================================================
# 12. Direct tests of coefficient differences between outcomes
#     (Supplementary Table S7)
# =============================================================================
# For each pair of within-user outcomes the two standardised outcomes are
# stacked (two rows per respondent) and regressed on the 49 predictors, an
# outcome indicator, and their products, with standard errors clustered by
# respondent. Because the regressors are identical across the two
# equations this is equivalent to seemingly unrelated regression; each
# product term estimates the difference between the two outcome-specific
# coefficients. Differences are tested individually (BH within the pair)
# and jointly (Wald test, 49 df).
profile_contrast <- function(data, yA, yB, nameA, nameB, predictors = all_predictors) {
  base <- data[, predictors, drop = FALSE]; base$id <- seq_len(nrow(data))
  dA <- base; dA$.y <- zs(data[[yA]]); dA$grp <- 0L
  dB <- base; dB$.y <- zs(data[[yB]]); dB$grp <- 1L
  st <- rbind(dA, dB); st <- st[complete.cases(st), ]
  rhs <- paste0("grp * (", paste(paste0("`", predictors, "`"), collapse = " + "), ")")
  m <- lm(as.formula(paste0(".y ~ ", rhs)), data = st)
  V <- sandwich::vcovCL(m, cluster = st$id, type = "HC1")
  b <- coef(m); b <- b[!is.na(b)]; cn <- gsub("`", "", names(b))
  int_idx  <- match(paste0("grp:", predictors), cn)
  main_idx <- match(predictors, cn)
  se <- sqrt(diag(V))
  diff <- b[int_idx]; diff_se <- se[int_idx]
  z <- diff / diff_se; p <- 2 * pnorm(-abs(z))
  W <- as.numeric(t(diff) %*% solve(V[int_idx, int_idx]) %*% diff)
  joint <- c(chi2 = W, df = length(diff), p = pchisq(W, length(diff), lower.tail = FALSE))
  tab <- data.frame(
    Contrast = paste0(nameB, " minus ", nameA), Predictor = predictors,
    Label = unname(pred_label[predictors]),
    diff = unname(diff), diff_SE = unname(diff_se),
    diff_CI_lo = unname(diff - 1.96 * diff_se), diff_CI_hi = unname(diff + 1.96 * diff_se),
    z = unname(z), p = unname(p), stringsAsFactors = FALSE)
  tab$p_BH <- p.adjust(tab$p, method = "BH")
  tab$BH_sig <- as.integer(tab$p_BH < 0.05)
  tab$beta_first  <- unname(b[main_idx])              # coefficient in the first outcome
  tab$beta_second <- unname(b[main_idx] + b[int_idx]) # coefficient in the second outcome
  list(table = tab, joint = joint, N = length(unique(st$id)))
}
contrast_specs <- list(
  c("AI_use_9", "AI_addiction_mean", "Intensity", "PCUS"),
  c("AI_use_9", "AI_purpose_productivity", "Intensity", "Productivity"),
  c("AI_use_9", "AI_purpose_dailyinfo", "Intensity", "DailyInfo"),
  c("AI_use_9", "AI_purpose_socialemo", "Intensity", "SocialEmo"),
  c("AI_purpose_productivity", "AI_purpose_dailyinfo", "Productivity", "DailyInfo"),
  c("AI_purpose_productivity", "AI_purpose_socialemo", "Productivity", "SocialEmo"),
  c("AI_purpose_dailyinfo", "AI_purpose_socialemo", "DailyInfo", "SocialEmo"),
  c("AI_purpose_productivity", "AI_addiction_mean", "Productivity", "PCUS"),
  c("AI_purpose_dailyinfo", "AI_addiction_mean", "DailyInfo", "PCUS"),
  c("AI_purpose_socialemo", "AI_addiction_mean", "SocialEmo", "PCUS"))
contrasts <- lapply(contrast_specs, function(s) profile_contrast(df_user, s[1], s[2], s[3], s[4]))
contrast_long <- dplyr::bind_rows(lapply(contrasts, `[[`, "table"))
contrast_joint <- dplyr::bind_rows(lapply(contrasts, function(cc)
  data.frame(Contrast = cc$table$Contrast[1], N = cc$N, Wald_chi2 = cc$joint["chi2"],
             df = cc$joint["df"], p = cc$joint["p"],
             n_BH_sig_differences = sum(cc$table$BH_sig), stringsAsFactors = FALSE)))
save_csv(contrast_long, "S07_cross_outcome_contrasts_long.csv",
         "Per-predictor coefficient differences between outcomes (stacked model, cluster-robust), BH within contrast")
save_csv(contrast_joint, "S07_cross_outcome_contrasts_joint.csv",
         "Joint Wald tests that all 49 coefficient differences are zero, all 10 outcome pairs")

# Profile similarity: correlation between the two outcomes' vectors of 49
# standardised coefficients (and of z-statistics), for all 10 pairs
short_name <- c(Intensity = "Intensity", Productivity = "Productivity_purpose",
                DailyInfo = "DailyInfo_purpose", SocialEmo = "SocialEmo_purpose", PCUS = "PCUS")
beta_mat <- sapply(names(within_outcome_vars), function(nm) {
  r <- main_tab[main_tab$Outcome == nm, ]; r$beta[match(all_predictors, r$Predictor)] })
z_mat <- sapply(names(within_outcome_vars), function(nm) {
  r <- main_tab[main_tab$Outcome == nm, ]; r$z_t[match(all_predictors, r$Predictor)] })
profile_sim <- dplyr::bind_rows(lapply(contrast_specs, function(s) {
  a <- short_name[[s[3]]]; b <- short_name[[s[4]]]
  data.frame(Outcome_pair = paste0(s[3], " and ", s[4]),
             r_coefficients = round(cor(beta_mat[, a], beta_mat[, b]), 3),
             r_z_statistics = round(cor(z_mat[, a], z_mat[, b]), 3))
}))
save_csv(profile_sim, "S07_profile_similarity.csv",
         "Correlation between the 49 standardised coefficients (and z-statistics) of two outcomes")

# Test that a predictor's coefficient is equal across all five within-user outcomes
hetero_5 <- local({
  base <- df_user[, all_predictors, drop = FALSE]; base$id <- seq_len(nrow(df_user))
  st <- dplyr::bind_rows(lapply(names(within_outcome_vars), function(nm) {
    d <- base; d$.y <- zs(df_user[[within_outcome_vars[[nm]]]]); d$oc <- nm; d }))
  st <- st[complete.cases(st), ]
  st$oc <- factor(st$oc, levels = names(within_outcome_vars))
  rhs <- paste0("oc * (", paste(paste0("`", all_predictors, "`"), collapse = " + "), ")")
  m <- lm(as.formula(paste0(".y ~ ", rhs)), data = st)
  V <- sandwich::vcovCL(m, cluster = st$id, type = "HC1")
  b <- coef(m); b <- b[!is.na(b)]; cn <- gsub("`", "", names(b))
  dplyr::bind_rows(lapply(all_predictors, function(p) {
    idx <- which(cn %in% paste0("oc", levels(st$oc)[-1], ":", p))
    W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx])
    data.frame(Predictor = p, Label = unname(pred_label[p]), Wald_chi2 = W, df = length(idx),
               p = pchisq(W, length(idx), lower.tail = FALSE), stringsAsFactors = FALSE)
  }))
})
hetero_5$p_BH <- p.adjust(hetero_5$p, method = "BH")
save_csv(hetero_5, "S07_heterogeneity_across_5_outcomes.csv",
         "Per predictor: joint test that its coefficient is equal across the five within-user outcomes")

# =============================================================================
# 13. Distinctness of problematic use from intensity, H4
#     (Supplementary Table S6; Supplementary Methods 2)
# =============================================================================
# Criterion (i), measurement-level non-identity: the heterotrait-monotrait
# ratio (HTMT) between the 11 PCUS items and the nine intensity items,
# tested against unity with a bootstrap confidence interval (the .85 and
# .90 thresholds are reported as a secondary reference); and a one-factor
# versus correlated two-factor CFA (WLSMV) of the 20 items, with the latent
# correlation tested against unity.
# Criterion (ii), predictor-profile divergence: the intensity-versus-PCUS
# contrast of Section 12 (joint Wald test; BH-significant differences;
# profile similarity as an internal benchmark).
items_all <- c(item_pcus, item_purpose)
d_items <- df_user[complete.cases(df_user[, items_all]), items_all]
alpha_p <- psych::alpha(d_items[, item_pcus],    warnings = FALSE, check.keys = FALSE)$total$raw_alpha
alpha_i <- psych::alpha(d_items[, item_purpose], warnings = FALSE, check.keys = FALSE)$total$raw_alpha
r_obs    <- cor(rowMeans(d_items[, item_pcus]), rowMeans(d_items[, item_purpose]))
r_disatt <- r_obs / sqrt(alpha_p * alpha_i)
htmt_fun <- function(d) {   # Henseler et al. (2015), absolute correlations
  R <- abs(cor(d))
  between  <- mean(R[item_pcus, item_purpose])
  within_p <- mean(R[item_pcus, item_pcus][lower.tri(R[item_pcus, item_pcus])])
  within_i <- mean(R[item_purpose, item_purpose][lower.tri(R[item_purpose, item_purpose])])
  between / sqrt(within_p * within_i)
}
htmt_obs <- htmt_fun(d_items)
set.seed(20260905)
htmt_boot <- replicate(N_BOOT, htmt_fun(d_items[sample(nrow(d_items), replace = TRUE), ]))
htmt_ci   <- quantile(htmt_boot, c(.025, .975))
htmt_poly <- tryCatch({   # HTMT on polychoric correlations
  m <- semTools::htmt(paste0("PCUS =~ ", paste(item_pcus, collapse = " + "), "\n",
                             "INT =~ ", paste(item_purpose, collapse = " + ")),
                      data = d_items, ordered = items_all)
  as.numeric(m["INT", "PCUS"])
}, error = function(e) NA_real_)
h4_disc <- data.frame(
  Statistic = c("Observed r (PCUS score, intensity score)", "Cronbach alpha, PCUS", "Cronbach alpha, intensity",
                "Correlation corrected for attenuation", "HTMT (Pearson)",
                "HTMT bootstrap 95% CI, lower", "HTMT bootstrap 95% CI, upper",
                "Bootstrap replications", "HTMT (polychoric)"),
  Value = c(r_obs, alpha_p, alpha_i, r_disatt, htmt_obs, htmt_ci[1], htmt_ci[2], N_BOOT, htmt_poly))
save_csv(h4_disc, "S06_H4_discriminant_validity.csv",
         "H4 criterion (i): HTMT with bootstrap CI, observed and disattenuated correlations")

fit_indices <- function(fit) {
  fm <- tryCatch(lavaan::fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                            "cfi.scaled", "tli.scaled", "rmsea.scaled",
                                            "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")),
                 error = function(e) rep(NA_real_, 9))
  setNames(as.numeric(fm), c("chisq", "df", "p", "CFI", "TLI", "RMSEA", "RMSEA_lo", "RMSEA_hi", "SRMR"))
}
run_cfa <- function(model, data, items) {
  tryCatch(lavaan::cfa(model, data = data, ordered = items, estimator = "WLSMV",
                       std.lv = TRUE, parameterization = "delta"),
           error = function(e) { cat("CFA failed:", conditionMessage(e), "\n"); NULL })
}
cfa_summary <- function(fit, label, sample_n) data.frame(Model = label, N = sample_n, t(fit_indices(fit)))
mod_2f <- paste0("PCUS =~ ", paste(item_pcus, collapse = " + "), "\n",
                 "INT =~ ",  paste(item_purpose, collapse = " + "))
mod_1f <- paste0("F =~ ", paste(items_all, collapse = " + "))
cfa_2f <- run_cfa(mod_2f, d_items, items_all)
cfa_1f <- run_cfa(mod_1f, d_items, items_all)
latent_r <- c(est = NA, lo = NA, hi = NA); lrt_p <- NA
if (!is.null(cfa_2f)) {
  pe <- lavaan::parameterEstimates(cfa_2f)
  rr <- pe[pe$lhs == "PCUS" & pe$op == "~~" & pe$rhs == "INT", ]
  latent_r <- c(est = rr$est, lo = rr$ci.lower, hi = rr$ci.upper)
  if (!is.null(cfa_1f))
    lrt_p <- tryCatch(lavaan::lavTestLRT(cfa_1f, cfa_2f)[2, "Pr(>Chisq)"], error = function(e) NA)
}
h4_cfa <- rbind(
  cfa_summary(cfa_1f, "One factor (20 items)", nrow(d_items)),
  cfa_summary(cfa_2f, "Two correlated factors (PCUS 11 items; intensity 9 items)", nrow(d_items)))
h4_cfa$latent_r    <- c(NA, latent_r["est"]); h4_cfa$latent_r_lo <- c(NA, latent_r["lo"])
h4_cfa$latent_r_hi <- c(NA, latent_r["hi"]); h4_cfa$scaled_chisq_diff_p <- c(NA, lrt_p)
save_csv(h4_cfa, "S06_H4_CFA_fit.csv",
         "H4 criterion (i): one-factor vs two-factor CFA (WLSMV), latent correlation with 95% CI")

# Supplementary Table S6: criteria, statistics, decision rules, verdicts
ip <- contrasts[[1]]                       # PCUS minus Intensity
sig_rows <- ip$table[ip$table$BH_sig == 1, ]
beta_int_adj <- main_tab$beta[main_tab$Outcome == "PCUS_intensity_adjusted" & main_tab$Predictor == "AI_use_9_z"]
r2_of <- function(o) r2_tab$R2[r2_tab$Outcome == o]
ps_int_pcus <- profile_sim$r_coefficients[profile_sim$Outcome_pair == "Intensity and PCUS"]
ps_bench <- profile_sim$r_coefficients[match(c("Intensity and Productivity", "Intensity and SocialEmo",
                                               "Intensity and DailyInfo"), profile_sim$Outcome_pair)]
yes_no <- function(x) ifelse(is.na(x), NA, ifelse(x, "Yes", "No"))
table_s6 <- rbind(
  data.frame(Criterion = "(i) Measurement-level non-identity",
             Statistic = c("HTMT ratio, Pearson [bootstrap 95% CI]",
                           "  Conventional thresholds (secondary)",
                           "HTMT ratio, polychoric",
                           "Observed correlation of the two scale scores",
                           "Correlation corrected for attenuation",
                           "One-factor model (20 items; WLSMV): CFI / TLI / RMSEA / SRMR",
                           "Two-factor model: CFI / TLI / RMSEA / SRMR",
                           "Scaled chi-square difference test, one vs two factors",
                           "Latent correlation, problematic use-intensity [95% CI]"),
             Value = c(sprintf("%s [%s, %s]", fmt(htmt_obs), fmt(htmt_ci[1]), fmt(htmt_ci[2])),
                       "", fmt(htmt_poly), fmt(r_obs), fmt(r_disatt),
                       sprintf("%s / %s / %s / %s", fmt(h4_cfa$CFI[1], 3), fmt(h4_cfa$TLI[1], 3), fmt(h4_cfa$RMSEA[1], 3), fmt(h4_cfa$SRMR[1], 3)),
                       sprintf("%s / %s / %s / %s", fmt(h4_cfa$CFI[2], 3), fmt(h4_cfa$TLI[2], 3), fmt(h4_cfa$RMSEA[2], 3), fmt(h4_cfa$SRMR[2], 3)),
                       format.pval(lrt_p, digits = 3, eps = .001),
                       sprintf("%s [%s, %s]", fmt(latent_r["est"]), fmt(latent_r["lo"]), fmt(latent_r["hi"]))),
             Decision_rule = c("95% CI excludes 1", "< .85 and < .90", "-", "-", "-", "-",
                               "Two-factor fits better", "p < .05", "95% CI excludes 1"),
             Met = c(yes_no(htmt_ci[2] < 1), yes_no(htmt_obs < .85), NA, NA, NA, NA,
                     yes_no(h4_cfa$CFI[2] > h4_cfa$CFI[1]), yes_no(lrt_p < .05), yes_no(latent_r["hi"] < 1)),
             stringsAsFactors = FALSE),
  data.frame(Criterion = "(ii) Predictor-profile divergence (intensity vs problematic use)",
             Statistic = c("Joint Wald test that all 49 coefficient differences are zero",
                           "Predictors with a BH-significant difference (n)",
                           if (nrow(sig_rows)) sprintf("  %s: beta = %s (intensity) vs %s (problematic use)",
                                                       sig_rows$Label, fmt(sig_rows$beta_first), fmt(sig_rows$beta_second)),
                           "Profile similarity (correlation of the 49 coefficients): intensity-problematic use",
                           "  intensity-productivity/creative; intensity-social/emotional; intensity-daily-life/information"),
             Value = c(sprintf("chi2(%d) = %s, p %s", as.integer(ip$joint["df"]), fmt(ip$joint["chi2"], 1),
                               ifelse(ip$joint["p"] < .001, "< .001", paste("=", fmt(ip$joint["p"], 3)))),
                       as.character(nrow(sig_rows)),
                       if (nrow(sig_rows)) sprintf("%s (%s, %s)*", fmt(sig_rows$diff), fmt(sig_rows$diff_CI_lo), fmt(sig_rows$diff_CI_hi)),
                       fmt(ps_int_pcus), paste(fmt(ps_bench), collapse = "; ")),
             Decision_rule = c("p < .05", ">= 1", rep("", nrow(sig_rows)), "Internal benchmark", ""),
             Met = c(yes_no(ip$joint["p"] < .05), yes_no(nrow(sig_rows) >= 1), rep(NA, nrow(sig_rows)), NA, NA),
             stringsAsFactors = FALSE),
  data.frame(Criterion = "Descriptive context (not a criterion)",
             Statistic = c("Residual correlation, problematic use-intensity, after 49 predictors",
                           "Model R2: problematic use / intensity",
                           "Standardized coefficient of intensity in the intensity-adjusted problematic-use model (R2)"),
             Value = c(fmt(resid_corr["Intensity", "PCUS"]),
                       sprintf("%s / %s", fmt(r2_of("PCUS"), 3), fmt(r2_of("Intensity"), 3)),
                       sprintf("%s (%s)", fmt(beta_int_adj), fmt(r2_of("PCUS_intensity_adjusted"), 3))),
             Decision_rule = "-", Met = NA, stringsAsFactors = FALSE))
save_csv(table_s6, "S06_H4_criteria_summary.csv",
         "Supplementary Table S6: H4 criteria, statistics, decision rules, and whether each was met")

# =============================================================================
# 14. Measurement validation: purpose composites (S1, S2), PCUS (S3)
# =============================================================================
# 14a. Purpose composites: polychoric EFA of the nine frequency items
#      (minimum residual, oblimin) with parallel analysis (Horn's method;
#      observed polychoric eigenvalues vs eigenvalues of random normal data
#      of the same n and number of items), new-onset users
efa_dat <- df_user[complete.cases(df_user[, item_purpose]), item_purpose]
names(efa_dat) <- paste0("item", 1:9)
set.seed(20260905)
R_poly  <- suppressMessages(suppressWarnings(psych::polychoric(efa_dat, correct = 0)$rho))
pa_poly <- suppressMessages(suppressWarnings(
  psych::fa.parallel(R_poly, n.obs = nrow(efa_dat), fa = "fa", fm = "minres",
                     n.iter = N_PARALLEL, plot = FALSE)))
efa_poly <- suppressMessages(suppressWarnings(
  psych::fa(efa_dat, nfactors = 3, rotate = "oblimin", fm = "minres", cor = "poly")))
efa_table <- function(fa, label) {
  L <- round(unclass(fa$loadings), 3); colnames(L) <- paste0("F", seq_len(ncol(L)))
  prim <- apply(abs(L), 1, which.max)
  data.frame(EFA = label, Item = paste0(1:9, ". ", purpose_item_labels), L,
             primary_factor = colnames(L)[prim],
             composite = c("Prod", "Prod", "Daily", "Prod", "Prod", "Daily", "Daily", "Social", "Social"),
             communality = round(fa$communality, 3), stringsAsFactors = FALSE, check.names = FALSE)
}
efa_fit_row <- function(fa, pa, label) {
  data.frame(EFA = label, N = nrow(efa_dat), n_factors_parallel = pa$nfact,
             PA_replications = N_PARALLEL, TLI = round(fa$TLI, 3), RMSEA = round(fa$RMSEA[1], 3),
             RMSEA_lo = round(fa$RMSEA[2], 3), RMSEA_hi = round(fa$RMSEA[3], 3),
             BIC = round(fa$BIC, 1), cum_var = round(max(fa$Vaccounted["Cumulative Var", ]), 3),
             phi_12 = round(fa$Phi[1, 2], 3), phi_13 = round(fa$Phi[1, 3], 3), phi_23 = round(fa$Phi[2, 3], 3))
}
save_csv(efa_table(efa_poly, "Polychoric, minimum residual, oblimin"),
         "S01_EFA_loadings.csv", "EFA loadings (polychoric correlations, minres, oblimin), new-onset users")
save_csv(efa_fit_row(efa_poly, pa_poly, "Polychoric, minimum residual, oblimin"),
         "S01_EFA_fit.csv", "EFA fit and number of factors retained by parallel analysis")
save_csv(data.frame(component = seq_along(pa_poly$fa.values),
                    eigen_observed = round(pa_poly$fa.values, 3),
                    eigen_simulated_mean = round(pa_poly$fa.sim, 3)),
         "S01_parallel_analysis_eigenvalues.csv", "Observed vs random-data eigenvalues (parallel analysis)")

item_dist <- function(d, items, labels, ncat) {
  dplyr::bind_rows(lapply(seq_along(items), function(i) {
    x <- d[[items[i]]]; x <- x[!is.na(x)]
    pct <- sapply(1:ncat, function(k) round(100 * mean(x == k), 1))
    data.frame(Item = labels[i], n = length(x), mean = round(mean(x), 2), sd = round(sd(x), 2),
               skew = round(psych::skew(x), 2), t(setNames(pct, paste0("pct_", 1:ncat))))
  }))
}
save_csv(item_dist(df_user, item_purpose, purpose_item_labels, 5),
         "S01_purpose_item_distributions.csv", "Frequency item response distributions (%), new-onset users")

# 14b. Holdout CFA of the three-factor purpose structure (WLSMV, ordinal)
#      in the pre-2025 initiators, who enter no regression model
mod_purpose_3f <- paste0(
  "PROD =~ ", paste(item_purpose[c(1, 2, 4, 5)], collapse = " + "), "\n",
  "DAILY =~ ", paste(item_purpose[c(3, 6, 7)], collapse = " + "), "\n",
  "SOCIAL =~ ", paste(item_purpose[c(8, 9)], collapse = " + "))
mod_purpose_1f <- paste0("F =~ ", paste(item_purpose, collapse = " + "))
cfa_loadings <- function(fit, label) {
  if (is.null(fit)) return(NULL)
  sl <- lavaan::standardizedSolution(fit); sl <- sl[sl$op == "=~", ]
  data.frame(Model = label, Factor = sl$lhs, Item = sl$rhs, std_loading = round(sl$est.std, 3),
             SE = round(sl$se, 3), p = signif(sl$pvalue, 3), stringsAsFactors = FALSE)
}
cfa_factor_cors <- function(fit, label) {
  if (is.null(fit)) return(NULL)
  pe <- lavaan::standardizedSolution(fit)
  pe <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% c("PROD", "DAILY", "SOCIAL") &
             pe$rhs %in% c("PROD", "DAILY", "SOCIAL"), ]
  data.frame(Model = label, Factor1 = pe$lhs, Factor2 = pe$rhs, r = round(pe$est.std, 3),
             ci_lo = round(pe$ci.lower, 3), ci_hi = round(pe$ci.upper, 3), stringsAsFactors = FALSE)
}
d_pre_items <- df_pre[complete.cases(df_pre[, item_purpose]), item_purpose]
cfa_pre_3f <- run_cfa(mod_purpose_3f, d_pre_items, item_purpose)
cfa_pre_1f <- run_cfa(mod_purpose_1f, d_pre_items, item_purpose)
save_csv(rbind(cfa_summary(cfa_pre_3f, "Three-factor model, pre-2025 initiators (holdout)", nrow(d_pre_items)),
               cfa_summary(cfa_pre_1f, "One-factor model, pre-2025 initiators (holdout)", nrow(d_pre_items))),
         "S02_CFA_purpose_fit.csv", "CFA fit of the three-factor vs one-factor purpose model in the holdout sample (WLSMV)")
save_csv(cfa_loadings(cfa_pre_3f, "Three-factor model, pre-2025 initiators"),
         "S02_CFA_purpose_loadings.csv", "Standardised loadings, three-factor model")
save_csv(cfa_factor_cors(cfa_pre_3f, "Three-factor model, pre-2025 initiators"),
         "S02_CFA_purpose_factor_correlations.csv", "Latent factor correlations with 95% CI")
save_csv(data.frame(Sample = "pre-2025 initiators",
                    scaled_chisq_diff_p_1F_vs_3F = tryCatch(lavaan::lavTestLRT(cfa_pre_1f, cfa_pre_3f)[2, "Pr(>Chisq)"],
                                                            error = function(e) NA)),
         "S02_CFA_purpose_LRT.csv", "Scaled chi-square difference test, one-factor vs three-factor model")

# 14c. PCUS: one-factor CFA in new-onset users and in the holdout sample,
#      categorical omega, measurement invariance across sex and age groups,
#      convergent validity, item distributions (Supplementary Table S3)
mod_pcus <- paste0("PCUS =~ ", paste(item_pcus, collapse = " + "))
cfa_pcus_user <- run_cfa(mod_pcus, df_user[complete.cases(df_user[, item_pcus]), item_pcus], item_pcus)
cfa_pcus_pre  <- run_cfa(mod_pcus, df_pre[complete.cases(df_pre[, item_pcus]), item_pcus], item_pcus)
omega_of <- function(fit) {   # categorical omega from the ordinal factor solution (Green & Yang, 2009)
  if (is.null(fit)) return(NA_real_)
  tryCatch(as.numeric(semTools::compRelSEM(fit)), error = function(e)
    tryCatch(as.numeric(semTools::reliability(fit)["omega3", 1]), error = function(e2) NA_real_))
}
pcus_fit_tab <- rbind(
  cfa_summary(cfa_pcus_user, "PCUS one-factor model, new-onset users", sum(complete.cases(df_user[, item_pcus]))),
  cfa_summary(cfa_pcus_pre,  "PCUS one-factor model, pre-2025 initiators", sum(complete.cases(df_pre[, item_pcus]))))
pcus_fit_tab$omega_categorical <- c(omega_of(cfa_pcus_user), omega_of(cfa_pcus_pre))
pcus_fit_tab$alpha <- c(alpha_df$alpha[alpha_df$Scale == "PCUS, 11 items (new-onset users)"],
                        alpha_df$alpha[alpha_df$Scale == "PCUS, 11 items (pre-2025 initiators)"])
save_csv(pcus_fit_tab, "S03_PCUS_CFA_fit.csv", "PCUS one-factor CFA fit, categorical omega, alpha")
save_csv(rbind(cfa_loadings(cfa_pcus_user, "new-onset users"), cfa_loadings(cfa_pcus_pre, "pre-2025 initiators")),
         "S03_PCUS_CFA_loadings.csv", "PCUS standardised loadings")

# Measurement invariance for ordered-categorical indicators (identification
# following Wu & Estabrook, 2016, through semTools::measEq.syntax);
# configural -> thresholds -> thresholds + loadings -> + intercepts
ordinal_invariance <- function(model, data, items, group, label) {
  d <- data[complete.cases(data[, c(items, group)]), c(items, group)]
  d[[group]] <- factor(d[[group]])
  # Every response category must be observed in every group: sparse
  # categories are merged with the adjacent lower category, item by item,
  # and the number of merges is reported.
  n_merged <- 0L
  for (it in items) {
    x <- d[[it]]
    ok_cats <- Reduce(intersect, lapply(split(x, d[[group]]), function(v) sort(unique(v))))
    if (length(ok_cats) < length(unique(x))) {
      lower <- sapply(x, function(v) { c <- ok_cats[ok_cats <= v]; if (length(c)) max(c) else min(ok_cats) })
      n_merged <- n_merged + (length(unique(x)) - length(ok_cats))
      x <- lower
    }
    d[[it]] <- as.integer(factor(x))
  }
  steps <- list(configural = character(0), thresholds = "thresholds",
                metric = c("thresholds", "loadings"),
                scalar = c("thresholds", "loadings", "intercepts"))
  res <- lapply(names(steps), function(st) {
    fit <- tryCatch(
      semTools::measEq.syntax(configural.model = model, data = d, ordered = items,
                              parameterization = "delta", ID.fac = "std.lv",
                              ID.cat = "Wu.Estabrook.2016", group = group,
                              group.equal = steps[[st]], return.fit = TRUE,
                              estimator = "WLSMV"),
      error = function(e) { cat("Invariance step", st, "failed:", conditionMessage(e), "\n"); NULL })
    data.frame(Scale = label, Grouping = group, Step = st, N = nrow(d),
               n_groups = nlevels(d[[group]]), categories_merged = n_merged, t(fit_indices(fit)))
  })
  out <- dplyr::bind_rows(res)
  out$delta_CFI <- c(NA, diff(out$CFI)); out$delta_RMSEA <- c(NA, diff(out$RMSEA))
  out
}
df_user$sex_grp <- ifelse(df_user$Sex_female == 1, "female", "male")
inv_sex <- ordinal_invariance(mod_pcus, df_user[, c(item_pcus, "sex_grp")], item_pcus, "sex_grp", "PCUS")
inv_age <- ordinal_invariance(mod_pcus, df_user[, c(item_pcus, "age3")], item_pcus, "age3", "PCUS")
save_csv(rbind(inv_sex, inv_age), "S03_PCUS_invariance.csv",
         "PCUS measurement invariance across sex and three age groups (configural / thresholds / metric / scalar)")

cor_ci <- function(x, y, label) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 10) return(data.frame(Measure = label, n = sum(ok), r = NA, ci_lo = NA, ci_hi = NA, p = NA))
  ct <- cor.test(x[ok], y[ok])
  data.frame(Measure = label, n = sum(ok), r = round(ct$estimate, 3), ci_lo = round(ct$conf.int[1], 3),
             ci_hi = round(ct$conf.int[2], 3), p = signif(ct$p.value, 3))
}
validity_tab <- dplyr::bind_rows(
  cor_ci(df_user$AI_addiction_mean, df_user$AI_use_9, "AI-use intensity"),
  cor_ci(df_user$AI_addiction_mean, df_user$AI_purpose_productivity, "Purpose: productivity/creative"),
  cor_ci(df_user$AI_addiction_mean, df_user$AI_purpose_dailyinfo, "Purpose: daily-life/information"),
  cor_ci(df_user$AI_addiction_mean, df_user$AI_purpose_socialemo, "Purpose: social/emotional"),
  cor_ci(df_user$AI_addiction_mean, df_user$K6_sum, "K6 distress, 2024 baseline"),
  cor_ci(df_user$AI_addiction_mean, df_user$UCLA3_sum, "UCLA-3 loneliness, 2024 baseline"),
  cor_ci(df_user$AI_addiction_mean, df_user$K6_sum_2025, "K6 distress, 2025 concurrent"),
  cor_ci(df_user$AI_addiction_mean, df_user$UCLA3_sum_2025, "UCLA-3 loneliness, 2025 concurrent"),
  cor_ci(df_user$AI_addiction_mean, df_user$BigFive_A, "Agreeableness, 2024"),
  cor_ci(df_user$AI_addiction_mean, df_user$BigFive_ES, "Emotional stability, 2024"),
  cor_ci(df_user$AI_addiction_mean, df_user$Age_years, "Age, years"))
save_csv(validity_tab, "S03_PCUS_convergent_validity.csv", "Correlations of the PCUS score with external measures (95% CI)")
save_csv(rbind(cbind(Sample = "new-onset users", item_dist(df_user, item_pcus, paste0("PCUS", 1:11), 7)),
               cbind(Sample = "pre-2025 initiators", item_dist(df_pre, item_pcus, paste0("PCUS", 1:11), 7))),
         "S03_PCUS_item_distributions.csv", "PCUS item response distributions (%)")

# =============================================================================
# 15. Sensitivity analyses
# =============================================================================
# 15a. Distributional specifications (Supplementary Table S9): outcome
#      distributions; proportional-odds ordinal regression of each
#      within-user outcome; for problematic use, log-transformed OLS and
#      quantile regression at the 90th percentile
dist_desc <- dplyr::bind_rows(lapply(names(within_outcome_vars), function(nm) {
  x <- df_user[[within_outcome_vars[[nm]]]]
  rng <- if (nm == "PCUS") c(1, 7) else c(1, 5)
  data.frame(Outcome = nm, n = length(x), n_distinct = length(unique(x)),
             mean = round(mean(x), 3), sd = round(sd(x), 3), median = median(x),
             skewness = round(psych::skew(x), 3), kurtosis = round(psych::kurtosi(x), 3),
             pct_at_floor = round(100 * mean(x == rng[1]), 1),
             pct_at_ceiling = round(100 * mean(x == rng[2]), 1))
}))
save_csv(dist_desc, "S09_outcome_distributions.csv", "Distributional descriptives of the five within-user outcomes")

# Ordinal outcome: raw levels when <= 10 distinct values, otherwise
# quantile-based categories (<= 5); model-based standard errors
make_ord <- function(x) {
  u <- sort(unique(x))
  if (length(u) <= 10) return(list(y = factor(x, levels = u, ordered = TRUE), rule = paste0(length(u), " raw levels")))
  br <- unique(quantile(x, probs = seq(0, 1, by = 0.2)))
  y <- cut(x, breaks = br, include.lowest = TRUE, ordered_result = TRUE)
  list(y = y, rule = paste0(nlevels(y), " quantile-based categories"))
}
fit_polr <- function(data, outcome_var, outcome_name, predictors = all_predictors) {
  o <- make_ord(data[[outcome_var]])
  d <- data[, predictors]; d$.y <- o$y
  fml <- as.formula(paste0(".y ~ ", paste(paste0("`", predictors, "`"), collapse = " + ")))
  m <- suppressWarnings(MASS::polr(fml, data = d, Hess = TRUE, method = "logistic"))
  ct <- coef(summary(m)); ct <- ct[gsub("`", "", rownames(ct)) %in% predictors, , drop = FALSE]
  out <- data.frame(Outcome = outcome_name, Predictor = gsub("`", "", rownames(ct)),
                    beta = ct[, 1], SE = ct[, 2], z_t = ct[, 3], p = 2 * pnorm(-abs(ct[, 3])),
                    N = nrow(d), Family = "prop_odds", categories = o$rule, stringsAsFactors = FALSE)
  out$CI_lo <- out$beta - 1.96 * out$SE; out$CI_hi <- out$beta + 1.96 * out$SE
  out$OR <- exp(out$beta)
  out$p_BH <- p.adjust(out$p, method = "BH"); out$BH_sig <- as.integer(out$p_BH < 0.05)
  out$sig_raw <- as.integer(out$p < 0.05)
  rownames(out) <- NULL
  out
}
polr_tab <- dplyr::bind_rows(lapply(names(within_outcome_vars), function(nm)
  fit_polr(df_user, within_outcome_vars[[nm]], nm)))
polr_tab$Outcome <- factor(polr_tab$Outcome, levels = outcome_levels); polr_tab$Spec <- "prop_odds"
save_csv(polr_tab, "S09_ordinal_regression_long.csv",
         "Proportional-odds logistic regression (log-odds and OR), five within-user outcomes")

df_user$log_pcus <- log(df_user$AI_addiction_mean)
pcus_log <- fit_outcome(df_user, "log_pcus", "linear", all_predictors, outcome_name = "PCUS")$table
pcus_log$Spec <- "log_OLS"
df_user$pcus_z <- zs(df_user$AI_addiction_mean)
fit_rq <- function(tau) {   # quantile regression; Hendricks-Koenker sandwich standard errors
  d <- df_user[, c("pcus_z", all_predictors)]
  fml <- as.formula(paste0("pcus_z ~ ", paste(paste0("`", all_predictors, "`"), collapse = " + ")))
  m <- suppressWarnings(quantreg::rq(fml, tau = tau, data = d))
  sm <- tryCatch(suppressWarnings(summary(m, se = "nid")), error = function(e) {
    message("Quantile regression: Hendricks-Koenker standard errors failed; bootstrap standard errors used")
    suppressWarnings(summary(m, se = "boot", R = 500)) })
  ct <- sm$coefficients
  ct <- ct[gsub("`", "", rownames(ct)) %in% all_predictors, , drop = FALSE]
  out <- data.frame(Outcome = "PCUS", Predictor = gsub("`", "", rownames(ct)), beta = ct[, 1], SE = ct[, 2],
                    z_t = ct[, 3], p = ct[, 4], N = nrow(d), Family = paste0("quantile_tau", tau),
                    Spec = paste0("quantile_", tau), stringsAsFactors = FALSE)
  out$CI_lo <- out$beta - 1.96 * out$SE; out$CI_hi <- out$beta + 1.96 * out$SE
  out$p_BH <- p.adjust(out$p, method = "BH"); out$BH_sig <- as.integer(out$p_BH < 0.05)
  out$sig_raw <- as.integer(out$p < 0.05); rownames(out) <- NULL
  out
}
rq_tab <- fit_rq(0.9)
pcus_alt <- dplyr::bind_rows(pcus_log, rq_tab)
save_csv(pcus_alt, "S09_PCUS_alternative_specifications_long.csv",
         "Problematic AI use: log-transformed OLS and quantile regression (tau = .90)")
main_within <- main_tab %>% dplyr::filter(Outcome %in% names(within_outcome_vars))
conc_dist <- dplyr::bind_rows(
  compare_specs(polr_tab, main_within, "prop_odds"),
  compare_specs(pcus_log, main_within, "log_OLS"),
  compare_specs(rq_tab, main_within, "quantile_0.9"))
save_csv(conc_dist, "S09_concordance_with_OLS.csv",
         "Concordance of the alternative specifications with the OLS models")

# 15b. Timing (Supplementary Table S10): baseline completion month; all
#      seven models after excluding respondents whose baseline was
#      completed on or after 1 January 2025 (JST); within-user models
#      with initiation half-year as a covariate
bl_dist <- df_init %>%
  dplyr::mutate(month = ifelse(is.na(baseline_month), "unknown", baseline_month)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(n = dplyr::n(), n_initiated = sum(AI_initiated),
                   pct_initiated = round(100 * mean(AI_initiated), 1),
                   n_initiated_JanJun = sum(AI_initiated == 1 & init_late2025 == 0, na.rm = TRUE),
                   n_initiated_JulDec = sum(AI_initiated == 1 & init_late2025 == 1, na.rm = TRUE), .groups = "drop")
save_csv(bl_dist, "S10_baseline_completion_month.csv", "Baseline completion month (JST) by initiation status")

timing_desc <- dplyr::bind_rows(lapply(names(within_outcome_vars), function(nm) {
  y <- df_user[[within_outcome_vars[[nm]]]]; grp <- df_user$init_late2025
  e <- y[grp %in% 0L]; l <- y[grp %in% 1L]
  data.frame(Outcome = nm, n_JanJun = length(e), mean_JanJun = round(mean(e), 3), sd_JanJun = round(sd(e), 3),
             n_JulDec = length(l), mean_JulDec = round(mean(l), 3), sd_JulDec = round(sd(l), 3),
             diff_JulDec_minus_JanJun = round(mean(l) - mean(e), 3),
             d_cohen = round((mean(l) - mean(e)) / sd(y), 3),
             welch_p = signif(t.test(l, e)$p.value, 3),
             wilcoxon_p = signif(wilcox.test(l, e)$p.value, 3))
}))
save_csv(timing_desc, "S10_outcomes_by_initiation_halfyear.csv",
         "Within-user outcomes by half-year of initiation (January-June vs July-December 2025)")

keep_init <- df_init$baseline_jan2025 %in% 0L
keep_user <- df_user$baseline_jan2025 %in% 0L
tim_excl <- fit_all_models(df_init[keep_init, ], df_user[keep_user, ], tag = "exclude_Jan2025")
save_csv(tim_excl$table, "S10_exclude_Jan2025_long.csv",
         sprintf("All seven models after excluding %d at-risk respondents (%d new-onset users) with baseline on/after 2025-01-01 JST",
                 sum(!keep_init), sum(!keep_user)))
save_csv(make_wide(tim_excl$table, all_predictors, "AI_use_9_z"), "S10_exclude_Jan2025_wide.csv",
         "Presentation table of the January-2025 exclusion models")
tim_cov <- fit_within(df_user, extra = "init_late2025", tag = "timing_covariate")
save_csv(tim_cov$table, "S10_timing_covariate_long.csv",
         "Within-user models with initiation half-year (July-December vs January-June 2025) as a covariate")
save_csv(make_wide(tim_cov$table, all_predictors, c("init_late2025", "AI_use_9_z")),
         "S10_timing_covariate_wide.csv", "Presentation table of the half-year covariate models")
save_csv(dplyr::bind_rows(compare_specs(tim_excl$table, main_tab, "exclude_Jan2025"),
                          compare_specs(tim_cov$table, main_tab, "timing_covariate")),
         "S10_concordance_timing.csv", "Concordance of the timing sensitivity analyses with the main models")

# 15c. Selection into the within-user sample (Supplementary Table S11):
#      each new-onset user weighted by the stabilised inverse of the
#      predicted probability of adoption (logistic model on the 49
#      baseline predictors in the at-risk sample; trimmed at the 99th
#      percentile), so that the weighted adopters resemble the at-risk
#      population on observed characteristics (Cole & Stuart, 2010)
sel_fit <- fit_outcome(df_init, "AI_initiated", "logistic", all_predictors, std = FALSE,
                       outcome_name = "Selection_logit")
sel_d <- sel_fit$data
p_sel <- predict(sel_fit$model, type = "response")
sel_d$ipsw <- ifelse(sel_d$.y == 1, mean(sel_d$.y) / p_sel, NA)
df_user$ipsw <- unname(setNames(sel_d$ipsw, rownames(sel_d))[rownames(df_user)])
w99 <- quantile(df_user$ipsw, 0.99, na.rm = TRUE)
df_user$ipsw_trim <- pmin(df_user$ipsw, w99)
ipsw_summary <- data.frame(
  Statistic = c("N adopters with weight", "Mean", "SD", "Min", "P1", "Median", "P99", "Max",
                "Trimming cap (P99)", "Effective sample size (Kish)"),
  Value = c(sum(!is.na(df_user$ipsw)), mean(df_user$ipsw, na.rm = TRUE), sd(df_user$ipsw, na.rm = TRUE),
            min(df_user$ipsw, na.rm = TRUE), quantile(df_user$ipsw, .01, na.rm = TRUE),
            median(df_user$ipsw, na.rm = TRUE), w99, max(df_user$ipsw, na.rm = TRUE), w99,
            sum(df_user$ipsw_trim, na.rm = TRUE)^2 / sum(df_user$ipsw_trim^2, na.rm = TRUE)))
save_csv(ipsw_summary, "S11_IPSW_weight_summary.csv", "Stabilised inverse-probability-of-adoption weights")
ipsw_fit <- fit_within(df_user, weights = df_user$ipsw_trim, tag = "IPSW")
save_csv(ipsw_fit$table, "S11_IPSW_within_user_long.csv",
         "Within-user models weighted by the stabilised selection weights (trimmed at P99), HC3")
save_csv(make_wide(ipsw_fit$table, all_predictors, "AI_use_9_z"), "S11_IPSW_within_user_wide.csv",
         "Presentation table of the selection-weighted models")
save_csv(compare_specs(ipsw_fit$table, main_tab, "IPSW"), "S11_concordance_IPSW.csv",
         "Concordance of the selection-weighted models with the unweighted models")

# 15d. Attrition (Supplementary Table S12): logistic model of retention in
#      the two-wave panel among all valid 2024 respondents; standardised
#      mean differences; all seven models refitted with stabilised
#      inverse-probability-of-attrition weights (trimmed at P99)
if (!file.exists(DATA_2024_PATH)) {
  cat("\nAll-2024 file not found (", DATA_2024_PATH, "); attrition analysis skipped\n", sep = "")
} else {
  all24 <- read.csv(DATA_2024_PATH, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  cat("\nLoaded ", nrow(all24), " valid 2024 respondents; ", sum(g(all24$panel_matched) == 1),
      " in the two-wave panel\n", sep = "")
  # Tolerate un-suffixed 2024 column names in the all-2024 file
  if (!"SEX_2024" %in% names(all24) && "SEX" %in% names(all24)) {
    keep_as_is <- c("panel_matched", "Monitor_ID")
    names(all24)[!names(all24) %in% keep_as_is] <- paste0(names(all24)[!names(all24) %in% keep_as_is], "_2024")
  }
  all24$panel_matched <- as.integer(g(all24$panel_matched))
  all24 <- construct_baseline(all24)
  ret <- fit_outcome(all24, "panel_matched", "logistic", all_predictors, std = FALSE,
                     outcome_name = "Retention_in_panel")
  ret_tab <- ret$table
  names(ret_tab)[names(ret_tab) %in% c("RR", "RR_lo", "RR_hi")] <- c("OR", "OR_lo", "OR_hi")
  ret_tab$Label <- unname(pred_label[ret_tab$Predictor])
  save_csv(ret_tab, "S12_attrition_retention_model.csv",
           sprintf("Logistic model of retention in the two-wave panel (N = %d valid 2024 respondents), OR (95%% CI), HC3, BH", ret_tab$N[1]))
  smd_tab <- dplyr::bind_rows(lapply(all_predictors, function(v) {
    x1 <- all24[[v]][all24$panel_matched == 1L]; x0 <- all24[[v]][all24$panel_matched == 0L]
    sp <- sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2)
    data.frame(Predictor = v, Label = unname(pred_label[v]),
               mean_retained = round(mean(x1, na.rm = TRUE), 3), mean_lost = round(mean(x0, na.rm = TRUE), 3),
               SMD = round((mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) / sp, 3))
  }))
  save_csv(smd_tab, "S12_attrition_SMD.csv", "Standardised mean differences, retained vs lost to follow-up")

  # Stabilised attrition weights for the panel: P(retained | X) predicted
  # from the retention model with the panel's own baseline predictors
  raw_cols24 <- names(df)[grepl("_2024$", names(df))]
  pnl24 <- construct_baseline(df[, raw_cols24])
  p_ret <- predict(ret$model, newdata = pnl24, type = "response")
  df$ipaw <- mean(all24$panel_matched) / p_ret
  w99a <- quantile(df$ipaw, 0.99, na.rm = TRUE); df$ipaw_trim <- pmin(df$ipaw, w99a)
  df_init$ipaw_trim <- df$ipaw_trim[match(rownames(df_init), rownames(df))]
  df_user$ipaw_trim <- df$ipaw_trim[match(rownames(df_user), rownames(df))]
  ipaw_summary <- data.frame(
    Weight = "IPAW (trimmed P99)",
    Statistic = c("Mean", "SD", "Min", "Median", "P99", "Max"),
    Value = c(mean(df$ipaw_trim, na.rm = TRUE), sd(df$ipaw_trim, na.rm = TRUE), min(df$ipaw_trim, na.rm = TRUE),
              median(df$ipaw_trim, na.rm = TRUE), quantile(df$ipaw_trim, .99, na.rm = TRUE), max(df$ipaw_trim, na.rm = TRUE)))
  save_csv(ipaw_summary, "S12_IPAW_weight_summary.csv", "Attrition weight distribution in the panel")
  ipaw_fit <- fit_all_models(df_init, df_user, w_init = df_init$ipaw_trim, w_user = df_user$ipaw_trim, tag = "IPAW")
  save_csv(ipaw_fit$table, "S12_IPAW_long.csv", "All seven models weighted by the attrition weights, HC3")
  save_csv(make_wide(ipaw_fit$table, all_predictors, "AI_use_9_z"), "S12_IPAW_wide.csv",
           "Presentation table of the attrition-weighted models")
  save_csv(compare_specs(ipaw_fit$table, main_tab, "IPAW"), "S12_concordance_IPAW.csv",
           "Concordance of the attrition-weighted models with the unweighted main models")
}

# =============================================================================
# 16. Output index and session information
# =============================================================================
write.csv(dplyr::bind_rows(out_index), file.path(OUT_DIR, "00_output_index.csv"), row.names = FALSE)
writeLines(c(capture.output(sessionInfo()), "",
             paste("N_BOOT =", N_BOOT), paste("N_PARALLEL =", N_PARALLEL),
             paste("Run completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
           file.path(OUT_DIR, "sessionInfo.txt"))
cat("\nAll outputs written to ", OUT_DIR, "/ (see 00_output_index.csv)\n", sep = "")
