# =============================================================================
# Two-wave (JACSIS 2024 -> 2025) prospective prediction of
# generative-AI initiation, intensity, purpose, and problematic use.
#
# Analysis script for:
#   Nakagomi, A., Inagaki, S., & Tabuchi, T. (2026).
#   [Prospective predictors of generative AI initiation, intensity, purposes, and problematic use among Japanese adults: A two-wave panel study]. 
#   [Journal]. DOI: [will be added on acceptance].
#
# Repository: https://github.com/AtsushiNakagomi/jacsis-ai-divide-prospective
# Author:     Atsushi Nakagomi (anakagomi0211@chiba-u.jp)
# Updated:    2026-05-10
# License:    MIT (see LICENSE file)
#
# DATA AVAILABILITY
# -----------------
# JACSIS data are not publicly available due to ethical restrictions on
# participant privacy. De-identified data may be shared upon reasonable
# request, subject to approval by the JACSIS steering committee and
# relevant ethics review boards. See manuscript Data Availability section
# for details. The data file expected at "data/df.csv" must be supplied
# by approved researchers; column names should match those documented in
# CODEBOOK.md.
#
# REPRODUCIBILITY
# ---------------
# - R version 4.5.3
# - Required packages: dplyr, tidyr, ggplot2, sandwich, lmtest, psych, car
# - Random seed: set.seed(20260427)
# - Run sessionInfo() at the end (saved to output/sessionInfo.txt) to
#   capture the full computational environment.
#
# USAGE
# -----
# 1. Place the panel CSV at the path defined in DATA_PATH below.
# 2. Run: Rscript analysis.R
# 3. Outputs are written to the directory defined in OUT_DIR.
# =============================================================================

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
DATA_PATH <- "data/df.csv"   # path to the panel CSV (not publicly distributed)
OUT_DIR   <- "output"        # output directory

set.seed(20260427)

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------
required <- c("dplyr", "tidyr", "ggplot2",
              "sandwich", "lmtest", "psych", "car")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cran.r-project.org")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

# -----------------------------------------------------------------------------
# Data loading
# -----------------------------------------------------------------------------
if (!file.exists(DATA_PATH))
  stop("Data file not found: ", DATA_PATH)

df <- read.csv(DATA_PATH, stringsAsFactors = FALSE,
               check.names = FALSE, fileEncoding = "UTF-8")
cat("Loaded ", nrow(df), " rows by ", ncol(df), " columns from ",
    DATA_PATH, "\n", sep = "")

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Helper: silent numeric coercion
g <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Helper: row mean with min-N-observed rule (returns NA when too few)
mean_minN <- function(mat, min_n) {
  n_obs <- rowSums(!is.na(mat))
  out <- ifelse(n_obs >= min_n, rowMeans(mat, na.rm = TRUE), NA_real_)
  out[is.nan(out)] <- NA_real_
  out
}

# =============================================================================
# 1. Outcome construction (2025 wave)
# =============================================================================
# Q37S1_2025 categories:
#   1 = never used; 2 = used in past, but quit; 3, 4 = pre-existing user;
#   5, 6 = self-reported new-onset 2025 user.
# Category 2 is excluded from all analyses (a heterogeneous and small group
# whose discontinuation context is not measured).

q37s1 <- g(df$Q37S1_2025)
df    <- df[!(q37s1 %in% 2), , drop = FALSE]
q37s1 <- g(df$Q37S1_2025)

# AI initiation indicator (binary): 1 if Q37S1 in {5, 6}
df$AI_initiated      <- as.integer(q37s1 %in% c(5, 6))
df$initiation_sample <- as.integer(q37s1 %in% c(1, 5, 6))   # at-risk frame
df$user_sample       <- as.integer(q37s1 %in% c(5, 6))      # within-user frame

# AI use purpose (Q37S3.1-9_2025; 5-pt frequency)
for (k in 1:9) {
  v <- paste0("Q37S3.", k, "_2025")
  df[[paste0(v, "_ord")]] <- g(df[[v]])
}
m_intensity <- as.matrix(df[, paste0("Q37S3.", 1:9, "_2025_ord")])
m_prod      <- as.matrix(df[, paste0("Q37S3.", c(1, 2, 4, 5), "_2025_ord")])
m_daily     <- as.matrix(df[, paste0("Q37S3.", c(3, 6, 7),    "_2025_ord")])
m_social    <- as.matrix(df[, paste0("Q37S3.", c(8, 9),       "_2025_ord")])
df$AI_use_9                <- mean_minN(m_intensity, 5)   # >= 5/9 items
df$AI_purpose_productivity <- mean_minN(m_prod,      2)
df$AI_purpose_dailyinfo    <- mean_minN(m_daily,     2)
df$AI_purpose_socialemo    <- mean_minN(m_social,    1)

# Problematic AI use (PCUS, Q38.1-11_2025; 7-pt; Yu et al. 2024)
for (k in 1:11) {
  v <- paste0("Q38.", k, "_2025")
  df[[paste0(v, "_ord")]] <- g(df[[v]])
}
m_pcus <- as.matrix(df[, paste0("Q38.", 1:11, "_2025_ord")])
df$AI_addiction_mean <- mean_minN(m_pcus, 8)              # >= 8/11 items

# =============================================================================
# 2. Predictor construction
# =============================================================================
# All predictors are baseline (2024 wave) except eHEALS, which is from the
# 2025 wave because it was not asked at baseline.

# Sex (binary, 1 = female)
df$Sex_female <- ifelse(g(df$SEX_2024) == 2, 1L, 0L)

# Age (10 bins; reference = 65+)
age <- g(df$AGE_2024)
age_breaks <- c(-Inf, 24, 29, 34, 39, 44, 49, 54, 59, 64, Inf)
age_labels <- c("lt25", "25_29", "30_34", "35_39", "40_44", "45_49",
                 "50_54", "55_59", "60_64", "65plus")
df$age_cat <- cut(age, breaks = age_breaks, labels = age_labels,
                   right = TRUE, include.lowest = TRUE)
for (lab in age_labels[age_labels != "65plus"])
  df[[paste0("age_", lab)]] <- as.integer(df$age_cat == lab)

# Physical health (Flourishing Q76.3 in 2024; 1-11 -> 0-10; standardised)
ph <- g(df$Q76.3_2024)
df$PhysicalHealth_raw <- ifelse(ph %in% 1:11, ph - 1, NA_real_)
df$PhysicalHealth_z   <- as.numeric(scale(df$PhysicalHealth_raw))

# Kessler 6 (Q65.1-6_2024; 1=always..5=never -> distress 4..0; sum 0-24; >=13)
k6_mat   <- sapply(paste0("Q65.", 1:6, "_2024"), function(v) g(df[[v]]))
k6_score <- 5 - k6_mat
k6_score[k6_score < 0 | k6_score > 4] <- NA
df$K6_sum  <- rowSums(k6_score, na.rm = FALSE)
df$K6_ge13 <- as.integer(df$K6_sum >= 13)

# UCLA-3 loneliness (Q66.1-3_2024; 4-pt; standardised)
ucla_mat   <- sapply(paste0("Q66.", 1:3, "_2024"), function(v) g(df[[v]]))
ucla_score <- 4 - ucla_mat
ucla_score[ucla_score < 0 | ucla_score > 3] <- NA
df$UCLA3_sum <- rowSums(ucla_score, na.rm = FALSE)
df$UCLA3_z   <- as.numeric(scale(df$UCLA3_sum))

# Adverse Childhood Experiences (CDC-Kaiser 10-item proxy)
# Q77.1-8 + Q77.13_2024 are coded 1=yes/2=no; Q77.9 is reverse-coded
# ("felt loved by parent": yes is protective, so flipped before summing).
ace_pos_items <- paste0("Q77.", c(1:8, 13), "_2024")
ace_pos <- sapply(ace_pos_items, function(v) {
  x <- g(df[[v]]); ifelse(x == 1, 1L, ifelse(x == 2, 0L, NA_integer_))
})
ace_q9 <- {
  x <- g(df$Q77.9_2024); ifelse(x == 1, 0L, ifelse(x == 2, 1L, NA_integer_))
}
ace_mat <- cbind(ace_pos, ace_q9)
df$ACE_sum <- rowSums(ace_mat, na.rm = FALSE)
df$ACE_cat <- cut(df$ACE_sum, breaks = c(-Inf, 0, 1, 2, 3, Inf),
                   labels = c("0", "1", "2", "3", "4plus"), right = TRUE)
df$ACE_1     <- as.integer(df$ACE_cat == "1")
df$ACE_2     <- as.integer(df$ACE_cat == "2")
df$ACE_3     <- as.integer(df$ACE_cat == "3")
df$ACE_4plus <- as.integer(df$ACE_cat == "4plus")

# Big Five (TIPI-10; Q79.1-10_2024; 7-pt). Items 2, 4, 6, 8, 10 are
# reverse-coded (Gosling, Rentfrow, & Swann, 2003; Japanese TIPI-J:
# Oshio, Abe, & Cutrone, 2014). Each dimension is a 2-item mean.
tipi_raw <- sapply(paste0("Q79.", 1:10, "_2024"), function(v) g(df[[v]]))
tipi_raw[tipi_raw < 1 | tipi_raw > 7] <- NA
tipi_score <- tipi_raw
tipi_score[, c(2, 4, 6, 8, 10)] <- 8 - tipi_score[, c(2, 4, 6, 8, 10)]
df$BigFive_E  <- rowMeans(tipi_score[, c(1, 6) ], na.rm = FALSE)
df$BigFive_A  <- rowMeans(tipi_score[, c(2, 7) ], na.rm = FALSE)
df$BigFive_C  <- rowMeans(tipi_score[, c(3, 8) ], na.rm = FALSE)
df$BigFive_ES <- rowMeans(tipi_score[, c(4, 9) ], na.rm = FALSE)
df$BigFive_O  <- rowMeans(tipi_score[, c(5, 10)], na.rm = FALSE)
for (v in c("BigFive_E", "BigFive_A", "BigFive_C", "BigFive_ES", "BigFive_O"))
  df[[paste0(v, "_z")]] <- as.numeric(scale(df[[v]]))

# Education (self) (Q21.1_2024; 3-level)
edu <- g(df$Q21.1_2024)
df$Education_cat <- factor(case_when(
  edu == 9                   ~ "Graduate",
  edu %in% c(4, 5, 6, 7, 8)  ~ "UnivCollege",
  TRUE                       ~ "HSorLess"
), levels = c("HSorLess", "UnivCollege", "Graduate"))
df$edu_UnivColl <- as.integer(df$Education_cat == "UnivCollege")
df$edu_Grad     <- as.integer(df$Education_cat == "Graduate")

# Living arrangement (3-level; derived from HH_size + spouse-in-HH count)
hh     <- g(df$Q1.1_2024)
spouse <- g(df$Q3.1_2024)
df$Living_arrangement <- factor(case_when(
  hh == 1                                ~ "Alone",
  hh >= 2 & !is.na(spouse) & spouse >= 1 ~ "WithSpouse",
  hh >= 2                                ~ "WithOthers",
  TRUE                                   ~ NA_character_
), levels = c("WithSpouse", "Alone", "WithOthers"))
df$living_Alone      <- as.integer(df$Living_arrangement == "Alone")
df$living_WithOthers <- as.integer(df$Living_arrangement == "WithOthers")

# Employment status (Q5.1_2024; 5-group + non-working captured by Q7=NA)
emp <- g(df$Q5.1_2024)
df$Employment <- factor(case_when(
  emp %in% c(5, 6)            ~ "Regular",
  emp == 1                    ~ "Executive",
  emp %in% c(2, 3, 4)         ~ "SelfEmployed",
  emp %in% c(7, 8, 9, 10, 11) ~ "NonRegular",
  emp %in% c(12, 13)          ~ "Student",
  emp %in% c(14, 15, 16)      ~ "NotWorking",
  TRUE                        ~ NA_character_
), levels = c("Regular", "Executive", "SelfEmployed",
              "NonRegular", "Student", "NotWorking"))
df$emp_Executive    <- as.integer(df$Employment == "Executive")
df$emp_SelfEmployed <- as.integer(df$Employment == "SelfEmployed")
df$emp_NonRegular   <- as.integer(df$Employment == "NonRegular")
df$emp_Student      <- as.integer(df$Employment == "Student")

# Occupation (Q7_2024; Office/Clerical reference + 5 collapsed groups;
# Q7 = NA is routed to "NotWorking" so non-employed respondents are not
# dropped from the model)
occ <- g(df$Q7_2024)
df$Occupation <- factor(case_when(
  occ == 2                  ~ "Office",
  occ == 1                  ~ "Professional",
  occ %in% c(3, 4)          ~ "SalesService",
  occ %in% c(5, 6, 7, 8, 9) ~ "Manual",
  occ == 10                 ~ "Other",
  is.na(occ)                ~ "NotWorking",
  TRUE                      ~ "Other"
), levels = c("Office", "Professional", "SalesService",
              "Manual", "Other", "NotWorking"))
df$occ_Professional <- as.integer(df$Occupation == "Professional")
df$occ_SalesService <- as.integer(df$Occupation == "SalesService")
df$occ_Manual       <- as.integer(df$Occupation == "Manual")
df$occ_Other        <- as.integer(df$Occupation == "Other")
df$occ_NotWorking   <- as.integer(df$Occupation == "NotWorking")

# Household income (Q80.1_2024; 18 bands -> million-JPY band-midpoints,
# then quartiles via dplyr::ntile to handle band-midpoint ties).
# Quartile cutoffs are computed on the at-risk sample (initiation_sample
# == 1L) so that the initiation regression refer to the same
# Q1-Q4 boundaries. Within-user models below re-quartile within their own
# sample so that "Q4" means the highest income quartile among new-onset
# 2025 users specifically.
q80_midpoints <- c(0, 0.25, 0.75, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
                   8.5, 9.5, 11, 13, 15, 17, 19, 22, NA, NA)
inc <- g(df$Q80.1_2024)
df$Income_million <- ifelse(inc %in% 1:18, q80_midpoints[inc], NA_real_)
df$Income_unknown <- as.integer(inc %in% c(19, 20) | is.na(inc))
init_idx <- which(df$initiation_sample == 1L)
df$Income_quartile <- factor(rep(NA_character_, nrow(df)),
                              levels = c("Q1", "Q2", "Q3", "Q4"))
df$Income_quartile[init_idx] <- factor(
  dplyr::ntile(df$Income_million[init_idx], 4),
  levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4")
)
df$inc_Q2 <- ifelse(df$Income_unknown == 1, 0L,
                     as.integer(df$Income_quartile == "Q2"))
df$inc_Q3 <- ifelse(df$Income_unknown == 1, 0L,
                     as.integer(df$Income_quartile == "Q3"))
df$inc_Q4 <- ifelse(df$Income_unknown == 1, 0L,
                     as.integer(df$Income_quartile == "Q4"))

# Lubben Social Network Scale - 6 (Q17.1-6_2024; family items 1-3,
# friend items 4-6; binary "low" indicators at <6, the conventional
# isolation cut-off)
lsns_mat   <- sapply(paste0("Q17.", 1:6, "_2024"), function(v) g(df[[v]]))
lsns_score <- lsns_mat - 1
lsns_score[lsns_score < 0 | lsns_score > 5] <- NA
df$LSNS_family_sum  <- rowSums(lsns_score[, 1:3], na.rm = FALSE)
df$LSNS_friends_sum <- rowSums(lsns_score[, 4:6], na.rm = FALSE)
df$LSNS_family_low  <- as.integer(df$LSNS_family_sum  < 6)
df$LSNS_friends_low <- as.integer(df$LSNS_friends_sum < 6)

# eHealth literacy (eHEALS-8; Norman & Skinner 2006). Q33.1-8 in 2025;
# this construct was not asked at the 2024 wave, so it enters as a
# same-wave covariate. The directional limitation is acknowledged in the
# manuscript.
eheals_mat <- sapply(paste0("Q33.", 1:8, "_2025"), function(v) g(df[[v]]))
eheals_mat[eheals_mat < 1 | eheals_mat > 5] <- NA
df$eHEALS_sum <- rowSums(eheals_mat, na.rm = FALSE)
df$eHEALS_z   <- as.numeric(scale(df$eHEALS_sum))

# Daily screen time (Q28_2024; 12 bins)
recode_screen_2024 <- function(x) {
  factor(case_when(
    x %in% 1:3   ~ "lt1h",
    x %in% 4:5   ~ "1to2h",
    x %in% 6:7   ~ "3to4h",
    x %in% 8:11  ~ "5plush",
    x == 12      ~ "Unknown",
    TRUE         ~ NA_character_
  ), levels = c("lt1h", "1to2h", "3to4h", "5plush", "Unknown"))
}
df$Smartphone_cat <- recode_screen_2024(g(df$Q28.13_2024))
df$sp_1to2h   <- as.integer(df$Smartphone_cat == "1to2h")
df$sp_3to4h   <- as.integer(df$Smartphone_cat == "3to4h")
df$sp_5plush  <- as.integer(df$Smartphone_cat == "5plush")
df$sp_Unknown <- as.integer(df$Smartphone_cat == "Unknown")
df$PCtab_cat <- recode_screen_2024(g(df$Q28.14_2024))
df$pc_1to2h   <- as.integer(df$PCtab_cat == "1to2h")
df$pc_3to4h   <- as.integer(df$PCtab_cat == "3to4h")
df$pc_5plush  <- as.integer(df$PCtab_cat == "5plush")
df$pc_Unknown <- as.integer(df$PCtab_cat == "Unknown")

# =============================================================================
# 3. Predictor list and labels
# =============================================================================
predictor_groups <- list(
  Personal = c(
    "Sex_female",
    paste0("age_", c("lt25", "25_29", "30_34", "35_39", "40_44", "45_49",
                      "50_54", "55_59", "60_64")),
    "PhysicalHealth_z", "K6_ge13", "UCLA3_z",
    paste0("ACE_", c("1", "2", "3", "4plus")),
    "BigFive_E_z", "BigFive_A_z", "BigFive_C_z",
    "BigFive_ES_z", "BigFive_O_z"
  ),
  Positional = c(
    "edu_UnivColl", "edu_Grad",
    "living_Alone", "living_WithOthers",
    "emp_Executive", "emp_SelfEmployed", "emp_NonRegular", "emp_Student",
    "occ_Professional", "occ_SalesService", "occ_Manual",
    "occ_Other", "occ_NotWorking"
  ),
  Resource = c(
    "inc_Q2", "inc_Q3", "inc_Q4", "Income_unknown",
    "LSNS_family_low", "LSNS_friends_low",
    "eHEALS_z",
    "sp_1to2h", "sp_3to4h", "sp_5plush", "sp_Unknown",
    "pc_1to2h", "pc_3to4h", "pc_5plush", "pc_Unknown"
  )
)
all_predictors <- unlist(predictor_groups, use.names = FALSE)

pred_label <- c(
  Sex_female          = "Sex: Female (vs. Male, ref.)",
  age_lt25            = "Age: <25",
  age_25_29           = "Age: 25-29",
  age_30_34           = "Age: 30-34",
  age_35_39           = "Age: 35-39",
  age_40_44           = "Age: 40-44",
  age_45_49           = "Age: 45-49",
  age_50_54           = "Age: 50-54",
  age_55_59           = "Age: 55-59",
  age_60_64           = "Age: 60-64",
  PhysicalHealth_z    = "Physical health (per 1 SD)",
  K6_ge13             = "K6 >= 13 (severe distress)",
  UCLA3_z             = "Loneliness (UCLA-3, per 1 SD)",
  ACE_1               = "ACE: 1 event (vs. 0)",
  ACE_2               = "ACE: 2 events",
  ACE_3               = "ACE: 3 events",
  ACE_4plus           = "ACE: 4+ events",
  BigFive_E_z         = "Big Five: Extraversion (per 1 SD)",
  BigFive_A_z         = "Big Five: Agreeableness (per 1 SD)",
  BigFive_C_z         = "Big Five: Conscientiousness (per 1 SD)",
  BigFive_ES_z        = "Big Five: Emotional stability (per 1 SD)",
  BigFive_O_z         = "Big Five: Openness (per 1 SD)",
  edu_UnivColl        = "Education: University/College (vs. <=High school, ref.)",
  edu_Grad            = "Education: Graduate school",
  living_Alone        = "Living alone (vs. with spouse/partner, ref.)",
  living_WithOthers   = "Living with others (non-spouse)",
  emp_Executive       = "Employment: Executive (vs. Regular employee, ref.)",
  emp_SelfEmployed    = "Employment: Self-employed",
  emp_NonRegular      = "Employment: Non-regular",
  emp_Student         = "Employment: Student",
  occ_Professional    = "Occupation: Professional/Technical (vs. Office/Clerical, ref.)",
  occ_SalesService    = "Occupation: Sales & Service",
  occ_Manual          = "Occupation: Manual/Physical",
  occ_Other           = "Occupation: Other",
  occ_NotWorking      = "Occupation: Not working (no occupation)",
  inc_Q2              = "Income: Q2 (vs. Q1, ref.)",
  inc_Q3              = "Income: Q3",
  inc_Q4              = "Income: Q4 (highest)",
  Income_unknown      = "Income: Unknown",
  LSNS_family_low     = "LSNS-6 family low (<6)",
  LSNS_friends_low    = "LSNS-6 friends low (<6)",
  eHEALS_z            = "eHealth literacy (eHEALS-8, per 1 SD; same-wave 2025)",
  sp_1to2h            = "Smartphone screen time: 1-2 h/day (vs. 0-<1h, ref.)",
  sp_3to4h            = "Smartphone: 3-4 h/day",
  sp_5plush           = "Smartphone: 5+ h/day",
  sp_Unknown          = "Smartphone: Unknown",
  pc_1to2h            = "PC/tablet: 1-2 h/day (vs. 0-<1h, ref.)",
  pc_3to4h            = "PC/tablet: 3-4 h/day",
  pc_5plush           = "PC/tablet: 5+ h/day",
  pc_Unknown          = "PC/tablet: Unknown"
)
pred_domain <- c(
  setNames(rep("Personal",   length(predictor_groups$Personal)),   predictor_groups$Personal),
  setNames(rep("Positional", length(predictor_groups$Positional)), predictor_groups$Positional),
  setNames(rep("Resource",   length(predictor_groups$Resource)),   predictor_groups$Resource)
)

# =============================================================================
# 4. Sample preparation
# =============================================================================
df_init <- df[df$initiation_sample == 1L, , drop = FALSE]   # at-risk
df_user <- df[df$user_sample       == 1L, , drop = FALSE]   # within-user

# Re-quartile household income within the within-user sample so that "Q4"
# in the within-user models refers to the highest-income quartile among
# new-onset 2025 users (rather than the highest-income quartile in the
# at-risk frame that happens to be a user).
df_user$Income_quartile_user <- factor(
  dplyr::ntile(df_user$Income_million, 4),
  levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4")
)
df_user$inc_Q2 <- ifelse(df_user$Income_unknown == 1, 0L,
                          as.integer(df_user$Income_quartile_user == "Q2"))
df_user$inc_Q3 <- ifelse(df_user$Income_unknown == 1, 0L,
                          as.integer(df_user$Income_quartile_user == "Q3"))
df_user$inc_Q4 <- ifelse(df_user$Income_unknown == 1, 0L,
                          as.integer(df_user$Income_quartile_user == "Q4"))

cat("\nInitiation at-risk sample N = ", nrow(df_init),
    " (events = ", sum(df_init$AI_initiated, na.rm = TRUE), ")\n", sep = "")
cat("Within-user sample N = ", nrow(df_user), "\n", sep = "")

# =============================================================================
# 5. Cronbach's alpha
# =============================================================================
alpha_safe <- function(mat, label) {
  mat <- mat[, apply(mat, 2, function(x) sum(!is.na(x))) > 1, drop = FALSE]
  if (ncol(mat) < 2) return(c(label, NA, NA))
  a <- tryCatch(suppressWarnings(psych::alpha(mat, na.rm = TRUE,
                                               warnings = FALSE)),
                 error = function(e) NULL)
  if (is.null(a)) return(c(label, NA, NA))
  c(label, round(a$total$raw_alpha, 3), ncol(mat))
}

alpha_rows <- list(
  alpha_safe(k6_score,                  "K6 (6 items)"),
  alpha_safe(ucla_score,                "UCLA-3 (3 items)"),
  alpha_safe(ace_mat,                   "ACE (10 items)"),
  alpha_safe(tipi_score[, c(1,  6) ],   "TIPI: Extraversion (2 items)"),
  alpha_safe(tipi_score[, c(2,  7) ],   "TIPI: Agreeableness (2 items)"),
  alpha_safe(tipi_score[, c(3,  8) ],   "TIPI: Conscientiousness (2 items)"),
  alpha_safe(tipi_score[, c(4,  9) ],   "TIPI: Emotional stability (2 items)"),
  alpha_safe(tipi_score[, c(5, 10)],    "TIPI: Openness (2 items)"),
  alpha_safe(lsns_score[, 1:3],         "LSNS-6: Family (3 items)"),
  alpha_safe(lsns_score[, 4:6],         "LSNS-6: Friends (3 items)"),
  alpha_safe(eheals_mat,                "eHEALS-8 (8 items, 2025)"),
  alpha_safe(do.call(cbind, lapply(paste0("Q37S3.", 1:9, "_2025_ord"),
                                    function(v) df[[v]])),
             "AI intensity Q37S3 (9 items, 2025)"),
  alpha_safe(do.call(cbind, lapply(paste0("Q37S3.", c(1,2,4,5), "_2025_ord"),
                                    function(v) df_user[[v]])),
             "AI purpose: Productivity/Creative (4 items, within-user)"),
  alpha_safe(do.call(cbind, lapply(paste0("Q37S3.", c(3,6,7),    "_2025_ord"),
                                    function(v) df_user[[v]])),
             "AI purpose: Daily/Info (3 items, within-user)"),
  alpha_safe(do.call(cbind, lapply(paste0("Q37S3.", c(8,9),       "_2025_ord"),
                                    function(v) df_user[[v]])),
             "AI purpose: Social/Emotional (2 items, within-user)"),
  alpha_safe(do.call(cbind, lapply(paste0("Q38.", 1:11, "_2025_ord"),
                                    function(v) df[[v]])),
             "PCUS Q38.1-11 (11 items, 2025)")
)
alpha_df <- as.data.frame(do.call(rbind, alpha_rows), stringsAsFactors = FALSE)
names(alpha_df) <- c("Scale", "Cronbach_alpha", "N_items")
write.csv(alpha_df, file.path(OUT_DIR, "Table_alpha.csv"), row.names = FALSE)

# =============================================================================
# 6. Multivariable regression
# =============================================================================
# - Modified Poisson with log link gives risk ratios that are not
#   inflated/deflated when the event rate is far from 0.1 (Zou 2004).
# - HC0 robust SEs accompany the modified Poisson; HC3 robust SEs are
#   used for the OLS within-user models. Within-user outcomes are
#   standardised so that beta is interpretable as the SD shift in the
#   outcome per 1 SD predictor (continuous) or per 0/1 contrast (dummy).
# - Multiple-testing correction is Benjamini-Hochberg FDR within each
#   outcome over the 50 main predictors. The intensity covariate in the
#   adjusted PCUS model is excluded from the FDR family because it is a
#   partialling covariate, not a hypothesis test.

fit_outcome <- function(data, outcome_var, family = "linear",
                         standardize_outcome = TRUE,
                         extra_predictors = character(0)) {
  use_preds <- c(all_predictors, extra_predictors)
  rhs <- paste(paste0("`", use_preds, "`"), collapse = " + ")

  d <- data
  if (family == "linear" && standardize_outcome)
    d[[outcome_var]] <- as.numeric(scale(d[[outcome_var]]))
  for (v in extra_predictors) {
    if (is.numeric(d[[v]]) && length(unique(stats::na.omit(d[[v]]))) > 2)
      d[[v]] <- as.numeric(scale(d[[v]]))
  }

  keep <- complete.cases(d[, c(outcome_var, use_preds)])
  d <- d[keep, , drop = FALSE]
  N <- nrow(d)

  if (family == "linear") {
    m <- lm(as.formula(paste0("`", outcome_var, "` ~ ", rhs)), data = d)
    V <- sandwich::vcovHC(m, type = "HC3")
  } else if (family == "poisson_log") {
    m <- suppressWarnings(glm(
      as.formula(paste0("`", outcome_var, "` ~ ", rhs)),
      data = d, family = poisson(link = "log")
    ))
    V <- sandwich::vcovHC(m, type = "HC0")
  }

  ct <- lmtest::coeftest(m, vcov. = V)
  ci <- lmtest::coefci(m, vcov. = V, level = 0.95)
  rn <- gsub("`", "", rownames(ct))
  pick <- match(use_preds, rn)
  out <- data.frame(
    Predictor = use_preds,
    beta  = unname(ct[pick, 1]),
    SE    = unname(ct[pick, 2]),
    z_t   = unname(ct[pick, 3]),
    p     = unname(ct[pick, 4]),
    CI_lo = unname(ci[pick, 1]),
    CI_hi = unname(ci[pick, 2]),
    N     = N,
    Family = family,
    stringsAsFactors = FALSE
  )
  if (family == "poisson_log") {
    out$RR    <- exp(out$beta)
    out$RR_lo <- exp(out$CI_lo)
    out$RR_hi <- exp(out$CI_hi)
  } else {
    out$RR <- NA_real_; out$RR_lo <- NA_real_; out$RR_hi <- NA_real_
  }
  main_idx <- match(all_predictors, out$Predictor)
  out$p_BH <- NA_real_
  out$p_BH[main_idx] <- p.adjust(out$p[main_idx], method = "BH")
  out$BH_sig <- as.integer(out$p_BH < 0.05)
  out
}

models_spec <- list(
  list(name = "Initiation",                data = df_init,
       y = "AI_initiated",            family = "poisson_log",
       std = FALSE, extra = character(0)),
  list(name = "Intensity",                 data = df_user,
       y = "AI_use_9",                family = "linear",
       std = TRUE,  extra = character(0)),
  list(name = "Productivity_purpose",      data = df_user,
       y = "AI_purpose_productivity", family = "linear",
       std = TRUE,  extra = character(0)),
  list(name = "DailyInfo_purpose",         data = df_user,
       y = "AI_purpose_dailyinfo",    family = "linear",
       std = TRUE,  extra = character(0)),
  list(name = "SocialEmo_purpose",         data = df_user,
       y = "AI_purpose_socialemo",    family = "linear",
       std = TRUE,  extra = character(0)),
  list(name = "PCUS",                      data = df_user,
       y = "AI_addiction_mean",       family = "linear",
       std = TRUE,  extra = character(0)),
  # Intensity-adjusted PCUS: tests whether problematic-use signal survives
  # partialling out overall AI volume (see manuscript "Dissociation of
  # problematic and heavy AI use").
  list(name = "PCUS_intensity_adjusted",   data = df_user,
       y = "AI_addiction_mean",       family = "linear",
       std = TRUE,  extra = "AI_use_9")
)
results_long <- do.call(rbind, lapply(models_spec, function(s) {
  cat("Fitting: ", s$name, " (", s$family, ")\n", sep = "")
  r <- fit_outcome(s$data, s$y, family = s$family,
                   standardize_outcome = s$std, extra_predictors = s$extra)
  r$Outcome <- s$name
  r
}))
results_long$Outcome <- factor(results_long$Outcome,
  levels = c("Initiation", "Intensity", "Productivity_purpose",
             "DailyInfo_purpose", "SocialEmo_purpose", "PCUS",
             "PCUS_intensity_adjusted"))
write.csv(results_long,
          file.path(OUT_DIR, "RegressionResults_long.csv"),
          row.names = FALSE)

results_long$beta_str <- ifelse(
  is.na(results_long$beta), "",
  sprintf("%.3f%s", results_long$beta,
          ifelse(!is.na(results_long$BH_sig) & results_long$BH_sig == 1, "*", ""))
)
wide <- results_long %>%
  dplyr::select(Predictor, Outcome, beta_str) %>%
  tidyr::pivot_wider(names_from = Outcome, values_from = beta_str)
wide <- wide[match(all_predictors, wide$Predictor), , drop = FALSE]
wide$Label  <- pred_label[wide$Predictor]
wide$Domain <- pred_domain[wide$Predictor]
wide <- wide[, c("Domain", "Predictor", "Label",
                  "Initiation", "Intensity", "Productivity_purpose",
                  "DailyInfo_purpose", "SocialEmo_purpose",
                  "PCUS", "PCUS_intensity_adjusted")]
write.csv(wide, file.path(OUT_DIR, "RegressionResults_wide.csv"),
          row.names = FALSE)

# =============================================================================
# 7. Outcome dissociation: residual correlations and per-outcome R^2
# =============================================================================
within_outcome_vars <- c(
  Intensity            = "AI_use_9",
  Productivity_purpose = "AI_purpose_productivity",
  DailyInfo_purpose    = "AI_purpose_dailyinfo",
  SocialEmo_purpose    = "AI_purpose_socialemo",
  PCUS                 = "AI_addiction_mean"
)
within_lm <- list(); within_r2 <- numeric(); within_n <- integer()
rhs_full <- paste(paste0("`", all_predictors, "`"), collapse = " + ")
for (k in seq_along(within_outcome_vars)) {
  name <- names(within_outcome_vars)[k]
  yvar <- within_outcome_vars[[k]]
  d <- df_user
  d[[yvar]] <- as.numeric(scale(d[[yvar]]))
  d <- d[complete.cases(d[, c(yvar, all_predictors)]), , drop = FALSE]
  m <- lm(as.formula(paste0("`", yvar, "` ~ ", rhs_full)), data = d)
  within_lm[[name]] <- m
  within_r2[name]   <- summary(m)$r.squared
  within_n[name]    <- nrow(d)
}

common_keys <- Reduce(intersect, lapply(within_lm,
                                         function(m) rownames(m$model)))
resid_mat <- sapply(within_lm, function(m) {
  rn <- rownames(m$model)
  residuals(m)[match(common_keys, rn)]
})
colnames(resid_mat) <- names(within_lm)
write.csv(round(cor(resid_mat, use = "pairwise.complete.obs"), 3),
          file.path(OUT_DIR, "Table_residual_correlations.csv"))
write.csv(data.frame(Outcome = names(within_r2),
                      N = unname(within_n),
                      R2 = round(unname(within_r2), 4)),
          file.path(OUT_DIR, "Table_R2_per_outcome.csv"),
          row.names = FALSE)

# =============================================================================
# 8. Generalised variance inflation factors
# =============================================================================
m_int <- within_lm[["Intensity"]]
ali   <- summary(m_int)$aliased
if (any(ali, na.rm = TRUE)) {
  good_preds <- setdiff(all_predictors, gsub("`", "", names(ali)[ali]))
  rhs_v <- paste(paste0("`", good_preds, "`"), collapse = " + ")
  m_vif_in <- lm(as.formula(paste0("`AI_use_9` ~ ", rhs_v)), data = m_int$model)
} else {
  m_vif_in <- m_int
}
v <- car::vif(m_vif_in)
if (is.matrix(v)) {
  vif_df <- data.frame(
    Predictor = rownames(v),
    GVIF      = round(v[, "GVIF"], 3),
    Df        = round(v[, "Df"]),
    GVIF_adj  = round(v[, "GVIF^(1/(2*Df))"], 3)
  )
} else {
  vif_df <- data.frame(Predictor = names(v), VIF = round(v, 3))
}
write.csv(vif_df, file.path(OUT_DIR, "Table_VIF.csv"), row.names = FALSE)

# =============================================================================
# 9. Exploratory factor analysis on the AI-purpose items
# =============================================================================
efa_data <- df_user[, paste0("Q37S3.", 1:9, "_2025_ord"), drop = FALSE]
efa_data <- efa_data[complete.cases(efa_data), , drop = FALSE]

fa_par <- suppressMessages(suppressWarnings(
  psych::fa.parallel(efa_data, fa = "fa", fm = "ml",
                     n.iter = 50, plot = FALSE)
))
fa_3   <- suppressMessages(suppressWarnings(
  psych::fa(efa_data, nfactors = 3, rotate = "oblimin", fm = "ml")
))

q37s3_labels <- c(
  "Q37S3.1_2025_ord" = "Q37S3.1 Document drafting",
  "Q37S3.2_2025_ord" = "Q37S3.2 Translation/summarization",
  "Q37S3.3_2025_ord" = "Q37S3.3 Information search",
  "Q37S3.4_2025_ord" = "Q37S3.4 Image/video generation",
  "Q37S3.5_2025_ord" = "Q37S3.5 Learning",
  "Q37S3.6_2025_ord" = "Q37S3.6 Daily-life planning",
  "Q37S3.7_2025_ord" = "Q37S3.7 Health advice",
  "Q37S3.8_2025_ord" = "Q37S3.8 Conversation",
  "Q37S3.9_2025_ord" = "Q37S3.9 Emotional support"
)
load_df <- as.data.frame(round(unclass(fa_3$loadings), 3))
load_df$Item  <- rownames(load_df)
load_df$Label <- unname(q37s3_labels[load_df$Item])
load_df <- load_df[, c("Item", colnames(unclass(fa_3$loadings)), "Label")]
write.csv(load_df, file.path(OUT_DIR, "Table_EFA_loadings.csv"),
          row.names = FALSE)

cum_var <- if (!is.null(fa_3$Vaccounted) &&
                "Cumulative Var" %in% rownames(fa_3$Vaccounted))
  round(max(fa_3$Vaccounted["Cumulative Var", ]), 3) else NA_real_
fit_df <- data.frame(
  Statistic = c("Sample N (within-user, complete cases)",
                 "n factors (parallel analysis)",
                 "n factors (pre-specified)",
                 "TLI", "RMSEA", "RMSEA 90% CI lower",
                 "RMSEA 90% CI upper", "BIC",
                 "Cumulative variance explained (3 factors)"),
  Value = c(nrow(efa_data),
             fa_par$nfact,
             3,
             round(fa_3$TLI, 3),
             round(fa_3$RMSEA[1], 3),
             round(fa_3$RMSEA[2], 3),
             round(fa_3$RMSEA[3], 3),
             round(fa_3$BIC, 1),
             cum_var)
)
write.csv(fit_df, file.path(OUT_DIR, "Table_EFA_fit.csv"),
          row.names = FALSE)

# =============================================================================
# 10. Table 1 descriptives (long format, stratified)
# =============================================================================
q37s1_post <- g(df$Q37S1_2025)
df$AI_strata <- factor(case_when(
  q37s1_post == 1            ~ "Never-used",
  q37s1_post %in% c(3, 4)    ~ "Pre-existing user",
  q37s1_post %in% c(5, 6)    ~ "New-onset user (2025)",
  TRUE                       ~ NA_character_
), levels = c("Never-used", "Pre-existing user", "New-onset user (2025)"))

# Table 1 columns: the at-risk frame (Never-used + New-onset 2025 user);
# pre-existing users are not part of any regression frame and are
# excluded from descriptives.
t1_subsets <- list(
  `Overall (at-risk)`          = df_init,
  `Never-used`                = df[df$AI_strata == "Never-used", , drop = FALSE],
  `New-onset user (2025)`     = df[df$AI_strata == "New-onset user (2025)",
                                     , drop = FALSE]
)

fmt_n_pct  <- function(n, denom)
  if (denom == 0) "-" else sprintf("%d (%.1f%%)", n, 100 * n / denom)
fmt_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) "-" else sprintf("%.2f (%.2f)", mean(x), sd(x))
}
t1_rows <- list()
add_row <- function(row) t1_rows[[length(t1_rows) + 1L]] <<- row

add_row(c(list(Variable = "Sample N", Category = ""),
          lapply(t1_subsets, function(d) sprintf("%d", nrow(d)))))

add_continuous <- function(var, label) {
  cells <- lapply(t1_subsets, function(d) fmt_mean_sd(d[[var]]))
  add_row(c(list(Variable = label, Category = "Mean (SD)"), cells))
}
add_categorical <- function(var, label, cat_labels = NULL) {
  vals <- t1_subsets[[1]][[var]]
  levs <- if (is.factor(vals)) levels(vals)
          else sort(unique(stats::na.omit(as.character(vals))))
  if (!is.null(cat_labels)) levs <- intersect(names(cat_labels), levs)
  for (i in seq_along(levs)) {
    lvl <- levs[i]
    pretty <- if (!is.null(cat_labels)) cat_labels[lvl] else lvl
    var_label <- if (i == 1) label else ""
    cells <- lapply(t1_subsets, function(d) {
      x <- as.character(d[[var]])
      n <- sum(!is.na(x) & x == lvl)
      fmt_n_pct(n, sum(!is.na(x)))
    })
    add_row(c(list(Variable = var_label, Category = pretty), cells))
  }
}

add_categorical("SEX_2024", "Sex",
                cat_labels = c("1" = "Male", "2" = "Female"))
add_continuous("AGE_2024", "Age (years)")
df$age_cat_lab <- as.character(df$age_cat)
t1_subsets <- lapply(t1_subsets,
                      function(d) { d$age_cat_lab <- as.character(d$age_cat); d })
add_categorical("age_cat_lab", "Age band",
                cat_labels = c("lt25"="<25","25_29"="25-29","30_34"="30-34",
                               "35_39"="35-39","40_44"="40-44","45_49"="45-49",
                               "50_54"="50-54","55_59"="55-59","60_64"="60-64",
                               "65plus"="65+"))
add_continuous("PhysicalHealth_raw", "Physical health (0-10)")
add_continuous("K6_sum",      "K6 distress sum (0-24)")
add_categorical("K6_ge13",   "K6 >= 13",
                cat_labels = c("0" = "No", "1" = "Yes"))
add_continuous("UCLA3_sum",   "UCLA-3 loneliness sum (0-9)")
add_continuous("ACE_sum",     "ACE sum (0-10)")
add_categorical("ACE_cat",   "ACE category",
                cat_labels = c("0" = "0 events", "1" = "1 event",
                               "2" = "2 events", "3" = "3 events",
                               "4plus" = "4+ events"))
add_continuous("BigFive_E",   "Big Five: Extraversion (1-7)")
add_continuous("BigFive_A",   "Big Five: Agreeableness (1-7)")
add_continuous("BigFive_C",   "Big Five: Conscientiousness (1-7)")
add_continuous("BigFive_ES",  "Big Five: Emotional stability (1-7)")
add_continuous("BigFive_O",   "Big Five: Openness (1-7)")
add_categorical("Education_cat", "Education (self)",
                cat_labels = c("HSorLess" = "High school or less",
                               "UnivCollege" = "University / College",
                               "Graduate" = "Graduate school"))
add_categorical("Living_arrangement", "Living arrangement",
                cat_labels = c("WithSpouse" = "With spouse / partner",
                               "Alone" = "Living alone",
                               "WithOthers" = "With others (non-spouse)"))
add_categorical("Employment", "Employment status",
                cat_labels = c("Regular" = "Regular employee",
                               "Executive" = "Executive",
                               "SelfEmployed" = "Self-employed",
                               "NonRegular" = "Non-regular",
                               "Student" = "Student",
                               "NotWorking" = "Not working"))
add_categorical("Occupation", "Occupation",
                cat_labels = c("Office" = "Office / Clerical",
                               "Professional" = "Professional / Technical",
                               "SalesService" = "Sales & Service",
                               "Manual" = "Manual / Physical",
                               "Other" = "Other",
                               "NotWorking" = "Not working (no occupation)"))
add_continuous("Income_million", "Household income (M JPY, midpoint)")
add_categorical("Income_quartile", "Household income quartile",
                cat_labels = c("Q1" = "Q1 (lowest)", "Q2" = "Q2",
                               "Q3" = "Q3", "Q4" = "Q4 (highest)"))
add_categorical("Income_unknown", "Income unknown",
                cat_labels = c("0" = "No", "1" = "Yes"))
add_continuous("LSNS_family_sum",  "LSNS-6 family sum (0-15)")
add_continuous("LSNS_friends_sum", "LSNS-6 friends sum (0-15)")
add_categorical("LSNS_family_low",  "LSNS-6 family low (<6)",
                cat_labels = c("0" = "No", "1" = "Yes"))
add_categorical("LSNS_friends_low", "LSNS-6 friends low (<6)",
                cat_labels = c("0" = "No", "1" = "Yes"))
add_continuous("eHEALS_sum",     "eHEALS-8 sum (8-40, 2025)")
add_categorical("Smartphone_cat", "Smartphone screen time",
                cat_labels = c("lt1h" = "0 to <1 h/day",
                               "1to2h" = "1-2 h/day",
                               "3to4h" = "3-4 h/day",
                               "5plush" = "5+ h/day",
                               "Unknown" = "Unknown"))
add_categorical("PCtab_cat",     "PC/tablet screen time",
                cat_labels = c("lt1h" = "0 to <1 h/day",
                               "1to2h" = "1-2 h/day",
                               "3to4h" = "3-4 h/day",
                               "5plush" = "5+ h/day",
                               "Unknown" = "Unknown"))
add_categorical("AI_strata", "AI use status (Q37S1_2025)",
                cat_labels = c("Never-used" = "Never used",
                               "New-onset user (2025)" = "New-onset 2025 user"))
add_categorical("AI_initiated", "AI initiated in 2025",
                cat_labels = c("0" = "No", "1" = "Yes"))
add_continuous("AI_use_9",                "AI use intensity (Q37S3 mean, 1-5)")
add_continuous("AI_purpose_productivity", "AI purpose: Productivity/Creative")
add_continuous("AI_purpose_dailyinfo",    "AI purpose: Daily/Info")
add_continuous("AI_purpose_socialemo",    "AI purpose: Social/Emotional")
add_continuous("AI_addiction_mean",       "Problematic AI use (PCUS, 1-7)")

table1 <- do.call(rbind, lapply(t1_rows,
                                  function(r) as.data.frame(r,
                                                              stringsAsFactors = FALSE)))
write.csv(table1, file.path(OUT_DIR, "Table1_descriptives.csv"),
          row.names = FALSE)

# =============================================================================
# 12. Figure 1 - multivariable forest
# =============================================================================
outcome_facet_labels <- c(
  Initiation              = "AI initiation\n(Users vs Non-users)",
  Intensity               = "AI use intensity\n(Users)",
  Productivity_purpose    = "Purpose:\nProductivity/Creative (Users)",
  DailyInfo_purpose       = "Purpose:\nDaily/Info (Users)",
  SocialEmo_purpose       = "Purpose:\nSocial/Emotional (Users)",
  PCUS                    = "Problematic AI use\n(Users)",
  PCUS_intensity_adjusted = "Problematic AI use\nIntensity-adjusted (Users)"
)

# Smartphone/PC "Unknown" categories are kept in the regression CSVs but
# dropped from the figure to avoid distraction from very small cells.
fig_drop      <- c("sp_Unknown", "pc_Unknown")
fig_personal  <- predictor_groups$Personal
fig_position  <- predictor_groups$Positional
fig_resource  <- setdiff(predictor_groups$Resource, fig_drop)

header_personal   <- "--- PERSONAL ---"
header_positional <- "--- POSITIONAL ---"
header_resource   <- "--- RESOURCE-BASED ---"

# Reference rows for multi-category predictors. Each is rendered as a
# grey diamond at beta = 0 with no CI to anchor the dummy block.
ref_specs <- list(
  list(before = "age_lt25",        label = "Age: 65+ (Ref.)",                       domain = "Personal"),
  list(before = "ACE_1",           label = "ACE: 0 events (Ref.)",                  domain = "Personal"),
  list(before = "edu_UnivColl",    label = "Education: High school or less (Ref.)", domain = "Positional"),
  list(before = "living_Alone",    label = "Living: With spouse/partner (Ref.)",    domain = "Positional"),
  list(before = "emp_Executive",   label = "Employment: Regular employee (Ref.)",   domain = "Positional"),
  list(before = "occ_Professional",label = "Occupation: Office/Clerical (Ref.)",    domain = "Positional"),
  list(before = "inc_Q2",          label = "Income: Q1 (lowest) (Ref.)",            domain = "Resource"),
  list(before = "sp_1to2h",        label = "Smartphone: 0 to <1 h/day (Ref.)",      domain = "Resource"),
  list(before = "pc_1to2h",        label = "PC/tablet: 0 to <1 h/day (Ref.)",       domain = "Resource")
)
build_y_block <- function(predictors_in_block, header_label) {
  out <- header_label
  for (p in predictors_in_block) {
    for (r in ref_specs) if (r$before == p) out <- c(out, r$label)
    out <- c(out, unname(pred_label[p]))
  }
  out
}
y_top_to_bottom <- c(build_y_block(fig_personal, header_personal),
                      build_y_block(fig_position, header_positional),
                      build_y_block(fig_resource, header_resource))
y_levels <- rev(y_top_to_bottom)

fig_pred_df <- results_long %>%
  dplyr::filter(Predictor %in% c(fig_personal, fig_position, fig_resource)) %>%
  dplyr::mutate(y_text = unname(pred_label[Predictor]),
                Domain = unname(pred_domain[Predictor]))

fig_outcomes <- levels(results_long$Outcome)
header_def <- data.frame(
  y_text = c(header_personal, header_positional, header_resource),
  Domain = c("Personal", "Positional", "Resource"),
  stringsAsFactors = FALSE
)
header_grid <- merge(header_def,
                      data.frame(Outcome = fig_outcomes,
                                  stringsAsFactors = FALSE))
header_grid$Predictor <- NA_character_
header_grid$beta <- NA_real_; header_grid$CI_lo <- NA_real_
header_grid$CI_hi <- NA_real_; header_grid$p <- NA_real_
header_grid$p_BH <- NA_real_; header_grid$BH_sig <- NA_integer_
header_grid$N <- NA_integer_

ref_def <- data.frame(
  y_text = vapply(ref_specs, function(r) r$label,  character(1)),
  Domain = vapply(ref_specs, function(r) r$domain, character(1)),
  stringsAsFactors = FALSE
)
ref_grid <- merge(ref_def,
                   data.frame(Outcome = fig_outcomes,
                               stringsAsFactors = FALSE))
ref_grid$Predictor <- "REF_ROW"
ref_grid$beta <- 0; ref_grid$CI_lo <- NA_real_
ref_grid$CI_hi <- NA_real_; ref_grid$p <- NA_real_
ref_grid$p_BH <- NA_real_; ref_grid$BH_sig <- NA_integer_
ref_grid$N <- NA_integer_

plot_df_fig <- dplyr::bind_rows(fig_pred_df, header_grid, ref_grid)
plot_df_fig$Outcome <- factor(plot_df_fig$Outcome, levels = fig_outcomes)
plot_df_fig$OutcomeLab <- factor(
  outcome_facet_labels[as.character(plot_df_fig$Outcome)],
  levels = unname(outcome_facet_labels)
)
plot_df_fig$y_label <- factor(plot_df_fig$y_text, levels = y_levels)

domain_colors <- c(Personal = "#fde4e1", Positional = "#dceffd",
                   Resource = "#e3f5db")
y_face <- ifelse(grepl("^---", levels(plot_df_fig$y_label)), "bold", "plain")
stripe_levels <- y_levels[seq(2, length(y_levels), by = 2)]
stripe_df <- data.frame(y_label = factor(stripe_levels, levels = y_levels))

build_forest <- function(plot_df, plot_title,
                          extra_layers = list()) {
  base <- ggplot(plot_df,
                 aes(x = beta, y = y_label, xmin = CI_lo, xmax = CI_hi)) +
    geom_tile(aes(x = 0, y = y_label, fill = Domain),
              width = Inf, height = 1, inherit.aes = FALSE,
              alpha = 0.18, show.legend = FALSE, na.rm = TRUE) +
    scale_fill_manual(values = domain_colors) +
    geom_tile(data = stripe_df, aes(x = 0, y = y_label),
              fill = "grey85", width = Inf, height = 1, inherit.aes = FALSE,
              alpha = 0.30, show.legend = FALSE) +
    geom_tile(data = subset(plot_df, is.na(Predictor)),
              aes(x = 0, y = y_label),
              fill = "grey70", width = Inf, height = 1, inherit.aes = FALSE,
              alpha = 0.55, show.legend = FALSE, na.rm = TRUE) +
    geom_vline(xintercept = 0, linewidth = 0.25, colour = "grey60") +
    geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi), height = 0,
                   na.rm = TRUE, colour = "grey45", orientation = "y") +
    geom_point(data = subset(plot_df, Predictor == "REF_ROW"),
                aes(x = beta, y = y_label),
                shape = 18, colour = "grey45", size = 2.4, na.rm = TRUE,
                inherit.aes = FALSE)
  for (lyr in extra_layers) base <- base + lyr
  base +
    geom_point(aes(colour = factor(BH_sig)), size = 1.7, na.rm = TRUE) +
    scale_colour_manual(values = c("0" = "grey55", "1" = "#1a4ea8"),
                         labels = c("BH-FDR n.s.", "BH-FDR < .05"),
                         name = NULL, na.translate = FALSE) +
    facet_wrap(~ OutcomeLab, nrow = 1, scales = "fixed") +
    labs(title = plot_title,
         x = paste0("Beta (per 1 SD predictor for continuous; per 0/1 ",
                    "contrast for categorical; log(RR) for initiation)"),
         y = NULL) +
    theme_bw(base_size = 9) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.15,
                                              colour = "grey92"),
          strip.background = element_rect(fill = "grey94", colour = NA),
          strip.text = element_text(size = 8.5, face = "bold"),
          plot.title = element_text(size = 11, face = "bold"),
          axis.text.y = element_text(size = 7, face = y_face),
          legend.position = "bottom",
          legend.key.size = unit(0.4, "cm"))
}

p_fig2 <- build_forest(
  plot_df_fig,
  paste0("Figure 2. Two-wave (2024 -> 2025) prospective predictors of ",
         "generative-AI initiation, intensity, purpose, and problematic use")
)
ggsave(file.path(OUT_DIR, "Figure2_forest.png"), p_fig2,
       width = 17, height = 14, units = "in", dpi = 220)



# =============================================================================
# Save session info for reproducibility
# =============================================================================
writeLines(capture.output(sessionInfo()),
           file.path(OUT_DIR, "sessionInfo.txt"))

# =============================================================================
# Done
# =============================================================================
cat("\nAll outputs written to ", OUT_DIR, "/\n", sep = "")
cat("  Table1_descriptives.csv\n")
cat("  Table_alpha.csv\n")
cat("  Table_VIF.csv\n")
cat("  Table_EFA_loadings.csv\n")
cat("  Table_EFA_fit.csv\n")
cat("  Table_residual_correlations.csv\n")
cat("  Table_R2_per_outcome.csv\n")
cat("  RegressionResults_long.csv\n")
cat("  RegressionResults_wide.csv\n")
cat("  Figure2_forest.png\n")
cat("  sessionInfo.txt\n")
