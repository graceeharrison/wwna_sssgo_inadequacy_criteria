# ============================================================================
# 00_setup.R
# Load libraries, set global variables, and define helper functions
# ============================================================================

# Libraries
library(dplyr)
library(readr)
library(lubridate)
library(writexl)
library(tidyr)
library(flextable)
library(officer)
library(zoo)
library(ggplot2)
library(here)

# ============================================================================
# Global Settings
# ============================================================================
options(scipen = 999)

# Analysis period bounds — spills outside this window are excluded
ANALYSIS_PERIOD_START <- as.Date("2021-01-01")
ANALYSIS_PERIOD_END   <- as.Date("2026-01-01")

# ============================================================================
# Snapshot Control
# ============================================================================
# USE_SNAPSHOT determines whether data is loaded from the live Water Boards
# URLs or from an archived snapshot in SNAPSHOT_DIR. NA loads fresh data;
# a date string (matching a folder name in SNAPSHOT_DIR) reproduces that run.

USE_SNAPSHOT  <- "2026-04-13"

SNAPSHOT_DIR  <- here("data", "snapshots")

# ============================================================================
# File Paths
# ============================================================================

# Remote data URLs
SSO_URL            <- "https://www.waterboards.ca.gov/water_issues/programs/sso/docs/data_files/SSO.txt"
QUESTIONNAIRE_URL  <- "https://www.waterboards.ca.gov/water_issues/programs/sso/docs/data_files/Questionnaire.txt"
CAT1_2_3_URL       <- "https://www.waterboards.ca.gov/water_issues/programs/sso/docs/data_files/Cat1-2-3-Spills.txt"
ANNUAL_REPORTS_URL <- "https://www.waterboards.ca.gov/water_issues/programs/sso/docs/data_files/annualReports.txt"
NEWORDER_URL       <- "https://www.waterboards.ca.gov/water_issues/programs/sso/docs/data_files/neworder_monthly_counts.txt"

# Local file paths
FACILITIES_LIST_PATH   <- here("data", "local", "CSV_CIWQS_Facilities_List_OWP_4_1_26.csv")
ENFORCEMENT_ORDERS_PATH <- here("data", "local", "CIWQS_Enforcement_Actions_4_14_2026.csv")

# Output path — overridden by run_all.R when using versioned output folders
if (!exists("OUTPUT_PATH")) {
  OUTPUT_PATH <- here("outputs")
}

# ============================================================================
# Helper Functions
# ============================================================================
clean_numeric <- function(x) {
  as.numeric(gsub(",", "", x))
}

# ============================================================================
# GESD Outlier Detection Function
# Generalized Extreme Studentized Deviate test
# Identifies statistically significant outliers in a numeric vector
# alpha: significance level (default 0.05)
# k:     maximum number of outliers to test for (default 10% of n)
# ============================================================================
gesd_test <- function(x, alpha = 0.05, k = NULL) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (is.null(k)) k <- max(1, floor(n * 0.10))
  
  outlier_indices <- c()
  remaining        <- x
  remaining_indices <- seq_along(x)
  
  R_stats <- numeric(k)
  lambdas  <- numeric(k)
  
  for (ii in 1:k) {
    m <- length(remaining)
    if (m < 3) break
    
    mean_r <- mean(remaining)
    sd_r   <- sd(remaining)
    if (sd_r == 0) break
    
    # Find most extreme value
    deviations <- abs(remaining - mean_r)
    max_idx    <- which.max(deviations)
    R_i        <- deviations[max_idx] / sd_r
    
    # Critical value from t-distribution
    p        <- 1 - alpha / (2 * (m - ii + 1))
    t_crit   <- qt(p, df = m - ii - 1)
    lambda_i <- ((m - ii) * t_crit) /
      sqrt((m - ii - 1 + t_crit^2) * (m - ii + 1))
    
    R_stats[ii] <- R_i
    lambdas[ii] <- lambda_i
    
    # Mark as outlier candidate and remove for next iteration
    outlier_indices   <- c(outlier_indices, remaining_indices[max_idx])
    remaining         <- remaining[-max_idx]
    remaining_indices <- remaining_indices[-max_idx]
  }
  
  # Work backwards to find how many outliers are significant
  n_outliers <- 0
  for (ii in k:1) {
    if (R_stats[ii] > lambdas[ii]) {
      n_outliers <- ii
      break
    }
  }
  
  # Return logical vector: TRUE = outlier
  is_outlier <- logical(length(x))
  if (n_outliers > 0) {
    is_outlier[outlier_indices[1:n_outliers]] <- TRUE
  }
  
  return(list(
    is_outlier = is_outlier,
    n_outliers = n_outliers,
    R_stats    = R_stats,
    lambdas    = lambdas
  ))
}