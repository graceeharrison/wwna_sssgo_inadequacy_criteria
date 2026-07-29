# ============================================================================
# 03_criteria1_spills.R
# Identify low performing systems based on spill metrics
# Criteria 1: System in low-performance band for their size category
# in both 5-year and 12-month rolling averages (system length / spill counts)
#
# Low performance threshold: above 75th percentile within size category,
# excluding known non/under-reporting systems from the baseline.
#
# For Medium and Large systems, an absolute numeric standard is applied on
# top of the percentile bands: a system is exempted from the spill-based
# Inadequacy List if it meets BOTH of the following, regardless of its
# percentile ranking:
#   - 5-year rolling average  <= 2 spills per 100 miles
#   - 12-month rolling average <= 1 spill per 100 miles
# Small and Micro systems are governed by the percentile bands only. See
# PART 1, Section 4 below.
#
# GESD test is run after the numeric standard above, as an informational
# marker only — it does NOT determine which systems are low performing. See
# PART 1, Section 5 below.
# ============================================================================

# ============================================================================
# PRE-STEP: Identify known non/under-reporting systems to exclude from baselines
# These systems meet the no-spill certification inadequacy criteria and should
# not be used to set spill-rate benchmarks.
# ============================================================================

nonreporting_wdids <- neworder_no_spills %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  mutate(month_year = as.Date(MONTH_YEAR, format = "%d-%b-%y")) %>%
  filter(
    month_year >= ANALYSIS_PERIOD_END - years(1),
    month_year <=  ANALYSIS_PERIOD_END
  ) %>%
  group_by(WDID) %>%
  mutate(
    yr_spill_counts     = sum(CAT_1_2_3_SPILL_COUNT, na.rm = TRUE),
    no_spill_cert_count = sum(`NO_SPILLS..0.No.1.Yes.`, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(yr_spill_counts < 1, no_spill_cert_count < 1) %>%
  distinct(WDID)

message("  Known non/under-reporting systems excluded from baselines: ",
        nrow(nonreporting_wdids))

# ============================================================================
# PART 1: SYSTEM LENGTH AND SPILL COUNTS
# ============================================================================

message("Calculating system length-based spill metrics...")

# ----------------------------------------------------------------------------
# 1. 5-Year Rolling Average - System Length / Spill Counts
# ----------------------------------------------------------------------------

combined_annual_spill_counts <- combined_annual_spill_counts %>%
  select(WDID, Year, spill_count) %>%
  left_join(
    recent_attributes %>%
      select(WDID, Size_Category_Length, TOTAL_SYS_LENGTH_COMBINED, STATUS,
             Total_System_Length_AR, Total_System_Length_SSOQ),
    by = "WDID"
  )

annual_spill_counts_agg <- combined_annual_spill_counts %>%
  group_by(WDID, Year, Size_Category_Length, TOTAL_SYS_LENGTH_COMBINED) %>%
  summarise(spill_count = sum(spill_count, na.rm = TRUE), .groups = "drop")

all_years <- seq(min(annual_spill_counts_agg$Year, na.rm = TRUE),
                 max(annual_spill_counts_agg$Year, na.rm = TRUE))

annual_spill_counts_complete <- annual_spill_counts_agg %>%
  group_by(WDID, Size_Category_Length, TOTAL_SYS_LENGTH_COMBINED) %>%
  complete(Year = all_years, fill = list(spill_count = 0)) %>%
  fill(TOTAL_SYS_LENGTH_COMBINED, Size_Category_Length, .direction = "downup") %>%
  arrange(WDID, Year) %>%
  ungroup()

annual_spill_counts_rolling <- annual_spill_counts_complete %>%
  group_by(WDID, Size_Category_Length) %>%
  mutate(
    rolling_avg_5yr = rollapply(
      spill_count,
      width = 5,
      FUN = mean,
      align = "right",
      fill = NA,
      na.rm = TRUE
    )
  ) %>%
  filter(!is.na(rolling_avg_5yr)) %>%
  ungroup()

annual_spill_counts_rolling <- annual_spill_counts_rolling %>%
  mutate(
    scale_miles = case_when(
      TOTAL_SYS_LENGTH_COMBINED < 5 ~ 5,
      TOTAL_SYS_LENGTH_COMBINED >= 5 & TOTAL_SYS_LENGTH_COMBINED < 25 ~ 25,
      TOTAL_SYS_LENGTH_COMBINED >= 25 ~ 100,
      TRUE ~ NA_real_
    ),
    rolling_avg_5yr_scaled = ifelse(
      is.na(scale_miles) | TOTAL_SYS_LENGTH_COMBINED == 0,
      NA_real_,
      rolling_avg_5yr / (TOTAL_SYS_LENGTH_COMBINED / scale_miles)
    )
  )

# Compute 75th percentile threshold per size category — this sets the
# low-performance boundary. Non/under-reporting systems are excluded so they
# do not distort the baseline.
annual_spills_quantiles <- annual_spill_counts_rolling %>%
  filter(!is.na(rolling_avg_5yr_scaled)) %>%
  filter(!WDID %in% nonreporting_wdids$WDID) %>%
  group_by(Size_Category_Length) %>%
  summarise(
    Q1 = quantile(rolling_avg_5yr_scaled, 0.25, na.rm = TRUE),
    Q3 = quantile(rolling_avg_5yr_scaled, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

annual_spill_counts_rolling <- annual_spill_counts_rolling %>%
  left_join(annual_spills_quantiles, by = "Size_Category_Length") %>%
  mutate(
    performance_band = case_when(
      rolling_avg_5yr_scaled < Q1                                        ~ "High Performing",
      rolling_avg_5yr_scaled >= Q1 & rolling_avg_5yr_scaled <= Q3        ~ "Medium Performing",
      rolling_avg_5yr_scaled > Q3                                         ~ "Low Performing",
      TRUE                                                                 ~ NA_character_
    )
  )

# Run GESD test as an informational marker only — does NOT set the threshold.
# is_outlier_5yr_length flags statistical outliers for reference; outlier
# status alone does not determine inadequacy.
gesd_results_5yr_length <- annual_spill_counts_rolling %>%
  filter(!is.na(rolling_avg_5yr_scaled)) %>%
  filter(!WDID %in% nonreporting_wdids$WDID) %>%
  group_by(Size_Category_Length) %>%
  group_modify(~ {
    result <- gesd_test(.x$rolling_avg_5yr_scaled)
    .x %>% mutate(is_outlier_5yr_length = result$is_outlier)
  }) %>%
  ungroup() %>%
  select(WDID, Year, Size_Category_Length, is_outlier_5yr_length)

annual_spill_counts_rolling <- annual_spill_counts_rolling %>%
  left_join(gesd_results_5yr_length,
            by = c("WDID", "Year", "Size_Category_Length"))

low_performing_systems_5yr_length <- annual_spill_counts_rolling %>%
  filter(performance_band == "Low Performing") %>%
  distinct(WDID, rolling_avg_5yr_scaled, performance_band, Size_Category_Length)

message("  5yr length low performers identified: ",
        nrow(low_performing_systems_5yr_length))

# ----------------------------------------------------------------------------
# 2. 12-Month Rolling Average - System Length / Spill Counts
# ----------------------------------------------------------------------------

Cat_spills_AnnualReports_combined_with_attrs <- Cat_spills_AnnualReports_combined %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  mutate(
    ESTIMATED_SPILL_START_DATE_AND_TIME = as.Date(
      ESTIMATED_SPILL_START_DATE_AND_TIME, format = "%m/%d/%Y %H:%M")
  ) %>%
  filter(!is.na(ESTIMATED_SPILL_START_DATE_AND_TIME)) %>%
  left_join(
    recent_attributes %>%
      select(WDID, TOTAL_SYS_LENGTH_COMBINED, Size_Category_Length),
    by = "WDID"
  )

monthly_spill_counts <- Cat_spills_AnnualReports_combined_with_attrs %>%
  mutate(YearMonth = floor_date(ESTIMATED_SPILL_START_DATE_AND_TIME, "month")) %>%
  group_by(WDID, YearMonth, TOTAL_SYS_LENGTH_COMBINED, Size_Category_Length) %>%
  summarise(spill_count_raw = n(), .groups = "drop") %>%
  arrange(WDID, YearMonth)

monthly_spill_counts_filled <- monthly_spill_counts %>%
  group_by(WDID, TOTAL_SYS_LENGTH_COMBINED) %>%
  summarise(
    min_month = min(YearMonth),
    max_month = max(YearMonth),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(month_seq = list(seq(min_month, max_month, by = "month"))) %>%
  ungroup() %>%
  select(WDID, TOTAL_SYS_LENGTH_COMBINED, month_seq) %>%
  unnest(month_seq) %>%
  rename(YearMonth = month_seq) %>%
  left_join(monthly_spill_counts,
            by = c("WDID", "TOTAL_SYS_LENGTH_COMBINED", "YearMonth")) %>%
  mutate(spill_count_raw = replace_na(spill_count_raw, 0)) %>%
  arrange(WDID, YearMonth)

rolling_avg_12mo_length <- monthly_spill_counts_filled %>%
  group_by(WDID, TOTAL_SYS_LENGTH_COMBINED) %>%
  arrange(YearMonth) %>%
  mutate(
    rolling_avg_12mo = rollapply(
      spill_count_raw,
      width = 12,
      FUN = mean,
      align = "right",
      fill = NA,
      na.rm = TRUE
    )
  ) %>%
  filter(!is.na(rolling_avg_12mo))

rolling_avg_12mo_length_latest <- rolling_avg_12mo_length %>%
  group_by(WDID, TOTAL_SYS_LENGTH_COMBINED) %>%
  slice_max(YearMonth, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    scale_miles = case_when(
      TOTAL_SYS_LENGTH_COMBINED < 5 ~ 5,
      TOTAL_SYS_LENGTH_COMBINED >= 5 & TOTAL_SYS_LENGTH_COMBINED < 25 ~ 25,
      TOTAL_SYS_LENGTH_COMBINED >= 25 ~ 100,
      TRUE ~ NA_real_
    ),
    rolling_avg_12mo_scaled = ifelse(
      is.na(scale_miles) | TOTAL_SYS_LENGTH_COMBINED == 0,
      NA_real_,
      rolling_avg_12mo / (TOTAL_SYS_LENGTH_COMBINED / scale_miles)
    )
  )

# Compute 75th percentile threshold per size category — this sets the
# low-performance boundary. Non/under-reporting systems are excluded.
quantiles_12mo_length <- rolling_avg_12mo_length_latest %>%
  filter(!is.na(rolling_avg_12mo_scaled)) %>%
  filter(!WDID %in% nonreporting_wdids$WDID) %>%
  group_by(Size_Category_Length) %>%
  summarise(
    Q1_12mo = quantile(rolling_avg_12mo_scaled, 0.25, na.rm = TRUE),
    Q3_12mo = quantile(rolling_avg_12mo_scaled, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

rolling_avg_12mo_length_latest <- rolling_avg_12mo_length_latest %>%
  left_join(quantiles_12mo_length, by = "Size_Category_Length") %>%
  mutate(
    performance_band = case_when(
      rolling_avg_12mo_scaled < Q1_12mo                                           ~ "High Performing",
      rolling_avg_12mo_scaled >= Q1_12mo & rolling_avg_12mo_scaled <= Q3_12mo     ~ "Medium Performing",
      rolling_avg_12mo_scaled > Q3_12mo                                            ~ "Low Performing",
      TRUE                                                                          ~ NA_character_
    )
  )

# Run GESD test as an informational marker only — does NOT set the threshold.
# is_outlier_12mo_length flags statistical outliers for reference; outlier
# status alone does not determine inadequacy.
gesd_results_12mo_length <- rolling_avg_12mo_length_latest %>%
  filter(!is.na(rolling_avg_12mo_scaled)) %>%
  filter(!WDID %in% nonreporting_wdids$WDID) %>%
  group_by(Size_Category_Length) %>%
  group_modify(~ {
    result <- gesd_test(.x$rolling_avg_12mo_scaled)
    .x %>% mutate(is_outlier_12mo_length = result$is_outlier)
  }) %>%
  ungroup() %>%
  select(WDID, Size_Category_Length, is_outlier_12mo_length)

rolling_avg_12mo_length_latest <- rolling_avg_12mo_length_latest %>%
  left_join(gesd_results_12mo_length,
            by = c("WDID", "Size_Category_Length"))

low_performing_systems_12mo_length <- rolling_avg_12mo_length_latest %>%
  filter(performance_band == "Low Performing") %>%
  distinct(WDID, rolling_avg_12mo_scaled, performance_band, Size_Category_Length)

message("  12mo length low performers identified: ",
        nrow(low_performing_systems_12mo_length))

# ----------------------------------------------------------------------------
# 3. Systems low performing in BOTH 5yr and 12mo - Length
# ----------------------------------------------------------------------------
low_perf_both_length_quantiles <- low_performing_systems_5yr_length %>%
  inner_join(low_performing_systems_12mo_length,
             by = "WDID", suffix = c("_5yr", "_12mo")) %>%
  mutate(
    Size_Category_Length_final = if_else(
      Size_Category_Length_5yr != Size_Category_Length_12mo,
      Size_Category_Length_12mo,
      Size_Category_Length_5yr
    )
  ) %>%
  distinct(WDID, .keep_all = TRUE)

message("  Systems low performing in both length metrics: ",
        n_distinct(low_perf_both_length_quantiles$FACILITY.NAME))

# ----------------------------------------------------------------------------
# 4. Numeric threshold exemption for Medium and Large systems
#
# Per the 2026-07-16 WWNA Project Team memo, Medium and Large systems are
# removed from the spill-based Inadequacy List when they meet BOTH numeric
# thresholds below (AND logic — the memo's recommended, stricter option,
# not the more inclusive OR logic shown in the memo's Table 2). This is
# evaluated only among systems already flagged as Low Performing in both
# rolling windows above; it does not change how the 75th-percentile bands
# themselves are computed.
# ----------------------------------------------------------------------------

NUMERIC_THRESHOLD_5YR_PER_100MI  <- 2  # 5-year rolling avg, spills/100 miles
NUMERIC_THRESHOLD_12MO_PER_100MI <- 1  # 12-month rolling avg, spills/100 miles
NUMERIC_THRESHOLD_SIZE_CATEGORIES <- c("Medium", "Large")

numeric_threshold_exempt_systems <- low_perf_both_length_quantiles %>%
  filter(
    Size_Category_Length_final %in% NUMERIC_THRESHOLD_SIZE_CATEGORIES,
    rolling_avg_5yr_scaled  <= NUMERIC_THRESHOLD_5YR_PER_100MI,
    rolling_avg_12mo_scaled <= NUMERIC_THRESHOLD_12MO_PER_100MI
  ) %>%
  left_join(Facilities_List %>% select(WDID, FACILITY.NAME), by = "WDID") %>%
  distinct(WDID, FACILITY.NAME, Size_Category_Length_final,
           rolling_avg_5yr_scaled, rolling_avg_12mo_scaled)

message("  Medium/Large systems exempted via numeric threshold (5yr <= ",
        NUMERIC_THRESHOLD_5YR_PER_100MI, " AND 12mo <= ",
        NUMERIC_THRESHOLD_12MO_PER_100MI, " spills/100mi): ",
        nrow(numeric_threshold_exempt_systems))

low_perf_both_length_quantiles <- low_perf_both_length_quantiles %>%
  filter(!WDID %in% numeric_threshold_exempt_systems$WDID)

message("  Systems remaining low performing in both length metrics after ",
        "numeric threshold exemption: ",
        n_distinct(low_perf_both_length_quantiles$FACILITY.NAME))

# ============================================================================
# PART 2: COMBINE INTO FINAL LOW PERFORMER LIST
# ============================================================================

message("Building final low performer list (system length criteria)...")

low_perf_any_metric <- low_perf_both_length_quantiles %>%
  mutate(Performance_Category = "Length") %>%
  left_join(Facilities_List %>% select(WDID, FACILITY.NAME), by = "WDID") %>%
  mutate(REGION = substr(WDID, 1, 1)) %>%
  select(REGION, WDID, FACILITY.NAME, Performance_Category,
         Size_Category_Length_final, everything())

message("Criteria 1 complete. Total low performers: ",
        n_distinct(low_perf_any_metric$FACILITY.NAME))