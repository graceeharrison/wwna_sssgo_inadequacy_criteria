# ============================================================================
# 06_export.R
# Build comprehensive inadequate systems dataset and export to Excel
#
# Inadequacy criteria included:
#   - Criteria 1: Low spill performance (system length, 5yr & 12mo)
#   - Criteria 2: Inadequate reporting (no annual report + no spill certs)
# ============================================================================

message("Building comprehensive inadequate systems dataset...")

# ----------------------------------------------------------------------------
# 1. Start with combined inadequate systems and add facility info
# ----------------------------------------------------------------------------
comprehensive_inadequate_systems <- combined_inadequate_sssgo_systems %>%
  left_join(
    Facilities_List %>%
      select(WDID, FACILITY.ID, FACILITY.NAME, PROGRAM.CATEGORY,
             COMMUNITY.TYPE, FEDERAL.FACILITY),
    by = "WDID"
  )

# ----------------------------------------------------------------------------
# 2. Add system characteristics from recent_attributes
# ----------------------------------------------------------------------------
comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  left_join(
    recent_attributes %>%
      select(WDID, POP_SERVED_COMBINED, TOTAL_SYS_LENGTH_COMBINED,
             Size_Category_Pop_Served, Size_Category_Length,
             Total_System_Length_AR, Total_System_Length_SSOQ,
             TOTAL_SYS_LENGTH_DIFF_PCT, TOTAL_SYS_LENGTH_FLAG_LARGE_DIFF),
    by = "WDID"
  )

# ----------------------------------------------------------------------------
# 3. Add performance metrics - System Length (5yr and 12mo)
# ----------------------------------------------------------------------------
length_5yr_perf <- annual_spill_counts_rolling %>%
  group_by(WDID) %>%
  slice_max(Year, with_ties = FALSE) %>%
  select(WDID,
         rolling_avg_5yr_length        = rolling_avg_5yr,
         rolling_avg_5yr_scaled_length = rolling_avg_5yr_scaled,
         performance_band_5yr_length   = performance_band,
         is_outlier_5yr_length) %>%
  ungroup()

length_12mo_perf <- rolling_avg_12mo_length_latest %>%
  select(WDID,
         spill_year_month_length        = YearMonth,
         rolling_avg_12mo_length        = rolling_avg_12mo,
         rolling_avg_12mo_scaled_length = rolling_avg_12mo_scaled,
         performance_band_12mo_length   = performance_band,
         is_outlier_12mo_length)

comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  left_join(length_5yr_perf, by = "WDID") %>%
  left_join(length_12mo_perf, by = "WDID")

# ----------------------------------------------------------------------------
# 4. Add detailed inadequacy flags
# ----------------------------------------------------------------------------
comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  mutate(
    Low_Perf_Length_5yr   = WDID %in% (annual_spill_counts_rolling %>%
                                         filter(performance_band == "Low Performing") %>%
                                         pull(WDID)),
    Low_Perf_Length_12mo  = WDID %in% (rolling_avg_12mo_length_latest %>%
                                         filter(performance_band == "Low Performing") %>%
                                         pull(WDID)),
    Low_Perf_Both_Length  = WDID %in% low_perf_both_length_quantiles$WDID,
    Missing_No_Spill_Cert = WDID %in% inad_no_spill_cert$WDID,
    Missing_Annual_Report = WDID %in% inad_annual_report$WDID,
    # Informational only — does NOT drive inadequacy determination
    Has_Enforcement_Order = WDID %in% enforcement_wdids$WDID
  )

# ----------------------------------------------------------------------------
# 5. Add enforcement order details (informational only)
# ----------------------------------------------------------------------------
enforcement_details <- SSSGO_enforcement_orders %>%
  group_by(WDID) %>%
  summarise(
    Enforcement_Count       = n(),
    Enforcement_Types       = paste(unique(ENFORCEMENT.ACTION.TYPE), collapse = "; "),
    Latest_Enforcement_Date = max(as.Date(EFFECTIVE.DATE.1, format = "%m/%d/%Y"),
                                  na.rm = TRUE),
    Total_Assessment_Amount = sum(as.numeric(TOTAL.ASSESSMENT.AMOUNT), na.rm = TRUE),
    .groups = "drop"
  )

comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  left_join(enforcement_details, by = "WDID")

# ----------------------------------------------------------------------------
# 6. Add recent spill summary (last 5 years)
# ----------------------------------------------------------------------------
recent_spill_summary <- bind_rows(
  cat1_2_3_clean %>% select(WDID, Spill_Year, SPILL_TYPE),
  sso_clean %>% select(WDID, Spill_Year, SPILL.TYPE) %>%
    rename(SPILL_TYPE = SPILL.TYPE)
) %>%
  group_by(WDID) %>%
  summarise(
    Total_Spills_5yr       = n(),
    Cat1_Spills_5yr        = sum(grepl("Category 1", SPILL_TYPE)),
    Cat2_Spills_5yr        = sum(grepl("Category 2", SPILL_TYPE)),
    Most_Recent_Spill_Year = max(Spill_Year, na.rm = TRUE),
    .groups = "drop"
  )

comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  left_join(recent_spill_summary, by = "WDID")

# ----------------------------------------------------------------------------
# 7. Clean up duplicate columns and add inadequacy reason
# ----------------------------------------------------------------------------
comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  mutate(FACILITY.NAME = coalesce(FACILITY.NAME.x, FACILITY.NAME.y)) %>%
  select(-matches("\\.x$|\\.y$")) %>%
  distinct()

comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  mutate(
    Detailed_Inadequacy_Reason = case_when(
      Low_Perf_Both_Length & Missing_No_Spill_Cert & Missing_Annual_Report ~
        "Low Performance (System Length, 5yr & 12mo) & Reporting Criteria",
      Low_Perf_Both_Length ~
        "Low Performance - System Length (5yr & 12mo)",
      Missing_No_Spill_Cert & Missing_Annual_Report ~
        "Reporting Criteria",
      TRUE ~ "Other"
    ),
    Number_of_Criteria_Met =
      Low_Perf_Both_Length +
      (Missing_No_Spill_Cert & Missing_Annual_Report)
  ) %>%
  select(
    REGION, WDID, FACILITY.NAME, FACILITY.ID, PROGRAM.CATEGORY,
    Source, Detailed_Inadequacy_Reason, Number_of_Criteria_Met,
    POP_SERVED_COMBINED, Size_Category_Pop_Served,
    TOTAL_SYS_LENGTH_COMBINED, Size_Category_Length,
    performance_band_5yr_length, rolling_avg_5yr_scaled_length,
    performance_band_12mo_length, rolling_avg_12mo_scaled_length,
    Total_Spills_5yr, Cat1_Spills_5yr, Cat2_Spills_5yr, Most_Recent_Spill_Year,
    Low_Perf_Length_5yr, Low_Perf_Length_12mo, Low_Perf_Both_Length,
    Missing_No_Spill_Cert, Missing_Annual_Report,
    Has_Enforcement_Order, Enforcement_Count, Enforcement_Types,
    Latest_Enforcement_Date, Total_Assessment_Amount,
    everything()
  )

# Final safety filter before export
comprehensive_inadequate_systems <- comprehensive_inadequate_systems %>%
  filter(WDID %in% (Facilities_List %>%
                      filter(CS.ASSESSMENT == "Yes") %>%
                      pull(WDID)))

message("Final system count after CS Assessment filter: ",
        nrow(comprehensive_inadequate_systems))

# ----------------------------------------------------------------------------
# 8. Print summary statistics
# ----------------------------------------------------------------------------
summary_stats <- comprehensive_inadequate_systems %>%
  summarise(
    Total_Systems        = n(),
    Low_Perf_Length_Only = sum(Low_Perf_Both_Length & !Missing_No_Spill_Cert),
    Reporting_Only       = sum(!Low_Perf_Both_Length & Missing_No_Spill_Cert),
    Multiple_Criteria    = sum(Number_of_Criteria_Met > 1)
  )

print(summary_stats)

# ----------------------------------------------------------------------------
# 9. Export to Excel
# ----------------------------------------------------------------------------
filename <- paste0("Inadequate_SSSGO_Systems_Comprehensive_", Sys.Date(), ".xlsx")
output_path <- file.path(OUTPUT_PATH, filename)

write_xlsx(comprehensive_inadequate_systems, output_path)

message("Export complete: ", nrow(comprehensive_inadequate_systems),
        " systems saved to ", output_path)