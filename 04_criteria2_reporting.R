# ============================================================================
# 04_criteria2_reporting.R
# Identify systems with inadequate reporting
# Criteria 2: Missing No Spill Certifications AND not in Annual Report dataset
# ============================================================================

message("Running Criteria 2 - Reporting checks...")

# ----------------------------------------------------------------------------
# 1. No Spill Certification check
# ----------------------------------------------------------------------------
neworder_no_spills <- neworder_no_spills %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  mutate(month_year = as.Date(MONTH_YEAR, format = "%d-%b-%y")) %>%
  filter(
    month_year >= ANALYSIS_PERIOD_END - years(1),
    month_year <  ANALYSIS_PERIOD_END
  )

# Filter for systems reporting 0 spills
neworder_no_spills <- neworder_no_spills %>%
  group_by(SEWER_SYSTEM_NAME) %>%
  mutate(yr_spill_counts = sum(CAT_1_2_3_SPILL_COUNT, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(yr_spill_counts < 1)

# Filter for systems with zero no-spill certifications
neworder_no_spills <- neworder_no_spills %>%
  group_by(SEWER_SYSTEM_NAME) %>%
  mutate(no_spill_cert_count = sum(NO_SPILLS..0.No.1.Yes., na.rm = TRUE)) %>%
  ungroup() %>%
  filter(no_spill_cert_count < 1)

inad_no_spill_cert <- neworder_no_spills %>%
  left_join(Facilities_List, by = "WDID") %>%
  filter(CS.ASSESSMENT == "Yes") %>%
  mutate(FLAG_REASON = "No spills reported") %>%
  select(WDID, FACILITY.NAME, PROGRAM.CATEGORY, CS.ASSESSMENT, FLAG_REASON) %>%
  distinct()

message("  Systems missing no-spill certifications: ", nrow(inad_no_spill_cert))

# ----------------------------------------------------------------------------
# 2. No Annual Report check
# ----------------------------------------------------------------------------
Annual_Reports <- AnnualReports_txt

Annual_Reports$LAST_UPDATED_ON <- dmy(Annual_Reports$LAST_UPDATED_ON)

Annual_Reports <- Annual_Reports %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  filter(is.na(LAST_UPDATED_ON) | LAST_UPDATED_ON <= ANALYSIS_PERIOD_END)

# Use full Annual Reports for presence check (not date filtered)
Annual_Reports_presence <- AnnualReports_txt %>%
  filter(WDID %in% Facilities_List$WDID)

no_annual_report <- Facilities_List %>%
  filter(CS.ASSESSMENT == "Yes") %>%
  filter(!(WDID %in% Annual_Reports_presence$WDID)) %>%
  distinct(WDID)

inad_annual_report <- no_annual_report %>%
  left_join(Facilities_List, by = "WDID") %>%
  filter(CS.ASSESSMENT == "Yes") %>%
  mutate(FLAG_REASON = "No annual report") %>%
  select(WDID, FACILITY.NAME, PROGRAM.CATEGORY, CS.ASSESSMENT, FLAG_REASON) %>%
  distinct()

message("  Systems missing annual report: ", nrow(inad_annual_report))

# ----------------------------------------------------------------------------
# 3. Combine both reporting criteria
# ----------------------------------------------------------------------------
inadequate_reporting_criteria_combined <- inner_join(
  inad_annual_report %>% select(-FLAG_REASON),
  inad_no_spill_cert %>% select(-FLAG_REASON),
  by = c("WDID", "FACILITY.NAME", "PROGRAM.CATEGORY", "CS.ASSESSMENT")
) %>%
  distinct() %>%
  mutate(REGION = substr(WDID, 1, 1)) %>%
  left_join(
    recent_attributes %>%
      select(WDID, Size_Category_Pop_Served, Size_Category_Length,
             POP_SERVED_COMBINED, TOTAL_SYS_LENGTH_COMBINED),
    by = "WDID"
  ) %>%
  relocate(REGION, WDID, FACILITY.NAME, PROGRAM.CATEGORY, CS.ASSESSMENT,
           Size_Category_Pop_Served, Size_Category_Length)

message("  Systems meeting both reporting criteria: ",
        nrow(inadequate_reporting_criteria_combined))

message("Criteria 2 complete.")

# ----------------------------------------------------------------------------
# 3. Build combined Inadequacy List (spill performance + reporting only)
#    Enforcement orders are intentionally excluded here and identified
#    separately as "Under Enforcement" systems.
# ----------------------------------------------------------------------------
message("Building combined Inadequacy List (spill performance + reporting criteria)...")

combined_inadequate_sssgo_systems <- bind_rows(
  inadequate_reporting_criteria_combined %>%
    select(REGION, WDID, FACILITY.NAME) %>%
    mutate(Source = "Reporting Criteria"),
  low_perf_any_metric %>%
    select(REGION, WDID, FACILITY.NAME) %>%
    mutate(Source = "Low Performance - Spills")
) %>%
  group_by(REGION, WDID, FACILITY.NAME) %>%
  summarise(Source = paste(unique(Source), collapse = " & "), .groups = "drop")

combined_inadequate_sssgo_systems <- combined_inadequate_sssgo_systems %>%
  filter(WDID %in% (Facilities_List %>%
                      filter(CS.ASSESSMENT == "Yes") %>%
                      pull(WDID)))

message("Combined Inadequacy List complete: ",
        n_distinct(combined_inadequate_sssgo_systems$FACILITY.NAME), " systems",
        " (enforcement orders excluded — identified separately as Under Enforcement)")