# ============================================================================
# 02_clean_data.R
# Clean and prepare datasets, create size categories and system attributes
# ============================================================================

# ============================================================================
# 1. Prepare Cat1-2-3 dataset
# ============================================================================
message("Cleaning Cat1-2-3 dataset...")

cat1_2_3_clean <- Cat_spills_AnnualReports_combined %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  mutate(
    ESTIMATED_SPILL_START_DATE_AND_TIME = as.Date(
      ESTIMATED_SPILL_START_DATE_AND_TIME, format = "%m/%d/%Y %H:%M"),
    Spill_St_Year_AR = year(ESTIMATED_SPILL_START_DATE_AND_TIME),
    Spill_Year = Spill_St_Year_AR,
    POP_SERVED_AR = clean_numeric(POP_SERVED),
    Total_System_Length_AR = clean_numeric(MILES_FORCED_MAINS) + clean_numeric(MILES_GRAVITY_SEWERS)
  ) %>%
  filter(
    ESTIMATED_SPILL_START_DATE_AND_TIME >= ANALYSIS_PERIOD_START,   
    ESTIMATED_SPILL_START_DATE_AND_TIME <  ANALYSIS_PERIOD_END,     
    SPILL_TYPE %in% c("Category 1 Spill", "Category 2 Spill"),
    !grepl("Lateral", APPEARANCE_POINTS, ignore.case = TRUE)
  )
message("  Cat1-2-3 clean: ", nrow(cat1_2_3_clean), " rows")

# ============================================================================
# 2. Prepare SSO dataset
# ============================================================================
message("Cleaning SSO dataset...")

sso_clean <- SSO_Questionnaire_combined %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  mutate(
    START.DT = as.Date(START.DT, format = "%m/%d/%Y"),
    Spill_St_Year_SSOQ = year(START.DT),
    Spill_Year = Spill_St_Year_SSOQ,
    POP_SERVED_SSOQ = clean_numeric(SSOQ.POPULATION.SERVED),
    Total_System_Length_SSOQ = clean_numeric(SSOQ_MILES_OF_FORCE_MAIN) + clean_numeric(SSOQ_MILES_OF_GRAVITY_SEWER)
  ) %>%
  filter(
    START.DT >= ANALYSIS_PERIOD_START,
    START.DT <  ANALYSIS_PERIOD_END,
    SPILL.TYPE %in% c("Category 1", "Category 2"),
    !grepl("Lateral", WHERE.FAILURE.OCCUR, ignore.case = TRUE)
  )

message("  SSO clean: ", nrow(sso_clean), " rows")

# ============================================================================
# 3. Extract most recent system attributes per WDID
# ============================================================================
message("Extracting system attributes...")

# System length from each source
length_ar <- cat1_2_3_clean %>%
  filter(!is.na(Total_System_Length_AR)) %>%
  group_by(WDID) %>%
  slice_max(Spill_Year, with_ties = FALSE) %>%
  select(WDID, Total_System_Length_AR)

length_ssoq <- sso_clean %>%
  filter(!is.na(Total_System_Length_SSOQ)) %>%
  group_by(WDID) %>%
  slice_max(Spill_Year, with_ties = FALSE) %>%
  select(WDID, Total_System_Length_SSOQ)

# Status from most recent record
status_data <- bind_rows(cat1_2_3_clean, sso_clean) %>%
  group_by(WDID) %>%
  slice_max(Spill_Year, with_ties = FALSE) %>%
  select(WDID, STATUS) %>%
  ungroup()

# ============================================================================
# 4. Population served (context only - not used in any inadequacy criteria)
# ============================================================================
pop_recent <- bind_rows(
  cat1_2_3_clean %>%
    filter(!is.na(POP_SERVED_AR)) %>%
    transmute(WDID, Spill_Year, POP_SERVED = POP_SERVED_AR),
  sso_clean %>%
    filter(!is.na(POP_SERVED_SSOQ)) %>%
    transmute(WDID, Spill_Year, POP_SERVED = POP_SERVED_SSOQ)
) %>%
  group_by(WDID) %>%
  slice_max(Spill_Year, with_ties = FALSE) %>%
  ungroup() %>%
  select(WDID, POP_SERVED_COMBINED = POP_SERVED)

# ============================================================================
# 5. Compare and combine system length estimates
# ============================================================================
length_compare <- full_join(length_ar, length_ssoq, by = "WDID") %>%
  mutate(
    TOTAL_SYS_LENGTH_DIFF_PCT = if_else(
      !is.na(Total_System_Length_AR) & !is.na(Total_System_Length_SSOQ),
      abs(Total_System_Length_AR - Total_System_Length_SSOQ) /
        pmax(Total_System_Length_AR, Total_System_Length_SSOQ),
      NA_real_
    ),
    TOTAL_SYS_LENGTH_FLAG_LARGE_DIFF = TOTAL_SYS_LENGTH_DIFF_PCT > 0.20,
    TOTAL_SYS_LENGTH_COMBINED = case_when(
      !is.na(Total_System_Length_AR) & is.na(Total_System_Length_SSOQ) ~ Total_System_Length_AR,
      is.na(Total_System_Length_AR) & !is.na(Total_System_Length_SSOQ) ~ Total_System_Length_SSOQ,
      TOTAL_SYS_LENGTH_FLAG_LARGE_DIFF ~ pmin(Total_System_Length_AR, Total_System_Length_SSOQ),
      TRUE ~ Total_System_Length_AR
    )
  )

# ============================================================================
# 6. Build recent_attributes with size categories
# ============================================================================
recent_attributes <- status_data %>%
  left_join(pop_recent, by = "WDID") %>%
  left_join(length_compare, by = "WDID") %>%
  mutate(
    Size_Category_Pop_Served = case_when(
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED > 100000 ~ "Large",
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED > 10000 & POP_SERVED_COMBINED <= 100000 ~ "Medium",
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED >= 2500 & POP_SERVED_COMBINED <= 10000 ~ "Small",
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED < 2500 ~ "Micro",
      TRUE ~ "Not Available"
    ),
    Size_Category_Length = case_when(
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED >= 500 ~ "Large",
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED >= 100 & TOTAL_SYS_LENGTH_COMBINED < 500 ~ "Medium",
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED >= 25 & TOTAL_SYS_LENGTH_COMBINED < 100 ~ "Small",
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED < 25 ~ "Micro",
      TRUE ~ "Not Available"
    )
  )

# ============================================================================
# Fill in missing size attributes for systems not in spill data
# ============================================================================

# Get population and system length from Annual Reports for systems
# not already in recent_attributes
annual_report_attributes <- AnnualReports_txt %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  mutate(
    POP_SERVED_AR = clean_numeric(POP_SERVED),
    Total_System_Length_AR = clean_numeric(MILES_FORCED_MAINS) + 
      clean_numeric(MILES_GRAVITY_SEWERS)
  ) %>%
  filter(!is.na(POP_SERVED_AR) | !is.na(Total_System_Length_AR)) %>%
  group_by(WDID) %>%
  slice_max(ANNUAL_REPORT_YEAR, with_ties = FALSE) %>%
  ungroup() %>%
  select(WDID, POP_SERVED_AR, Total_System_Length_AR)

# Get population and system length from Questionnaire for systems
# not already in recent_attributes
questionnaire_attributes <- SSO_Questionnaire_combined %>%
  filter(WDID %in% Facilities_List$WDID) %>%
  mutate(
    POP_SERVED_SSOQ = clean_numeric(SSOQ.POPULATION.SERVED),
    Total_System_Length_SSOQ = clean_numeric(SSOQ_MILES_OF_FORCE_MAIN) + 
      clean_numeric(SSOQ_MILES_OF_GRAVITY_SEWER)
  ) %>%
  filter(!is.na(POP_SERVED_SSOQ) | !is.na(Total_System_Length_SSOQ)) %>%
  mutate(START.DT = as.Date(START.DT, format = "%m/%d/%Y")) %>%
  group_by(WDID) %>%
  slice_max(START.DT, with_ties = FALSE) %>%
  ungroup() %>%
  select(WDID, POP_SERVED_SSOQ, Total_System_Length_SSOQ)

# Build supplementary attributes for systems missing from recent_attributes
supplementary_attributes <- Facilities_List %>%
  filter(CS.ASSESSMENT == "Yes") %>%
  filter(!(WDID %in% recent_attributes$WDID)) %>%
  distinct(WDID) %>%
  left_join(annual_report_attributes, by = "WDID") %>%
  left_join(questionnaire_attributes, by = "WDID") %>%
  mutate(
    POP_SERVED_COMBINED = coalesce(POP_SERVED_AR, POP_SERVED_SSOQ),
    TOTAL_SYS_LENGTH_COMBINED = case_when(
      !is.na(Total_System_Length_AR) & is.na(Total_System_Length_SSOQ) ~ 
        Total_System_Length_AR,
      is.na(Total_System_Length_AR) & !is.na(Total_System_Length_SSOQ) ~ 
        Total_System_Length_SSOQ,
      !is.na(Total_System_Length_AR) & !is.na(Total_System_Length_SSOQ) ~ 
        pmin(Total_System_Length_AR, Total_System_Length_SSOQ),
      TRUE ~ NA_real_
    ),
    Size_Category_Pop_Served = case_when(
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED > 100000 ~ "Large",
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED > 10000 & 
        POP_SERVED_COMBINED <= 100000 ~ "Medium",
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED >= 2500 & 
        POP_SERVED_COMBINED <= 10000 ~ "Small",
      !is.na(POP_SERVED_COMBINED) & POP_SERVED_COMBINED < 2500 ~ "Micro",
      TRUE ~ "Not Available"
    ),
    Size_Category_Length = case_when(
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED >= 500 ~ "Large",
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED >= 100 & 
        TOTAL_SYS_LENGTH_COMBINED < 500 ~ "Medium",
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED >= 25 & 
        TOTAL_SYS_LENGTH_COMBINED < 100 ~ "Small",
      !is.na(TOTAL_SYS_LENGTH_COMBINED) & TOTAL_SYS_LENGTH_COMBINED < 25 ~ "Micro",
      TRUE ~ "Not Available"
    ),
    STATUS = NA_character_,
    Total_System_Length_AR = Total_System_Length_AR,
    Total_System_Length_SSOQ = Total_System_Length_SSOQ,
    TOTAL_SYS_LENGTH_DIFF_PCT = NA_real_,
    TOTAL_SYS_LENGTH_FLAG_LARGE_DIFF = NA
  ) %>%
  select(-POP_SERVED_AR, -POP_SERVED_SSOQ)

# Combine with existing recent_attributes
recent_attributes <- bind_rows(recent_attributes, supplementary_attributes)

message("  recent_attributes after supplementary fill: ",
        nrow(recent_attributes), " systems (",
        sum(!is.na(recent_attributes$POP_SERVED_COMBINED)), 
        " with population data, ",
        sum(!is.na(recent_attributes$TOTAL_SYS_LENGTH_COMBINED)),
        " with system length data)")

# ============================================================================
# 7. Define category description lookup table
# ============================================================================
category_description_system_length <- tibble::tibble(
  Size_Category_Length = c("Large", "Medium", "Small", "Micro", "Not Available"),
  Description = c(
    ">= 500 miles",
    "100 - 499 miles",
    "25 - 99 miles",
    "< 25 miles",
    "NA"
  )
)

# ============================================================================
# 8. Build combined spill counts and volumes datasets
# ============================================================================
message("Building combined spill datasets...")

# Annual spill counts
annual_spill_cat1_2_3 <- cat1_2_3_clean %>%
  group_by(WDID, Spill_St_Year_AR) %>%
  summarise(spill_count = n(), .groups = "drop") %>%
  rename(Year = Spill_St_Year_AR)

annual_spill_sso <- sso_clean %>%
  group_by(WDID, Spill_St_Year_SSOQ) %>%
  summarise(spill_count = n(), .groups = "drop") %>%
  rename(Year = Spill_St_Year_SSOQ)

combined_annual_spill_counts <- bind_rows(
  annual_spill_cat1_2_3,
  annual_spill_sso
) %>%
  left_join(recent_attributes, by = "WDID")


message("Data cleaning complete.")