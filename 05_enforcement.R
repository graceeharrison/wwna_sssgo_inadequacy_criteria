# ============================================================================
# 05_enforcement.R
# Load and prepare enforcement order data, then build the combined
# Inadequacy List from spill performance and reporting criteria only.
#
# NOTE: Systems with active enforcement orders are no longer included in the
# Inadequacy List. They are identified separately as "Under Enforcement"
# systems.
# ============================================================================

message("Preparing enforcement order data...")

# ----------------------------------------------------------------------------
# 1. Load and filter enforcement orders
# ----------------------------------------------------------------------------
enforcement_orders_CIWQS <- enforcement_orders_CIWQS %>%
  filter(FACILITY.ID %in% Facilities_List$FACILITY.ID)

SSSGO_enforcement_orders <- enforcement_orders_CIWQS %>%
  filter(
    PROGRAM.CATEGORY == "SSO",
    STATUS == "Active",
    STATUS.1 == "Active",
    ENFORCEMENT.ACTION.TYPE %in% c(
      "Admin Civil Liability",
      "Cease and Desist Order",
      "Clean-up and Abatement Order",
      "Time Schedule Order",
      "Notice of Violation"
    ),
    as.Date(EFFECTIVE.DATE.1, format = "%m/%d/%Y") >= ANALYSIS_PERIOD_START,
    as.Date(EFFECTIVE.DATE.1, format = "%m/%d/%Y") <  ANALYSIS_PERIOD_END
  )

message("  Active enforcement orders found: ", nrow(SSSGO_enforcement_orders))

# ----------------------------------------------------------------------------
# 2. Add region and size info to enforcement orders, then exclude systems 
#    already on the combined inadequacy list
# ----------------------------------------------------------------------------
SSSGO_enforcement_orders_detailed <- SSSGO_enforcement_orders %>%
  filter(!is.na(WDID)) %>%
  mutate(REGION = substr(WDID, 1, 1)) %>%
  left_join(
    recent_attributes %>%
      select(WDID, Size_Category_Pop_Served, Size_Category_Length,
             POP_SERVED_COMBINED, TOTAL_SYS_LENGTH_COMBINED),
    by = "WDID"
  ) %>%
  select(
    REGION, WDID, FACILITY.NAME,
    Size_Category_Pop_Served, POP_SERVED_COMBINED,
    Size_Category_Length, TOTAL_SYS_LENGTH_COMBINED,
    ENFORCEMENT.ACTION.TYPE, EFFECTIVE.DATE.1,
    TOTAL.ASSESSMENT.AMOUNT, STATUS, STATUS.1,
    everything()
  )

# Exclude systems already identified in the combined inadequacy list
# (Criteria 1: Spills or Criteria 2: Reporting)
SSSGO_enforcement_orders_detailed <- SSSGO_enforcement_orders_detailed %>%
  filter(!(WDID %in% combined_inadequate_sssgo_systems$WDID))

message("  Systems with enforcement orders (not already inadequate): ", 
        n_distinct(SSSGO_enforcement_orders_detailed$FACILITY.NAME))

enforcement_wdids <- SSSGO_enforcement_orders_detailed %>%
  distinct(WDID, FACILITY.NAME, REGION, Size_Category_Pop_Served,
           Size_Category_Length, POP_SERVED_COMBINED, TOTAL_SYS_LENGTH_COMBINED)

message("  Unique systems with active enforcement orders: ",
        n_distinct(enforcement_wdids$FACILITY.NAME))
