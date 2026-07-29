# ============================================================================
# 01_load_data.R
# Load all raw data from remote URLs and local files
#
# USE_SNAPSHOT and SNAPSHOT_DIR are set in 00_setup.R. Snapshots are saved
# to data/snapshots/<date>/ each time live data is downloaded.
# ============================================================================

# ============================================================================
# 1. Load Remote Data (or snapshot)
# ============================================================================

if (!is.na(USE_SNAPSHOT)) {
  
  # --------------------------------------------------------------------------
  # SNAPSHOT MODE — load archived data from a specific past run
  # --------------------------------------------------------------------------
  snap_path <- file.path(SNAPSHOT_DIR, USE_SNAPSHOT)
  
  if (!dir.exists(snap_path)) {
    stop("Snapshot directory not found: ", snap_path,
         "\nCheck that SNAPSHOT_DIR and USE_SNAPSHOT are set correctly in 00_setup.R")
  }
  
  message("Loading data from snapshot: ", USE_SNAPSHOT)
  
  SSO_txt <- read.csv(
    file.path(snap_path, "SSO.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  SSO data loaded: ", nrow(SSO_txt), " rows")
  
  Questionnaire_txt <- read.csv(
    file.path(snap_path, "Questionnaire.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  Questionnaire data loaded: ", nrow(Questionnaire_txt), " rows")
  
  Cat1_2_3_txt <- read.csv(
    file.path(snap_path, "Cat1_2_3.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  Cat1-2-3 spills loaded: ", nrow(Cat1_2_3_txt), " rows")
  
  AnnualReports_txt <- read.csv(
    file.path(snap_path, "AnnualReports.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  Annual reports loaded: ", nrow(AnnualReports_txt), " rows")
  
  neworder_no_spills <- read.csv(
    file.path(snap_path, "neworder_no_spills.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  New order monthly counts loaded: ", nrow(neworder_no_spills), " rows")
  
  message("Snapshot data loaded successfully from: ", snap_path)
  
} else {
  
  # --------------------------------------------------------------------------
  # LIVE MODE — download fresh data from remote URLs
  # --------------------------------------------------------------------------
  message("Loading remote data...")
  
  SSO_txt <- read.delim(SSO_URL)
  message("  SSO data loaded: ", nrow(SSO_txt), " rows")
  
  Questionnaire_txt <- read.delim(QUESTIONNAIRE_URL)
  message("  Questionnaire data loaded: ", nrow(Questionnaire_txt), " rows")
  
  Cat1_2_3_txt <- read.delim(CAT1_2_3_URL)
  message("  Cat1-2-3 spills loaded: ", nrow(Cat1_2_3_txt), " rows")
  
  AnnualReports_txt <- read.delim(ANNUAL_REPORTS_URL)
  message("  Annual reports loaded: ", nrow(AnnualReports_txt), " rows")
  
  neworder_no_spills <- read.delim(NEWORDER_URL)
  message("  New order monthly counts loaded: ", nrow(neworder_no_spills), " rows")
  
  # --------------------------------------------------------------------------
  # Save snapshot of this download for future reproducibility
  # --------------------------------------------------------------------------
  snap_path <- file.path(SNAPSHOT_DIR, as.character(Sys.Date()))
  
  if (dir.exists(snap_path)) {
    message("Snapshot already exists for today (", Sys.Date(), ") — overwriting.")
  } else {
    dir.create(snap_path, recursive = TRUE)
  }
  
  write.csv(SSO_txt,
            file.path(snap_path, "SSO.csv"),
            row.names = FALSE)
  
  write.csv(Questionnaire_txt,
            file.path(snap_path, "Questionnaire.csv"),
            row.names = FALSE)
  
  write.csv(Cat1_2_3_txt,
            file.path(snap_path, "Cat1_2_3.csv"),
            row.names = FALSE)
  
  write.csv(AnnualReports_txt,
            file.path(snap_path, "AnnualReports.csv"),
            row.names = FALSE)
  
  write.csv(neworder_no_spills,
            file.path(snap_path, "neworder_no_spills.csv"),
            row.names = FALSE)
  
  message("Remote data downloaded and snapshot saved to: ", snap_path)
  
}

# ============================================================================
# 2. Load Local Data
# ============================================================================
message("Loading local data...")

# Load facilities list (complete WDID list, all needed columns)
Facilities_List <- read.csv(here("data", "local", "CSV_CIWQS_Facilities_List_OWP_4_1_26.csv"),
                            na.strings = c("", "NA", "#N/A"))

message("Facilities list loaded: ", nrow(Facilities_List), " rows, ",
        n_distinct(Facilities_List$WDID), " unique WDIDs")

# Load enforcement order data
enforcement_orders_CIWQS <- read.csv(ENFORCEMENT_ORDERS_PATH)
message("  Enforcement orders loaded: ", nrow(enforcement_orders_CIWQS), " rows")

# ============================================================================
# 3. Initial Combines
# ============================================================================
message("Combining datasets...")

# Standardize Questionnaire column names
names(Questionnaire_txt) <- toupper(names(Questionnaire_txt))

# Combine SSO and Questionnaire
SSO_Questionnaire_combined <- left_join(SSO_txt, Questionnaire_txt, by = "WDID")

# Combine Cat1-2-3 and Annual Reports
Cat_spills_AnnualReports_combined <- left_join(Cat1_2_3_txt, AnnualReports_txt, by = "WDID")

message("Data loading complete.")