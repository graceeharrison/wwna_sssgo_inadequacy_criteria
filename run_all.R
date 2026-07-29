# ============================================================================
# run_all.R
# Master script to run full analysis pipeline in order
# ============================================================================

# Set version-specific output folder BEFORE sourcing setup
OUTPUT_PATH <- here::here("outputs", "inadequacy_revisions_5_22_26")

# Create the folder if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
  message("Created output folder: ", OUTPUT_PATH)
}

source("R/00_setup.R")
source("R/01_load_data.R")
source("R/02_clean_data.R")
source("R/03_criteria1_spills.R")              # Inadequacy: system length spill metrics only
source("R/04_criteria2_reporting.R")            # Inadequacy: reporting criteria
source("R/05_enforcement.R")                   # Prepare enforcement data; build inadequacy list
source("R/06_export.R")                        # Export Inadequacy List
source("R/08_under_enforcement.R")             # Export separate Under Enforcement list
source("R/09_risk_assessment_spill_volume.R")  # Risk Assessment: spill volume per population
source("R/10_risk_assessment_spill_count_mileage.R")  # Risk Assessment: spill count per mileage