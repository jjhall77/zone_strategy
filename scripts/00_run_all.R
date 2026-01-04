# =============================================================================
# 00_run_all.R
# Master script to run all analyses
# =============================================================================

library(here)

message("="*60)
message("NYPD SUMMER ZONE STRATEGY EVALUATION")
message("="*60)

# Create directories
dir.create(here("data"), showWarnings = FALSE)
dir.create(here("output"), showWarnings = FALSE)
dir.create(here("output", "figures"), showWarnings = FALSE)

# Run scripts in order
scripts <- c(
  "01_load_data.R",
  "02_snap_to_segments.R",
  "03_build_analysis_data.R",
  "04_targeting_efficiency.R",
  "05_give_comparison.R",
  "06_treatment_effects.R"
)

for (script in scripts) {
  message("\n", strrep("=", 60))
  message(sprintf("Running %s...", script))
  message(strrep("=", 60), "\n")
  
  source(here(script))
}

message("\n", strrep("=", 60))
message("ALL ANALYSES COMPLETE")
message(strrep("=", 60))
