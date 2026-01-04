# =============================================================================
# 04_targeting_efficiency.R
# Evaluate zone targeting efficiency
# Core question: Did NYPD target the right places?
# =============================================================================

library(here)
library(tidyverse)
library(sf)
library(knitr)
library(scales)

# Load data
load(here("data", "loaded_data.RData"))
load(here("data", "analysis_data.RData"))

# =============================================================================
# 1. TOP-N SEGMENT COVERAGE ANALYSIS
# =============================================================================

message("=== TOP-N SEGMENT COVERAGE ===\n")

# Calculate coverage for various thresholds
thresholds <- c(50, 100, 150, 200, 250, 300, 400, 500, 750, 1000)

# Function to calculate coverage stats at threshold N
calc_coverage <- function(df, n, rank_col, value_col) {
  
  top_n <- df %>%
    filter(.data[[rank_col]] <= n)
  
  tibble(
    threshold = n,
    
    # Zone coverage of top segments
    n_in_zone = sum(top_n$in_zone),
    n_out_zone = sum(!top_n$in_zone),
    pct_in_zone = 100 * mean(top_n$in_zone),
    
    # Crime concentration in top segments
    crime_in_top_n = sum(top_n[[value_col]]),
    crime_total = sum(df[[value_col]]),
    pct_crime_in_top_n = 100 * sum(top_n[[value_col]]) / sum(df[[value_col]]),
    
    # Of crime in top segments, how much is in zones?
    crime_in_top_n_in_zone = sum(top_n[[value_col]] * top_n$in_zone),
    pct_crime_captured_by_zones = 100 * sum(top_n[[value_col]] * top_n$in_zone) / 
                                   sum(top_n[[value_col]])
  )
}

# --- Shootings ---
coverage_shootings <- map_dfr(
  thresholds,
  ~calc_coverage(segments, .x, "rank_shootings", "shootings_pre")
) %>%
  mutate(measure = "shootings")

# --- Gun Violence ---
coverage_gun_violence <- map_dfr(
  thresholds,
  ~calc_coverage(segments, .x, "rank_gun_violence", "gun_violence_pre")
) %>%
  mutate(measure = "gun_violence")

# Combine
coverage_results <- bind_rows(coverage_shootings, coverage_gun_violence)

# Display results
message("Top-N Segment Coverage (Shootings):")
coverage_shootings %>%
  select(threshold, n_in_zone, n_out_zone, pct_in_zone, 
         pct_crime_in_top_n, pct_crime_captured_by_zones) %>%
  kable(digits = 1, col.names = c("Top N", "In Zone", "Out Zone", "% In Zone",
                                   "% Citywide Shootings", "% of Top-N Captured"))

message("\nTop-N Segment Coverage (Gun Violence = Shootings + Shots Fired):")
coverage_gun_violence %>%
  select(threshold, n_in_zone, n_out_zone, pct_in_zone, 
         pct_crime_in_top_n, pct_crime_captured_by_zones) %>%
  kable(digits = 1, col.names = c("Top N", "In Zone", "Out Zone", "% In Zone",
                                   "% Citywide GV", "% of Top-N Captured"))

# =============================================================================
# 2. ZONE EFFICIENCY METRICS
# =============================================================================

message("\n=== ZONE EFFICIENCY METRICS ===\n")

# Calculate zone-level statistics
zone_stats <- segments_sf_final %>%
  st_drop_geometry() %>%
  group_by(in_zone) %>%
  summarise(
    n_segments = n(),
    total_length_ft = sum(total_length_ft, na.rm = TRUE),
    
    # Pre-period crime
    shootings_pre = sum(shootings_pre),
    shots_fired_pre = sum(shots_fired_pre),
    gun_violence_pre = sum(gun_violence_pre),
    violent_crime_pre = sum(violent_crime_pre),
    property_crime_pre = sum(property_crime_pre),
    
    # Treatment period crime
    shootings_treatment = sum(shootings_treatment),
    shots_fired_treatment = sum(shots_fired_treatment),
    gun_violence_treatment = sum(gun_violence_treatment),
    
    # Zero-crime segments
    n_zero_shootings = sum(shootings_pre == 0),
    n_zero_gun_violence = sum(gun_violence_pre == 0),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Percentages
    pct_zero_shootings = 100 * n_zero_shootings / n_segments,
    pct_zero_gun_violence = 100 * n_zero_gun_violence / n_segments,
    
    # Area in square miles (length in ft -> rough area proxy)
    length_miles = total_length_ft / 5280,
    
    # Crime density (per mile of street)
    shootings_per_mile = shootings_pre / length_miles,
    gun_violence_per_mile = gun_violence_pre / length_miles
  )

message("Zone vs Non-Zone Comparison:")
zone_stats %>%
  select(in_zone, n_segments, shootings_pre, gun_violence_pre, 
         pct_zero_shootings, pct_zero_gun_violence,
         shootings_per_mile, gun_violence_per_mile) %>%
  kable(digits = 1)

# =============================================================================
# 3. "COLD DEPLOYMENT" ANALYSIS
# =============================================================================

message("\n=== COLD DEPLOYMENT ANALYSIS ===\n")

# Segments in zones with zero pre-period gun violence
cold_segments <- segments %>%
  filter(in_zone, gun_violence_pre == 0)

warm_segments <- segments %>%
  filter(in_zone, gun_violence_pre > 0)

message(sprintf("Zone segments with zero pre-period gun violence (2018-2022):"))
message(sprintf("  Cold segments: %d (%.1f%% of zone segments)",
                nrow(cold_segments),
                100 * nrow(cold_segments) / sum(segments$in_zone)))
message(sprintf("  Warm segments: %d (%.1f%% of zone segments)",
                nrow(warm_segments),
                100 * nrow(warm_segments) / sum(segments$in_zone)))

# Break down by gun violence level
zone_segments <- segments %>%
  filter(in_zone) %>%
  mutate(
    gv_category = case_when(
      gun_violence_pre == 0 ~ "0 (cold)",
      gun_violence_pre <= 2 ~ "1-2",
      gun_violence_pre <= 5 ~ "3-5",
      gun_violence_pre <= 10 ~ "6-10",
      TRUE ~ "11+"
    )
  )

message("\nZone segments by pre-period gun violence level:")
zone_segments %>%
  count(gv_category) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  kable(digits = 1, col.names = c("Gun Violence (2018-22)", "N Segments", "% of Zone"))

# =============================================================================
# 4. CONCENTRATION COMPARISON: ZONES VS OPTIMAL TARGETING
# =============================================================================

message("\n=== CONCENTRATION ANALYSIS ===\n")

# What share of citywide gun violence is captured by:
# (A) The actual zones
# (B) The same NUMBER of top segments (optimal targeting)
# (C) The same AREA of top segments

n_zone_segments <- sum(segments$in_zone)
zone_length_ft <- sum(segments$total_length_ft[segments$in_zone], na.rm = TRUE)

total_gv <- sum(segments$gun_violence_pre)
total_shootings <- sum(segments$shootings_pre)

# (A) Actual zone capture
zone_capture_gv <- sum(segments$gun_violence_pre[segments$in_zone])
zone_capture_shootings <- sum(segments$shootings_pre[segments$in_zone])

# (B) Optimal by same number of segments
optimal_by_count <- segments %>%
  arrange(desc(gun_violence_pre)) %>%
  head(n_zone_segments) %>%
  summarise(
    gun_violence = sum(gun_violence_pre),
    shootings = sum(shootings_pre)
  )

# (C) Optimal by same total length (area proxy)
segments_sorted <- segments %>%
  arrange(desc(gun_violence_pre)) %>%
  mutate(
    cum_length = cumsum(total_length_ft)
  )

optimal_by_area <- segments_sorted %>%
  filter(cum_length <= zone_length_ft) %>%
  summarise(
    n_segments = n(),
    gun_violence = sum(gun_violence_pre),
    shootings = sum(shootings_pre)
  )

# Build comparison table
concentration_comparison <- tibble(
  targeting = c("Actual Zones", 
                sprintf("Optimal (same # segments: %d)", n_zone_segments),
                sprintf("Optimal (same length: %.0f mi)", zone_length_ft/5280)),
  n_segments = c(n_zone_segments, n_zone_segments, optimal_by_area$n_segments),
  gun_violence = c(zone_capture_gv, optimal_by_count$gun_violence, optimal_by_area$gun_violence),
  shootings = c(zone_capture_shootings, optimal_by_count$shootings, optimal_by_area$shootings),
  pct_gv = c(
    100 * zone_capture_gv / total_gv,
    100 * optimal_by_count$gun_violence / total_gv,
    100 * optimal_by_area$gun_violence / total_gv
  ),
  pct_shootings = c(
    100 * zone_capture_shootings / total_shootings,
    100 * optimal_by_count$shootings / total_shootings,
    100 * optimal_by_area$shootings / total_shootings
  )
)

message("Concentration Comparison: Actual Zones vs Optimal Targeting")
concentration_comparison %>%
  kable(digits = 1, col.names = c("Targeting", "N Segments", "Gun Violence", 
                                   "Shootings", "% Citywide GV", "% Citywide Shootings"))

# Efficiency gap
efficiency_gap <- tibble(
  measure = c("Gun Violence", "Shootings"),
  zone_capture_pct = c(100 * zone_capture_gv / total_gv,
                       100 * zone_capture_shootings / total_shootings),
  optimal_capture_pct = c(100 * optimal_by_count$gun_violence / total_gv,
                          100 * optimal_by_count$shootings / total_shootings),
  gap_pct_points = optimal_capture_pct - zone_capture_pct,
  efficiency_ratio = zone_capture_pct / optimal_capture_pct
)

message("\nTargeting Efficiency Gap:")
efficiency_gap %>%
  kable(digits = 1, col.names = c("Measure", "Zone Capture %", "Optimal Capture %",
                                   "Gap (pp)", "Efficiency Ratio"))

# =============================================================================
# 5. MISSED HIGH-VIOLENCE SEGMENTS
# =============================================================================

message("\n=== MISSED HIGH-VIOLENCE SEGMENTS ===\n")

# Identify top segments NOT in zones
missed_segments <- segments %>%
  filter(!in_zone) %>%
  arrange(desc(gun_violence_pre)) %>%
  head(100) %>%
  select(segment_id, streets, boro, gun_violence_pre, shootings_pre, 
         rank_gun_violence, rank_shootings)

message("Top 20 High-Violence Segments OUTSIDE Zones:")
missed_segments %>%
  head(20) %>%
  kable(col.names = c("Segment ID", "Streets", "Boro", "Gun Violence", 
                       "Shootings", "GV Rank", "Shooting Rank"))

# Borough breakdown of missed high-violence segments
message("\nMissed Top-200 Gun Violence Segments by Borough:")
segments %>%
  filter(top_200_gun_violence, !in_zone) %>%
  count(boro) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  arrange(desc(n)) %>%
  kable(col.names = c("Borough", "N Missed", "% of Missed"))

# =============================================================================
# 6. COMPARISON GROUP BALANCE
# =============================================================================

message("\n=== TREATMENT-CONTROL BALANCE (Top Segments) ===\n")

# Compare in-zone vs out-of-zone among TOP segments
# This is our quasi-experimental comparison

for (n_top in c(100, 200, 500)) {
  
  rank_col <- "rank_gun_violence"
  
  balance_check <- segments %>%
    filter(.data[[rank_col]] <= n_top) %>%
    group_by(in_zone) %>%
    summarise(
      n = n(),
      
      # Pre-period outcomes
      mean_shootings_pre = mean(shootings_pre),
      mean_gv_pre = mean(gun_violence_pre),
      mean_violent_pre = mean(violent_crime_pre),
      mean_property_pre = mean(property_crime_pre),
      
      # Segment characteristics
      mean_length = mean(total_length_ft, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(threshold = n_top)
  
  message(sprintf("\nTop %d Gun Violence Segments:", n_top))
  balance_check %>%
    select(-threshold) %>%
    kable(digits = 2)
}

# =============================================================================
# 7. SAVE RESULTS
# =============================================================================

message("\n=== Saving Results ===\n")

# Create output directory
dir.create(here("output"), showWarnings = FALSE)

# Save tables
write_csv(coverage_results, here("output", "top_n_coverage.csv"))
write_csv(zone_stats, here("output", "zone_stats.csv"))
write_csv(concentration_comparison, here("output", "concentration_comparison.csv"))
write_csv(efficiency_gap, here("output", "efficiency_gap.csv"))
write_csv(missed_segments, here("output", "missed_high_violence_segments.csv"))

# Save R objects
save(
  coverage_results,
  zone_stats,
  concentration_comparison,
  efficiency_gap,
  missed_segments,
  file = here("output", "targeting_efficiency_results.RData")
)

message("Done! Results saved to output/")

# =============================================================================
# 8. SUMMARY STATISTICS FOR PAPER
# =============================================================================

message("\n" , strrep("=", 60))
message("KEY FINDINGS FOR PAPER")
message(strrep("=", 60))

message(sprintf("\n1. ZONE COVERAGE"))
message(sprintf("   - %d summer zones covering %d street segments (%.1f%% of city)",
                n_distinct(segments$zone_id[segments$in_zone]),
                n_zone_segments,
                100 * n_zone_segments / nrow(segments)))

message(sprintf("\n2. TOP-100 SEGMENT COVERAGE (Gun Violence)"))
t100 <- coverage_gun_violence %>% filter(threshold == 100)
message(sprintf("   - %d of top 100 in zones (%.0f%%)",
                t100$n_in_zone, t100$pct_in_zone))
message(sprintf("   - %d of top 100 OUTSIDE zones (%.0f%%)",
                t100$n_out_zone, 100 - t100$pct_in_zone))

message(sprintf("\n3. COLD DEPLOYMENT"))
message(sprintf("   - %.1f%% of zone segments had ZERO gun violence in pre-period",
                100 * nrow(cold_segments) / sum(segments$in_zone)))

message(sprintf("\n4. TARGETING EFFICIENCY"))
message(sprintf("   - Zones capture %.1f%% of citywide pre-period gun violence",
                100 * zone_capture_gv / total_gv))
message(sprintf("   - Optimal targeting (same # segments) would capture %.1f%%",
                100 * optimal_by_count$gun_violence / total_gv))
message(sprintf("   - Efficiency gap: %.1f percentage points",
                efficiency_gap$gap_pct_points[1]))
message(sprintf("   - Zones are %.0f%% as efficient as optimal",
                100 * efficiency_gap$efficiency_ratio[1]))
