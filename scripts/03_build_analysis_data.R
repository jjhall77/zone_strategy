# =============================================================================
# 03_build_analysis_data.R
# Build segment-level analysis dataset
# - Aggregate crime counts by segment and period
# - Assign zone treatment status
# - Create gun violence rankings
# =============================================================================

library(here)
library(tidyverse)
library(sf)

# Load data
load(here("data", "loaded_data.RData"))
load(here("data", "processed_events.RData"))

# =============================================================================
# 1. AGGREGATE TO SEGMENT-PERIOD LEVEL
# =============================================================================

message("Aggregating crime counts by segment...")

# --- Shootings ---
shooting_counts <- shootings_processed %>%
  filter(snapped) %>%  # Only successfully snapped
  group_by(segment_id, period) %>%
  summarise(
    shootings = sum(weight, na.rm = TRUE),
    shooting_victims = sum(weight * as.numeric(!is.na(vic_age_group)), na.rm = TRUE),
    shooting_murders = sum(weight * (statistical_murder_flag == "TRUE"), na.rm = TRUE),
    .groups = "drop"
  )

# --- Shots Fired ---
shots_fired_counts <- shots_fired_processed %>%
  filter(snapped) %>%
  group_by(segment_id, period) %>%
  summarise(
    shots_fired = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# --- Violent Crime ---
violent_counts <- violent_processed %>%
  filter(snapped) %>%
  group_by(segment_id, period) %>%
  summarise(
    violent_crime = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# --- Property Crime ---
property_counts <- property_processed %>%
  filter(snapped) %>%
  group_by(segment_id, period) %>%
  summarise(
    property_crime = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 2. CREATE SEGMENT-LEVEL DATASET
# =============================================================================

message("Building segment-level dataset...")

# Start with all physical blocks
segments <- physical_blocks %>%
  st_drop_geometry() %>%
  select(physical_id, n_segments, total_length_ft, streets, boro, cd, ct2020) %>%
  rename(segment_id = physical_id)

# Pivot and join crime counts
pivot_and_join <- function(counts_df, value_cols, segments_df) {
  
  # Pivot to wide format (one column per period)
  wide <- counts_df %>%
    pivot_wider(
      id_cols = segment_id,
      names_from = period,
      values_from = all_of(value_cols),
      values_fill = 0
    )
  
  # Join to segments
  segments_df %>%
    left_join(wide, by = "segment_id")
}

# Join all crime types
segments <- segments %>%
  pivot_and_join(shooting_counts, c("shootings", "shooting_victims", "shooting_murders"), .) %>%
  pivot_and_join(shots_fired_counts, "shots_fired", .) %>%
  pivot_and_join(violent_counts, "violent_crime", .) %>%
  pivot_and_join(property_counts, "property_crime", .)

# Replace NAs with 0
segments <- segments %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# =============================================================================
# 3. CREATE GUN VIOLENCE MEASURES
# =============================================================================

message("Creating gun violence measures...")

# Gun violence = shootings + shots fired
# Calculate for each period

# Get column names by period
shooting_cols <- names(segments)[str_detect(names(segments), "^shootings_")]
shots_cols <- names(segments)[str_detect(names(segments), "^shots_fired_")]

# Pre-period gun violence (2018-2022)
segments <- segments %>%
  mutate(
    # Pre-period (for ranking)
    shootings_pre = shootings_pre,
    shots_fired_pre = shots_fired_pre,
    gun_violence_pre = shootings_pre + shots_fired_pre,
    
    # Treatment period
    shootings_treatment = shootings_treatment,
    shots_fired_treatment = shots_fired_treatment,
    gun_violence_treatment = shootings_treatment + shots_fired_treatment,
    
    # Post period (if exists)
    shootings_post = if("shootings_post" %in% names(.)) shootings_post else 0,
    shots_fired_post = if("shots_fired_post" %in% names(.)) shots_fired_post else 0,
    gun_violence_post = shootings_post + shots_fired_post
  )

# =============================================================================
# 4. ASSIGN ZONE TREATMENT STATUS
# =============================================================================

message("Assigning zone treatment status...")
 
# Need geometry for spatial join
segments_sf <- physical_blocks %>%
  select(physical_id) %>%
  rename(segment_id = physical_id) %>%
  left_join(segments, by = "segment_id")

# Spatial join to summer zones
# Use centroid for assignment (or could use majority overlap)
segment_centroids <- segments_sf %>%
  st_centroid()

zone_join <- st_join(
  segment_centroids,
  summer_zones %>% select(zone_id),
  join = st_within,
  left = TRUE
)

# Extract zone assignment
zone_assignment <- zone_join %>%
  st_drop_geometry() %>%
  select(segment_id, zone_id) %>%
  mutate(in_zone = !is.na(zone_id))

# Add to segments
segments <- segments %>%
  left_join(zone_assignment, by = "segment_id") %>%
  mutate(in_zone = replace_na(in_zone, FALSE))

message(sprintf("  Segments in zones: %d (%.1f%%)",
                sum(segments$in_zone),
                100 * mean(segments$in_zone)))

# =============================================================================
# 5. CREATE RANKINGS
# =============================================================================

message("Creating gun violence rankings...")

# Rank by pre-period gun violence (descending)
segments <- segments %>%
  mutate(
    # Shooting rank
    rank_shootings = rank(-shootings_pre, ties.method = "min"),
    
    # Gun violence rank (shootings + shots fired)
    rank_gun_violence = rank(-gun_violence_pre, ties.method = "min"),
    
    # Violent crime rank
    rank_violent = rank(-violent_crime_pre, ties.method = "min")
  )

# Flag top segments
segments <- segments %>%
  mutate(
    top_100_shootings = rank_shootings <= 100,
    top_200_shootings = rank_shootings <= 200,
    top_500_shootings = rank_shootings <= 500,
    
    top_100_gun_violence = rank_gun_violence <= 100,
    top_200_gun_violence = rank_gun_violence <= 200,
    top_500_gun_violence = rank_gun_violence <= 500
  )

# =============================================================================
# 6. SUMMARY STATISTICS
# =============================================================================

message("\n=== Segment Summary ===")

# Zone coverage
zone_summary <- segments %>%
  summarise(
    n_segments = n(),
    n_in_zone = sum(in_zone),
    pct_in_zone = 100 * mean(in_zone),
    
    # Pre-period shootings
    total_shootings_pre = sum(shootings_pre),
    shootings_in_zone = sum(shootings_pre * in_zone),
    pct_shootings_in_zone = 100 * sum(shootings_pre * in_zone) / sum(shootings_pre),
    
    # Pre-period gun violence
    total_gun_violence_pre = sum(gun_violence_pre),
    gun_violence_in_zone = sum(gun_violence_pre * in_zone),
    pct_gun_violence_in_zone = 100 * sum(gun_violence_pre * in_zone) / sum(gun_violence_pre)
  )

print(zone_summary)

# Top segment coverage
top_coverage <- segments %>%
  summarise(
    # Top 100
    top_100_in_zone_shootings = sum(top_100_shootings & in_zone),
    top_100_out_zone_shootings = sum(top_100_shootings & !in_zone),
    
    top_100_in_zone_gv = sum(top_100_gun_violence & in_zone),
    top_100_out_zone_gv = sum(top_100_gun_violence & !in_zone),
    
    # Top 200
    top_200_in_zone_shootings = sum(top_200_shootings & in_zone),
    top_200_out_zone_shootings = sum(top_200_shootings & !in_zone),
    
    top_200_in_zone_gv = sum(top_200_gun_violence & in_zone),
    top_200_out_zone_gv = sum(top_200_gun_violence & !in_zone)
  )

message("\nTop Segment Coverage (Shootings):")
message(sprintf("  Top 100: %d in zone, %d out of zone",
                top_coverage$top_100_in_zone_shootings,
                top_coverage$top_100_out_zone_shootings))
message(sprintf("  Top 200: %d in zone, %d out of zone",
                top_coverage$top_200_in_zone_shootings,
                top_coverage$top_200_out_zone_shootings))

message("\nTop Segment Coverage (Gun Violence):")
message(sprintf("  Top 100: %d in zone, %d out of zone",
                top_coverage$top_100_in_zone_gv,
                top_coverage$top_100_out_zone_gv))
message(sprintf("  Top 200: %d in zone, %d out of zone",
                top_coverage$top_200_in_zone_gv,
                top_coverage$top_200_out_zone_gv))

# =============================================================================
# 7. SAVE ANALYSIS DATASET
# =============================================================================

message("\nSaving analysis dataset...")

# Save non-spatial version
write_csv(segments, here("data", "segment_analysis_data.csv"))

# Save spatial version
segments_sf_final <- physical_blocks %>%
  select(physical_id) %>%
  rename(segment_id = physical_id) %>%
  left_join(segments, by = "segment_id")

st_write(segments_sf_final, here("data", "segment_analysis_data.gpkg"), 
         delete_dsn = TRUE)

# Save R objects
save(
  segments,
  segments_sf_final,
  zone_summary,
  top_coverage,
  file = here("data", "analysis_data.RData")
)

message("Done building analysis dataset!")
