# =============================================================================
# 02_snap_to_segments.R
# Snap crime events to physical blocks (street segments)
# Handle intersections by splitting events across adjacent segments
# =============================================================================

library(here)
library(tidyverse)
library(sf)
library(nngeo)  # for st_nn (nearest neighbor)

# Load processed data
load(here("data", "loaded_data.RData"))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Snap points to nearest line segments
#' Returns the original points with segment ID attached
snap_to_segments <- function(points_sf, segments_sf, segment_id_col = "physical_id", 
                             max_dist = 500) {
  
  message(sprintf("  Snapping %d points to segments...", nrow(points_sf)))
  
  # Find nearest segment for each point
  nearest <- st_nearest_feature(points_sf, segments_sf)
  
  # Calculate distances
  distances <- st_distance(points_sf, segments_sf[nearest, ], by_element = TRUE)
  distances <- as.numeric(distances)
  
  # Attach segment info
  points_sf$segment_id <- segments_sf[[segment_id_col]][nearest]
  points_sf$snap_dist <- distances
  
  # Flag points too far from any segment
  points_sf$snapped <- distances <= max_dist
  
  n_snapped <- sum(points_sf$snapped)
  n_failed <- sum(!points_sf$snapped)
  
  message(sprintf("  Snapped: %d (%.1f%%), Too far: %d", 
                  n_snapped, 100 * n_snapped / nrow(points_sf), n_failed))
  
  return(points_sf)
}

#' Check if points are at intersections
#' Returns points with intersection info
flag_intersections <- function(points_sf, intersection_buffers) {
  
  message("  Flagging intersection points...")
  
  # Spatial join to intersection buffers
  int_join <- st_join(
    points_sf,
    intersection_buffers %>% select(nodeid, n_streets),
    join = st_within,
    left = TRUE
  )
  
  # Flag as intersection if within buffer
  int_join$at_intersection <- !is.na(int_join$nodeid)
  
  n_int <- sum(int_join$at_intersection)
  message(sprintf("  Points at intersections: %d (%.1f%%)", 
                  n_int, 100 * n_int / nrow(int_join)))
  
  return(int_join)
}

#' Get adjacent segments for intersection nodes
#' Returns a lookup table: nodeid -> list of segment IDs
get_intersection_segments <- function(lion_streets, segments_sf, 
                                       segment_id_col = "physical_id") {
  
  message("  Building intersection-segment lookup...")
  

  # Get segment centroids
  segment_centroids <- segments_sf %>%
    st_centroid()
  
  # For each segment, find which nodes are nearby (within 50 ft of endpoints)
  # This is a simplified approach - assumes physical_blocks align with LION
  
  # Get endpoints of each segment
  segment_endpoints <- segments_sf %>%
    st_cast("POINT") %>%
    mutate(point_id = row_number())
  
  # Actually, let's use a buffer approach:
  # For each intersection node, find segments within small buffer
  
  node_segment_lookup <- list()
  
  # Use intersection_nodes (already have the geometry)
  for (i in seq_len(nrow(intersection_nodes))) {
    node <- intersection_nodes[i, ]
    node_buffer <- st_buffer(node, dist = 50)  # 50 ft buffer
    
    # Find segments that intersect this buffer
    intersecting <- st_intersects(node_buffer, segments_sf)[[1]]
    
    if (length(intersecting) > 0) {
      node_segment_lookup[[as.character(node$nodeid)]] <- 
        segments_sf[[segment_id_col]][intersecting]
    }
  }
  
  return(node_segment_lookup)
}

#' Process crime events: snap to segments and handle intersections
#' Returns segment-level counts
process_events <- function(events_sf, segments_sf, intersection_buffers,
                           node_segment_lookup, segment_id_col = "physical_id",
                           date_col = "occur_date") {
  
  # Step 1: Snap to nearest segment
  events_snapped <- snap_to_segments(events_sf, segments_sf, segment_id_col)
  

  # Step 2: Flag intersections
  events_flagged <- flag_intersections(events_snapped, intersection_buffers)
  
  # Step 3: Split intersection events
  message("  Splitting intersection events...")
  
  # Non-intersection events: weight = 1
  non_int_events <- events_flagged %>%
    filter(!at_intersection | is.na(nodeid)) %>%
    st_drop_geometry() %>%
    mutate(weight = 1.0)
  
  # Intersection events: weight = 1 / n_adjacent_segments
  int_events <- events_flagged %>%
    filter(at_intersection & !is.na(nodeid)) %>%
    st_drop_geometry()
  
  if (nrow(int_events) > 0) {
    # Expand intersection events to all adjacent segments
    int_expanded <- int_events %>%
      rowwise() %>%
      mutate(
        adjacent_segments = list(
          node_segment_lookup[[as.character(nodeid)]]
        )
      ) %>%
      ungroup() %>%
      unnest(adjacent_segments, keep_empty = TRUE) %>%
      group_by(across(c(-adjacent_segments, -segment_id))) %>%
      mutate(
        n_adjacent = n(),
        weight = 1 / n_adjacent,
        segment_id = coalesce(adjacent_segments, segment_id)
      ) %>%
      ungroup() %>%
      select(-adjacent_segments, -n_adjacent)
    
    # Combine
    all_events <- bind_rows(non_int_events, int_expanded)
  } else {
    all_events <- non_int_events
  }
  
  message(sprintf("  Final event records: %d (original: %d)",
                  nrow(all_events), nrow(events_sf)))
  
  return(all_events)
}

# =============================================================================
# BUILD INTERSECTION-SEGMENT LOOKUP
# =============================================================================

message("Building intersection-segment lookup table...")

# Simplified approach: for each intersection buffer, find overlapping segments
int_segment_list <- vector("list", nrow(intersection_buffers))
names(int_segment_list) <- as.character(intersection_buffers$nodeid)

# This can be slow - do in batches
pb <- txtProgressBar(min = 0, max = nrow(intersection_buffers), style = 3)

for (i in seq_len(nrow(intersection_buffers))) {
  buf <- intersection_buffers[i, ]
  
  # Find segments that intersect this buffer
  hits <- st_intersects(buf, physical_blocks, sparse = TRUE)[[1]]
  
  if (length(hits) > 0) {
    int_segment_list[[i]] <- physical_blocks$physical_id[hits]
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

# Remove empty entries
int_segment_lookup <- int_segment_list[!sapply(int_segment_list, is.null)]

message(sprintf("Built lookup for %d intersections", length(int_segment_lookup)))

# Save lookup for reuse
saveRDS(int_segment_lookup, here("data", "intersection_segment_lookup.rds"))

# =============================================================================
# PROCESS SHOOTINGS
# =============================================================================

message("\n=== Processing Shootings ===")

shootings_processed <- process_events(
  events_sf = shootings_sf,
  segments_sf = physical_blocks,
  intersection_buffers = intersection_buffers,
  node_segment_lookup = int_segment_lookup,
  segment_id_col = "physical_id",
  date_col = "occur_date"
)

# Add period flags
shootings_processed <- shootings_processed %>%
  mutate(
    period = case_when(
      occur_date >= PRE_PERIOD_START & occur_date <= PRE_PERIOD_END ~ "pre",
      occur_date >= INITIATIVE_START & occur_date <= INITIATIVE_END ~ "treatment",
      occur_date > INITIATIVE_END ~ "post",
      TRUE ~ "other"
    )
  )

# =============================================================================
# PROCESS SHOTS FIRED
# =============================================================================

message("\n=== Processing Shots Fired ===")

shots_fired_processed <- process_events(
  events_sf = shots_fired_sf,
  segments_sf = physical_blocks,
  intersection_buffers = intersection_buffers,
  node_segment_lookup = int_segment_lookup,
  segment_id_col = "physical_id",
  date_col = "occur_date"
)

shots_fired_processed <- shots_fired_processed %>%
  mutate(
    period = case_when(
      occur_date >= PRE_PERIOD_START & occur_date <= PRE_PERIOD_END ~ "pre",
      occur_date >= INITIATIVE_START & occur_date <= INITIATIVE_END ~ "treatment",
      occur_date > INITIATIVE_END ~ "post",
      TRUE ~ "other"
    )
  )

# =============================================================================
# PROCESS VIOLENT CRIME
# =============================================================================

message("\n=== Processing Violent Crime ===")

violent_crime_sf <- csb_sf %>%
  filter(crime_category == "violent")

violent_processed <- process_events(
  events_sf = violent_crime_sf,
  segments_sf = physical_blocks,
  intersection_buffers = intersection_buffers,
  node_segment_lookup = int_segment_lookup,
  segment_id_col = "physical_id",
  date_col = "occur_date"
)

violent_processed <- violent_processed %>%
  mutate(
    period = case_when(
      occur_date >= PRE_PERIOD_START & occur_date <= PRE_PERIOD_END ~ "pre",
      occur_date >= INITIATIVE_START & occur_date <= INITIATIVE_END ~ "treatment",
      occur_date > INITIATIVE_END ~ "post",
      TRUE ~ "other"
    )
  )

# =============================================================================
# PROCESS PROPERTY CRIME
# =============================================================================

message("\n=== Processing Property Crime ===")

property_crime_sf <- csb_sf %>%
  filter(crime_category == "property")

property_processed <- process_events(
  events_sf = property_crime_sf,
  segments_sf = physical_blocks,
  intersection_buffers = intersection_buffers,
  node_segment_lookup = int_segment_lookup,
  segment_id_col = "physical_id",
  date_col = "occur_date"
)

property_processed <- property_processed %>%
  mutate(
    period = case_when(
      occur_date >= PRE_PERIOD_START & occur_date <= PRE_PERIOD_END ~ "pre",
      occur_date >= INITIATIVE_START & occur_date <= INITIATIVE_END ~ "treatment",
      occur_date > INITIATIVE_END ~ "post",
      TRUE ~ "other"
    )
  )

# =============================================================================
# SAVE PROCESSED DATA
# =============================================================================

message("\nSaving processed event data...")

save(
  shootings_processed,
  shots_fired_processed,
  violent_processed,
  property_processed,
  int_segment_lookup,
  file = here("data", "processed_events.RData")
)

message("Done processing events!")

# Quick summary
message("\n=== Summary ===")
message(sprintf("Shootings: %d events -> %d segment-weighted records",
                nrow(shootings_sf), nrow(shootings_processed)))
message(sprintf("Shots fired: %d events -> %d segment-weighted records",
                nrow(shots_fired_sf), nrow(shots_fired_processed)))
message(sprintf("Violent crime: %d events -> %d segment-weighted records",
                nrow(violent_crime_sf), nrow(violent_processed)))
message(sprintf("Property crime: %d events -> %d segment-weighted records",
                nrow(property_crime_sf), nrow(property_processed)))
