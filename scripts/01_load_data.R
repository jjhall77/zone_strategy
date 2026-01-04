# =============================================================================
# 01_load_data.R
# NYPD Summer Zone Strategy Evaluation
# Clean data loading script
# =============================================================================

library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(readxl)

# Set coordinate reference system for NYC
NYC_CRS <- 2263  # NAD83 / New York Long Island (ftUS)

# =============================================================================
# KEY DATES
# =============================================================================

INITIATIVE_START <- ymd("2023-05-04")
INITIATIVE_END   <- ymd("2023-09-18")

# Pre-period for ranking: use 2018-2022 (5 years before initiative)
PRE_PERIOD_START <- ymd("2018-01-01")
PRE_PERIOD_END   <- ymd("2022-12-31")

# =============================================================================
# 1. SPATIAL LAYERS
# =============================================================================

message("Loading spatial layers...")

# Census blocks (2020)
nycb <- st_read(here("data", "nycb2020_25b"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(NYC_CRS) %>%
  clean_names()

# Neighborhood Tabulation Areas (2020)
nynta <- st_read(here("data", "nynta2020_25b"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(NYC_CRS) %>%
  clean_names()

# NYPD sectors
nypd_sectors <- st_read(here("data", "nypd_sectors"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(NYC_CRS) %>%
  clean_names()

# Summer violence zones (the treatment)
summer_zones <- st_read(here("data", "Summer_Zones_Shapefile_Final_GCS"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_set_crs(4326) %>%
  st_transform(NYC_CRS) %>%
  clean_names() %>%
  rename(zone_id = zone)

message(sprintf("  Loaded %d summer zones", nrow(summer_zones)))

# Physical blocks (street segments)
physical_blocks <- st_read(here("data", "physical_blocks.gpkg"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(NYC_CRS) %>%
  clean_names()

message(sprintf("  Loaded %d physical blocks (street segments)", nrow(physical_blocks)))

# =============================================================================
# 2. LION NETWORK (for intersection identification)
# =============================================================================

message("Loading LION street network...")

lion_gdb <- here("data", "lion", "lion.gdb")

lion <- st_read(lion_gdb, layer = "lion", quiet = TRUE) %>%
  st_transform(NYC_CRS) %>%
  clean_names()

# Filter to city streets
lion_streets <- lion %>%
  filter(
    feature_typ %in% c("0", "6"),
    status == "2",
    segment_typ == "U",
    traf_dir %in% c("T", "A", "W"),
    !rw_type %in% c("2", "3", "4", "9")
  )

# Load nodes for intersection identification
nodes <- st_read(lion_gdb, layer = "node", quiet = TRUE) %>%
  st_transform(NYC_CRS) %>%
  clean_names() %>%
  mutate(nodeid = as.integer(nodeid))

# Count street names per node to identify intersections
node_street_counts <- lion_streets %>%
  st_drop_geometry() %>%
  select(node_id_from, node_id_to, street) %>%
  pivot_longer(cols = c(node_id_from, node_id_to), values_to = "nodeid") %>%
  filter(!is.na(nodeid)) %>%
  distinct(nodeid, street) %>%
  count(nodeid, name = "n_streets") %>%
  mutate(nodeid = as.integer(nodeid))

# Intersections = nodes with 2+ street names
intersection_nodes <- nodes %>%
  inner_join(node_street_counts %>% filter(n_streets >= 2), by = "nodeid")

# Create intersection buffers (25ft radius)
intersection_buffers <- intersection_nodes %>%
  st_buffer(dist = 25) %>%
  select(nodeid, n_streets)

message(sprintf("  Identified %d intersections", nrow(intersection_nodes)))

# =============================================================================
# 3. SHOOTING DATA
# =============================================================================

message("Loading shooting data...")

shooting_historic <- read_csv(
  here("data", "NYPD_Shooting_Incident_Data_(Historic)_20251117.csv"),
  show_col_types = FALSE
) %>% 
  clean_names() %>%
  mutate(statistical_murder_flag = as.character(statistical_murder_flag))

shooting_ytd <- read_csv(
  here("data", "NYPD_Shooting_Incident_Data_(Year_To_Date)_20251117.csv"),
  show_col_types = FALSE
) %>% 
  clean_names() %>%
  mutate(statistical_murder_flag = as.character(statistical_murder_flag))

shootings <- bind_rows(shooting_historic, shooting_ytd) %>%
  mutate(occur_date = mdy(occur_date)) %>%
  distinct(incident_key, .keep_all = TRUE)

# Convert to sf
shootings_sf <- shootings %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
  st_as_sf(
    coords = c("x_coord_cd", "y_coord_cd"),
    crs = NYC_CRS,
    remove = FALSE
  ) %>%
  mutate(
    year = year(occur_date),
    month = floor_date(occur_date, "month"),
    event_type = "shooting"
  )

message(sprintf("  Loaded %d shooting incidents (%s to %s)", 
                nrow(shootings_sf),
                min(shootings_sf$occur_date),
                max(shootings_sf$occur_date)))

# =============================================================================
# 4. SHOTS FIRED DATA
# =============================================================================

message("Loading shots fired data...")

# Older shots fired data (through Sept 2022)
sf2017 <- read_csv(here("data", "sf_since_2017.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    date = mdy(cmplnt_fr_dt),
    source = "sf2017"
  ) %>%
  filter(!is.na(date), date <= ymd("2022-09-30"))

# Newer shots fired data (Oct 2022 onward)
shots_fired_new <- read_csv(here("data", "shots_fired_new.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    date = mdy(cmplnt_fr_dt),
    source = "shots_fired_new"
  ) %>%
  rename(
    pct = cmplnt_pct_cd,
    x_coord_cd = x_coordinate_code,
    y_coord_cd = y_coordinate_code
  ) %>%
  filter(!is.na(date), date >= ymd("2022-10-01"))

# Combine shots fired datasets
shots_fired <- bind_rows(sf2017, shots_fired_new) %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd))

# Convert to sf
shots_fired_sf <- shots_fired %>%
  st_as_sf(
    coords = c("x_coord_cd", "y_coord_cd"),
    crs = NYC_CRS,
    remove = FALSE
  ) %>%
  mutate(
    year = year(date),
    month = floor_date(date, "month"),
    event_type = "shots_fired"
  ) %>%
  rename(occur_date = date)

message(sprintf("  Loaded %d shots fired incidents (%s to %s)",
                nrow(shots_fired_sf),
                min(shots_fired_sf$occur_date),
                max(shots_fired_sf$occur_date)))

# =============================================================================
# 5. COMPLAINT DATA (for violent/property crime)
# =============================================================================

message("Loading complaint data...")

csb <- read_csv(
  here("data", "NYPD_Complaint_Data_Historic_20251117 (1).csv"),
  show_col_types = FALSE
) %>% 
  clean_names() %>%
  mutate(
    rpt_dt = mdy(rpt_dt),
    cmplnt_fr_dt = mdy(cmplnt_fr_dt)
  )

# Define crime categories by ky_cd
VIOLENT_CRIME_CODES <- c(101, 105, 106)   # Murder, Robbery, Felony Assault
PROPERTY_CRIME_CODES <- c(107, 109, 110)  # Burglary, Grand Larceny, GLA

csb_sf <- csb %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
  mutate(
    crime_category = case_when(
      ky_cd %in% VIOLENT_CRIME_CODES ~ "violent",
      ky_cd %in% PROPERTY_CRIME_CODES ~ "property",
      TRUE ~ "other"
    )
  ) %>%
  st_as_sf(
    coords = c("x_coord_cd", "y_coord_cd"),
    crs = NYC_CRS,
    remove = FALSE
  ) %>%
  mutate(
    year = year(cmplnt_fr_dt),
    month = floor_date(cmplnt_fr_dt, "month"),
    occur_date = cmplnt_fr_dt
  )

message(sprintf("  Loaded %d complaints", nrow(csb_sf)))

# =============================================================================
# 6. GIVE JURISDICTION DATA (for external comparison)
# =============================================================================

message("Loading GIVE jurisdiction shooting data...")

shoot_dir <- here("data", "shooting_data")
files <- list.files(shoot_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Helper function to parse month-year strings
to_month_date <- function(x) {
  x <- as.character(x)
  x <- str_replace(x, "^x", "")
  x <- str_replace_all(x, "_", " ")
  x <- str_squish(x)
  d1 <- suppressWarnings(my(x))
  d2 <- suppressWarnings(ym(x))
  out <- coalesce(as_date(d1), as_date(d2))
  floor_date(out, "month")
}

load_one_agency <- function(path) {
  agency_name <- tools::file_path_sans_ext(basename(path))
  df <- read_excel(path) %>% clean_names()
  
  df %>%
    pivot_longer(
      cols = -1,
      names_to = "period",
      values_to = "count"
    ) %>%
    rename(measure = 1) %>%
    mutate(
      agency = agency_name,
      month_date = to_month_date(period)
    ) %>%
    select(agency, measure, month_date, count)
}

give_shootings <- map_dfr(files, load_one_agency) %>%
  filter(measure == "Shooting Incidents Involving Injury") %>%
  filter(!is.na(month_date))

message(sprintf("  Loaded GIVE data for %d agencies (%s to %s)",
                n_distinct(give_shootings$agency),
                min(give_shootings$month_date),
                max(give_shootings$month_date)))

# =============================================================================
# 7. PRECINCT LOCATIONS
# =============================================================================

nypd_precinct_locations <- read_csv(
  here("data", "nypd_precinct_locations.csv"),
  show_col_types = FALSE
) %>% 
  clean_names() %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(NYC_CRS)

# =============================================================================
# 8. SAVE LOADED DATA
# =============================================================================

message("Saving loaded data to RDS...")

save(
  # Key dates
  INITIATIVE_START, INITIATIVE_END,
  PRE_PERIOD_START, PRE_PERIOD_END,
  NYC_CRS,
  
  # Spatial layers
  nycb, nynta, nypd_sectors, summer_zones, physical_blocks,
  lion_streets, intersection_nodes, intersection_buffers,
  
  # Crime data
  shootings_sf, shots_fired_sf, csb_sf,
  
  # External comparison data
  give_shootings,
  
  # Other
  nypd_precinct_locations,
  
  # Crime category codes
  VIOLENT_CRIME_CODES, PROPERTY_CRIME_CODES,
  
  file = here("data", "loaded_data.RData")
)

message("Done loading data!")
message(sprintf("  Summer zones: %d", nrow(summer_zones)))
message(sprintf("  Physical blocks: %d", nrow(physical_blocks)))
message(sprintf("  Shootings: %d", nrow(shootings_sf)))
message(sprintf("  Shots fired: %d", nrow(shots_fired_sf)))
message(sprintf("  Complaints: %d", nrow(csb_sf)))
