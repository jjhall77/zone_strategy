library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(sf)

list.files(here("data"))

# ── Load spatial + shooting + complaint files (zone_strategy.Rproj) ────────────


# ── 1) Spatial layers (shapefiles / folders) ──────────────────────────────────
# Each of these entries is a folder containing a shapefile set
nycb <- st_read(dsn = here("data", "nycb2020_25b"), quiet = TRUE) %>%
  st_make_valid()%>%
  st_transform(2263)

nynta <- st_read(dsn = here("data", "nynta2020_25b"), quiet = TRUE) %>%
  st_make_valid()%>%
  st_transform(2263)

nypd_sectors <- st_read(dsn = here("data", "nypd_sectors"), quiet = TRUE) %>%
  st_make_valid()

summer_zones <- st_read(dsn = here("data", "Summer_Zones_Shapefile_Final_GCS"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_set_crs(4326) %>%
  st_transform(2263)

# ── 2) Physical blocks geopackage ─────────────────────────────────────────────
physical_blocks <- st_read(dsn = here("data", "physical_blocks.gpkg"), quiet = TRUE) %>%
  st_make_valid()

# ── 3) Shooting incident data (load + row-bind) ───────────────────────────────
shooting_historic <- read_csv(
  here("data", "NYPD_Shooting_Incident_Data_(Historic)_20251117.csv"),
  show_col_types = FALSE
) %>% clean_names()%>%
  mutate(statistical_murder_flag = as.character(statistical_murder_flag))

shooting_ytd <- read_csv(
  here("data", "NYPD_Shooting_Incident_Data_(Year_To_Date)_20251117.csv"),
  show_col_types = FALSE
) %>% clean_names() %>%
  mutate(statistical_murder_flag = as.character(statistical_murder_flag))

shootings <- bind_rows(shooting_historic, shooting_ytd) %>%
  mutate(occur_date = mdy(occur_date)) 

shootings_sf <- shootings %>%
  distinct(incident_key, .keep_all = T) %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
  st_as_sf(
    coords = c("x_coord_cd", "y_coord_cd"),
    crs = 2263,     # NY State Plane (US feet) – this is what NYPD uses
    remove = FALSE
  )


# ── 4) Complaint data (load only; name it csb) ────────────────────────────────
csb <- read_csv(
  here("data", "NYPD_Complaint_Data_Historic_20251117 (1).csv")
) %>% clean_names() %>%
  mutate(rpt_dt = mdy(rpt_dt))

csb_sf <- csb %>%
  filter(
    !is.na(x_coord_cd),
    !is.na(y_coord_cd)
  ) %>%
  st_as_sf(
    coords = c("x_coord_cd", "y_coord_cd"),
    crs = 2263,      # NY State Plane (NAD83 / Long Island)
    remove = FALSE
  )

# ── 5) Precinct locations (load only) ─────────────────────────────────────────
nypd_precinct_locations <- read_csv(
  here("data", "nypd_precinct_locations.csv"),
  show_col_types = FALSE
) %>% clean_names()

nypd_precinct_locations_sf <- nypd_precinct_locations %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,      # WGS84 (lon/lat)
    remove = FALSE  # keep original columns
  ) %>%
  st_transform(2263)


#shots fired
shots_fired <- read_csv(here("data","shots_fired_new.csv")) %>%
  clean_names()

shots_fired <- shots_fired %>%
  mutate(date = mdy(cmplnt_fr_dt))
min(shots_fired$date)
max(shots_fired$date)

sf2017 <- read_csv(here("data","sf_since_2017.csv")) %>%
  clean_names()

sf2017 <- sf2017 %>%
  mutate(date = mdy(cmplnt_fr_dt))
max(sf2017$date, na.rm = T)

cutoff_end   <- ymd("2022-09-30")
cutoff_start <- ymd("2022-10-01")

# 1) enforce dates + filter windows
sf2017_trim <- sf2017 %>%
  mutate(date = mdy(cmplnt_fr_dt)) %>%
  filter(!is.na(date), date <= cutoff_end) %>%
  mutate(source = "sf2017")

shots_fired_trim <- shots_fired %>%
  mutate(date = mdy(cmplnt_fr_dt)) %>%  # you already did this, but safe
  filter(!is.na(date), date >= cutoff_start) %>%
  mutate(source = "shots_fired_new")

# 2) make a harmonized schema
# - shots_fired_new uses x_coordinate_code / y_coordinate_code
# - sf2017 uses x_coord_cd / y_coord_cd
shots_fired_trim <- shots_fired_trim %>%
  rename(
    pct        = cmplnt_pct_cd,
    x_coord_cd = x_coordinate_code,
    y_coord_cd = y_coordinate_code
  )

# OPTIONAL: if you want a shared "pd_desc" field from shots_fired_new
# (sf2017 has pd_desc; shots has rpt_classfctn_desc; keep both if you want)
# shots_fired_trim <- shots_fired_trim %>%
#   mutate(pd_desc = rpt_classfctn_desc)

# 3) bind with column alignment (adds NAs where a dataset lacks a field)
shots_fired_all <- bind_rows(sf2017_trim, shots_fired_trim) %>%
  clean_names()

# 4) quick audits
shots_fired_all %>%
  count(source)

range(shots_fired_all$date, na.rm = TRUE)

# check there is no overlap around the seam
shots_fired_all %>%
  filter(date >= ymd("2022-09-20"), date <= ymd("2022-10-10")) %>%
  count(source, date) %>%
  arrange(date, source)

shots_fired_all_sf <- shots_fired_all %>%
  filter(!is.na(x_coord_cd), !is.na(y_coord_cd)) %>%
  st_as_sf(coords = c("x_coord_cd", "y_coord_cd"), crs = 2263, remove = FALSE)





# Path to the geodatabase
lion_gdb <- here("data", "lion", "lion.gdb")

# List available layers inside the .gdb
st_layers(lion_gdb)

lion_gdb <- here("data", "lion", "lion.gdb")

lion <- st_read(lion_gdb, layer = "lion") %>%
  st_transform(2263) %>% 
  clean_names() # NYC coordinates, ft

#-------------------------------------------------
# PART 1: Prepare LION - identify intersections
#-------------------------------------------------

# Filter LION to city streets (same as ebc-geography)
lion_streets <- lion |>
  filter(
    feature_typ %in% c("0", "6"),
    status == "2",
    segment_typ == "U",
    traf_dir %in% c("T", "A", "W"),
    !rw_type %in% c("2", "3", "4", "9")
  ) 

# Get intersection nodes (nodes where 2+ streets meet)
# Load nodes
nodes <- st_read(lion_gdb, layer = "node") |>
  st_transform(2263) |>
  clean_names() %>%
  mutate(nodeid = as.integer(nodeid))

node_stname <- st_read(lion_gdb, layer = "node_stname") |>
  clean_names()

# Count street names per node
node_street_counts <- lion_streets |>
  st_drop_geometry() |>
  select(node_id_from, node_id_to, street) |>
  pivot_longer(cols = c(node_id_from, node_id_to), values_to = "nodeid") |>
  filter(!is.na(nodeid)) |>
  distinct(nodeid, street) |>
  count(nodeid, name = "n_streets")%>%
  mutate(nodeid = as.integer(nodeid))

# Intersections = nodes with 2+ street names
intersection_nodes <- nodes |>
  inner_join(node_street_counts |> filter(n_streets >= 2), by = "nodeid")

cat("Total intersection nodes:", nrow(intersection_nodes), "\n")

# Create intersection buffers (25ft radius - typical intersection size)
intersection_buffers <- intersection_nodes |>
  st_buffer(dist = 25) |>  # 25 feet
  select(nodeid, n_streets)

list.files(here("data","shooting_data"))


library(readxl)

shoot_dir <- here("data", "shooting_data")
files <- list.files(shoot_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Parse month-year strings that have been clean_names()'d (e.g., jan_2025, 2025_01)
to_month_date <- function(x) {
  x <- as.character(x)
  x <- str_replace(x, "^x", "")          # janitor sometimes prefixes names starting with numbers
  x <- str_replace_all(x, "_", " ")
  x <- str_squish(x)
  
  # 1) "jan 2025" style
  d1 <- suppressWarnings(my(x))
  
  # 2) "2025 01" or "2025 1" style
  d2 <- suppressWarnings(ym(x))
  
  # coalesce, then force to first-of-month Date
  out <- coalesce(as_date(d1), as_date(d2))
  floor_date(out, "month")
}

load_one_agency <- function(path) {
  agency_name <- tools::file_path_sans_ext(basename(path))
  
  df <- read_excel(path) %>% clean_names()
  
  out <- df %>%
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
  
  out
}

shooting_long <- map_dfr(files, load_one_agency)

# QA: show periods that didn't parse (should be near-zero)
shooting_long %>%
  filter(is.na(month_date)) %>%
  count(agency, period, sort = TRUE) %>%
  print(n = 50)

range(shooting_long$month_date, na.rm = TRUE)


shooting_long <- shooting_long %>%
  group_by(agency) %>%
  mutate(program_start_date = min(month_date, na.rm = TRUE)) %>%
  ungroup()

#ky_cd in csb_sf
violent_crime <- c(101,105,106); property_crime <- c(107,109,100)

glimpse(csb_sf)
glimpse(intersection_buffers)
glimpse(nycb)
glimpse(nynta)
glimpse(physical_blocks)
glimpse(shooting_long)
glimpse(shootings_sf)
glimpse(summer_zones)
glimpse(shots_fired_all_sf)
