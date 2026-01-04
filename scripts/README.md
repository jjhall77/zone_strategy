# NYPD Summer Zone Strategy Evaluation

## Overview

This project evaluates the effectiveness and targeting efficiency of NYPD's Summer 2023 Violence Zone Strategy. The initiative deployed officers to 67 zones (~10 square miles) from May 4 - September 18, 2023, with the goal of reducing gun violence.

## Research Questions

1. **Did the zones work?** Did gun violence decline more in zone areas than in comparable non-zone areas?

2. **Was targeting efficient?** Did NYPD deploy resources to the highest-risk micro-locations, or was there significant "cold deployment" to low-risk areas?

3. **Was any observed decline attributable to the zones?** Or did it reflect broader secular trends happening across NYS jurisdictions?

## Key Dates

- **Initiative Start**: May 4, 2023
- **Initiative End**: September 18, 2023
- **Pre-period for ranking**: January 1, 2018 - December 31, 2022 (5 years)

## Data Sources

### Crime Data
- `NYPD_Shooting_Incident_Data_(Historic/YTD)` - Shooting incidents with victims
- `shots_fired_new.csv` + `sf_since_2017.csv` - Confirmed shots fired (no victim)
- `NYPD_Complaint_Data_Historic` - All crime complaints (for violent/property crime)

### Spatial Data
- `Summer_Zones_Shapefile_Final_GCS` - Zone boundaries
- `physical_blocks.gpkg` - Street segment geometries (unit of analysis)
- `lion/lion.gdb` - NYC street network (for intersection identification)
- `nycb2020_25b` - Census blocks
- `nynta2020_25b` - Neighborhood Tabulation Areas

### External Comparison
- `shooting_data/*.xlsx` - Monthly shootings for NYS GIVE jurisdictions (Buffalo, Rochester, Syracuse, Albany, etc.)

## Analysis Scripts

Run in order, or use `00_run_all.R`:

| Script | Purpose |
|--------|---------|
| `01_load_data.R` | Load and clean all data sources |
| `02_snap_to_segments.R` | Snap crime events to street segments; handle intersections |
| `03_build_analysis_data.R` | Build segment-level panel with zone assignment and rankings |
| `04_targeting_efficiency.R` | Evaluate zone targeting - coverage of top segments, cold deployment |
| `05_give_comparison.R` | Compare NYC trends to GIVE jurisdictions (external validation) |
| `06_treatment_effects.R` | Difference-in-differences estimation |

## Methodology

### Unit of Analysis
- **Street segment** (physical block) - approximately 89,000 segments citywide
- Crimes snapped to nearest segment; intersection crimes split proportionally across adjacent segments

### Gun Violence Measure
- **Gun violence = Shootings + Confirmed Shots Fired**
- Can also analyze shootings only for robustness

### Identification Strategy

#### Primary: Within-NYC Concentration Comparison
Compare high-violence segments **inside zones** to high-violence segments **outside zones**:

1. Rank all segments by pre-treatment gun violence (2018-2022)
2. Define analysis sample: top 100/200/500 segments
3. Compare post-treatment trajectories for in-zone vs out-of-zone segments
4. Difference-in-differences with segment and time fixed effects

Key assumption: Among high-violence segments, zone assignment was quasi-random (driven by boundary-drawing convenience, not unobserved confounders).

#### Secondary: External Comparison (GIVE Jurisdictions)
Compare NYC aggregate shooting trends to upstate NY jurisdictions to assess whether any citywide decline was unique to NYC or reflected broader regional patterns.

### Key Outputs

#### Targeting Efficiency
- Share of top-N segments covered by zones
- "Cold deployment" rate: zone segments with zero pre-period gun violence
- Efficiency gap: actual zone coverage vs optimal targeting

#### Treatment Effects
- DiD estimates of zone impact on gun violence
- Event study plots showing pre-treatment parallel trends
- Robustness to sample size (top 100, 200, 500 segments)
- Placebo test (fake treatment in 2022)
- Heterogeneity by pre-treatment violence level

## Output Files

```
output/
├── figures/
│   ├── annual_index_comparison.png    # NYC vs GIVE trends
│   ├── monthly_comparison.png         # Monthly shooting comparison
│   ├── event_study.png               # Event study plot
│   └── robustness_top_n.png          # DiD by sample size
├── top_n_coverage.csv                # Coverage of top segments
├── zone_stats.csv                    # Zone-level statistics
├── concentration_comparison.csv      # Actual vs optimal targeting
├── efficiency_gap.csv                # Targeting efficiency metrics
├── missed_high_violence_segments.csv # Top segments not in zones
├── treatment_effects_summary.csv     # Main DiD results
├── robustness_by_sample_size.csv     # DiD varying N
└── event_study_data.csv              # Event study data
```

## Dependencies

```r
install.packages(c(
  "tidyverse", "sf", "here", "lubridate", "janitor",
  "readxl", "fixest", "did", "broom", "knitr", "scales"
))
```

## Notes

- All spatial data in EPSG:2263 (NAD83 / New York Long Island, US feet)
- Shootings/crimes weighted when at intersections (split across adjacent segments)
- Pre-period = 2018-2022 for segment ranking; 2019+ for DiD panel
- Treatment effect interpretation: negative = zones reduced crime

## Author

John Hall  
PhD Candidate, Cambridge University  
Criminal Justice Coordinator, Mayor's Office of Criminal Justice (NYC)
