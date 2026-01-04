# =============================================================================
# 06_treatment_effects.R
# Estimate causal effects of zone strategy
# Compare high-violence segments inside vs outside zones
# =============================================================================

library(here)
library(tidyverse)
library(lubridate)
library(fixest)      # for fast fixed effects estimation
library(did)         # for Callaway-Sant'Anna DiD
library(broom)
library(sf)

# Load data
load(here("data", "loaded_data.RData"))
load(here("data", "analysis_data.RData"))
load(here("data", "processed_events.RData"))

# =============================================================================
# 1. BUILD MONTHLY PANEL FOR TOP SEGMENTS
# =============================================================================

message("Building monthly panel dataset...")

# Define analysis sample: top segments by pre-period gun violence
TOP_N <- 200  # Start with top 200, can vary for robustness

analysis_segments <- segments %>%
  filter(rank_gun_violence <= TOP_N) %>%
  pull(segment_id)

message(sprintf("Analysis sample: top %d segments by pre-period gun violence", TOP_N))
message(sprintf("  In zone: %d (%.0f%%)", 
                sum(segments$in_zone[segments$rank_gun_violence <= TOP_N]),
                100 * mean(segments$in_zone[segments$rank_gun_violence <= TOP_N])))

# Create monthly shootings by segment
monthly_shootings <- shootings_processed %>%
  filter(snapped, segment_id %in% analysis_segments) %>%
  mutate(month = floor_date(occur_date, "month")) %>%
  group_by(segment_id, month) %>%
  summarise(
    shootings = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# Create monthly shots fired by segment
monthly_shots_fired <- shots_fired_processed %>%
  filter(snapped, segment_id %in% analysis_segments) %>%
  mutate(month = floor_date(occur_date, "month")) %>%
  group_by(segment_id, month) %>%
  summarise(
    shots_fired = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# Create balanced panel (all segment-month combinations)
months <- seq(ymd("2019-01-01"), ymd("2024-06-30"), by = "month")

panel <- expand_grid(
  segment_id = analysis_segments,
  month = months
) %>%
  left_join(monthly_shootings, by = c("segment_id", "month")) %>%
  left_join(monthly_shots_fired, by = c("segment_id", "month")) %>%
  mutate(
    shootings = replace_na(shootings, 0),
    shots_fired = replace_na(shots_fired, 0),
    gun_violence = shootings + shots_fired
  )

# Add segment characteristics
segment_chars <- segments %>%
  filter(segment_id %in% analysis_segments) %>%
  select(segment_id, in_zone, zone_id, boro, cd, 
         total_length_ft, rank_gun_violence,
         gun_violence_pre, shootings_pre)

panel <- panel %>%
  left_join(segment_chars, by = "segment_id")

# Add time variables
panel <- panel %>%
  mutate(
    year = year(month),
    month_num = month(month),
    year_month = as.numeric(month),
    
    # Treatment indicators
    post = month >= INITIATIVE_START,
    during_treatment = month >= INITIATIVE_START & month <= INITIATIVE_END,
    
    # Event time (months relative to treatment start)
    event_time = interval(INITIATIVE_START, month) %/% months(1),
    
    # Treatment dummy
    treated = in_zone * post,
    treated_during = in_zone * during_treatment
  )

message(sprintf("Panel: %d segment-months", nrow(panel)))

# =============================================================================
# 2. DESCRIPTIVE COMPARISONS
# =============================================================================

message("\n=== PRE-TREATMENT BALANCE CHECK ===")

pre_balance <- panel %>%
  filter(month < INITIATIVE_START, month >= ymd("2019-01-01")) %>%
  group_by(in_zone) %>%
  summarise(
    n_segments = n_distinct(segment_id),
    mean_monthly_shootings = mean(shootings),
    mean_monthly_gv = mean(gun_violence),
    sd_monthly_gv = sd(gun_violence),
    total_shootings = sum(shootings),
    total_gv = sum(gun_violence),
    .groups = "drop"
  )

message("\nPre-Treatment (2019 - Apr 2023) Summary:")
print(pre_balance)

# =============================================================================
# 3. EVENT STUDY PLOT
# =============================================================================

message("\n=== EVENT STUDY ===")

# Aggregate by event time and treatment status
event_study_data <- panel %>%
  filter(event_time >= -24, event_time <= 18) %>%  # 2 years pre, 1.5 years post
  group_by(event_time, in_zone) %>%
  summarise(
    mean_gv = mean(gun_violence),
    se_gv = sd(gun_violence) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Calculate difference (treatment - control) by event time
event_study_diff <- event_study_data %>%
  pivot_wider(
    names_from = in_zone,
    values_from = c(mean_gv, se_gv, n),
    names_sep = "_"
  ) %>%
  mutate(
    diff = mean_gv_TRUE - mean_gv_FALSE,
    se_diff = sqrt(se_gv_TRUE^2 + se_gv_FALSE^2)
  )

# Plot
p_event_study <- event_study_diff %>%
  ggplot(aes(x = event_time, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = diff - 1.96*se_diff, ymax = diff + 1.96*se_diff), 
              alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text", x = 0, y = max(event_study_diff$diff, na.rm = TRUE), 
           label = "Treatment\nStarts", hjust = 0, size = 3, color = "red") +
  labs(
    title = "Event Study: Gun Violence in Zone vs Non-Zone Segments",
    subtitle = sprintf("Top %d segments by pre-period gun violence", TOP_N),
    x = "Months Relative to Treatment Start (May 2023)",
    y = "Difference in Mean Monthly Gun Violence\n(In-Zone minus Out-of-Zone)",
    caption = "Shaded area = 95% confidence interval"
  ) +
  theme_minimal(base_size = 12)

ggsave(here("output", "figures", "event_study.png"), p_event_study,
       width = 10, height = 6, dpi = 300)

message("Event study plot saved")

# =============================================================================
# 4. DIFFERENCE-IN-DIFFERENCES ESTIMATION
# =============================================================================

message("\n=== DIFFERENCE-IN-DIFFERENCES ===")

# Simple DiD
did_simple <- feols(
  gun_violence ~ treated | segment_id + month,
  data = panel,
  cluster = ~segment_id
)

message("\nSimple DiD (Gun Violence):")
summary(did_simple)

# DiD with controls
did_controlled <- feols(
  gun_violence ~ treated + i(month_num) | segment_id + year,
  data = panel,
  cluster = ~segment_id
)

message("\nDiD with Month-of-Year FE (Gun Violence):")
summary(did_controlled)

# Shootings only
did_shootings <- feols(
  shootings ~ treated | segment_id + month,
  data = panel,
  cluster = ~segment_id
)

message("\nSimple DiD (Shootings Only):")
summary(did_shootings)

# =============================================================================
# 5. TREATMENT PERIOD ONLY
# =============================================================================

message("\n=== TREATMENT PERIOD ANALYSIS ===")

# Focus on May-Sep 2023 vs same months in prior years
summer_months <- 5:9

summer_panel <- panel %>%
  filter(month_num %in% summer_months) %>%
  mutate(
    treatment_year = year == 2023,
    treated_summer = in_zone * treatment_year
  )

did_summer <- feols(
  gun_violence ~ treated_summer | segment_id + year,
  data = summer_panel,
  cluster = ~segment_id
)

message("\nSummer-Only DiD (May-Sep, comparing 2023 to prior years):")
summary(did_summer)

# =============================================================================
# 6. HETEROGENEITY BY PRE-TREATMENT VIOLENCE
# =============================================================================

message("\n=== HETEROGENEITY ANALYSIS ===")

# Split sample by pre-treatment gun violence terciles
panel <- panel %>%
  mutate(
    gv_tercile = ntile(gun_violence_pre, 3),
    gv_tercile_label = case_when(
      gv_tercile == 1 ~ "Low Pre-GV",
      gv_tercile == 2 ~ "Medium Pre-GV",
      gv_tercile == 3 ~ "High Pre-GV"
    )
  )

# DiD by tercile
did_by_tercile <- panel %>%
  nest_by(gv_tercile_label) %>%
  mutate(
    model = list(feols(gun_violence ~ treated | segment_id + month, 
                       data = data, cluster = ~segment_id)),
    coef = coef(model)["treated"],
    se = sqrt(vcov(model)["treated", "treated"]),
    n_segments = n_distinct(data$segment_id)
  ) %>%
  select(gv_tercile_label, coef, se, n_segments) %>%
  ungroup()

message("\nDiD Estimates by Pre-Treatment Violence Tercile:")
print(did_by_tercile)

# =============================================================================
# 7. ROBUSTNESS: VARYING TOP-N
# =============================================================================

message("\n=== ROBUSTNESS: VARYING SAMPLE SIZE ===")

run_did_for_top_n <- function(n, data) {
  
  sample_segments <- segments %>%
    filter(rank_gun_violence <= n) %>%
    pull(segment_id)
  
  sample_panel <- data %>%
    filter(segment_id %in% sample_segments)
  
  model <- feols(
    gun_violence ~ treated | segment_id + month,
    data = sample_panel,
    cluster = ~segment_id
  )
  
  tibble(
    top_n = n,
    n_segments = length(sample_segments),
    n_in_zone = sum(segments$in_zone[segments$segment_id %in% sample_segments]),
    coef = coef(model)["treated"],
    se = sqrt(vcov(model)["treated", "treated"]),
    pvalue = 2 * pnorm(-abs(coef/se))
  )
}

# Need to rebuild panel with all segments for this
all_segments <- segments %>%
  filter(rank_gun_violence <= 500) %>%
  pull(segment_id)

panel_full <- expand_grid(
  segment_id = all_segments,
  month = months
) %>%
  left_join(monthly_shootings, by = c("segment_id", "month")) %>%
  left_join(monthly_shots_fired, by = c("segment_id", "month")) %>%
  mutate(
    shootings = replace_na(shootings, 0),
    shots_fired = replace_na(shots_fired, 0),
    gun_violence = shootings + shots_fired
  ) %>%
  left_join(segment_chars %>% select(segment_id, in_zone), by = "segment_id") %>%
  mutate(
    post = month >= INITIATIVE_START,
    treated = in_zone * post
  )

robustness_results <- map_dfr(
  c(50, 100, 150, 200, 250, 300, 400, 500),
  ~run_did_for_top_n(.x, panel_full)
)

message("\nDiD Estimates by Sample Size:")
robustness_results %>%
  mutate(
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se,
    sig = if_else(pvalue < 0.05, "*", "")
  ) %>%
  print()

# Plot robustness
p_robustness <- robustness_results %>%
  ggplot(aes(x = top_n, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), 
              alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "DiD Treatment Effect by Sample Size",
    subtitle = "Varying threshold for 'top' segments by pre-period gun violence",
    x = "Top N Segments Included",
    y = "Treatment Effect\n(Monthly Gun Violence)",
    caption = "Shaded area = 95% CI. Negative = zones reduced crime."
  ) +
  theme_minimal(base_size = 12)

ggsave(here("output", "figures", "robustness_top_n.png"), p_robustness,
       width = 10, height = 6, dpi = 300)

# =============================================================================
# 8. PLACEBO TEST
# =============================================================================

message("\n=== PLACEBO TEST ===")

# Run same analysis with fake treatment in May 2022
panel_placebo <- panel %>%
  filter(month < INITIATIVE_START) %>%  # Only pre-period

  mutate(
    placebo_post = month >= ymd("2022-05-01"),
    placebo_treated = in_zone * placebo_post
  )

did_placebo <- feols(
  gun_violence ~ placebo_treated | segment_id + month,
  data = panel_placebo,
  cluster = ~segment_id
)

message("\nPlacebo Test (Fake Treatment May 2022):")
summary(did_placebo)

# =============================================================================
# 9. SUMMARY TABLE
# =============================================================================

message("\n=== SUMMARY OF RESULTS ===")

results_summary <- tibble(
  specification = c(
    "Main DiD (Gun Violence)",
    "Main DiD (Shootings Only)",
    "Summer Only (May-Sep)",
    "Placebo (May 2022)"
  ),
  coefficient = c(
    coef(did_simple)["treated"],
    coef(did_shootings)["treated"],
    coef(did_summer)["treated_summer"],
    coef(did_placebo)["placebo_treated"]
  ),
  std_error = c(
    sqrt(vcov(did_simple)["treated", "treated"]),
    sqrt(vcov(did_shootings)["treated", "treated"]),
    sqrt(vcov(did_summer)["treated_summer", "treated_summer"]),
    sqrt(vcov(did_placebo)["placebo_treated", "placebo_treated"])
  )
) %>%
  mutate(
    t_stat = coefficient / std_error,
    p_value = 2 * pnorm(-abs(t_stat)),
    sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

message("\nTreatment Effect Estimates:")
print(results_summary)

# =============================================================================
# 10. SAVE RESULTS
# =============================================================================

message("\nSaving treatment effect results...")

save(
  panel,
  did_simple,
  did_controlled,
  did_shootings,
  did_summer,
  did_placebo,
  event_study_data,
  event_study_diff,
  robustness_results,
  results_summary,
  did_by_tercile,
  file = here("output", "treatment_effects_results.RData")
)

write_csv(results_summary, here("output", "treatment_effects_summary.csv"))
write_csv(robustness_results, here("output", "robustness_by_sample_size.csv"))
write_csv(event_study_diff, here("output", "event_study_data.csv"))

message("\n", strrep("=", 60))
message("TREATMENT EFFECTS SUMMARY")
message(strrep("=", 60))

message(sprintf("\nSample: Top %d segments by pre-period gun violence", TOP_N))
message(sprintf("  In zone: %d (%.0f%%)", 
                sum(segments$in_zone[segments$rank_gun_violence <= TOP_N]),
                100 * mean(segments$in_zone[segments$rank_gun_violence <= TOP_N])))

message(sprintf("\nMain DiD Estimate: %.3f (SE: %.3f)",
                results_summary$coefficient[1],
                results_summary$std_error[1]))

if (results_summary$p_value[1] < 0.05) {
  message("  Statistically significant at 5% level")
} else {
  message("  NOT statistically significant at 5% level")
}

message("\nInterpretation:")
if (results_summary$coefficient[1] < 0) {
  message("  Zones REDUCED gun violence relative to comparison segments")
} else {
  message("  Zones did NOT reduce gun violence relative to comparison segments")
}

message("\nPlacebo test (fake treatment May 2022):")
message(sprintf("  Coefficient: %.3f (p = %.3f)",
                results_summary$coefficient[4],
                results_summary$p_value[4]))
if (results_summary$p_value[4] > 0.10) {
  message("  Placebo insignificant - supports parallel trends assumption")
} else {
  message("  WARNING: Placebo significant - parallel trends may not hold")
}
