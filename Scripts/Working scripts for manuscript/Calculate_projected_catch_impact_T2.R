# Calculate projected climate impact on catch
# 4/30/24

# For use in Table 2

library (tidyverse)

# catch ratios for each species and rcp. made in calculate_projected_nutritional_upsides.R
catch_upside_relative <- readRDS("Data/catch_upside_relative_repaired.Rds")

# baseline catch from compiled data; from plot_contextual_landings.forecasts.R
full_baseline <- readRDS("Data/baseline_catch_sau_chl_ihh.Rds")


# only need rcp 6.0, midcentury, bau
catch_upside <- catch_upside_relative %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (catch_mt = bau_ratio_midcentury * bl_tonnes,
          # make peru anchoveta its own country
          country = case_when (
            country == "Peru" & species == "Engraulis ringens" ~ "Peru, anchoveta only",
            country == "Peru" & species !="Engraulis ringens" ~ "Peru, anchoveta removed",
            country == "Chile" & species == "Engraulis ringens" ~ "Chile, anchoveta only",
            country == "Chile" & species !="Engraulis ringens" ~ "Chile, anchoveta removed",
            TRUE ~ country)
  ) %>%
  group_by (country, rcp) %>%
  summarize (tot_bl = sum (bl_tonnes), tot_midcentury = sum (catch_mt), perc_bl = tot_midcentury / tot_bl * 100)

catch_upside %>% filter (rcp == "RCP60") %>% View()
