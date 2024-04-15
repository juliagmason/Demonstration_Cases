# Plot supplemental catch projections
# 12/24/23
# JGM

# Plot Free et al. catch projections for contextual supplemental figures. Moving code from Plot_nutricast_projections.R
# Also want to plot catch projections clipped to the species for which we have nutrient data so results make more sense

library (tidyverse)

# Total catch projections ----
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

# separate out peru anchovy
ds_spp_anchov <- ds_spp %>%
  filter (scenario %in% c ("No Adaptation", "Productivity Only", "Full Adaptation"), year > 2017) %>%
  mutate (country = case_when (
    country == "Peru" & species == "Engraulis ringens" ~ "Peru, anchoveta only",
    country == "Peru" & species !="Engraulis ringens" ~ "Peru, anchoveta removed",
    country == "Chile" & species == "Engraulis ringens" ~ "Chile, anchoveta only",
    country == "Chile" & species !="Engraulis ringens" ~ "Chile, anchoveta removed",
    TRUE ~ country))

# set levels of scenarios
ds_spp_anchov$scenario <- factor (ds_spp_anchov$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))

plot_total_catch_projections <- function (country_name) {
  # country names are Chile, "Peru, anchoveta only", "Peru, anchoveta removed", Sierra Leone, Indonesia 
 
  catch_projection_plot <- ds_spp_anchov %>%
    filter (country == country_name) %>%
    group_by (year, rcp, scenario) %>%
    summarise (tot_catch = sum (catch_mt, na.rm = TRUE)) %>%
    ggplot (aes (x = year, y = tot_catch/1000000, col = scenario), lwd = 1.5) +
    geom_line() +
    theme_bw() +
    facet_wrap ( ~ rcp, scales = "free_y") +
    labs (x = "", y = "Catch, million metric tons", col = "Mgmt. scenario") +
    ggtitle (paste0("Total catch projections for ", country_name,"\nFree et al. (2020)")) +
    theme (plot.title = element_text (size = 18),
           axis.text = element_text (size = 12),
           strip.text = element_text (size = 14),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 12),
           axis.title = element_text (size = 14))
  
  #valid filenames
  if (country_name == "Peru, anchoveta only") {
    country_filename = "Peru_anchov"} else if (country_name == "Peru, anchoveta removed") {
      country_filename = "Peru_noanchov"} else {
        country_filename = country_name }
  
  
  png(paste0("Figures/Total_catch_projections_", country_filename, ".png"), width = 8, height = 6, units = "in", res = 300)
  print (catch_projection_plot)
  dev.off()
}

# clip to species for which we have nutrition and production data ----
# based on production baselines 

# repaired names from calculate_projected nutritional_upsides.R
# this is just a catch ratio
catch_upside_annual <- readRDS("Data/catch_upside_relative_annual_repaired.Rds")

# baseline catch from compiled data; from plot_contextual_landings.forecasts.R
full_baseline <- readRDS("Data/baseline_catch_sau_chl_ihh.Rds")

catch_upside_ts <- catch_upside_annual %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (catch_mt = catch_ratio * bl_tonnes,
          # make peru anchoveta its own country
          country = case_when (
            country == "Peru" & species == "Engraulis ringens" ~ "Peru, anchoveta only",
            country == "Peru" & species !="Engraulis ringens" ~ "Peru, anchoveta removed",
            TRUE ~ country)
  )

# set levels of scenarios
catch_upside_ts$scenario <- factor (catch_upside_ts$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))


plot_clipped_catch_projections <- function (country_name) {
  # country names are Chile, "Peru, anchoveta only", "Peru, anchoveta removed", Sierra Leone, Indonesia 
  
  catch_projection_plot <- catch_upside_ts %>%
    filter (country == country_name) %>%
    group_by (year, rcp, scenario) %>%
    summarise (tot_catch = sum (catch_mt, na.rm = TRUE)) %>%
    ggplot (aes (x = year, y = tot_catch/1000000, col = scenario), lwd = 1.5) +
    geom_line() +
    theme_bw() +
    facet_wrap ( ~ rcp, scales = "free_y") +
    ggtitle (paste0("Catch projections for ", country_name,"; Free et al. (2020)")) +
    labs (x = "", y = "Catch, million metric tons", col = "Mgmt. scenario", subtitle = "Species with production & nutrition data only; harmonized to baseline catch") +
    theme (plot.title = element_text (size = 18),
           plot.subtitle = element_text(size = 12),
           axis.text = element_text (size = 12),
           strip.text = element_text (size = 14),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 12),
           axis.title = element_text (size = 14))
  
  #valid filenames
  if (country_name == "Peru, anchoveta only") {
    country_filename = "Peru_anchov"} else if (country_name == "Peru, anchoveta removed") {
      country_filename = "Peru_noanchov"} else {
        country_filename = country_name }
  
  
  png(paste0("Figures/Clipped_catch_projections_", country_filename, ".png"), width = 8, height = 6, units = "in", res = 300)
  print (catch_projection_plot)
  dev.off()
}

map (c("Chile", "Peru, anchoveta only", "Peru, anchoveta removed", "Sierra Leone", "Indonesia"), plot_clipped_catch_projections)
