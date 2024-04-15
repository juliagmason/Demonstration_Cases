# Plot supplemental figs, projected nutrient yield in terms of percent demand met

# 4/15/24
#moving from Calculate_population_percent_nutrition...R


# plot population requirements met time series ----

# These will go in supplement. Plot for all RCPs

# this takes the forecasted nutrients in tonnes from the above chunk of code and relates to populatio nnutrient needs

# if running from here:
wpp_country_aggregate <- readRDS("Data/annual_nutr_demand_rni_by_country.Rds")

plot_pop_needs_met_proj <- function (country_name, RCP) {
  
  tonnes_nutr_ts <- readRDS(paste0("Data/", country_name, "_annual_ts_forecasted_nutrients_tonnes.Rds"))
  
  # aggregate by nutrient, rcp, scenario
  tonnes_nutr_agg_ts <-  tonnes_nutr_ts %>%
    group_by (year, nutrient, rcp, scenario) %>%
    summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE))
  
  
  # join to population
  ts_perc_pop <- wpp_country_aggregate %>%
    filter (country == country_name, Time > 2022) %>%
    rename (year = Time) %>%
    left_join (tonnes_nutr_agg_ts, by = c ("year", "nutrient")) %>%
    mutate (prop_demand_met = tot_tonnes / tot_nutr_annual_demand)
  
  # fix levels
  ts_perc_pop$scenario  <- factor(ts_perc_pop$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  # plot
  plot <- ts_perc_pop %>%
    filter (rcp == RCP, !nutrient %in% c("Protein")) %>%
    ggplot (aes (x = year, y = prop_demand_met * 100, col = scenario, group = scenario)) +
    #geom_point() +
    geom_line() +
    facet_wrap (~nutrient, scales = "free_y") +
    theme_bw() +
    labs (y = "% population RNI equiv.", x = "", col = "Mgmt\nscenario") +
    ggtitle (paste0("Projected nutrient yield for ", country_name, ", ", RCP)) +
    theme (axis.text.y = element_text (size = 14),
           axis.text.x = element_text (size = 13),
           axis.title = element_text (size = 16),
           strip.text = element_text (size = 15),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18))
  
  #return (plot)
  
  # save png
  png (paste0("Figures/annual_nutr_ts_population_", country_name, "_", RCP, ".png"), width = 9, height = 6, units = "in", res = 300)
  print(plot + theme(legend.position="bottom"))
  dev.off()
}

# Apply to all countries, all RCPs
pmap (expand_grid(country_name = c ("Chile", "Peru", "Sierra Leone", "Indonesia"), RCP = c("RCP26", "RCP45", "RCP60", "RCP85")), plot_pop_needs_met_proj)

### Special graphs for anchovy/no anchovy
plot_pop_needs_met_proj_anchov <- function (country_name, RCP, anchovy = FALSE) {
  
  tonnes_nutr_ts <- readRDS(paste0("Data/", country_name, "_annual_ts_forecasted_nutrients_tonnes.Rds"))
  
  if (anchovy == TRUE) {
    tonnes_nutr_agg_ts <-  tonnes_nutr_ts %>%
      filter (species == "Engraulis ringens") %>%
      group_by (year, nutrient, rcp, scenario) %>%
      summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE))
    
    plot_title <- paste0("Projected nutrient yield for ", country_name, "; ", RCP, "; Anchoveta only")
    
    file_title <- paste0("Figures/Annual_nutr_ts_population_", country_name, "_anchov_", RCP, ".png")
    
  } else {
    tonnes_nutr_agg_ts <-  tonnes_nutr_ts %>%
      filter (species != "Engraulis ringens") %>%
      group_by (year, nutrient, rcp, scenario) %>%
      summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE)) 
    
    plot_title <- paste0("Projected nutrient yield for ", country_name, "; ", RCP, "; Anchoveta removed")
    
    file_title <- paste0("Figures/Annual_nutr_ts_population_", country_name, "_NOanchov_", RCP, ".png")
    } 

    
    # join to population
    ts_perc_pop <- wpp_country_aggregate %>%
      filter (country == country_name, Time > 2022) %>%
      rename (year = Time) %>%
      left_join (tonnes_nutr_agg_ts, by = c ("year", "nutrient")) %>%
      mutate (prop_demand_met = tot_tonnes / tot_nutr_annual_demand)
    
    # fix levels
    ts_perc_pop$scenario  <- factor(ts_perc_pop$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
    
    # plot
      plot <- ts_perc_pop %>%
        filter (rcp == RCP, !nutrient %in% c("Protein")) %>%
        ggplot (aes (x = year, y = prop_demand_met * 100, col = scenario, group = scenario)) +
        geom_line() +
        facet_wrap (~nutrient, scales = "free_y") +
        theme_bw() +
        labs (y = "% population RNI equiv.", x = "", col = "Mgmt\nscenario") +
        ggtitle (plot_title) +
        theme (axis.text.y = element_text (size = 14),
               axis.text.x = element_text (size = 13),
               axis.title = element_text (size = 16),
               strip.text = element_text (size = 15),
               legend.text = element_text (size = 12),
               legend.title = element_text (size = 14),
               plot.title = element_text (size = 18))
      
      png (file_title, width = 9, height = 6, units = "in", res = 300)
      print(plot + theme(legend.position="bottom"))
      dev.off()
  }
  
  
  
}



# aggregate by nutrient, rcp, scenario
noanchov_tonnes_nutr_agg_ts <-  peru_nutr_ts %>%
  filter (species != "Engraulis ringens") %>%
  group_by (year, nutrient, rcp, scenario) %>%
  summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE))

# join to population
noanchov_ts_perc_pop <- wpp_country_aggregate %>%
  filter (country == "Peru", Time > 2022) %>%
  rename (year = Time) %>%
  left_join (noanchov_tonnes_nutr_agg_ts, by = c ("year", "nutrient")) %>%
  mutate (prop_demand_met = tot_tonnes / tot_nutr_annual_demand)

# fix levels
noanchov_ts_perc_pop$scenario  <- factor(noanchov_ts_perc_pop$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))

# plot
for (RCP in c("RCP26", "RCP45", "RCP60", "RCP85")) {
  plot_noanchov <- noanchov_ts_perc_pop %>%
    filter (rcp == RCP, !nutrient %in% c("Protein")) %>%
    ggplot (aes (x = year, y = prop_demand_met * 100, col = scenario, group = scenario)) +
    #geom_point() +
    geom_line() +
    facet_wrap (~nutrient, scales = "free_y") +
    theme_bw() +
    labs (y = "% population RNI equiv.", x = "", col = "Mgmt\nscenario") +
    ggtitle (paste0("Projected nutrient yield for Peru, ", RCP, "; Anchoveta removed")) +
    theme (axis.text.y = element_text (size = 14),
           axis.text.x = element_text (size = 13),
           axis.title = element_text (size = 16),
           strip.text = element_text (size = 15),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18))
  
  png (paste0("Figures/Annual_nutr_ts_population_Peru_NOanchov_", RCP, ".png"), width = 9, height = 6, units = "in", res = 300)
  print(plot_noanchov + theme(legend.position="bottom"))
  dev.off()
  
}


# peru ONLY anchov----
# aggregate by nutrient, rcp, scenario
anchov_tonnes_nutr_agg_ts <-  peru_nutr_ts %>%
  filter (species == "Engraulis ringens") %>%
  group_by (year, nutrient, rcp, scenario) %>%
  summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE))


# join to population
anchov_ts_perc_pop <- wpp_country_aggregate %>%
  filter (country == "Peru", Time > 2022) %>%
  rename (year = Time) %>%
  left_join (anchov_tonnes_nutr_agg_ts, by = c ("year", "nutrient")) %>%
  mutate (prop_demand_met = tot_tonnes / tot_nutr_annual_demand)

# fix levels
anchov_ts_perc_pop$scenario  <- factor(anchov_ts_perc_pop$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))

# plot
for (RCP in c("RCP26", "RCP45", "RCP60", "RCP85")) {
  plot_anchov <- anchov_ts_perc_pop %>%
    filter (rcp == RCP, !nutrient %in% c("Protein")) %>%
    ggplot (aes (x = year, y = prop_demand_met * 100, col = scenario, group = scenario)) +
    #geom_point() +
    geom_line() +
    facet_wrap (~nutrient, scales = "free_y") +
    theme_bw() +
    labs (y = "% population RNI equiv.", x = "", col = "Mgmt\nscenario") +
    ggtitle(paste0("Projected nutrient yield for Peru, ", RCP, "; Anchoveta only")) +
    theme (axis.text.y = element_text (size = 14),
           axis.text.x = element_text (size = 13),
           axis.title = element_text (size = 16),
           strip.text = element_text (size = 15),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18))
  
  
  png (paste0("Figures/Annual_nutr_ts_population_Peru_anchov_", RCP, ".png"), width = 9, height = 6, units = "in", res = 300)
  print(plot_anchov + theme(legend.position="bottom"))
  dev.off()
  
}