# Plot supplemental figs, projected nutrient yield in terms of percent demand met

# 4/15/24
#moving from Calculate_population_percent_nutrition...R

library (tidyverse)

# plot population requirements met time series ----

# These will go in supplement. Plot for all RCPs

# this takes the forecasted nutrients in tonnes from the above chunk of code and relates to population nutrient needs

# population growth
wpp_country_aggregate <- readRDS("Data/annual_nutr_demand_rni_by_country.Rds")

# facet by RCP and nutrient for fewer figures

plot_pop_needs_proj_facet <- function (country_name, anchovy = FALSE) {
  
  tonnes_nutr_ts <- readRDS(paste0("Data/", country_name, "_annual_ts_forecasted_nutrients_tonnes.Rds"))
  
  if (country_name %in% c("Chile", "Peru") & anchovy == TRUE) {
    tonnes_nutr_agg_ts <-  tonnes_nutr_ts %>%
      filter (species == "Engraulis ringens") %>%
      group_by (year, nutrient, rcp, scenario) %>%
      summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE))
    
    plot_title <- paste0("Projected nutrient yield for ", strsplit (country_name, "_")[[1]][1], "; Anchoveta only")
    
    plot_filename <- paste0("Figures/annual_nutr_ts_population_facet_", country_name, "_anchov.png")
    
  } else if (country_name %in% c("Chile", "Peru") & anchovy == FALSE) {
    
    tonnes_nutr_agg_ts <-  tonnes_nutr_ts %>%
      filter (species != "Engraulis ringens") %>%
      group_by (year, nutrient, rcp, scenario) %>%
      summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE))
    
    plot_title <- paste0("Projected nutrient yield for ", country_name, "; Anchoveta removed")
    
    plot_filename <- paste0("Figures/annual_nutr_ts_population_facet_", country_name, "_noanchov.png")
    
  } else {
    
    tonnes_nutr_agg_ts <-  tonnes_nutr_ts %>%
      group_by (year, nutrient, rcp, scenario) %>%
      summarise (tot_tonnes = sum (nutr_tonnes, na.rm = TRUE))
    
    plot_title <- paste0("Projected nutrient yield for ", country_name)
    
    plot_filename <- paste0("Figures/annual_nutr_ts_population_facet_", country_name, ".png")
  }
  
  # aggregate by nutrient, rcp, scenario
  
  
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
    filter (!nutrient %in% c("Protein"), !is.na (rcp)) %>%
    mutate (nutrient = gsub ("_", " ", nutrient),
            # https://stackoverflow.com/questions/13863599/insert-a-character-at-a-specific-location-in-a-string
            # clumsy but here we go
            # insert decimal between the digits
            rcp = gsub('^(.{4})(.*)$', '\\1.\\2', rcp),
            # insert space after the letters
            rcp = gsub('^(.{3})(.*)$', '\\1 \\2', rcp)) %>%
    ggplot (aes (x = year, y = prop_demand_met * 100, col = scenario, group = scenario)) +
    #geom_point() +
    geom_line() +
    facet_wrap (nutrient~rcp, scales = "free_y", ncol = 4) +
    theme_bw() +
    labs (y = "% population RNI equiv.", x = "", col = "Mgmt. scenario") +
    ggtitle (plot_title) +
    theme (axis.text.y = element_text (size = 12),
           axis.text.x = element_text (size = 10),
           axis.title = element_text (size = 14),
           strip.text = element_text (size = 10),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18),
           # bring legend closer to plot
           # legend.margin=margin(1,1,1,1),
           # legend.box.margin=margin(-10,-10,-10,0),
           legend.box.spacing = margin(0.2),
           legend.position = "bottom",
           plot.margin=unit(c(1,1,1,1), 'mm'))
  
  #return (plot)
  
  # save png
  png (plot_filename, width = 7.5, height = 9.5, units = "in", res = 300)
  print(plot)
  dev.off()
  
}

country_names <- c("Peru", "Chile", "Sierra Leone", "Indonesia")

map (country_names, plot_pop_needs_proj_facet)
map (c("Chile", "Peru"), plot_pop_needs_proj_facet, anchovy = TRUE)


# One RCP focal plots ----
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
  
  
pmap (expand_grid(country_name = "Chile", RCP = c("RCP26", "RCP45", "RCP60", "RCP85"), anchovy = c(TRUE, FALSE)), plot_pop_needs_met_proj_anchov)



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