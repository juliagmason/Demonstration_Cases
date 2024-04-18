# Plot Figure XE projected nutrient upsides
# 11 17 23
# JGM

# plot annual timeseries of projected nutrient yield, in child RNI equivalents
# calculated these in Calculate_projected_nutritional_upsides.R
# note--Peru and Chile rds does not include anchoveta. Peru_anchoveta is anchoveta only. treat these as different country names

library (tidyverse)
# plot annual ts-----

#Plot for each country and RCP
plot_child_RNI_proj <- function (country_name, RCP) {
  
  #projected nutrient yield in rni_equivalents for each country
  nutr_ts <- readRDS(paste0("Data/annual_nutr_upside_childRNI_", country_name, ".Rds"))
  
  # aggregate by rcp, scenario, year, nutrient
  nutr_agg_ts <- nutr_ts %>%
    group_by (rcp, scenario, year, nutrient) %>%
    summarise (tot_fed = sum (rni_equivalents, na.rm = TRUE)) %>%
    filter (rcp == RCP, !nutrient %in% c("Protein"), scenario %in% c("Full Adaptation", "No Adaptation"), year <= 2060)
  
  # for supplement only
  # # specify anchoveta in title
  # # https://stackoverflow.com/questions/40113963/how-to-extract-everything-until-first-occurrence-of-pattern
  # if (country_name %in% c("Peru_anchoveta", "Chile_anchoveta")) {
  #   country_title <-  gsub("(.+?)(\\_.*)", "\\1", country_name)
  #   plot_title <- paste0("Projected nutrient yield for ", country_title, "; ", RCP, "; Anchoveta only")
  # } else if (country_name %in% c("Peru", "Chile")) {
  #   plot_title <- paste0("Projected nutrient yield for ", country_name, "; ", RCP, "; Anchoveta removed")
  #   } else {plot_title <- paste0("Projected nutrient yield for ", country_name, "; ", RCP)}
  # 
  plot_ts <- nutr_agg_ts %>% 
    ggplot (aes (x = year, y = tot_fed/1000000, col = scenario, group = scenario)) +
    geom_line() +
    scale_x_continuous (breaks = c(2020, 2040, 2060), limits = c(2020, 2060)) +
    facet_wrap (~nutrient, scales = "free_y") +
    theme_bw() +
    labs (y = "Child RNI equivalents, millions", x = "", col = "Mgmt\nscenario") +
    #ggtitle (plot_title) +
    ggtitle (paste0("Projected nutrient yield for ", country_name, "; ", RCP)) +
    theme (axis.text.y = element_text (size = 14),
           axis.text.x = element_text (size = 13),
           axis.title = element_text (size = 16),
           strip.text = element_text (size = 15),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18))
  
  plot_ts 
  
  # # save png - use this for supplemental figs only
  # png (paste0("Figures/annual_nutr_ts_childRNI_", country_name, "_", RCP, ".png"), width = 9, height = 6, units = "in", res = 300)
  # print (plot_ts + theme(legend.position="bottom"))
  # dev.off()
  
}

# plot_child_RNI_proj("Chile", RCP = "RCP26")
# plot_child_RNI_proj("Peru_anchoveta", RCP = "RCP60")

# # Apply to all countries, all RCPs for supplement
# pmap (expand_grid(country_name = c ("Chile", "Peru", "Sierra Leone", "Indonesia"), RCP = c("RCP26", "RCP45", "RCP60", "RCP85")), plot_child_RNI_proj)                   
# 


countries <- c("Peru", "Peru_anchoveta", "Chile", "Chile_anchoveta", "Indonesia", "Sierra Leone")

# cut to 2060 for manuscript
for (country in countries) {
  
   plot_child_RNI_proj(country, RCP = "RCP60") +
    labs (y = "Child RNI equiv., millions", x = "", col = "Mgmt. scenario") +
    ggtitle ("Projected nutrient yield, RCP 6.0") +
    scale_x_continuous (breaks = c(2020, 2040, 2060), limits = c(2020, 2060)) +
    theme ( 
      axis.text.x = element_text (size = 9),
      axis.text.y = element_text (size = 9),
      axis.title = element_text (size = 12),
      legend.text = element_text (size = 9),
      legend.title = element_text (size = 9),
      legend.position = "bottom",
      legend.margin = margin (t = -25),
      strip.text = element_text (size = 9, margin = margin(0.5,0.5,0.5,0.5, "mm")),
      plot.title = element_text (size = 13),
      plot.margin=unit(c(1,1,1,1), 'mm'),
      panel.grid.minor = element_blank())
  
  ggsave (paste0("Figures/FigXE_projected_", country, ".eps"), width = 97, height = 70, units = "mm")
  
}



# # just chile redo
# pmap (expand_grid(country_name = c ("Chile", "Chile_anchoveta"), RCP = c("RCP26", "RCP45", "RCP60", "RCP85")), plot_child_RNI_proj)

# peru anchoveta fix y lim ssues
# aggregate by rcp, scenario, year, nutrient
nutr_agg_ts <- nutr_ts %>%
  group_by (rcp, scenario, year, nutrient) %>%
  summarise (tot_fed = sum (rni_equivalents, na.rm = TRUE)) %>%
  filter (rcp == RCP, !nutrient %in% c("Protein"), scenario %in% c("Full Adaptation", "No Adaptation"), year <= 2060)

plot_ts <- nutr_agg_ts %>% 
  ggplot (aes (x = year, y = tot_fed/1000000, col = scenario, group = scenario)) +
  geom_line() +
  scale_x_continuous (breaks = c(2020, 2040, 2060), limits = c(2020, 2060)) +
  facet_wrap (~nutrient, scales = "free_y") +
  theme_bw() +
  labs (y = "Child RNI equivalents, millions", x = "", col = "Mgmt\nscenario") +
  ggtitle ("Projected nutrient yield, RCP 6.0") +
theme ( 
  axis.text.x = element_text (size = 9),
  axis.text.y = element_text (size = 9),
  axis.title = element_text (size = 12),
  legend.text = element_text (size = 9),
  legend.title = element_text (size = 9),
  legend.position = "bottom",
  legend.margin = margin (t = -25),
  strip.text = element_text (size = 9, margin = margin(0.5,0.5,0.5,0.5, "mm")),
  plot.title = element_text (size = 13),
  plot.margin=unit(c(1,1,1,1), 'mm'),
  panel.grid.minor = element_blank())
