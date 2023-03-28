# radar plots of their desired species nutrient content----
library (ggradar)


# filter nutrient content data
nutr_radar_plot <- chl_pri_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  select (species, nutrient, perc_rda) %>%
  pivot_wider (
    names_from = nutrient,
    values_from = perc_rda) %>%
  # radar plot can't deal with NA and non-fish don't have selenium
  replace_na (list (Selenium = 0)) 

# set levels so colors stay the same
nutr_radar_plot$species <- factor (nutr_radar_plot$species, levels = c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus"))


png ("Figures/Chl_radar_reg_tm_spp.png", res = 300, width = 8, height = 5, units = "in")  
ggradar(nutr_radar_plot,
        grid.min = 0, grid.max = 100, 
        group.point.size = 2,
        group.line.width = 1,
        legend.text.size = 8,
        axis.label.size = 4,
        grid.label.size = 4,
        legend.position = "right") +
  ggtitle ("Child's daily nutrition needs from one serving") +
  theme (plot.title = element_text (size = 14))
dev.off()

# compare to anchovy and jurel

# filter nutrient content data
nutr_radar_plot_anchov <- chl_spp_nutr %>%
  filter (species %in% c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"), !nutrient %in% c("Protein", "Selenium")) %>%
  select (species, nutrient, perc_rda) %>%
  pivot_wider (
    names_from = nutrient,
    values_from = perc_rda) %>%
  # radar plot can't deal with NA and non-fish don't have selenium
  replace_na (list (Selenium = 0)) 

# set levels so colors stay the same
nutr_radar_plot_anchov$species <- factor (nutr_radar_plot_anchov$species, levels = c ("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"))


png ("Figures/Chl_radar_reg_tm_spp_anchov_ref.png", res = 300, width = 8, height = 5, units = "in")  
ggradar(nutr_radar_plot_anchov,
        grid.min = 0, grid.max = 100, 
        group.point.size = 2,
        group.line.width = 1,
        legend.text.size = 8,
        axis.label.size = 4,
        grid.label.size = 4,
        legend.position = "right") +
  ggtitle ("Child's daily nutrition needs from one serving") +
  theme (plot.title = element_text (size = 14))
dev.off()
