# radar plots of their desired species nutrient content----
# moved from regional_team_pri_spp_figs 3/28/23
library (tidyverse)
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)
library (ggradar)

# nutrient data ----

# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
# this is per 100g raw, edible portion
fishnutr_long <- read_csv ("Data/Species_Nutrient_Predictions.csv") %>%
  select (species, ends_with ("_mu")) %>% 
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4))

# bring in afcd etc.?

# RNI data ----
# from RNI_explore; WHO
rni_child <- readRDS("Data/RNI_child.Rds")


spp_rni_met <- fishnutr_long %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rni = amount/RNI * 100,
          perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup()


# for sci team deep dive, choose 3 spp including anchovy

nutr_radar_plot_anchov <- spp_rni_met  %>%
  filter (species %in% c("Engraulis ringens", "Trachurus murphyi", "Merluccius gayi gayi"), !nutrient %in% c("Protein", "Selenium")) %>%
  select (species, nutrient, perc_rni) %>%
  pivot_wider (
    names_from = nutrient,
    values_from = perc_rni) 


png ("Figures/Anchovy_radar.png", res = 300, width = 5, height = 5, units = "in")  
ggradar(nutr_radar_plot_anchov,
        grid.min = 0, grid.max = 100, 
        group.point.size = 2,
        group.line.width = 1,
        legend.text.size = 8,
        axis.label.size = 4,
        grid.label.size = 4,
        legend.position = "bottom") +
  
  theme (plot.title = element_text (size = 14))
dev.off()

# filter nutrient content data
nutr_radar_plot <- spp_rni_met %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  select (species, nutrient, perc_rni) %>%
  pivot_wider (
    names_from = nutrient,
    values_from = perc_rda) 




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
