# Ghana figs for willow
# 11/29/22

library (tidyverse)

# round sardinella (Sardinella aurita), flat sardinella (S. maderensis), European anchovy (Engraulis encrasicolus), and Atlantic chub mackerel (Scomber colias)
# 2:54
# would love to see: catch with climate change, catch with full adaptive management under climate change, nutritional content graphs....
# 2:54
# basically just all the easier graphs you've done for the BF optimization project
# 2:55
# we can just use RCP 6.0

# ds_spp smaller has ghana

# climate projection data ----
# smaller, just rcp 60 and 85. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

ghana_spp <- c("Sardinella aurita", "Sardinella maderensis", "Engraulis encrasicolus")
ghana_spp <- c("Sardinella aurita", "Sardinella maderensis", "Engraulis encrasicolus", "Scomber colias")

which (!ghana_spp %in% ds_spp$species)

# to do: italicize titles???
# https://stackoverflow.com/questions/31927984/using-italic-with-a-variable-in-ggplot2-title-expression
  
  plot_nutricast_proj <- function (country_name, spp, clim_scenario) {
    
  
  dat <- ds_spp %>% 
    filter (country == country_name, species == spp, year  >2030, rcp == clim_scenario)
  
  dat$scenario <- factor (dat$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  png(file = paste0("Figures/", country_name, "_fishcast_timeseries_", spp, "_", clim_scenario, ".png"), width = 9, height = 5, units = "in", res = 300)
print(
  dat %>% ggplot (aes (x = year, y = catch_mt, col = scenario)) +
    geom_line(lwd = 1.5) +
    theme_bw() +
    #facet_grid (species ~ rcp, scales = "free_y") +
    labs (x = "", y = "Catch, metric tons", col = "Mgmt scenario") +
    ggtitle (paste0 ("Free et al. (2020) projections for ", spp, ", ", clim_scenario)) +
    theme (plot.title = element_text (size = 14),
           axis.text = element_text (size = 12), 
           axis.title = element_text (size = 12))
  
)
  dev.off()
  
}


mapply (plot_nutricast_proj, "Ghana", ghana_spp, "RCP26")
mapply (plot_nutricast_proj, "Ghana", ghana_spp, "RCP60")


# nutritional content graphs---
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
# could bring in range with error bars...

fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu"))

# RNIs
rni_child <- readRDS("Data/RNI_child.Rds") 
rni_adult <- readRDS ("Data/RNI_adult.Rds")

png(file = "Figures/Ghana_spp_nutrient_bar_RNI_child.png", width = 10, height = 5, units = "in", res = 300)
fishnutr_mu %>% 
  filter (species %in% ghana_spp) %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  # join to rni
  left_join (rni_child, by = "nutrient") %>%
  mutate (
    perc_RNI = amount/RNI * 100,
    nutrient = sub ("_", " ", nutrient),          
# # add units; repetitive
#           nutrient = case_when (
#             nutrient %in% c ("Calcium", "Iron", "Zinc") ~ paste (nutrient, "(mg)", sep = " "),
#             nutrient %in% c("Selenium", "Vitamin A") ~ paste (nutrient, "(mcg)", sep = " "),
#             nutrient %in% c("Protein", "Omega 3") ~ paste (nutrient, "(g)", sep = " ")
#           ),
          spp_abbrev = paste0 (substr (species, 1, 1), ". ",sub (".* ", "", species))) %>%
  filter (!nutrient == "Protein") %>%
  ggplot (aes (x = spp_abbrev, y = perc_RNI)) +
  geom_col () +
  facet_wrap (~nutrient, scales = "free_y") +
  theme_bw() +
  ggtitle ("Nutrient content of a 100g serving \nRecommended Nutrient Intake (RNI) for children 6 months to 6 years") +
  labs (y = "Percent RNI met", x = "") +
  theme (plot.title = element_text (size = 14),
         axis.text.x = element_text (size = 8), 
         axis.text.y = element_text (size = 12),
         axis.title = element_text (size = 12), 
         legend.title = element_text (size = 12),
         legend.text = element_text (size = 10),
         strip.text = element_text (size = 12))

dev.off()

png(file = "Figures/Ghana_spp_nutrient_bar_RNI_adult.png", width = 10, height = 5, units = "in", res = 300)
fishnutr_mu %>% 
  filter (species %in% ghana_spp) %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  # join to rni
  left_join (rni_adult, by = "nutrient") %>%
  mutate (
    perc_RNI = amount/RNI * 100,
    nutrient = sub ("_", " ", nutrient),          
    spp_abbrev = paste0 (substr (species, 1, 1), ". ",sub (".* ", "", species))) %>%
  filter (!nutrient == "Protein") %>%
  ggplot (aes (x = spp_abbrev, y = perc_RNI)) +
  geom_col () +
  facet_wrap (~nutrient, scales = "free_y") +
  theme_bw() +
  ggtitle ("Nutrient content of a 100g serving \nRecommended Nutrient Intake (RNI) for adults") +
  labs (y = "Percent RNI met", x = "") +
  theme (plot.title = element_text (size = 14),
         axis.text.x = element_text (size = 8), 
         axis.text.y = element_text (size = 12),
         axis.title = element_text (size = 12), 
         legend.title = element_text (size = 12),
         legend.text = element_text (size = 10),
         strip.text = element_text (size = 12))

dev.off()



# expression (paste (nutrient, " (", mu, ")")

## SAU graphs ----
# downloaded 12/1/22

sau_ghana <- read.csv("Data/Ghana_SAU.csv")
which (!ghana_spp %in% sau_ghana$scientific_name) # all accounted for

ghana_sau_2019 <- sau_ghana %>% filter (year == 2019, scientific_name %in% ghana_spp)

png(file = "Figures/Ghana_SAU_sector_catch.png", width = 10, height = 5, units = "in", res = 300)
ghana_sau_2019 %>%
  mutate (spp_abbrev = paste0 (substr (scientific_name, 1, 1), ". ",sub (".* ", "", scientific_name))) %>%
  ggplot (aes (x = spp_abbrev, y = tonnes, fill = fishing_sector)) +
  geom_col() +
  theme_bw() +
  labs (y = "Catch (metric tons)", x = "", fill = "Fishing sector") +
  ggtitle ("Catch by fishing sector, 2019 (SAU)") +
  theme (plot.title = element_text (size = 14),
         axis.text = element_text (size = 12), 
         axis.title = element_text (size = 12), 
         legend.title = element_text (size = 12),
         legend.text = element_text (size = 10))
dev.off()

png(file = "Figures/Ghana_SAU_country_catch.png", width = 10, height = 5, units = "in", res = 300)
ghana_sau_2019 %>%
  mutate (spp_abbrev = paste0 (substr (scientific_name, 1, 1), ". ",sub (".* ", "", scientific_name))) %>%
  ggplot (aes (x = spp_abbrev, y = tonnes, fill = fishing_entity)) +
  geom_col() +
  theme_bw() +
  labs (y = "Catch (metric tons)", x = "", fill = "Country") +
  ggtitle ("Catch by fishing country, 2019 (SAU)") +
  theme (plot.title = element_text (size = 14),
         axis.text = element_text (size = 12), 
         axis.title = element_text (size = 12), 
         legend.title = element_text (size = 12),
         legend.text = element_text (size = 10))
dev.off()


ghana_sau_2019$reporting_status <- factor(ghana_sau_2019$reporting_status, levels = c ("Unreported", "Reported"))

png(file = "Figures/Ghana_SAU_reported_catch.png", width = 10, height = 5, units = "in", res = 300)
ghana_sau_2019 %>%
  mutate (spp_abbrev = paste0 (substr (scientific_name, 1, 1), ". ",sub (".* ", "", scientific_name))) %>%
  ggplot (aes (x = spp_abbrev, y = tonnes, fill = reporting_status)) +
  geom_col() +
  theme_bw() +
  labs (y = "Catch (metric tons)", x = "", fill = "Reporting status") +
  ggtitle ("Catch by reporting status, 2019 (SAU)") +
  theme (plot.title = element_text (size = 14),
         axis.text = element_text (size = 12), 
         axis.title = element_text (size = 12), 
         legend.title = element_text (size = 12),
         legend.text = element_text (size = 10))
dev.off()

png(file = "Figures/Ghana_SAU_discards_catch.png", width = 10, height = 5, units = "in", res = 300)
ghana_sau_2019 %>%
  mutate (spp_abbrev = paste0 (substr (scientific_name, 1, 1), ". ",sub (".* ", "", scientific_name))) %>%
  ggplot (aes (x = spp_abbrev, y = tonnes, fill = catch_type)) +
  geom_col() +
  theme_bw() +
  labs (y = "Catch (metric tons)", x = "", fill = "Catch use") +
  ggtitle ("Catch by use, 2019 (SAU)") +
  theme (plot.title = element_text (size = 14),
         axis.text = element_text (size = 12), 
         axis.title = element_text (size = 12), 
         legend.title = element_text (size = 12),
         legend.text = element_text (size = 10))
dev.off()

# double check if values are stacking appropriately
ghana_sau_2019 %>%
  group_by (fishing_sector, scientific_name) %>%
  summarise (sum = sum (tonnes))
