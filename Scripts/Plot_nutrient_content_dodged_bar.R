## Plot spp nutrient content dodged bar
# 3/2/23, moving from regional_team_priority_spp_figs

library (tidyverse)
library (stringr)
library (AFCD)

# priority species ----
# top 5-7 priority species identified by regional teams
# as of 8/4/22  have peru and chile, mexico (limited data avail). took indo spp from willow spreadsheet, but don't know where they came from
priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv") %>%
  # just change S. japonicus peruanus to S. japonicus; no nutrient or SAU or nutricast data
  mutate (species = case_when (species == "Scomber japonicus peruanus" ~ "Scomber japonicus",
                               TRUE ~ species)
  )



# nutrient data ----

# bring in compiled instead; compile_species_nutrition_data.R
compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")


# RNI data ----
# from RNI_explore; WHO
rni_child <- readRDS("Data/RNI_child.Rds")


# join nutrients and rni ----
spp_nutr <-  compiled_nutr %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rni = amount/RNI * 100,
          #perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup()




# order by overall nutrient density 

# shorten spp names
# https://stackoverflow.com/questions/8299978/splitting-a-string-on-the-first-space


plot_colorful_spp_nutr_dodge_bar <- function (species_names, Selenium = FALSE) {
  #species_names is a vector of scientific names
  
  if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}
  
  spp_nutr %>%
    filter (!nutrient %in% omit_nutrients, 
            species %in% species_names) %>%
    group_by (species) %>%
    mutate (micronutrient_density = sum (perc_rni),
            spp_short = ifelse (
              grepl(" ", species),
              paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
              species)
    ) %>%
    ungroup() %>%
    
    ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
    geom_col (position = "dodge") +
    theme_bw() +
    labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") +
    #ylim (c(0,100)) +
    ggtitle ("Nutrient content of selected species") +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 14),
      plot.title = element_text (size = 18))
}

# try to find high selenium indo??
indo_spp <- sau_2019 %>%
  filter (country == "Indonesia") %>%
  group_by (species) %>%
  summarise (tot_catch = sum (tonnes)) %>%
  top_n(20, tot_catch)

plot_colorful_spp_nutr_dodge_bar(indo_spp$species, Selenium = TRUE)
# Sardinella species, and scomberomorous

# quick peru plot
peru_spp <- priority_spp %>% filter (country == "Peru", rank <6)
png ("Figures/Peru_pri_spp_nutr_dodge_bar_sm.png", width = 7, height = 4, units = "in", res = 300)
print (
  plot_colorful_spp_nutr_dodge_bar(peru_spp$species, Selenium = FALSE)
)
dev.off()


png ("Figures/SL_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
print (
  plot_colorful_spp_nutr_dodge_bar("Sierra Leone")
)
dev.off()

png ("Figures/CHL_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
print (
  plot_colorful_spp_nutr_dodge_bar("Chile")
)
dev.off()

png ("Figures/PER_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
print (
  plot_colorful_spp_nutr_dodge_bar("Peru")
)
dev.off()

png ("Figures/IDN_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
print (
  plot_colorful_spp_nutr_dodge_bar("Indonesia")
)
dev.off()

# Malawi ----
# malawi
library (googlesheets4)
gs_id <- "1_apQe54xNsP0-bGHkGBUa75hJR5WX0pd"
# nutrition info in species_nutrition_info
mwi_nutr <- read_sheet(ss = gs_id, sheet = "Species_nutrition_info") 
# gargle error...do manually
mwi_nutr <- read.csv("Data/MWI_spp_nutr.csv")

png ("Figures/MWI_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
mwi_nutr %>%
  # nutrients in different format
  mutate (nutrient = gsub (" ", "_", nutrient),
          nutrient = ifelse (nutrient == "Vit_A", "Vitamin_A", nutrient)) %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rni = amount/RNI * 100,
          perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup() %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  group_by (species) %>%
  mutate (micronutrient_density = sum (perc_rni),
          spp_short = ifelse (
            species != "Stolephorus",
            paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
            species)
  ) %>%
  ungroup() %>%
  
  ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
  geom_col (position = "dodge") +
  theme_bw() +
  labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") +
  ylim (c(0,100)) +
  ggtitle ("Nutrient content of selected species, Malawi") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

# facet ----
png ("Figures/Facet_pri_spp_nutr_dodge_bar.png", width = 11, height = 12, units = "in", res = 300)
pri_spp_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  group_by (species) %>%
  mutate (micronutrient_density = sum (perc_rni),
          spp_short = ifelse (
            species != "Stolephorus",
            paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
            species)
  ) %>%
  ungroup() %>%
  filter (!country == "Mexico", !is.na(nutrient)) %>%
  ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
  geom_col (position = "dodge") +
  theme_bw() +
  facet_wrap (~country, ncol = 1, scales = "free_x") +
  labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") +
  ylim (c(0,100)) +
  #ggtitle (country_name)+
  theme ( 
    axis.text.y = element_text (size = 12),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()
