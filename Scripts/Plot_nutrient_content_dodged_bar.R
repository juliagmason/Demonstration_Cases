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

# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
# this is per 100g raw, edible portion
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")

# truncate to just summary predicted value. eventually will want range?
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu"))


fishnutr_long <- fishnutr_mu %>% 
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4))

# d gigas data from Bianchi et al. 2022 supp table 2. Vita A is retinol equiv; omega 3 is n-3 fatty acids
d_gigas_nutr <- data.frame (
  species = "Dosidicus gigas", 
  nutrient = c ("Calcium", "Iron", "Omega_3", "Protein", "Selenium", "Vitamin_A", "Zinc"),
  amount = c(37.5, 3.3, 0.6, 16.4, 50.9, 0, 2.8)
)

# scylla serrata for indonesia
# picking somewhat randomly. note that there's a separate dha + epa for omegas, and different vitamin As

s_serrata_nutr <- afcd_sci %>% 
  filter (sciname == "Scylla serrata",
          nutrient_code_fao %in% c(
            "CA", "ZN", "FE", "SE", "Protein", "FAPU", "VITA")) %>%
  group_by(nutrient_code_fao) %>%
  summarise (amount = mean (value, na.rm = TRUE)) %>%
  mutate (nutrient =
            case_when (nutrient_code_fao == "CA" ~ "Calcium",
                       nutrient_code_fao == "FE" ~ "Iron",
                       nutrient_code_fao == "SE" ~ "Selenium", 
                       nutrient_code_fao == "ZN" ~ "Zinc",
                       nutrient_code_fao == "FAPU" ~ "Omega_3",
                       nutrient_code_fao == "VITA" ~ "Vitamin_A"),
          species = "Scylla serrata"
  ) %>%
  # reorder to match
  select (species, nutrient, amount)

# composite Stolephorus spp for Indonesia
indo_stolephorus <- readRDS("Data/indo_stolephorus.Rds")

stoleph_nutr <- fishnutr_long %>%
  filter (species %in% indo_stolephorus) %>%
  group_by (nutrient) %>%
  summarise (amount = mean (amount)) %>%
  mutate (species = "Stolephorus") %>%
  select (species, nutrient, amount)

# genus data for nonfish [eventually could use AFCD]
spp_key <- read.csv(file.path ("Data/Gaines_species_nutrient_content_key.csv"), as.is=T)
spp_key_long <- spp_key %>% 
  select (species, major_group, genus_food_name, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg) %>%
  
  pivot_longer (calcium_mg:zinc_mg,
                names_to = "nutrient", 
                values_to = "amount") %>%
  # recode major_Group_ name from nutricast code
  mutate (
    taxa=recode(genus_food_name,
                "Cephalopod"="Cephalopods",
                "Crustacean"="Crustaceans",
                "Demersal Fish"="Finfish",
                "Marine Fish; Other"="Finfish",
                "Mollusc; Other"="Molluscs",
                "Pelagic Fish"="Finfish"),
    nutrient = recode (nutrient, 
                       "calcium_mg" = "Calcium",
                       "iron_mg" = "Iron",
                       "polyunsaturated_fatty_acids_g" = "Omega_3",
                       "protein_g" = "Protein", 
                       "vitamin_a_mcg_rae" = "Vitamin_A", 
                       "zinc_mg" = "Zinc")
    
  )


# RNI data ----
# from RNI_explore; WHO
rni_child <- readRDS("Data/RNI_child.Rds")


# join nutrients and rni ----

pri_spp_nutr <- fishnutr_long %>%
  rbind (d_gigas_nutr) %>%
  rbind (s_serrata_nutr) %>%
  rbind (stoleph_nutr) %>%
  right_join (priority_spp, by = "species")  %>%
  
  
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




# order by overall nutrient density 

# shorten spp names
# https://stackoverflow.com/questions/8299978/splitting-a-string-on-the-first-space


plot_colorful_spp_nutr_dodge_bar <- function (country_name) {
  pri_spp_nutr %>%
    filter (!nutrient %in% c("Protein", "Selenium"), 
            country == country_name) %>%
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
    ggtitle (country_name)+
    theme ( 
      axis.text.y = element_text (size = 14),
      axis.text.x = element_text (size = 10),
      axis.title = element_text (size = 14),
      plot.title = element_text (size = 18))
}

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

# facet?
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
