# Link Free projections to FishNutrients data 
# 3/9/22
# JGM

# Just want to look at the most nutritious species/compare species. Not looking at projections. 

library (tidyverse)

# Species specific projection data from Chris Free 2020 ----

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 



# nutrient data ----
# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")

# truncate to just summary predicted value. eventually will want range?
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 

# genus data for nonfish [eventually could use AFCD]
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T)

#truncate to the nutrients we're using for fishnutrients. have to figure out how to get omegas? for now, assume PUFAS = omega 3s
spp_key_sm <- spp_key %>% 
  select (species, major_group, genus_food_name, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg) %>%
  # recode major_Group_ name from nutricast code
  mutate (
    major_group=recode(genus_food_name,
                       "Cephalopods"="Cephalopods",
                       "Crustaceans"="Crustaceans",
                       "Demersal Fish"="Finfish",
                       "Marine Fish; Other"="Finfish",
                       "Molluscs; Other"="Molluscs",
                       "Pelagic Fish"="Finfish")
  )


# are there fish species that don't have data?

ds_spp_fish <- ds_spp %>%
  filter (year == 2012, rcp == "RCP26", scenario == "No Adaptation", catch_mt > 0) %>%
  left_join (spp_key_sm) %>%
  filter (major_group == "Finfish")
  
ds_spp_fish$species[which (!ds_spp_fish$species %in% fishnutr$species)]
# Sebastes levis
# Cynoponticus coniceps
# Sebastes jordani ?


# From Maire et al. 2021, use RDAs to calculate micronutrient density. Take percent of RDA met for each nutrient, capped at 100%, and then take sum for micronutrient density score. Maire et al did children between 6 mos and 5 years. 

# DRIS data from Free nutrient_endowment --> data/ears/data
dris <- readRDS("Data/dietary_reference_intake_data.Rds")

# no RDA for omega 3, only AI, adequate intake. 
ai_omega <- dris %>%
  filter (grepl("Linolenic", nutrient), !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  " 4-8 yr")) %>%
  summarise (unit = first(units),
             mean_rda = mean (value, na.rm = TRUE)) %>%
  # match nutrient names; assume linolenic is omega 3
  mutate (nutrient = "Omega_3")

rda_adult <- dris %>%
  filter (dri_type == "Recommended Dietary Allowance (RDA)", !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  " 4-8 yr")) %>%
  group_by (nutrient) %>%
  summarise (unit = first(units),
             mean_rda = mean (value, na.rm = TRUE)) %>%
  mutate (nutrient = ifelse (nutrient == "Vitamin A", "Vitamin_A", nutrient))

# add omega 3, REMEMBER THIS IS AI
rda_adult <- rbind (rda_adult, ai_omega)


# join species and nutrients data ----
# just get relevant species for each country. Baseline year (2012) where catch > 0, unique species

ds_spp_nutr_content <- ds_spp %>%
  filter (year == 2012, rcp == "RCP26", scenario == "No Adaptation", catch_mt > 0) %>%
  # only select relevant columns
  select (country, species, catch_mt)  %>% 
  # join nutrient data
  left_join (spp_key_sm, by = "species") %>%
  left_join (fishnutr_mu, by = "species") %>%
  
  mutate (
    # select appropriate source for finfish vs. other
    Selenium =  Selenium_mu, 
    Zinc = ifelse (major_group == "Finfish", 
                    Zinc_mu,
                   zinc_mg),
    Protein = ifelse (major_group == "Finfish",
                       Protein_mu,
                      protein_g),
    #### NEED to figure out omega 3 situation #####
    Omega_3 = ifelse (major_group == "Finfish",
                      Omega_3_mu, 
                      polyunsaturated_fatty_acids_g),
    Calcium = ifelse (major_group == "Finfish",
                       Calcium_mu,
                       calcium_mg),
    Iron = ifelse (major_group == "Finfish",
                    Iron_mu,
                    iron_mg),
    Vitamin_A = ifelse (major_group == "Finfish",
                         Vitamin_A_mu,
                         vitamin_a_mcg_rae),
    .keep = "unused"
  ) %>%
  pivot_longer (Selenium:Vitamin_A,
                names_to = "nutrient",
                values_to = "amount") %>%
  # join to rda data
  left_join (rda_adult, by = "nutrient") %>%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda))
  
# this is quite different from maire et al. not also very different calcium values, must be updated model from fishnutrients? does track when we look at children's needs. 
ai_omega_child <- dris %>%
  filter (grepl("Linolenic", nutrient), age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  " 4-8 yr")) %>%
  summarise (unit = first(units),
             mean_rda = mean (value, na.rm = TRUE)) %>%
  # match nutrient names; assume linolenic is omega 3
  mutate (nutrient = "Omega_3")

rda_child <- dris %>%
  filter (dri_type == "Recommended Dietary Allowance (RDA)", age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  " 4-8 yr")) %>%
  group_by (nutrient) %>%
  summarise (unit = first(units),
             mean_rda = mean (value, na.rm = TRUE)) %>%
  mutate (nutrient = ifelse (nutrient == "Vitamin A", "Vitamin_A", nutrient))

# add omega 3, REMEMBER THIS IS AI
rda_child <- rbind (rda_child, ai_omega_child)


saveRDS(ds_spp_nutr_content, file = "Data/ds_spp_nutr_content_FishNutrientsGENuS.Rds")

## What are the most nutritious species?

ds_spp_nutr_content %>%
  select (species, nutrient, perc_rda) %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda)) %>%
  arrange (desc (micronutrient_density))
