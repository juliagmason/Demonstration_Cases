# Link Free projections to FishNutrients data 
# 3/9/22
# JGM

# Just want to look at the most nutritious species/compare species. Not looking at projections. 

library (tidyverse)

# Species specific projection data from Chris Free 2020 ----

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 

# "baseline" catch data, arbitrarily picking. mean of 2012-2020 catch for a sense of catch proportions
ds_baseline <- ds_spp %>% 
  filter (year %in% c(2012:2020), catch_mt > 0, rcp == "RCP26", scenario == "No Adaptation") %>%
  group_by (country, rcp, scenario, species) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE))

# nutrient data ----
# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
# this is per 100g raw, edible portion
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")

# truncate to just summary predicted value. eventually will want range?
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 

# genus data for nonfish [eventually could use AFCD]
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T)

#truncate to the nutrients we're using for fishnutrients. have to figure out how to get omegas? for now, assume PUFAS = omega 3s
# this is per 100g portion, but need to calculate proportion edible
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

# Recommended dietary allowance for each nutrient ----
# From Maire et al. 2021, use RDAs to calculate micronutrient density. Take percent of RDA met for each nutrient, capped at 100%, and then take sum for micronutrient density score. Maire et al did children between 6 mos and 5 years. 

# DRIS data from Free nutrient_endowment --> data/ears/data
dris <- readRDS("Data/dietary_reference_intake_data.Rds")

# no RDA for omega 3, only AI, adequate intake. 
ai_omega <- dris %>%
  filter (grepl("Linolenic", nutrient)) %>%
  mutate (group = 
            case_when (
              age_range %in% c("6-12 mo",  "1-3 yr",  "4-8 yr") ~ "Child",
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "None" ~ "Females", 
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "Pregnancy" ~ "Pregnant", 
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "Lactation" ~ "Lactating", 
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Males" ~ "Males",
              TRUE ~ NA_character_
            )) %>%
  group_by (nutrient, group) %>% 
  summarise (unit = first(units),
             mean_rda = mean (value, na.rm = TRUE)) %>%
  filter (!is.na(group)) %>%
  # match nutrient names; assume linolenic is omega 3
  mutate (nutrient = "Omega_3")

rda_groups <- dris %>%
  filter (dri_type == "Recommended Dietary Allowance (RDA)") %>%
  mutate (group = 
            case_when (
              age_range %in% c("6-12 mo",  "1-3 yr",  "4-8 yr") ~ "Child",
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "None" ~ "Females", 
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "Pregnancy" ~ "Pregnant", 
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "Lactation" ~ "Lactating", 
              !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Males" ~ "Males",
              TRUE ~ NA_character_
            )) %>%
  group_by (nutrient, group) %>% 
  summarise (unit = first(units),
             mean_rda = mean (value, na.rm = TRUE)) %>%
  mutate (nutrient = ifelse (nutrient == "Vitamin A", "Vitamin_A", nutrient)) %>%
  filter (!is.na(group), !is.na (mean_rda))

# add omega 3, REMEMBER THIS IS AI
# add group column to bind with children
rda_groups <- rbind (rda_groups, ai_omega) 



# join species and nutrients data ----
# just get relevant species for each country. Baseline year (2012-2020 mean) where catch > 0, unique species

ds_spp_nutr_content <- ds_baseline %>%
  # only select relevant columns
  select (country, species, catch_mt)  %>% 
  # join nutrient data
  left_join (spp_key_sm, by = "species") %>%
  left_join (fishnutr_mu, by = "species") %>%
  mutate(
    # select appropriate source for finfish vs. other. multiply by proportion edible for non fish
    Selenium =  Selenium_mu, 
    # dumb hack to keep major_group for later 
    Zinc = ifelse (grepl("Fish", genus_food_name), 
                    Zinc_mu,
                   zinc_mg),
    Protein = ifelse (grepl("Fish", genus_food_name),
                       Protein_mu,
                     protein_g),
    #### NEED to figure out omega 3 situation #####
    Omega_3 = ifelse (grepl("Fish", genus_food_name),
                      Omega_3_mu, 
                      polyunsaturated_fatty_acids_g),
    Calcium = ifelse (grepl("Fish", genus_food_name),
                       Calcium_mu,
                      calcium_mg),
    Iron = ifelse (grepl("Fish", genus_food_name),
                    Iron_mu,
                   iron_mg),
    Vitamin_A = ifelse (grepl("Fish", genus_food_name),
                         Vitamin_A_mu,
                        vitamin_a_mcg_rae),
    .keep = "unused"
  ) %>%
  pivot_longer (Selenium:Vitamin_A,
                names_to = "nutrient",
                values_to = "amount") %>%
  # join to rda data
  left_join (rda_groups, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup()
  



saveRDS(ds_spp_nutr_content, file = "Data/ds_spp_nutr_content_FishNutrientsGENuS_RDA_groups.Rds")

## What are the overall most nutritious species? ----

ds_spp_nutr_content %>%
  filter (group == "Child") %>%
  select (species, nutrient, perc_rda) %>%
  distinct() %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda)) %>%
  arrange (desc (micronutrient_density))


## Which species provide the most needed nutrients in each country? ----
# Chile: Calcium and vitamin A. 
# Indo: calcium and vitamin A, maybe zinc with golden
# Malawi: calcium, maybe vitamin A
# Peru: calcium, zinc
#Sierra leone: calcium, viatmin A, iron, zinc

# for this, doesn't matter RDA? but would want amount_mt?

# two ways of looking at this. the species that are the most nutrient dense for that nutrient, and the species that are frequently caught and therefore providing a lot of that nutrient. 

ds_spp_nutr_content %>%
  select (country, species, catch_mt, nutrient, amount) %>%
  distinct() %>%
  group_by (country, nutrient) %>%
  slice_max (amount, n = 5) %>%
  arrange (country, nutrient, desc (amount)) %>% View()
  
