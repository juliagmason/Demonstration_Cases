# Pull malawi species nutrition information
# 6/23/23
# for Emma

library (tidyverse)


# using code from Species_level_nutrient_content.R

# nutrient data ----
# compiled in compile_species_nutrition_data.R
#this has Fishnutr, AFCD, and D. gigas
compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")


# matched fishnutr data for missing species ----
# depends on country. Finfish only; potentially could do with nonfish, but might have them all through AFCD?
fish_taxamatch_nutr <- readRDS("Data/Matched_finfish_nutr.Rds") 


# use WHO RNI
rni_child <- readRDS("Data/RNI_child.Rds") 
 

# Clean_Malawi_landings.R
mal_landings <- readRDS("Data/Malawi_landings_cleaned.Rds")


mal_spp_nutr <- compiled_nutr %>%
  filter (species %in% mal_landings$species) %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (nutr_units = case_when (
    nutrient %in% c("Calcium", "Iron", "Zinc") ~ "mg",
    nutrient %in% c("Selenium", "Vitamin_A") ~ "mcg",
    nutrient %in% c("Protein", "Omega_3") ~ "g"),
          perc_rni = amount/RNI * 100,
          #perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup() %>%
  select (species, nutrient, amount, nutr_units, RNI, perc_rni)

write.csv(mal_spp_nutr, file = "Data/Malawi_spp_nutrient_content_20230626.csv", row.names = FALSE)


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
  ungroup() =
