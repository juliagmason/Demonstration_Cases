# Match SAU genus/family to Fishnutr data 
# 3/27/23
# started in check_SAU_nutricast_species

library (tidyverse)
library (stringr)


# For SAU data at genus or family level, take country-appropriate means from fishnutr data

# to make life a bit easier, do country-level nutrients search with lookup tool: https://fishbase.ca/Nutrients/NutrientSearch.php

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
# just grab species names
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

# actually do need country
sau_2019_country_spp <- readRDS("Data/SAU_2019.Rds") %>%
  ungroup() %>%
  select (country, species) %>%
  distinct()

# nutrient data ----
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")

# Function to match SAU and country-specific fishnutr data ----

# could prob expand this to country

match_fishnutr_taxa <- function (fishnutr_df, species_name) {
  
  species_name <- as.character(species_name)
  
  # if identified to species level, clip to Genus
  if (grepl(" ", species_name)) {species_name = word(species_name, 1)}
  
  if (grepl ("ae", str_sub(species_name, -2, -1))) {
    match <- filter (fishnutr_df, Family == species_name)
  } else {
    match <- filter (fishnutr_df, Genus == species_name)
  }
  
  match_nutr <- match %>%
    group_by (nutrient) %>%
    summarise (amount = mean (amount, na.rm = TRUE))
  
  return (tibble(match_nutr))
  
}

# Peru ----

sau_peru_missing <- sau_2019_country_spp %>%
  filter (country == "Peru") %>%
  left_join (sau_2019_taxa, by = "species") %>%
  filter (taxa == "Finfish",  !species %in% fishnutr$species) 
# 28, of which matched 19


# country specific fishnutr, convert to long
fishnutr_peru <- read.csv("Data/FishNutrients_country/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_PERU.csv") %>%
  # just select means
  select (Scientific.Name, Family, ends_with ("100g.")) %>%
  pivot_longer (Calcium..mg.100g.:Zinc..mg.100g.,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (Genus = word (Scientific.Name, 1),
          nutrient = case_when (
            grepl ("Calc", nutrient) ~ "Calcium",
            grepl ("Iron", nutrient) ~ "Iron",
            grepl ("Prot", nutrient) ~ "Protein",
            grepl ("Sel", nutrient) ~ "Selenium",
            grepl ("Omeg", nutrient) ~ "Omega_3",
            grepl ("Vitam", nutrient) ~"Vitamin_A",
            grepl ("Zinc", nutrient) ~ "Zinc"
          ))


peru_match_taxa <- sau_peru_missing %>%
  # had a TON of trouble with this for some reason!! not having the fishnutr_df in the list was what fixed it. 
  mutate (nutr = pmap(list(species_name = species), match_fishnutr_taxa, fishnutr_df = fishnutr_peru)
  ) %>%
  unnest (cols = c(nutr))

# Indonesia ----

sau_indo_missing <- sau_2019_country_spp %>%
  filter (country == "Indonesia") %>%
  left_join (sau_2019_taxa, by = "species") %>%
  filter (taxa == "Finfish",  !species %in% fishnutr$species) 
# 72, of which got 62


# country specific fishnutr, convert to long
fishnutr_indo <- read.csv("Data/FishNutrients_country/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_INDONESIA.csv") %>%
  # just select means
  select (Scientific.Name, Family, ends_with ("100g.")) %>%
  pivot_longer (Calcium..mg.100g.:Zinc..mg.100g.,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (Genus = word (Scientific.Name, 1),
          nutrient = case_when (
            grepl ("Calc", nutrient) ~ "Calcium",
            grepl ("Iron", nutrient) ~ "Iron",
            grepl ("Prot", nutrient) ~ "Protein",
            grepl ("Sel", nutrient) ~ "Selenium",
            grepl ("Omeg", nutrient) ~ "Omega_3",
            grepl ("Vitam", nutrient) ~"Vitamin_A",
            grepl ("Zinc", nutrient) ~ "Zinc"
          ))


indo_match_taxa <- sau_indo_missing %>%
  mutate (nutr = pmap(list(species_name = species), match_fishnutr_taxa, fishnutr_df = fishnutr_indo)
  ) %>%
  unnest (cols = c(nutr))

length (unique (indo_match_taxa$species))

# Sierra Leone ----
sau_sl_missing <- sau_2019_country_spp %>%
  filter (country == "Sierra Leone") %>%
  left_join (sau_2019_taxa, by = "species") %>%
  filter (taxa == "Finfish",  !species %in% fishnutr$species) 
# 66, of which got 48


# country specific fishnutr, convert to long
fishnutr_sl <- read.csv("Data/FishNutrients_country/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_SIERRA_LEONE.csv") %>%
  # just select means
  select (Scientific.Name, Family, ends_with ("100g.")) %>%
  pivot_longer (Calcium..mg.100g.:Zinc..mg.100g.,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (Genus = word (Scientific.Name, 1),
          nutrient = case_when (
            grepl ("Calc", nutrient) ~ "Calcium",
            grepl ("Iron", nutrient) ~ "Iron",
            grepl ("Prot", nutrient) ~ "Protein",
            grepl ("Sel", nutrient) ~ "Selenium",
            grepl ("Omeg", nutrient) ~ "Omega_3",
            grepl ("Vitam", nutrient) ~"Vitamin_A",
            grepl ("Zinc", nutrient) ~ "Zinc"
          ))


sl_match_taxa <- sau_sl_missing %>%
  mutate (nutr = pmap(list(species_name = species), match_fishnutr_taxa, fishnutr_df = fishnutr_sl)
  ) %>%
  unnest (cols = c(nutr))

length (unique (sl_match_taxa$species))


#Chile ----
chl_landings  <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

chl_missing <- chl_landings %>%
  filter (taxa == "Finfish", 
          !species %in% fishnutr$species) %>%
  select (species, taxa) %>%
  distinct()
# 13, got 6

fishnutr_chl <- read.csv("Data/FishNutrients_country/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_CHILE.csv") %>%
  # just select means
select (Scientific.Name, Family, ends_with ("100g.")) %>%
  pivot_longer (Calcium..mg.100g.:Zinc..mg.100g.,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (Genus = word (Scientific.Name, 1),
          nutrient = case_when (
            grepl ("Calc", nutrient) ~ "Calcium",
            grepl ("Iron", nutrient) ~ "Iron",
            grepl ("Prot", nutrient) ~ "Protein",
            grepl ("Sel", nutrient) ~ "Selenium",
            grepl ("Omeg", nutrient) ~ "Omega_3",
            grepl ("Vitam", nutrient) ~"Vitamin_A",
            grepl ("Zinc", nutrient) ~ "Zinc"
          ))

chl_match_taxa <- chl_missing %>%
  mutate (nutr = pmap(list(species_name = species), match_fishnutr_taxa, fishnutr_df = fishnutr_chl)
  ) %>%
  unnest (cols = c(nutr)) %>%
  mutate (country = "Chile") %>%
  select (country, species, taxa, nutrient, amount)

length (unique (chl_match_taxa$species))

# compile as rds ----
matched_sau <- rbind (indo_match_taxa, peru_match_taxa, sl_match_taxa) %>%
  select (country, species, taxa, nutrient, amount) %>%
  rbind (chl_match_taxa)


saveRDS(matched_sau, file = "Data/Matched_finfish_nutr.Rds")
