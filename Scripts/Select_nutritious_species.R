# select most nutritious species 
# 3/25/2022
# JGM

library (tidyverse)

# species nutrient content, has baseline catch in mt
ds_spp_nutr_content <- readRDS("Data/ds_spp_nutr_content_FishNutrientsGENuS_RDA_groups.Rds")

# baseline catch with nutrition content in servings and mt
ds_catch_nutr_yield_baseline <- readRDS("Data/ds_catch_nutr_yield_baseline.Rds")

# catch proportions for context
ds_catch_props_baseline <- readRDS("Data/ds_spp_catch_proportions_baseline.Rds")

# projected upside
nutr_upside <- readRDS("Data/ds_nutr_upside.Rds")

# # two ways of looking at this. the species that are the most nutrient dense for that nutrient, and the species that are frequently caught and therefore providing a lot of that nutrient. 


# later could bring in which species will show the most gains under management reforms; which are the most climate vulnerable/fishing vulnerable from Maire et al



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

# looking at amounts per serving
ds_spp_nutr_content %>%
  select (country, species, catch_mt, nutrient, amount) %>%
  distinct() %>%
  left_join (ds_catch_props_baseline, by = c ("country", "species")) %>%
  group_by (country, nutrient) %>%
  slice_max (amount, n = 5) %>%
  select (country, nutrient, species, amount, prop_catch, rank_catch) %>%
  arrange (country, nutrient, desc (amount)) %>% View()

# important species in terms of density--one serving meets 25%, 30%? some cutoff of child rda ----

ds_spp_nutr_content %>%
  filter (country == "Sierra Leone", group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc"), perc_rda > 25) %>%
  left_join (ds_catch_props_baseline, by = c ("country", "species")) %>%
  arrange (nutrient, desc (perc_rda)) %>%
  select (nutrient, species, perc_rda, prop_catch, rank_catch) 

# important species in terms of total yield ----
ds_catch_nutr_yield_baseline %>%
  filter (country == "Sierra Leone",  nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc")) %>%
  select (nutrient, species, nutr_mt, amount) %>%
  group_by (nutrient) %>%
  slice_max (nutr_mt, n = 10) %>% 
  View()

# important species in terms of MEY upside ----
nutr_upside %>%
  filter (period == "2050-2060", country == "Sierra Leone") %>%
  group_by (rcp, nutrient) %>%
  slice_max (mey_diff_mt, n = 5) %>%
  arrange (rcp, nutrient, mey_diff_mt) %>%
  View()
  
nutr_upside %>%
  filter (period == "2050-2060", country == "Sierra Leone") %>%
  group_by (rcp, nutrient) %>%
  slice_max (mey_diff_child_rda, n = 5) %>%
  select (rcp, nutrient, species, mey_diff_child_rda) %>%
  View()
