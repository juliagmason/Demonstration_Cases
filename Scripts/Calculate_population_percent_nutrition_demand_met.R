# Plot nutrient contribution as percent population demand met
# 4/3/23
# JGM

library (tidyverse)

# revisiting nutricast code, demonstration_cases_SAU_SSF_explore
# https://github.com/cfree14/nutrient_endowment/blob/master/code/calc_nutr_deficiencies/Step4_calculate_nutrient_demand_hist_proj.R

# joins to nutrient data in shiny/v3/page3_Fig2c at the bottom

pop_proj_req_supply <- readRDS("../nutrient_endowment/output/1960_2100_nutrient_demand_by_country.Rds")
# this is nutrient supply required (in metric tons) to meet projected and historical nutrient demand of 50 and 95% of the population, sensitive to age and sex. 
# based on EARs and excludes children
# this is per year, has been multiplied by 365

# take current landings, convert to metric tons of nutrients, convert to proportion of current population
# start with Peru

sau_2019 <- readRDS("Data/SAU_2019.Rds")

sau_peru <- sau_2019 %>%
  filter (country == "Peru") %>%
  group_by (species) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE))

# nutrient data ----
# compiled in compile_species_nutrition_data.R
#this has Fishnutr, AFCD, and D. gigas
#this is amount in native units per 100g serving

compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")
# matched fishnutr data for missing species ----
# depends on country. Finfish only; potentially could do with nonfish, but might have them all through AFCD?
fish_taxamatch_nutr <- readRDS("Data/Matched_finfish_nutr.Rds") 

#this will return the amount of edible nutrient yield in metric tons
convert_catch_to_nutr_tons <- function (species_name, catch_mt, country_name) {
  
  if (species_name %in% compiled_nutr$species) {
    nutr_content <- compiled_nutr %>% filter (species == species_name)
  } else  {
    nutr_content <- fish_taxamatch_nutr %>% filter (species == species_name, country == country_name)
  }
  
  catch_nutrients <- nutr_content %>%
    mutate (
      p_edible = case_when (
        taxa == "Finfish" ~ 0.87,
        taxa == "Crustacean" ~ 0.36,
        taxa == "Mollusc" ~ 0.17,
        # GENuS/nutricast is 0.21 for cephalopods. Using 0.67, Bianchi et al. 2022 value for D. gigas; only cephalopod in our priority species. They also have a blanket 0.7 value for cephalopods.  
        taxa == "Cephalopod" ~ 0.67,
        taxa == "Other" ~ 1),
      # have to put back into units
      scalar = case_when (
        nutrient %in% c("Protein", "Omega_3") ~ 1,
        nutrient %in% c("Calcium", "Zinc", "Iron") ~ 1/1000,
        nutrient %in% c("Vitamin_A", "Selenium") ~ 1/1e6
      ),
      # input (catch_mt) is in metric tons. amount is in units / 100g. so divide by 100 to account for serving size, and multiply by scalar to cancel out g
      nutr_tonnes = catch_mt * p_edible * amount * scalar / 100 ) %>%
    select (nutrient, nutr_tonnes)
  
  return (catch_nutrients)
}

#convert_catch_to_nutr_tons ("Engraulis ringens", 100, "Peru")

sau_peru_nutr <- sau_peru %>%
  mutate (nutr_yield = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Peru"), convert_catch_to_nutr_tons)) %>%
             unnest(cols = c(nutr_yield),  names_repair = "check_unique") %>%
  group_by (nutrient) %>%
  summarise (nutr_tonnes = sum (nutr_tonnes, na.rm = TRUE))

t <- pop_proj_req_supply %>%
  filter (country == "Peru", year == 2020) %>%
  mutate (nutrient = case_when (nutrient == "Vitamin A" ~ "Vitamin_A",
                                TRUE ~ nutrient)) %>%
  left_join (sau_peru_nutr, by = "nutrient") %>%
  mutate (prop_demand_met = nutr_tonnes / supply_req_mt_yr_50perc)


# plot aggregate landings as demand met ----
# bring back in sau_2019_taxa for groups
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

peru_demand_current <- pop_proj_req_supply %>%
  filter (country == "Peru", year == 2020) %>%
  mutate (nutrient = case_when (nutrient == "Vitamin A" ~ "Vitamin_A",
                                TRUE ~ nutrient))

a <- sau_2019 %>%
  filter(country == "Peru") %>%
  left_join (sau_2019_taxa, by = "species") %>%
  group_by (species, commercial_group) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Peru"), convert_catch_to_nutr_tons)) %>%
  unnest(cols = c(nutr_tonnes),  names_repair = "check_unique") %>%
  left_join (peru_demand_current, by = "nutrient") %>%
  mutate (prop_demand_met = nutr_tonnes / supply_req_mt_yr_50perc)

a %>%
  group_by (nutrient, commercial_group) %>%
  summarise (prop_demand_met = sum (prop_demand_met, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(nutrient, -prop_demand_met, na.rm = TRUE), y = prop_demand_met*100, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Proportion demand met\nMost recent year of landings, Peru") +
  labs (x = "", y = "% population EARs met", fill = "Comm. group") 

# png ("Figures/Peru_aggregate_landings_perc_demand_met.png", width = 5, height = 4, units = "in", res = 300)  
# print(
#   plot_sau_rnis_met("Peru") +
#     theme ( 
#       axis.text.y = element_text (size = 13),
#       axis.text.x = element_text (size = 11),
#       axis.title = element_text (size = 16),
#       strip.text = element_text(size = 16),
#       legend.text = element_text (size = 10),
#       legend.title = element_text (size = 12),
#       plot.title = element_text (size = 18),
#       legend.position = "none"
#     )
# )
# dev.off()


indo_pop_needs_met <- sau_2019 %>%
  filter(country == "Indonesia") %>%
  left_join (sau_2019_taxa, by = "species") %>%
  group_by (species, commercial_group) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Indonesia"), convert_catch_to_nutr_tons)) %>%
  unnest(cols = c(nutr_tonnes),  names_repair = "check_unique") %>%
  left_join (peru_demand_current, by = "nutrient") %>%
  mutate (prop_demand_met = nutr_tonnes / supply_req_mt_yr_50perc)

indo_pop_needs_met %>%
  group_by (nutrient, commercial_group) %>%
  summarise (prop_demand_met = sum (prop_demand_met, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(nutrient, -prop_demand_met, na.rm = TRUE), y = prop_demand_met*100, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Proportion demand met\nMost recent year of landings, Indonesia") +
  labs (x = "", y = "% population EARs met", fill = "Comm. group") 
