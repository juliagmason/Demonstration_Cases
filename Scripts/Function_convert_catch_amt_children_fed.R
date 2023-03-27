# Function to convert amount of catch to nutrients/children fed
# 9/27/22

# just want a function where I can plug in a species and an amount and it spits out the children fed for each nutrient?



library (tidyverse)
#library (AFCD)

# using code from Species_level_nutrient_content.R

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

# # scylla serrata for indonesia
# s_serrata_nutr <- afcd_sci %>% 
#   filter (sciname == "Scylla serrata",
#           nutrient_code_fao %in% c(
#             "CA", "ZN", "FE", "SE", "Protein", "FAPU", "VITA")) %>%
#   group_by(nutrient_code_fao) %>%
#   summarise (amount = mean (value, na.rm = TRUE)) %>%
#   mutate (nutrient =
#             case_when (nutrient_code_fao == "CA" ~ "Calcium",
#                        nutrient_code_fao == "FE" ~ "Iron",
#                        nutrient_code_fao == "SE" ~ "Selenium", 
#                        nutrient_code_fao == "ZN" ~ "Zinc",
#                        nutrient_code_fao == "FAPU" ~ "Omega_3",
#                        nutrient_code_fao == "VITA" ~ "Vitamin_A"),
#           species = "Scylla serrata"
#   ) %>%
#   # reorder to match
#   select (species, nutrient, amount)

# # composite Stolephorus spp for Indonesia
# indo_stolephorus <- readRDS("Data/indo_stolephorus.Rds")
# # this is slightly different from the genus match bc brought in other anchovies. just use for species_specific
# 
# stoleph_nutr <- fishnutr_long %>%
#   filter (species %in% indo_stolephorus) %>%
#   group_by (nutrient) %>%
#   summarise (amount = mean (amount)) %>%
#   mutate (species = "Stolephorus") %>%
#   select (species, nutrient, amount)
# 
# # composite sardinella spp for Sierra Leone instead of full sardinella 
# sardinella_nutr <- fishnutr_long %>%
#   filter (species %in% c("Sardinella aurita", "Sardinella maderensis")) %>%
#   group_by (nutrient) %>%
#   summarise (amount = mean (amount)) %>%
#   mutate (species = "Sardinella") %>%
#   select (species, nutrient, amount)

# matched fishnutr data for missing species ----
# depends on country
fish_taxamatch_nutr <- readRDS("Data/Matched_finfish_nutr.Rds") 

# afcd data for nonfish ----

nonfish_afcd_nutr <- readRDS("Data/nonfish_afcd_nutr_compiled.Rds") %>%
  select (species, nutrient, amount)

# match back to taxa
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

nonfish_afcd_nutr <- nonfish_afcd_nutr %>%
  left_join (sau_2019_taxa, by = "species")


# use WHO RNI
rni_child <- readRDS("Data/RNI_child.Rds") 



calc_children_fed_func <- function (species_name, taxa, amount_mt, country_name) {
  
  p_edible <- case_when (
    taxa == "Finfish" ~ 0.87,
    taxa == "Crustacean" ~ 0.36,
    taxa == "Mollusc" ~ 0.17,
    taxa == "Cephalopod" ~ 0.67,
    taxa == "Other" ~ 1)
 # GENuS/nutricast is 0.21 for cephalopods. Using 0.67, Bianchi et al. 2022 value for D. gigas; only cephalopod in our priority species. They also have a blanket 0.7 value for cephalopods.  
  #if (taxa == "Finfish" & !species_name %in% c("Stolephorus", "Sardinella")) { # for pri_spp
  if (taxa == "Finfish" & species_name %in% fishnutr_long$species) {
    nutr_content <- fishnutr_long %>% filter (species == species_name)
  # } else if (species_name == "Stolephorus") {
  #   nutr_content = stoleph_nutr
  # } else if (species_name == "Sardinella") {
  #   nutr_content = sardinella_nutr
} else if (taxa == "Finfish" & !species_name %in% fishnutr_long$species) {
  nutr_content <- fish_taxamatch_nutr %>% filter (species == species_name, country == country_name)
  }
    else if (species_name == "Dosidicus gigas") {
    nutr_content = d_gigas_nutr
    }
  # } else if (species_name == "Scylla serrata") {
  #   nutr_content = s_serrata_nutr
  # }
  else if (taxa != "Finfish" & species_name != "Dosdicicus gigas") {
    nutr_content <- nonfish_afcd_nutr %>% filter (species == species_name)
  }   
  
  catch_nutrients <- nutr_content %>%
    mutate (catch_mt = amount_mt,
            # convert tons per year to 100g /day, proportion edible
            edible_servings = catch_mt * p_edible * 1000 * 1000 /100 / 365,
            nutrient_servings = edible_servings * amount) %>%
    left_join (rni_child, by = "nutrient") %>%
    mutate (children_fed = nutrient_servings / RNI) %>%
    select (nutrient, children_fed)
  

}

# s <- calc_children_fed_func("Dosidicus gigas", "Cephalopod", 151407)
# t <- calc_children_fed_func("Trachurus murphyi", "Finfish", 15429)
# m <- calc_children_fed_func ("Crassostrea gigas", "Mollusc", 200)
# s <- calc_children_fed_func ("Scylla serrata", "Crustacean", 55)
#a <- calc_children_fed_func("Stolephorus", "Finfish", 1000)
