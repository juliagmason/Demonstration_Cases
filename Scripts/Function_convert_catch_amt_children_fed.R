# Function to convert amount of catch to nutrients/children fed
# 9/27/22

# just want a function where I can plug in a species and an amount and it spits out the children fed for each nutrient?

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



calc_children_fed_func <- function (species_name, amount_mt, country_name) {

  if (species_name %in% compiled_nutr$species) {
    nutr_content <- compiled_nutr %>% filter (species == species_name)
    } else  {
  nutr_content <- fish_taxamatch_nutr %>% filter (species == species_name, country == country_name)
  }

  
  catch_nutrients <- nutr_content %>%
    mutate (catch_mt = amount_mt,
            p_edible = case_when (
              taxa == "Finfish" ~ 0.87,
              taxa == "Crustacean" ~ 0.36,
              taxa == "Mollusc" ~ 0.17,
              # GENuS/nutricast is 0.21 for cephalopods. Using 0.67, Bianchi et al. 2022 value for D. gigas; only cephalopod in our priority species. They also have a blanket 0.7 value for cephalopods.  
              taxa == "Cephalopod" & species == "Dosidicus gigas" ~ 0.67,
              taxa == "Cephalopod" & species != "Dosidicus gigas" ~ 0.21,
              taxa == "Other" ~ 1,
              taxa == "Algae" ~ 1),
            # convert tons per year to 100g /day, proportion edible
            edible_servings = catch_mt * p_edible * 1000 * 1000 /100 / 365,
            nutrient_servings = edible_servings * amount) %>%
    left_join (rni_child, by = "nutrient") %>%
    mutate (rni_equivalents = nutrient_servings / RNI) %>%
    select (nutrient, rni_equivalents)
  

}

# s <- calc_children_fed_func("Dosidicus gigas", 151407, "Chile")
# t <- calc_children_fed_func("Trachurus murphyi", "Finfish", 15429)
# m <- calc_children_fed_func ("Crassostrea gigas", "Mollusc", 200)
# s <- calc_children_fed_func ("Scylla serrata", "Crustacean", 55)
#a <- calc_children_fed_func("Stolephorus", 1000, "Indonesia")

#ef_nutr <- calc_children_fed_func("Ethmalosa fimbriata",  128815, "Sierra Leone")
# alg <- calc_children_fed_func("Callophyllis variegata", 1000, "Chile")
# oct <- calc_children_fed_func("Octopus vulgaris", 199, "Chile")
# 
# png ("Figures/SL_Efim_children_fed.png", height = 4, width = 4, units = "in", res = 300)
# ef_nutr %>%
#   filter (!nutrient %in% c("Selenium", "Vitamin_A", "Iron")) %>%
#   ggplot (aes (x = reorder(nutrient, -children_fed), y= children_fed/1000000)) +
#   geom_col() +
#   theme_bw() +
#   theme (axis.text = element_text (size = 15)) +
#   labs (x = "", y = "")
# dev.off()
