# Function to convert amount of catch to nutrients/children fed
# 9/27/22

# just want a function where I can plug in a species and an amount and it spits out the children fed for each nutrient?



library (tidyverse)

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

# genus data for nonfish [eventually could use AFCD]
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T)
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
  


rda_groups <- readRDS("Data/RDAs_5groups.Rds")
rda_child <- rda_groups %>% filter (group == "Child")

# use WHO RNI
rni_child <- readRDS("Data/RNI_child.Rds") 



calc_children_fed_func <- function (species_name, taxa, amount_mt) {
  
  p_edible <- case_when (
    taxa == "Finfish" ~ 0.87,
    taxa == "Crustacean" ~ 0.36,
    taxa == "Mollusc" ~ 0.17,
    taxa == "Cephalopod" ~ 0.67)
 # GENuS/nutricast is 0.21 for cephalopods. Using 0.67, Bianchi et al. 2022 value for D. gigas; only cephalopod in our priority species. They also have a blanket 0.7 value for cephalopods.  
  if (taxa == "Finfish") {
    nutr_content <- fishnutr_long %>% filter (species == species_name)
  } else if (species_name == "Dosidicus gigas") {
    nutr_content = d_gigas_nutr
  } else {
    nutr_content <- spp_key_long %>% filter (species == species_name)
  }   
  
  catch_nutrients <- nutr_content %>%
    mutate (catch_mt = amount_mt,
            # convert tons per year to 100g /day, proportion edible
            edible_servings = catch_mt * p_edible * 1000 * 1000 /100 / 365,
            nutrient_servings = edible_servings * amount) %>%
    left_join (rni_child, by = "nutrient") %>%
    mutate (children_fed = nutrient_servings / RNI)
  

}

s <- calc_children_fed_func("Dosidicus gigas", "Cephalopod", 151407)
t <- calc_children_fed_func("Trachurus murphyi", "Finfish", 15429)
m <- calc_children_fed_func ("Crassostrea gigas", "Mollusc", 200)
