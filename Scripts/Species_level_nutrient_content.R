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
fishnutr_mu <- fishnutr_prod %>%
  select (species, ends_with ("_mu")) 

# genus data for nonfish [eventually could use AFCD]
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T)

#truncate to the nutrients we're using for fishnutrients. have to figure out how to get omegas? for now, assume PUFAS = omega 3s
spp_key_sm <- spp_key %>% 
  select (species, major_group, genus_food_name, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg)


# join species and nutrients data
# just get relevant species for each country. Baseline year (2012) where catch > 0, unique species

ds_spp_nutr_content <- ds_spp %>%
  filter (year == 2012, rcp == "RCP26", scenario == "No Adaptation", catch_mt > 0) %>%
  # only select relevant columns
  select (country, species, catch_mt)  %>% 
  # join nutrient data
  left_join (spp_key_sm, by = "species") %>%
  left_join (fishnutr_mu, by = "species") %>%
  
  mutate (
    #recategorize, from nutricast code
    major_group=recode(genus_food_name,
                             "Cephalopods"="Cephalopods",
                             "Crustaceans"="Crustaceans",
                             "Demersal Fish"="Finfish",
                             "Marine Fish; Other"="Finfish",
                             "Molluscs; Other"="Molluscs",
                             "Pelagic Fish"="Finfish"),
    
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
                values_to = "amount")

saveRDS(ds_spp_nutr_content, file = "Data/ds_spp_nutr_content_FishNutrientsGENuS.Rds")


# What are the top five species that provide each nutrient? ----

# careful, get a lot of ties for nonfish. high iron and zinc levels

# also show proportion of overall catch
catch_tot <- ds_spp %>% 
  filter (year == 2012, rcp == "RCP26", scenario == "No Adaptation", catch_mt > 0) %>%
  group_by (country) %>%
  summarize (tot_cat = sum(catch_mt))

ds_spp_nutr_content %>%
  filter (!is.na (amount)) %>%
  group_by (country, nutrient) %>%
  slice_max (amount, n = 10) %>%
  ungroup() %>%
  arrange (country, nutrient, desc(amount)) %>% 
  left_join (catch_tot, by = "country") %>%
  mutate (prop_catch = catch_mt/tot_cat, .keep = "unused")

# spider plots for top catch for each country ----
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)
library (ggradar)

# make a function to do this by country

plot_spp_nutr_radar <- function (country_name, n_spp) {
  
  # grab desired # of species
  top_catch <- ds_spp %>% 
    filter (country == country_name, year == 2012, rcp == "RCP26", scenario == "No Adaptation", catch_mt > 0) %>%
    slice_max (catch_mt, n = n_spp)
  
  # filter nutrient content data
  nutr_radar_plot <- ds_spp_nutr_content %>%
    # from nutricast, express in terms of proportion of maximum. so first get proportion of maximum from within country catch, and then filter the top species
    filter (country == country_name) %>%
    group_by (nutrient) %>%
    mutate(amount_prop=amount/max(amount, na.rm = TRUE)) %>% 
    ungroup() %>%
    filter (species %in% top_catch$species) %>%
    select (-c(country, catch_mt, major_group, amount)) %>%
    pivot_wider (
                 names_from = nutrient,
                 values_from = amount_prop) %>%
    # radar plot can't deal with NA and non-fish don't have selenium
    replace_na (list (Selenium = 0)) 
  
  # calculate maximum value for plot specification; looks like will be calcium
  #max_value = max(nutr_radar_plot$Calcium)
  
  
    ggradar(nutr_radar_plot,
    grid.min = 0, grid.max = 1, 
    group.point.size = 1.5,
    group.line.width = 1,
    legend.text.size = 8,
    legend.position = "right") 
}

plot_spp_nutr_radar(country_name = "Sierra Leone", n_spp = 5)



  
