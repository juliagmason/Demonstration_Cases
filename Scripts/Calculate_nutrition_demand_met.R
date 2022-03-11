# Calculate nutritional demand met
# 3/11/22

library (tidyverse)
library (measurements) # for unit conversions

# nutrient data ----
# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
#"a flat file with summary statistics for each nutrient for each species, including the scientific name of the species (species), the FishBase species code (spec_code), a highest posterior predictive density value (X_mu), a lower 95% highest posterior predictive density interval (X_l95), a lower 50% highest posterior predictive density interval (X_l50), an upper 50% highest posterior predictive density interval (X_h95), and an upper 95% highest posterior predictive density interval (X_h95)."

# from fishbase: Calcium is mg/100g. Iron is mg/100g, Selenium ug/100g, Zinc mg, Vit A ug, Omega 3 g, Protein g

# summarise fishnutr, just estimate and an average?? for most nutritious?? robinson et al. 2022 sums, but that can't be right for different units? check their code

# do fishnutr
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 



# use genus data for non-fish
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T)

#truncate to the nutrients we're using for fishnutrients. have to figure out how to get omegas? use PUFAs for now
spp_key_sm <- spp_key %>% 
  select (species, major_group, genus_food_name, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg)

# Species specific projection data from Chris Free 2020 ----

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 

# nutrition demand by country ----
#from nutricast code --> calc_nutr_deficiencies/Step4_calculate_nutrient_demand_hist_proj.R
# supply required is in metric tons for the whole year
nutr_demand <- readRDS(file.path ("../nutrient_endowment/output/1960_2100_nutrient_demand_by_country.Rds"))
# CF used _50perc for calculations
# this is summed for the whole country, with projected population growth (50% estimate; this is where the 50% comes from)


# from nutrient_endowment --> shiny --> v3 --> :Page3_Fig2c_reforms_prop_demand, line 84

# Function to calculate mt of nutrient from mt of edible meat ----
# units: mg, ug=mcg 
# meat_mt <- 29.88111; nutr_dens <- 35.5; nutr_dens_units <- "mg"

# adding--divide by 100 in here. I think Chris did that in his head but making me doubt everything
calc_nutr_supply_mt <- function(meat_mt, nutr_dens, nutr_dens_units){
  
  # Convert meat to grams
  # "Mg" is metric tons
  meat_g <- measurements::conv_unit(meat_mt, "Mg", "g")
  
  # Calculate amount of nutrient in density units. divide by 100 because density units are per 100g
  nutrient_q <- meat_g *  nutr_dens / 100
  
  # Calculate amount of nutrient in metric tons
  nutrient_mt <- measurements::conv_unit(nutrient_q, nutr_dens_units, "Mg")
  
  # Return
  return(nutrient_mt)
  
}

nutr_avail_allscen_spp <- ds_spp %>%
  filter (year > 2025, catch_mt > 0) %>%
  select (rcp, scenario, country, species, year, catch_mt) %>%
  # add genus nutrient data/edible proportion
  left_join (spp_key_sm, by = "species") %>%
  # determine amt edible meat from cath
  filter(!is.na(genus_food_name)) %>% 
  mutate(major_group=recode(genus_food_name,
                            "Cephalopods"="Cephalopods",
                            "Crustaceans"="Crustaceans",
                            "Demersal Fish"="Finfish",
                            "Marine Fish; Other"="Finfish",
                            "Molluscs; Other"="Molluscs",
                            "Pelagic Fish"="Finfish"),
         pedible=recode(major_group, 
                        "Finfish"=0.87, 
                        "Crustaceans"=0.36, 
                        "Molluscs"=0.17, 
                        "Cephalopods"=0.21), 
         # edible meat in mt
         meat_mt = catch_mt * pedible
  ) %>%
  # add Fishnutrient data 
  left_join (fishnutr_mu, by = "species") %>%
  # convert to amount nutrient available. have to convert mt to grams, and divide by 100 grams
  mutate (
    # no selenium in genus
    Selenium = calc_nutr_supply_mt(meat_mt, Selenium_mu, "ug"), 
    Zinc = ifelse (major_group == "Finfish", 
                   calc_nutr_supply_mt(meat_mt, Zinc_mu, "mg"),
                   calc_nutr_supply_mt(meat_mt, zinc_mg, "mg")),
    Protein = ifelse (major_group == "Finfish",
                      calc_nutr_supply_mt(meat_mt, Protein_mu, "g"),
                      calc_nutr_supply_mt(meat_mt, protein_g, "g")),
    #### NEED to figure out omega 3 situation #####
    Omega_3 = ifelse (major_group == "Finfish",
                      calc_nutr_supply_mt(meat_mt, Omega_3_mu, "g"), 
                      calc_nutr_supply_mt(meat_mt, polyunsaturated_fatty_acids_g, "g")),
    Calcium = ifelse (major_group == "Finfish",
                      calc_nutr_supply_mt(meat_mt, Calcium_mu, "mg"),
                      calc_nutr_supply_mt(meat_mt, calcium_mg, "mg")),
    Iron = ifelse (major_group == "Finfish",
                   calc_nutr_supply_mt(meat_mt, Iron_mu, "mg"),
                   calc_nutr_supply_mt(meat_mt, iron_mg, "mg")),
    Vitamin_A = ifelse (major_group == "Finfish",
                        calc_nutr_supply_mt(meat_mt, Vitamin_A_mu, "ug"),
                        calc_nutr_supply_mt(meat_mt, vitamin_a_mcg_rae, "ug")),
    .keep = "unused"
  ) %>%
  pivot_longer (Selenium:Vitamin_A,
                names_to = "nutrient",
                values_to = "amount_mt") 


# doesn't make sense to do by year bc nutr_demand is in 5 year blocks. do three periods, 2020-2030, 2050-2060, 2090-2100
nutr_demand_periods <- nutr_demand %>%
  mutate (period = case_when (
    year %in% c(2020:2030) ~ "2020-2030",
    year %in% c(2050:2060) ~ "2050-2060",
    year %in% c(2090:2100) ~ "2090-2100"
  )) %>%
  filter (!is.na(period)) %>%
  group_by (country, nutrient, period) %>%
  summarise (mean_supply_req = mean (supply_req_mt_yr_50perc, na.rm = TRUE))


demand_met_allscen <- nutr_avail_allscen_spp %>%
  group_by (country, rcp, scenario, year, nutrient) %>%
  summarise (amount_mt = sum (amount_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  # break into periods, convert nutrient names
  mutate (
    nutrient = 
      case_when (nutrient == "Vitamin_A" ~ "Vitamin A", 
                 TRUE ~ nutrient),
    period = case_when (
      year %in% c(2020:2030) ~ "2020-2030",
      year %in% c(2050:2060) ~ "2050-2060",
      year %in% c(2090:2100) ~ "2090-2100"
    )) %>%
  filter (!is.na(period)) %>%
  # summarise by period 
  group_by (country, rcp, scenario, period, nutrient) %>%
  summarise (mean_avail_mt = mean (amount_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  # add nutrient demand data
  left_join (nutr_demand_periods, by = c("country", "nutrient", "period")) %>%
  mutate (demand_prop = mean_avail_mt/mean_supply_req) 

saveRDS(demand_met_allscen, file = "Data/nutr_demand_met_allscen.Rds")
