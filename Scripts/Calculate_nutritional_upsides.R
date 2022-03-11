## Calculate nutritional upside
# 2 14 22
# JGM

library (tidyverse)


# Species specific projection data from Chris Free 2020

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 

# imperfect scenarios have big fluctuations. stick to the 4 main ones, and skip burn-in
# I think what we want is productivity only, compare to BAU

# filtering out zero catch, hopefully helps make the dataset more manageable
ds_prod_diff <- ds_spp %>%
  filter (year > 2025, catch_mt > 0, scenario %in% c("No Adaptation", "Productivity Only")) %>%
  select (rcp, scenario, country, species, year, msy_mt, biomass_mt, catch_mt, profits_usd, bbmsy, ffmsy) %>%
  pivot_longer(c(msy_mt:ffmsy),
               names_to = "metric",
               values_to = "value") %>%
  group_by (rcp, country, species, year, metric) %>%
  summarize (diff = value[scenario == "Productivity Only"] - value[scenario == "No Adaptation"]) %>%
  filter (!is.na(diff))

saveRDS(ds_prod_diff, file = "Data/ds_prod_diff.Rds")


# differences in other metrics--catch, profits ----

png ("Figures/Diff_catch_MEY_byspecies.png", width = 14, height = 6, units = "in", res = 300)
ds_prod_diff %>%
  filter (metric == "catch_mt") %>%
  ggplot (aes (x = year, y = diff)) +
  geom_area (aes (fill = species)) +
  facet_grid(country ~ rcp, scales = "free") +
  guides (fill = "none") + 
  theme_bw() +
  ggtitle ("Difference in catch (MT) by achieving MEY")
dev.off()

png ("Figures/Diff_profits_MEY_byspecies.png", width = 14, height = 6, units = "in", res = 300)
ds_prod_diff %>%
  filter (metric == "profits_usd") %>%
  ggplot (aes (x = year, y = diff)) +
  geom_area (aes (fill = species)) +
  facet_grid(country ~ rcp, scales = "free") +
  guides (fill = "none") + 
  theme_bw() +
  ggtitle ("Difference in profits (USD) by achieving MEY")
dev.off()

# Convert difference to nutrients using FishNutrients data ----
# tidied productivity only vs BAU
ds_prod_diff <- readRDS("Data/ds_prod_diff.Rds")

# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
#"a flat file with summary statistics for each nutrient for each species, including the scientific name of the species (species), the FishBase species code (spec_code), a highest posterior predictive density value (X_mu), a lower 95% highest posterior predictive density interval (X_l95), a lower 50% highest posterior predictive density interval (X_l50), an upper 50% highest posterior predictive density interval (X_h95), and an upper 95% highest posterior predictive density interval (X_h95)."

# summarise fishnutr, just estimate and an average?? for most nutritious?? robinson et al. 2022 sums, but that can't be right for different units? check their code

# do fishnutr
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 



# use genus data for non-fish
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T)

#truncate to the nutrients we're using for fishnutrients. have to figure out how to get omegas? use PUFAs for now
spp_key_sm <- spp_key %>% 
  select (species, major_group, genus_food_name, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg)


# total nutrient availability ----
ds_prod_diff_nutr_totavail <- ds_prod_diff %>%
  filter (metric == "catch_mt") %>%
  left_join (spp_key_sm, by = "species") %>%
  # Step 3. Convert catch to edible meat
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
         # multiply by diff by proportion edible, convert to grams. d
         meat_g= diff * pedible * 1000*1000/100) %>% 
  left_join (fishnutr_mu, by = "species") %>%
# convert to amount available. have to convert mt to grams, and divide by 100 grams
  mutate (
    # no selenium in genus
    Selenium = meat_g * Selenium_mu, 
    Zinc = ifelse (major_group == "Finfish", 
                   meat_g * Zinc_mu,
                   meat_g * zinc_mg),
    Protein = ifelse (major_group == "Finfish",
                      meat_g * Protein_mu,
                      meat_g * protein_g),
    #### NEED to figure out omega 3 situation #####
    Omega_3 = ifelse (major_group == "Finfish",
                         meat_g * Omega_3_mu, 
                         meat_g * polyunsaturated_fatty_acids_g),
    Calcium = ifelse (major_group == "Finfish",
                      meat_g * Calcium_mu,
                      meat_g * calcium_mg),
    Iron = ifelse (major_group == "Finfish",
                   meat_g * Iron_mu,
                   meat_g * iron_mg),
    Vitamin_A = ifelse (major_group == "Finfish",
                        meat_g * Vitamin_A_mu,
                        meat_g * vitamin_a_mcg_rae),
    .keep = "unused"
  ) %>%
  pivot_longer (Selenium:Vitamin_A,
                names_to = "nutrient",
                values_to = "amount") 


# plot available nutrients by country ----

# code vaguely following nutrient_endownment --> shiny --> edible_meat_bau_reforms.R
plot_nutr_avail_upside <- function (country_name) {
  
  nutr_avail_plot <- ds_prod_diff_nutr_totavail %>%
    filter (country == country_name,
            year %in% c(2040:2060)) %>%
    group_by (rcp, nutrient) %>%
    summarise (mean_avail = mean (amount, na.rm = TRUE)) %>%
    ungroup() 
  
    ggplot (nutr_avail_plot, aes (y = nutrient, x = mean_avail)) +
    geom_bar (stat = "identity") +
    facet_wrap (~rcp, ncol = 4, scales = "free_x") +
      geom_vline (xintercept = 0, lty = 2) +
    theme_bw() +
      labs(x = "Mean availability of nutrient, average of 2040-2060, units vary", y = "") +
    ggtitle (paste0("Additional available nutrients from fisheries reforms at mid-century (2040-2060), ", country_name))
  
}

plot_nutr_avail_upside ("Chile")





# plot nutrient needs met ----

#### *** also check out ouptut --> nutr_deficiencies_by_cntry_Sex_age

# looking at nutrient_endowment --> shiny -->v3 --> Page3_Figc_reforms_prop_demand.R, code 


# nutrition demand by country, from nutricast code --> calc_nutr_deficiencies/Step4_calculate_nutrient_demand_hist_proj.R
# supply required is in metric tons for the whole year
nutr_demand <- readRDS(file.path ("../nutrient_endowment/output/1960_2100_nutrient_demand_by_country.Rds"))

# take mid century average
nutr_demand_midcentury <- nutr_demand %>%
  filter (year %in% c(2040:2060)) %>%
  group_by (country, nutrient) %>%
  mutate (across (starts_with ("supply"), ~mean (.x))) %>%
  # this is doing what I want, but retaining the years; they all have the same value
  select (-year) %>%
  distinct()


# so i have to convert the totavail nutrients back into mt. right now they're in their native units. 
# can do overall by nutrient, and also by species?

# mot all of the nutrients are available. calcium, iron, selenium, vitamin a, zinc
library (measurements)

# can't use conv_unit inside mutate. make a new function? from nutricast code
# Function to calculate mt of nutrient from mt of edible meat
# units: mg, ug=mcg 
# meat_mt <- 29.88111; nutr_dens <- 35.5; nutr_dens_units <- "mg"
convert_supply_mt <- function(nutrient_input, units_input){
  unit <- ifelse (units_input == "µg", "ug", units_input)
  avail_mt <- conv_unit (nutrient_input, from = unit, to = "metric_ton")
  return (avail_mt)
}
  
  
# first group by nutrient
demand_met_by_nutr <- ds_prod_diff_nutr_totavail %>%
  filter (year %in% c(2040:2060)) %>%
  group_by (country, rcp, nutrient) %>%
  summarise (mean_avail = mean (amount, na.rm = TRUE)) %>%
  left_join (nutr_demand_midcentury, by = c("country", "nutrient")) %>%
  # get rid of omega 3 and protein, don't have that info
  filter (!is.na (units)) 

tmp <- demand_met_by_nutr %>%
  # convert to units
  mutate (
    #avail_mt = convert_supply_mt (mean_avail, units_input = units_short)
    avail_mt = ifelse (
      units_short == "µg",
      conv_unit (mean_avail, from = "ug", to = "metric_ton"),
      conv_unit (mean_avail, from = "mg", to = "metric_ton")
    )
  )

  

conv_unit (1, "mg", "metric_ton")























ds_nutr_diff_ear <- ds_prod_diff %>%
  filter (metric == "catch_mt") %>%
  left_join (spp_key, by = "species") %>%
  # Step 3. Convert catch to edible meat
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
         meat_mt= diff * pedible) %>%
  # second mutate to keep diff column
  mutate(
         # multiply meat mt by amount of nutrient per 100 g. would be meat_mt * 1000 kg/mt * 1000 g /kg / 100g. so meat_mt * 10000
         # Some need to be changed to match values for ear
         Calcium = meat_mt *10000 * calcium_mg,
         #calories_kcal = meat_mt *10000* calories_kcal,
         Carbohydrates = meat_mt *10000* carbohydrates_g,
         # copper in ug --- check this one, seems correct, might even want another 100x more?
         Copper = meat_mt *10000* copper_mg * 1000, 
         Fat = meat_mt *10000* fat_g,
         Folate = meat_mt *10000* folate_mcg,
         Iron = meat_mt *10000* iron_mg,
         Magnesium = meat_mt *10000* magnesium_mg,
         #monounsaturated_fatty_acids_g = meat_mt *10000* monounsaturated_fatty_acids_g,
         Niacin = meat_mt *10000* niacin_mg,
         Phosphorus = meat_mt *10000* phosphorus_mg,
         # linolenic is di-unsaturated? linoleic is highly consumed PUFA
         `Linoleic acid` = meat_mt *10000* polyunsaturated_fatty_acids_g,
         Potassium = meat_mt *10000* potassium_mg / 1000,
         Protein = meat_mt *10000* protein_g,
         Riboflavin = meat_mt *10000* riboflavin_mg,
         #saturated_fatty_acids_g = meat_mt *10000* saturated_fatty_acids_g,
         Sodium = meat_mt *10000* sodium_mg / 1000,
         Thiamin = meat_mt *10000* thiamin_mg,
         `Vitamin A` = meat_mt *10000* vitamin_a_mcg_rae,
         `Vitamin B6` = meat_mt *10000* vitamin_b6_mg,
         `Vitamin C` = meat_mt *10000* vitamin_c_mg,
         Zinc = meat_mt *10000* zinc_mg,
         .keep = "unused") %>% 
  pivot_longer (Calcium:Zinc,
                names_to = "nutrient",
                values_to = "amount") %>%
         
left_join (ear_avg, by = "nutrient") %>%
  mutate (inds_per_yr = (amount / mn_value / 365)) 

saveRDS(ds_nutr_diff_ear, file = "Data/ds_nutr_diff_inds_per_year.Rds")


# amount of nutrients available per capita per day
ds_nutr_diff_percap <- readRDS("Data/ds_nutr_diff_dcap.Rds")

# number of individuals each year for whom you could achieve EARs
ds_nutr_diff_ear <- readRDS("Data/ds_nutr_diff_inds_per_year.Rds")


# maybe take out protein
ds_nutr_diff_ear %>%
  filter (country == "Sierra Leone") %>%
  group_by (rcp, year, nutrient) %>%
  summarise (net_inds = sum(inds_per_yr, na.rm = TRUE)) %>% 
  ggplot () +
  #geom_bar (aes (x = year, y = net_inds, fill = nutrient), stat = "identity") +
    geom_area (aes (x = year, y = net_inds, fill = nutrient)) +
  facet_wrap (~rcp,  scales = "free")
# i think bar is easier to interpret

ds_nutr_diff_ear %>%
  filter (country == "Chile") %>%
  group_by (rcp, year, nutrient) %>%
  summarise (net_inds = sum(inds_per_yr, na.rm = TRUE)) %>%
  ggplot () +
  geom_bar (aes (x = year, y = net_inds, fill = nutrient), stat = "identity") +
  facet_wrap (~rcp,  scales = "free")

# Delve into AFCD data ----
sort (unique (afcd1$country))


# just look at top catch spp for each country

# sierra leone
sl_catch <- ds_spp %>%
  filter (country == "Sierra Leone", rcp == "RCP60", year == 2012, scenario == "No Adaptation", catch_mt > 0)
sl_top_catch <- ds_spp %>%
  filter (country == "Sierra Leone", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

sl_top5_afcd <- afcd1 %>%
  filter (sciname %in% sl_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "FAO INFOODS West Africa", "FAO Biodiv 3", "Liberia", "Côte d’Ivoire", "Ghana", "Senegal", "Mauritania", "Nigeria" )) 



# chile 
chl_top_catch <- ds_spp %>%
  filter (country == "Chile", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

chl_top5_afcd <- afcd1 %>%
  filter (sciname %in% chl_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "Chile", "FAO Latin Foods", "Peru", "Argentina", "FAO Biodiv 3", "Not provided in unformatted AFCD")) 

# Peru
peru_top_catch <- ds_spp %>%
  filter (country == "Peru", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

# can probably use the same sources?
peru_top5_afcd <- afcd1 %>%
  filter (sciname %in% peru_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "Chile", "FAO Latin Foods", "Peru", "Argentina", "FAO Biodiv 3", "Not provided in unformatted AFCD")) 

# indo
indo_top_catch <- ds_spp %>%
  filter (country == "Indonesia", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

indo_top5_afcd <- afcd1 %>%
  filter (sciname %in% indo_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "FAO Biodiv 3", "Not provided in unformatted AFCD", "Indonesia", "Pacific Region", "Malaysia"))


