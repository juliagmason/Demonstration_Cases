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
# amount is in original units, e.g. ug Selenium, mg Zinc

# plot available nutrients by country ----

# code vaguely following nutrient_endownment --> shiny --> edible_meat_bau_reforms.R
plot_nutr_avail_upside <- function (country_name) {
  
  nutr_avail_plot <- ds_prod_diff_nutr_totavail %>%
    filter (country == country_name,
            year %in% c(2040:2060)) %>%
    group_by (rcp, nutrient) %>%
    summarise (mean_avail = mean (amount, na.rm = TRUE)) %>%
    ungroup()
  # amount is in original units, e.g. ug Selenium, mg Zinc
  
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
# CF used _50perc for calculations
# this is summed for the whole country, with projected population growth (50% estimate; this is where the 50% comes from)

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
  # rename to match nutr_demand 
  mutate (nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vitamin A", 
                       TRUE ~ nutrient)) %>%
  left_join (nutr_demand_midcentury, by = c("country", "nutrient")) %>%
  # get rid of omega 3 and protein, don't have that info
  filter (!is.na (units))  %>%
  # convert to units
  mutate (
    #avail_mt = convert_supply_mt (mean_avail, units_input = units_short)
    avail_mt = ifelse (
      units_short == "µg",
      conv_unit (mean_avail, from = "ug", to = "metric_ton"),
      conv_unit (mean_avail, from = "mg", to = "metric_ton")
    ),
    demand_prop = avail_mt/supply_req_mt_yr_50perc
    )



demand_met_by_nutr %>%
    filter (country == "Peru") %>%
ggplot(aes(x=demand_prop, y=nutrient)) +
    facet_wrap(~ rcp, ncol = 4, scales = "free_x") +
    geom_bar(stat="identity") +
    # Labels
    labs(x="Percent of nutrient demand\nmet from capture fisheries reforms", y="") +
    # Theme
    theme_bw()


# just show percent -- this seems extremely low?
demand_met_by_nutr %>%
  filter (country == "Sierra Leone") %>%
  select (rcp, nutrient, demand_prop) %>%
  pivot_wider (names_from = nutrient,
               values_from = demand_prop)

# sanity check. peru supply req is 29036 MT CA, 300 MT iron, 20.2 MT vitamin A, 1.58 ug selenium. per year. 288 MT zinc. for avg 2040-2060
nutr_avail_allscen %>%
  filter (country == "Peru", year == 2050, rcp == "RCP26", scenario == "No Adaptation")
# mora moro, about 1 mt catch in 2050. that's 3642 mg zinc. There are 0.421 mg zinc/100g in fishnutrients
#1 mt * 1000kg/ton * 1000g/kg * 0.421 mg/100g = 4210 mg
1 * 1000 * 1000 * 0.421 / 100 

# from nutrient endowment code --> Step4_calculate_nutrient_demand_hist, DRIs intake for adult man is ~ 10mg/day for zinc. intake requirement maybe to overcome deficiency is more like 15-16. 
# peru population 2020 50% for 55 yo men is 757902. supply req is 12099939.812
12099939.812/757902 # 15.9, intake required. per day
# this is then summed across the whole population. peru required zinc, mg/day is 670619039 for 2020
# supply_req_mt_yr_50perc is the supply required * units / 1000 / 1000 * 365
# 670619039 mg/day * 1g/1000mg * 1kg/1000g * 1mt/1000 kg * 365 day/yr
670619039 /1000 /1000/1000 * 365 # 245, same as CF's calculation. so this is what the whole population needs in a year, in METRIC TONS

# in rcp26, mean avail zinc for peru is 13017943 mg
13017943 /1000/1000/1000 # 0.013 metric tons
# but nutricast has  significant zinc needs met, more like 5%. is there just a really big difference between MEY only and fully adaptive? he is only showing the reforms scenario. so showing total with reforms, not the difference. 

conv_unit (1, "Mg", "g")
conv_unit (1, "metric_ton", "g")

  

##### Do this by all management scenarios ----

# try to consolidate?

# from nutrient_endowment --> shiny --> v3 --> :Page3_Fig2c_reforms_prop_demand, line 84

# Function to calculate mt of nutrient from mt of edible meat
# units: mg, ug=mcg 
# meat_mt <- 29.88111; nutr_dens <- 35.5; nutr_dens_units <- "mg"
calc_nutr_supply_mt <- function(meat_mt, nutr_dens, nutr_dens_units){
  
  # Convert meat to grams
  # "Mg" is metric tons
  meat_g <- measurements::conv_unit(meat_mt, "Mg", "g")
  
  # Calculate amount of nutrient in density units
  nutrient_q <- meat_g *  nutr_dens
  
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



plot_nutr_avail_upside_scen <- function (country_name) {
  
  nutr_avail_plot <- nutr_avail_allscen %>%
    filter (country == country_name,
            year %in% c(2040:2060)) %>%
    group_by (rcp, scenario, nutrient) %>%
    summarise (mean_avail = mean (amount, na.rm = TRUE)) %>%
    ungroup() 
  # set levels
  nutr_avail_plot$scenario <- factor (nutr_avail_plot$scenario, levels = c("No Adaptation", "Productivity Only", "Range Shift Only", "Imperfect Productivity Only", "Imperfect Full Adaptation", "Full Adaptation"))
  
  ggplot (nutr_avail_plot, aes (y = mean_avail, x = scenario, fill = nutrient)) +
    geom_bar (stat = "identity", position = "dodge") +
    facet_wrap (~rcp) +
    geom_vline (xintercept = 0, lty = 2) +
    theme_bw() +
    labs(y = "Mean availability of nutrient, average of 2040-2060, units vary", y = "") +
    ggtitle (paste0("Nutrient availability under fisheries management scenarios at mid-century (2040-2060), ", country_name))
  
}

plot_demand_met_scen <- function (country_name) {
  
}



demand_met_allscen %>%
  filter (country == "Peru", !is.na(demand_prop)) %>%
  ggplot(aes(x=period, y=demand_prop, fill = nutrient)) +
  facet_grid(scenario~ rcp) +
  geom_bar(stat="identity", position = "dodge") +
  # Labels
  labs(x="Percent of nutrient demand\nmet from capture fisheries reforms", y="") +
  # Theme
  theme_bw()


demand_met_allscen %>%
  filter (country == "Peru", !is.na(demand_prop)) %>%
  ggplot(aes(x=demand_prop, y=period, fill = scenario)) +
  facet_grid(nutrient~ rcp) +
  geom_bar(stat="identity", position = "dodge") +
  # Labels
  labs(x="Percent of nutrient demand\nmet from capture fisheries reforms", y="") +
  # Theme
  theme_bw()




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

