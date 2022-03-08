## Calculate nutritional upside
# 2 14 22
# JGM

library (tidyverse)
library (AFCD)


# Species specific projection data from Chris Free 2020

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 

# data overview: plot different management scenarios for each country and rcp ----

# imperfect scenarios have big fluctuations. stick to the 4 main ones, and skip burn-in
ds_spp %>%
  filter (!scenario %in% c("Imperfect Full Adaptation", "Imperfect Productivity Only"), year > 2025) %>%
  group_by (country, rcp, scenario, year) %>%
  summarise (tot_cat = sum (catch_mt)) %>%
  ggplot (aes (x = year, y = tot_cat, col = scenario)) +
  geom_line () +
  facet_grid (~country ~ rcp, scales = "free") +
  theme_bw() +
  labs (y = "Total Catch, mt", x = "") +
  ggtitle ("Total projected catch under four management scenarios, \n Costello/Gaines")

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

# Convert difference to nutrients using GENUS data ----
# nutrient key from nutrient_endowment
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T) 

# Read projected human population growth
pop_growth <- readRDS(file.path( "../nutrient_endowment/data/population_growth/processed/WB_UN_1960_2100_human_population_by_country.Rds"))


# from nutrient_endowment, gives per capita availability by dividing by population growth. but we might want to say how many additional people you could feed?

# taking forever, so just play with 1% of the data until the code is cleaner
ds_prod_diff <- readRDS("Data/ds_prod_diff.Rds")

ds_prod_diff_small <- ds_prod_diff %>% group_by (rcp, country) %>% sample_frac (0.01)

ds_nutr_diff_percap <- ds_prod_diff %>%
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
  # Step 4. Add human population size
  left_join(pop_growth %>% select(country, year, pop_size_50perc), by=c("country", "year")) %>% 
  rename(human_pop_size=pop_size_50perc) %>% 
  # Step 5. Calculate daily per capita meat availability [ now ADDITIONAL availability from improved mgmt]
  mutate(meat_g_dcap= (meat_mt*1000*1000) / 365 / human_pop_size) %>% 
  # Step 6. Calculate daily per capita nutrient availability
  # ** divide by 100 because genus data provides values per 100g serving
  mutate(calcium = meat_g_dcap * calcium_mg / 100,
         calories = meat_g_dcap * calories_kcal / 100,
         carbohydrates = meat_g_dcap * carbohydrates_g / 100,
         copper = meat_g_dcap * copper_mg / 100,
         fat = meat_g_dcap * fat_g / 100,
         folate = meat_g_dcap * folate_mcg / 100,
         iron = meat_g_dcap * iron_mg / 100,
         magnesium = meat_g_dcap * magnesium_mg / 100,
         monounsaturated_fatty_acids = meat_g_dcap * monounsaturated_fatty_acids_g / 100,
         niacin = meat_g_dcap * niacin_mg / 100,
         phosphorus = meat_g_dcap * phosphorus_mg / 100,
         polyunsaturated_fatty_acids = meat_g_dcap * polyunsaturated_fatty_acids_g / 100,
         potassium = meat_g_dcap * potassium_mg / 100,
         protein = meat_g_dcap * protein_g / 100,
         riboflavin = meat_g_dcap * riboflavin_mg / 100,
         saturated_fatty_acids = meat_g_dcap * saturated_fatty_acids_g / 100,
         sodium = meat_g_dcap * sodium_mg / 100,
         thiamin_mg = meat_g_dcap * thiamin_mg / 100,
         vitamin_a = meat_g_dcap * vitamin_a_mcg_rae / 100,
         vitamin_b6 = meat_g_dcap * vitamin_b6_mg / 100,
         vitamin_c = meat_g_dcap * vitamin_c_mg / 100,
         zinc = meat_g_dcap * zinc_mg / 100,
         .keep = "unused") %>% 
  # Step 7. Final cleanup
  # Eliminate regions without population growth data
  # Format RCP scenarios
  filter(!is.na(human_pop_size))  %>%
  pivot_longer (calcium:zinc,
                names_to = "nutrient",
                values_to = "amount") 


saveRDS(ds_nutr_diff_percap, file = "Data/ds_nutr_diff_dcap.Rds")


ds_nutr_diff_percap %>%
  filter (country == "Sierra Leone") %>%
  ggplot () +
  geom_bar (aes (x = year, y = amount, fill = nutrient), stat = "identity") +
  facet_wrap (~rcp,  scales = "free")

ds_nutr_diff_percap %>%
  filter (country == "Sierra Leone") %>%
  group_by (rcp, year, nutrient) %>%
  summarise (net_amount = sum(amount, na.rm = TRUE)) %>%
  ggplot () +
  geom_area (aes (x = year, y = net_amount, fill = nutrient)) +
  facet_wrap (~rcp,  scales = "free")

# what do the negatives mean??

ds_sl_temp <- ds_nutr_diff_percap %>%
  filter (country == "Sierra Leone", rcp == "RCP26", year == 2040)
# some species do have decreased catch, but it looks like the nutrients are reflected? more that I want to get a net. so would need to group by species


## instead of nutrients available per capita; the number of additional people you could feed/meet nutrient requirements

ear <- readRDS ("Data/dietary_reference_intake_data.Rds")
# take average for adult men and women
ear_avg <- ear %>%
  filter (stage == "None") %>%
  group_by (nutrient) %>%
  summarize (mn_value = mean(value, na.rm = TRUE))

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


