### explore chris free data
# 12/9/2021




library (tidyverse)

# cleaned GENuS data
#https://github.com/cfree14/GENuS
#library (devtools)
#devtools::install_github("cfree14/GENuS", force=T)
library(GENuS)

colnames (genus_fcts)
# nutrient key from nutrient_endowment
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T) 

# Read projected human population growth
pop_growth <- readRDS(file.path( "../nutrient_endowment/data/population_growth/processed/WB_UN_1960_2100_human_population_by_country.Rds"))

# is that different from genus_fcts?
genus_fcts %>% 
  filter (genus_food_name %in% spp_key$genus_food_name) %>% View()
# this has more country specific product data, with common names. could use if we have more info. 
  

# genus food level data
ds <- readRDS ("Data/nutricast_data_for_julia.Rds")
str (ds) # too many columns
colnames (ds)


# Has RCP 2, 4.5, 6, 8.5 and full adaptation or no adaptation. 2012-2100, in five year increments (2012, 2015, 2020 to 2100)
# Catch is at the GeNUS group level. 6 genus food names, 4 major groups. so not SAU level. 

# nutrient endowment step5 code
# _dcap is daily availability per capita. divided by 365
# meat_mt is catch_mt_scaled * pedible. meat_g_decap is meat_mt * 1000 * 1000 / 365 / human pop size
# pedible is a constant for each of the 4 major groups. finfish, crustaceans, molluscs, cephalopods
# msy and catch_scaled is multiplied by a scalar, observed FAO catch 2012 / predicted 2012 catch from CF's model?
# nutrient _dcap values are meat_g_dcap * calcium_mg / 100
# looks like didn't change conversions for things in grams vs mg? why all 100? is the calcium in mg per 100g meat? Yes--looking at GENUS metadata


ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps, msy, catch, profits, bbmsy, ffmsy. but no nutrient data. 

# 779 spp for each country, so some must have NAs

# how many relevant species for each country? catch_mt > 0
chl_spp <- ds_spp %>%
  filter (country == "Chile", rcp == "RCP85", scenario == "Full Adaptation") %>%
  group_by (species) %>%
  summarise (tot_cat = sum (catch_mt)) %>%
  filter (tot_cat > 0) # 133 spp

# plot timeseries of total catch by rcp and management scenario
ds_spp %>%
  filter (country == "Chile") %>%
  group_by (year, rcp, scenario) %>%
  summarise (tot_cat = sum (catch_mt, na.rm = TRUE)) %>%
  ggplot () +
  geom_line (aes (x = year, y = tot_cat)) +
  facet_grid(scenario ~ rcp)

indo_spp <- ds_spp %>%
  filter (country == "Indonesia", rcp == "RCP60", scenario == "Full Adaptation") %>%
  group_by (species) %>%
  summarise (tot_cat = sum (catch_mt)) %>%
  filter (tot_cat > 0) # 227 spp

# match to gaines genus data
ds_spp_nutr <- ds_spp %>%
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
         meat_mt=catch_mt * pedible) %>% 
  # Step 4. Add human population size
  left_join(pop_growth %>% select(country, year, pop_size_50perc), by=c("country", "year")) %>% 
  rename(human_pop_size=pop_size_50perc) %>% 
  # Step 5. Calculate daily per capita meat availability
  mutate(meat_g_dcap= (meat_mt*1000*1000) / 365 / human_pop_size) %>% 
  # Step 6. Calculate daily per capita nutrient availability
# ** divide by 100 because genus data provides values per 100g serving
  mutate(calcium_mg_dcap = meat_g_dcap * calcium_mg / 100,
         calories_kcal_dcap = meat_g_dcap * calories_kcal / 100,
         carbohydrates_g_dcap = meat_g_dcap * carbohydrates_g / 100,
         copper_mg_dcap = meat_g_dcap * copper_mg / 100,
         fat_g_dcap = meat_g_dcap * fat_g / 100,
         folate_mcg_dcap = meat_g_dcap * folate_mcg / 100,
         iron_mg_dcap = meat_g_dcap * iron_mg / 100,
         magnesium_mg_dcap = meat_g_dcap * magnesium_mg / 100,
         monounsaturated_fatty_acids_g_dcap = meat_g_dcap * monounsaturated_fatty_acids_g / 100,
         niacin_mg_dcap = meat_g_dcap * niacin_mg / 100,
         phosphorus_mg_dcap = meat_g_dcap * phosphorus_mg / 100,
         polyunsaturated_fatty_acids_g_dcap = meat_g_dcap * polyunsaturated_fatty_acids_g / 100,
         potassium_mg_dcap = meat_g_dcap * potassium_mg / 100,
         protein_g_dcap = meat_g_dcap * protein_g / 100,
         riboflavin_mg_dcap = meat_g_dcap * riboflavin_mg / 100,
         saturated_fatty_acids_g_dcap = meat_g_dcap * saturated_fatty_acids_g / 100,
         sodium_mg_dcap = meat_g_dcap * sodium_mg / 100,
         thiamin_mg_dcap = meat_g_dcap * thiamin_mg / 100,
         vitamin_a_mcg_rae_dcap = meat_g_dcap * vitamin_a_mcg_rae / 100,
         vitamin_b6_mg_dcap = meat_g_dcap * vitamin_b6_mg / 100,
         vitamin_c_mg_dcap = meat_g_dcap * vitamin_c_mg / 100,
         zinc_mg_dcap = meat_g_dcap * zinc_mg / 100,
         .keep = "unused") %>% 
  # Step 7. Final cleanup
  # Eliminate regions without population growth data
  # Format RCP scenarios
  filter(!is.na(human_pop_size), catch_mt > 0)
  

# look at just no adaptation, for a few nutrients

# this table will show for each country what species will provide the most of that nutrient or metric under BAU management--which ones you would want to prioritize
ds_BAU <- ds_spp_nutr %>%
filter (rcp == "RCP60", year > 2025, scenario == "No Adaptation") %>%
  group_by (country, species, scenario) %>%
  summarise_if (is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  select (country, species, catch_mt, profits_usd, calories_kcal_dcap, vitamin_a_mcg_rae_dcap, calcium_mg_dcap, folate_mcg_dcap, polyunsaturated_fatty_acids_g_dcap, zinc_mg_dcap) %>%
  pivot_longer (catch_mt:zinc_mg_dcap, 
                names_to = "Metric",
                values_to = "Amount"
  ) %>%
  group_by (country, Metric) %>%
  slice (which.max (Amount)) %>%
  pivot_wider (!Amount, names_from = Metric, 
               values_from = species) 

# for one country, look at amounts and top 5species
chl_BAU <- ds_spp_nutr %>%
  filter (year > 2025, country == "Chile", scenario == "No Adaptation") %>%
  group_by (species, rcp) %>%
  summarise_if (is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  select (rcp, species, catch_mt, profits_usd, calories_kcal_dcap, vitamin_a_mcg_rae_dcap, calcium_mg_dcap, folate_mcg_dcap, polyunsaturated_fatty_acids_g_dcap, zinc_mg_dcap) %>%
  pivot_longer (catch_mt:zinc_mg_dcap, 
                names_to = "Metric",
                values_to = "Amount"
  ) %>%
  group_by (rcp, Metric) %>%
  slice_max (order_by = Amount, n=5, with_ties = TRUE)


# look at full minus no difference
minus <- function(x,y){x-y}


# this will tell us which species gives you the biggest bang for your buck by adopting adaptive management
ds_adapt <- ds_spp_nutr %>%
  filter (rcp == "RCP60", year > 2025, scenario %in% c("Full Adaptation", "No Adaptation")) %>%
  group_by (country, species, scenario) %>%
  summarise_if (is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  select (country, scenario, species, catch_mt, profits_usd, calories_kcal_dcap, vitamin_a_mcg_rae_dcap, calcium_mg_dcap, folate_mcg_dcap, polyunsaturated_fatty_acids_g_dcap, zinc_mg_dcap) %>%
  pivot_wider (names_from = scenario, 
               names_sep = ".", 
               values_from = catch_mt:zinc_mg_dcap) %>%
  mutate (minus (across (ends_with (".Full Adaptation"), .names = "{col}._adapt_diff"), 
                 across (ends_with (".No Adaptation"))),
          .keep = "unused") %>%
  rename_with (~ sub ("\\..*?\\.", "", .), ends_with ("_diff")) %>%
  pivot_longer (catch_mt_adapt_diff:zinc_mg_dcap_adapt_diff, 
                names_to = "Metric",
                values_to = "Adapt_difference"
  ) %>%
  group_by (country, Metric) %>%
  slice (which.max (Adapt_difference)) %>% 
  pivot_wider (!Adapt_difference, names_from = Metric, 
               values_from = species) 

# would this differ by scenario? only for 8.5 in indonesia
ds_adapt_rcp <- ds_spp_nutr %>%
  filter ( year > 2025, scenario %in% c("Full Adaptation", "No Adaptation")) %>%
  group_by (country, rcp, species, scenario) %>%
  summarise_if (is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  select (country, rcp, scenario, species, catch_mt, profits_usd, calories_kcal_dcap, vitamin_a_mcg_rae_dcap, calcium_mg_dcap, folate_mcg_dcap, polyunsaturated_fatty_acids_g_dcap, zinc_mg_dcap) %>%
  pivot_wider (names_from = scenario, 
               names_sep = ".", 
               values_from = catch_mt:zinc_mg_dcap) %>%
  mutate (minus (across (ends_with (".Full Adaptation"), .names = "{col}._adapt_diff"), 
                 across (ends_with (".No Adaptation"))),
          .keep = "unused") %>%
  rename_with (~ sub ("\\..*?\\.", "", .), ends_with ("_diff")) %>%
  pivot_longer (catch_mt_adapt_diff:zinc_mg_dcap_adapt_diff, 
                names_to = "Metric",
                values_to = "Adapt_difference"
  ) %>%
  group_by (country, rcp, Metric) %>%
  slice (which.max (Adapt_difference)) %>% 
  pivot_wider (!Adapt_difference, names_from = Metric, 
               values_from = species) 

# copy to email
# function for copying tables----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(ds_BAU)
write.excel (ds_adapt)


# below is with species groups (ds), not species specific

# Could take a cumulative sum or an average over time, compare full adaptation and no adaptation [biggest bang for buck for climate adaptation] or just look at no adaptation [priority stocks to conserve]. If taking a cumulative sum don't need to do the differences for each year. 

# make a table by country and metric (profit, catch, calories, a few micronutrients), where it shows the top species group for each

ds_sum <- ds %>%
  filter (rcp == "RCP 6.0", year > 2025) %>%
  group_by (country, genus_food_name, scenario) %>%
  summarise_if (is.numeric, sum)

# first, which are the top under no adaptation
ds_tbl <- ds_sum %>%
  ungroup() %>%
  filter (scenario == "No Adaptation") %>%
  select (country, genus_food_name, catch_mt_scaled, profits_usd_scaled, calories_kcal_dcap, vitamin_a_mcg_rae_dcap, calcium_mg_dcap, folate_mcg_dcap, polyunsaturated_fatty_acids_g_dcap, zinc_mg_dcap) %>%
  pivot_longer (catch_mt_scaled:zinc_mg_dcap, 
                names_to = "Metric",
                values_to = "Amount"
                ) %>%
  group_by (country, Metric) %>%
  slice (which.max (Amount))

# then, which have the most to gain from adaptive management
# https://stackoverflow.com/questions/65849383/finding-the-differences-of-paired-columns-using-dplyr
# https://stackoverflow.com/questions/8758653/regular-expression-to-remove-a-substring-between-two-characters
minus <- function(x,y){x-y}

ds_tbl_adapt <- ds_sum %>%
  ungroup() %>%
  select (country, scenario, genus_food_name, catch_mt_scaled, profits_usd_scaled, calories_kcal_dcap, vitamin_a_mcg_rae_dcap, calcium_mg_dcap, folate_mcg_dcap, polyunsaturated_fatty_acids_g_dcap, zinc_mg_dcap) %>%
  pivot_wider (names_from = scenario, 
               names_sep = ".", 
               values_from = catch_mt_scaled:zinc_mg_dcap) %>%
  mutate (minus (across (ends_with (".Full Adaptation"), .names = "{col}._adapt_diff"), 
                 across (ends_with (".No Adaptation"))),
          .keep = "unused") %>%
  rename_with (~ sub ("\\..*?\\.", "", .), ends_with ("_diff")) %>%
  pivot_longer (catch_mt_scaled_adapt_diff:zinc_mg_dcap_adapt_diff, 
                names_to = "Metric",
                values_to = "Adapt_difference"
  ) %>%
  group_by (country, Metric) %>%
  slice (which.max (Adapt_difference))
  
  

  
# the nutrient columns that aren't _dcap are constant, so must be per kg or whatever metric. 
# test this
ds$catch_mt_scaled[1] * ds$calories_kcal[1] / ds$human_pop_size[1] = ds$calories_kcal_dcap[1]
(ds$meat_mt[1] * 1000 * 1000) /ds$human_pop_size[1] 
ds$meat_g_dcap[1]

ds$catch_mt_scaled[1] * ds$pedible[1]
# meat_mt is catch_nt_scaled * pedible

(ds$meat_mt[1] * 1000000)/ ds$human_pop_size[1] / 365
ds$meat_g_dcap[1]

(ds$msy_mt_scaled[1] * ds$pedible[1] * 1000000)/ds$human_pop_size[1]

unique (ds$iso3)
unique (ds$rcp)
unique (ds$scenario)

chl <- ds %>% filter (iso3 == "IDN")

chl %>%
  ggplot () +
  geom_line (aes (x = year, y = human_pop_size, col = rcp))

ds %>%
  filter (!country == "Indonesia") %>%
  ggplot () +
  geom_line (aes (x = year, y = human_pop_size, col = country))

chl %>%
  filter (!major_group == "Finfish") %>%
  ggplot () +
  geom_line (aes (x = year, y = catch_mt_scaled, col = major_group)) +
  facet_grid (scenario~rcp)

# to examine nutrients per capita over time, need to pivot_longer
chl_dcap_ts <- chl %>%
  filter (scenario == "No Adaptation", rcp == "RCP 6.0") %>%
  pivot_longer(calcium_mg_dcap:zinc_mg_dcap,
               names_to = "Nutrient",
               values_to = "amt_dcap"
  )

chl_dcap_ts %>%
  ggplot () +
  geom_line (aes (x = year, y = amt_dcap, col = major_group)) +
  facet_wrap (~Nutrient, scales = "free")

# can also look at difference between full and no?
# https://stackoverflow.com/questions/65849383/finding-the-differences-of-paired-columns-using-dplyr
minus <- function(x,y){x-y}

chl_adapt_diff <- chl %>%
  filter (rcp == "RCP 6.0", genus_food_name == "Demersal Fish") %>%
  pivot_wider (names_from = scenario, 
               names_sep = ".", 
               values_from = catch_mt_scaled:zinc_mg_dcap) %>%
  mutate (minus (across (ends_with (".Full Adaptation"), .names = "{col}._adapt_diff"), 
                 across (ends_with (".No Adaptation"))))

# did that work?
chl_adapt_diff$`catch_mt_scaled.Full Adaptation`
chl_adapt_diff$`catch_mt_scaled.No Adaptation`
chl_adapt_diff$`catch_mt_scaled.Full Adaptation_adapt_diff`

# another option
chl_adapt_diff <- chl %>%
  filter (rcp == "RCP 6.0") %>%
  pivot_longer(c(msy_mt_scaled:meat_g_dcap, calcium_mg_dcap:zinc_mg_dcap),
               names_to = "Metric",
               values_to = "amt_dcap") %>%
  group_by (Metric, genus_food_name, year) %>%
  summarize (diff = amt_dcap[scenario == "Full Adaptation"] - amt_dcap[scenario == "No Adaptation"])


chl_adapt_diff %>%
  filter (year > 2025) %>%
  ggplot () +
  geom_line (aes (x = year, y = diff, col = genus_food_name)) +
  facet_wrap (~Metric, scales = "free")

# look at cumulative sum?
chl_adapt_diff_sum <- chl_adapt_diff %>%
  filter (year > 2025) %>% # something weird with the initial conditions
  group_by (Metric, genus_food_name) %>%
  summarize (sum_diff = sum(diff)) 

chl_adapt_diff_sum %>%
  filter (Metric == "catch_mt_scaled") %>%
  arrange (desc (sum_diff))

chl_adapt_diff_sum %>%
  filter (Metric == "profits_usd_scaled") %>%
  arrange (desc (sum_diff))

chl_adapt_diff_sum %>%
  filter (Metric == "polyunsaturated_fatty_acids_g_dcap") %>%
  arrange (desc (sum_diff))

# and then just cumulative sum with no adaptation?
chl_noadapt_sum <- chl %>%
  filter (rcp == "RCP 6.0", scenario == "No Adaptation", year > 2025) %>%
  pivot_longer(c(msy_mt_scaled:meat_g_dcap, calcium_mg_dcap:zinc_mg_dcap),
               names_to = "Metric",
               values_to = "amt_dcap") %>%
  group_by (Metric, genus_food_name) %>%
  summarise (sum_noadapt = sum (amt_dcap) )


chl_noadapt_sum %>%
  filter (Metric == "catch_mt_scaled") %>%
  arrange (desc (sum_noadapt))

chl_noadapt_sum %>%
  filter (Metric == "profits_usd_scaled") %>%
  arrange (desc (sum_noadapt))

chl_noadapt_sum %>%
  filter (Metric == "polyunsaturated_fatty_acids_g_dcap") %>%
  arrange (desc (sum_noadapt))
