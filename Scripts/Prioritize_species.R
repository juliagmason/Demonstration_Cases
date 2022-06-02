# select most nutritious species 
# 3/25/2022
# JGM

# later could bring in which are the most climate vulnerable/fishing vulnerable from Maire et al.

library (tidyverse)
library (stringr)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# species nutrient content, has baseline catch in mt
ds_spp_nutr_content <- readRDS("Data/ds_spp_nutr_content_FishNutrientsGENuS_RDA_groups.Rds")

# baseline catch with nutrition content in servings and mt
ds_catch_nutr_yield_baseline <- readRDS("Data/ds_catch_nutr_yield_baseline.Rds")

# catch proportions for context
ds_catch_props_baseline <- readRDS("Data/ds_spp_catch_proportions_baseline.Rds")

# projected upside
nutr_upside <- readRDS("Data/ds_nutr_upside.Rds") # this is by nutrient

# nonzero upside
upside_nonzero <- readRDS("Data/nutricast_catch_upside.Rds") # this is by overall catch

# truncated upside, just adapt_diff_mt, just midcentury, just rcp 6
upside_sm <- upside_nonzero %>%
  filter (rcp == "RCP60", period == "2050-2060") %>%
  ungroup() %>%
  select (country, species, adapt_diff_mt)
  


# SAU baseline nutrient content by sector and enduse
sau_enduse_nutr_contribution <- readRDS("Data/SAU_nutr_content_sector_enduse.Rds")

# also just want top ssf spp for each country
sau_ds <- readRDS ("Data/SAU_countries_dirhumcons_2000_2015.Rds")

# have to go back to fishnutr_mu because a lot of these aren't in nutricast
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu"))
rda_groups <- readRDS("Data/RDAs_5groups.Rds")

sau_spp_nutr <- fishnutr_mu %>%
  filter (species %in% sau_ds$scientific_name)  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup()

sau_spp_micronutr_density <- sau_spp_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  select (species, nutrient, perc_rda) %>%
  distinct() %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda))


# top ssf species for each country ----

sau_ssf_top_spp <- sau_ds %>%
  filter (fishing_sector == "Artisanal", !scientific_name %in% c("Marine fishes not identified", "Miscellaneous marine crustaceans")) %>%
  group_by (country, scientific_name) %>%
  summarise (sum_tonnes = sum(tonnes_tot),
             sum_value = sum (landed_value_tot)) 

sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name)) %>%
  ungroup() %>%
  group_by (country) %>%
  slice_max (sum_tonnes, n = 5)

sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name)) %>%
  ungroup() %>%
  group_by (country) %>%
  slice_max (sum_value, n = 5)


# show nutrient content and adaptive upside for top ssf species----

# lots of NAs, build by hand

# need genus key for squid. 
spp_key <- read.csv("Data/Gaines_species_nutrient_content_key.csv")

squid_nutr <- spp_key %>% filter (species == "Dosidicus gigas") %>%
  select (species, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg) %>%
  pivot_longer (calcium_mg:zinc_mg,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = recode (nutrient,
                             "calcium_mg" = "Calcium",
                             "iron_mg" = "Iron",
                             "polyunsaturated_fatty_acids_g" = "Omega_3",
                             "protein_g" = "Protein",
                             "vitamin_a_mcg_rae" = "Vitamin_A",
                             "zinc_mg" = "Zinc")) %>%
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup()

squid_nutr %>% filter (!nutrient %in% c("Protein")) %>% pull(perc_rda) %>% sum()

# Chile
 
chl_ssf_vol <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Chile") %>%
  slice_max (sum_tonnes, n = 5) %>%
  rename (species = scientific_name) %>%
  
  # show needed nutrients
  left_join(sau_spp_nutr, by = "species") %>%

  filter (nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc")) %>%
  select (species, sum_tonnes, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  # show overall micronutrient density
left_join (sau_spp_micronutr_density, by = "species")

write.excel (chl_ssf_vol)
  
chl_ssf_vol <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Chile") %>%
  slice_max (sum_tonnes, n = 5) %>%
  rename (species = scientific_name) %>%
  # show upside
  left_join (upside_sm, by = c("country", "species"))


write.excel (chl_ssf_vol)

chl_ssf_val <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Chile") %>%
  slice_max (sum_value, n = 5) %>%
  rename (species = scientific_name) %>%
  
  # show needed nutrients
  left_join(sau_spp_nutr, by = "species") %>%
  
  filter (nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc")) %>%
  select (species, sum_tonnes, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  # show overall micronutrient density
  left_join (sau_spp_micronutr_density, by = "species")

write.excel (chl_ssf_val)

chl_ssf_val <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Chile") %>%
  slice_max (sum_value, n = 5) %>%
  rename (species = scientific_name) %>%
  # show upside
  left_join (upside_sm, by = c("country", "species"))


# Peru

peru_ssf_vol <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Peru") %>%
  slice_max (sum_tonnes, n = 5) %>%
  rename (species = scientific_name) %>%
  
  # show needed nutrients
  left_join(sau_spp_nutr, by = "species") %>%
  
  filter (nutrient %in% c("Calcium", "Vitamin_A", "Iron")) %>%
  select (species, sum_tonnes, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  # show overall micronutrient density
  left_join (sau_spp_micronutr_density, by = "species")



peru_ssf_vol <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Peru") %>%
  slice_max (sum_tonnes, n = 5) %>%
  rename (species = scientific_name) %>%
  # show upside
  left_join (upside_sm, by = c("country", "species"))

peru_ssf_val <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Peru") %>%
  slice_max (sum_value, n = 5) %>%
  rename (species = scientific_name) %>%
  
  # show needed nutrients
  left_join(sau_spp_nutr, by = "species") %>%
  
  filter (nutrient %in% c("Calcium", "Vitamin_A", "Iron")) %>%
  select (species, sum_tonnes, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  # show overall micronutrient density
  left_join (sau_spp_micronutr_density, by = "species")
 
write.excel (peru_ssf_val)

peru_ssf_val <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Peru") %>%
  slice_max (sum_value, n = 5) %>%
  rename (species = scientific_name) %>%
  # show upside
  left_join (upside_sm, by = c("country", "species"))

# Indo
indo_ssf_vol <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Indonesia") %>%
  slice_max (sum_tonnes, n = 5) %>%
  rename (species = scientific_name) %>%
  
  # show needed nutrients
  left_join(sau_spp_nutr, by = "species") %>%
  
  filter (nutrient %in% c("Calcium", "Vitamin_A", "Zinc")) %>%
  select (species, sum_tonnes, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  # show overall micronutrient density
  left_join (sau_spp_micronutr_density, by = "species")

write.excel(indo_ssf_vol)

# upside
indo_ssf_vol <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Indonesia") %>%
  slice_max (sum_tonnes, n = 5) %>%
  rename (species = scientific_name) %>%
  # show upside
  left_join (upside_sm, by = c("country", "species"))


# top value
indo_ssf_val <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Indonesia") %>%
  slice_max (sum_value, n = 5) %>%
  rename (species = scientific_name) %>%
  
  # show needed nutrients
  left_join(sau_spp_nutr, by = "species") %>%
  
  filter (nutrient %in% c("Calcium", "Vitamin_A", "Zinc")) %>%
  select (species, sum_tonnes, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  # show overall micronutrient density
  left_join (sau_spp_micronutr_density, by = "species")

write.excel(indo_ssf_val)

# upside
indo_ssf_val <- sau_ssf_top_spp %>% 
  filter (grepl (" ", scientific_name), country == "Indonesia") %>%
  slice_max (sum_value, n = 5) %>%
  rename (species = scientific_name) %>%
  # show upside
  left_join (upside_sm, by = c("country", "species"))

## biggest adaptive mgmt upsides ----
## Just start with biggest changes for now. species that show more than 10% difference?
# nutr_upside_adapt_change <- nutr_upside %>%
#   group_by (country, rcp, period, nutrient species)
  
nutr_upside %>%
  filter (country == "Indonesia", rcp == "RCP60", nutrient == "Calcium", period == "2050-2060") %>%
  arrange (desc (mey_diff_mt)) %>% View()

# maybe make a scatterplot where y axis is micronutrient density and x axis is % change by midcentury. size could be initial catch_mt
# can use ds_Spp_nutr for catch_mt and micronutrient density. need to make a % change upside ds

# ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# 
# ds_sm <- sample_n(ds_spp, 1000)
# ds_sm2 <- ds_spp %>% 
#   filter (country == "Indonesia", rcp == "RCP60", scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), species %in% c ("Trachurus murphyi", "Engraulis ringens", "Brama australis", "Engraulis japonicus"))
# 
# 
# spp_mt_upside <- ds_spp %>%
#   mutate (
#     period = case_when (
#       year %in% c(2025:2035) ~ "2025-2035",
#       year %in% c(2050:2060) ~ "2050-2060",
#       year %in% c(2090:2100) ~ "2090-2100"
#     )) %>%
#   filter (!is.na (period), scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), !is.na(catch_mt)) %>%
#   group_by (country, rcp, period, species) %>%
#   # double checked this method and seems sound
#   summarise ( mey_diff_mt = mean(catch_mt[scenario == "Productivity Only"]) - mean(catch_mt[scenario == "No Adaptation"]),
#             adapt_diff_mt = mean(catch_mt[scenario == "Full Adaptation"]) - mean(catch_mt[scenario == "No Adaptation"]))
# 
# 
# upside_nonzero <- spp_mt_upside %>%
#   filter (abs(mey_diff_mt) > 0 | abs(adapt_diff_mt) > 0)
# 
# saveRDS(upside_nonzero, file = "Data/nutricast_catch_upside.Rds")

upside_nonzero <- readRDS("Data/nutricast_catch_upside.Rds")


spp_micronutr_density <- ds_spp_nutr_content %>%
  filter (!nutrient %in% c("Protein", "Selenium"), group == "Child") %>%
  select (species, nutrient, perc_rda) %>%
  distinct() %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda))

spp_catch_baseline <- ds_spp_nutr_content %>%
  select (country, species, catch_mt) %>% distinct()


spp_ch <- upside_nonzero %>%
  left_join (spp_micronutr_density, by = "species") %>%
  left_join (spp_catch_baseline, by = c ("species", "country"))

spp_ch %>%
  filter (period == "2050-2060", rcp == "RCP60") %>%
  ggplot (aes (x = mey_diff_mt, y = micronutrient_density, size = catch_mt)) +
  geom_point () +
  facet_wrap (~country, scales = "free_x") +
  theme_bw()

spp_ch %>%
  filter (period == "2050-2060", rcp == "RCP60") %>%
  ggplot (aes (x = adapt_diff_mt, y = micronutrient_density, size = catch_mt)) +
  geom_point () +
  facet_wrap (~country, scales = "free_x") +
  theme_bw()


spp_ch %>%
  filter (abs(mey_diff_mt) > 100) %>%
  ggplot (aes (x = mey_diff_mt, y = micronutrient_density, size = catch_mt, col = period)) +
  geom_point () +
  facet_grid (rcp~country, scales = "free_x")+
  theme_bw()

# this does pull out 3 - ~7 spp for each country, looking at midcentury rcp 6 for now

chl_upside_spp <- 
          adapt_diff_mt > 5000)
# 5 spp with mey_diff > 10k, 6 with > 5k (normanic; pretty high catch; medium nutrition)

ds_spp_nutr_content %>% 
  filter (species %in% chl_upside_spp$species, group == "Child") %>%
  select (species, nutrient, amount) %>%
  distinct() %>%
  pivot_wider (names_from = nutrient,
                values_from = amount)

# which of these are in SAU data, and SAU ssf?
sau_chl <- sau_enduse_nutr_contribution %>% filter (country == "Chile")
sau_chl_ssf <- sau_chl %>% filter (fishing_sector == "Artisanal")

chl_upside_spp$species[which (!chl_upside_spp$species %in% sau_chl$species)]
# T. lepturus not in SAU data for Chile, only in SL and indonesia
chl_upside_spp$species[which (!chl_upside_spp$species %in% sau_chl_ssf$species)]
# mostly D. gigas and N. crockeri in ssf; other species start showing up in 2010s...


# where do our regional team's priority spp, M. gayi, M. australis, G. maculatus fit in? M. gayi no nutricast data
spp_ch %>%
  filter (period == "2050-2060", rcp == "RCP60", country == "Chile", species %in% c("Merluccius australis", "Genypterus maculatus"))

# Peru

per_upside_spp <- spp_ch %>%
  filter (period == "2050-2060", rcp == "RCP60", country == "Peru", 
          adapt_diff_mt > 5000)

write.excel (per_upside_spp %>% select (-c(rcp, period)) %>% arrange (desc(adapt_diff_mt)))

# which of these are in SAU data, and SAU ssf?
sau_peru <- sau_enduse_nutr_contribution %>% filter (country == "Peru")
sau_peru_ssf <- sau_peru %>% filter (fishing_sector == "Artisanal")

per_upside_spp$species[which (!per_upside_spp$species %in% sau_peru$species)]
# T. lepturus not in SAU data for Chile/Peru, only in SL and indonesia
per_upside_spp$species[which (!per_upside_spp$species %in% sau_peru_ssf$species)]


indo_upside_spp <- spp_ch %>%
  filter (period == "2050-2060", rcp == "RCP60", country == "Indonesia", 
          adapt_diff_mt > 20000)
# maybe some decisionmaking here. spp with 70 micronutrient density has more to gain than species with 197 density. but 197 is a mollusc so who knows. let's just get rid of them both. go with 20k cutoff. 
ds_spp_nutr_content %>% filter (species %in% c("Scomberomorus commerson", "Tegillarca granosa"), group == "Child")

#BUT s. commerson and t. granosa are SSF species with SAU data

write.excel (indo_upside_spp %>% ungroup() %>% select (-c(rcp, period)) %>% arrange (desc(adapt_diff_mt)))

# which of these are in SAU data, and SAU ssf?
sau_indo <- sau_enduse_nutr_contribution %>% filter (country == "Indonesia")
sau_indo_ssf <- sau_indo %>% filter (fishing_sector == "Artisanal")

indo_upside_spp$species[which (!indo_upside_spp$species %in% sau_indo$species)]
# "Portunus trituberculatus" "Sardinops sagax"          "Scomber japonicus" 
indo_upside_spp$species[which (!indo_upside_spp$species %in% sau_indo_ssf$species)]
# "Muraenesox cinereus"      "Portunus trituberculatus" "Sardinops sagax"          "Scomber japonicus"        "Trichiurus lepturus"   


sl_upside_spp <- spp_ch %>%
  filter (period == "2050-2060", rcp == "RCP60", country == "Sierra Leone", 
          adapt_diff_mt > 500 | mey_diff_mt > 200)

write.excel (sl_upside_spp %>% ungroup() %>% select (-c(rcp, period)) %>% arrange (desc(adapt_diff_mt)))

# which of these are in SAU data, and SAU ssf?
sau_sl <- sau_enduse_nutr_contribution %>% filter (country == "Sierra Leone")
sau_sl_ssf <- sau_sl %>% filter (fishing_sector == "Artisanal")

sl_upside_spp$species[which (!sl_upside_spp$species %in% sau_sl$species)]

sl_upside_spp$species[which (!sl_upside_spp$species %in% sau_sl_ssf$species)]
# "Sardinella aurita"   "Trachurus trachurus"  


  
ds_spp_nutr_content %>% 
  filter (species %in% per_upside_spp$species, group == "Child") %>%
  select (species, nutrient, amount) %>%
  distinct() %>%
  pivot_wider (names_from = nutrient,
               values_from = amount)


## What are the overall most nutritious species? ----

ds_spp_nutr_content %>%
  filter (group == "Child") %>%
  select (species, nutrient, perc_rda) %>%
  distinct() %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda)) %>%
  arrange (desc (micronutrient_density))


## Which species provide the most needed nutrients in each country? ----


# Chile: Calcium and vitamin A. maybe iron, zinc
# Indo: calcium and vitamin A, maybe zinc with golden
# Malawi: calcium, maybe vitamin A
# Peru: calcium, iron, vitamin A
#Sierra leone: calcium, viatmin A, iron, zinc

# chile, do look at rda and amount_mt
sau_chl <- sau_ds %>% filter (country == "Chile")

chl_nutr_spp <- sau_chl %>%
  rename (species = scientific_name) %>%
  group_by (country, species) %>%
  summarise (catch_mt = mean (tonnes_tot)) %>%
  ungroup() %>%
  left_join (sau_spp_nutr, by = "species") %>%
  filter (group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc")) %>%
  select (country, species, catch_mt, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  filter (catch_mt > 5000, Calcium > 10 | Vitamin_A > 10 | Iron > 10 | Zinc > 10) %>%
  arrange (desc (catch_mt)) %>%
  left_join (upside_sm, by = c("country", "species"))

write.excel (chl_nutr_spp)

chl_nutr_spp$species[which (!chl_nutr_spp$species %in% sau_chl$species)]
# "Trichiurus lepturus"       "Macruronus novaezelandiae"
chl_nutr_spp$species[which (!chl_nutr_spp$species %in% sau_chl_ssf$species)]


ds_spp_nutr_content %>%
  filter (country == "Chile", group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron")) %>%
  select (species, catch_mt, nutrient, perc_rda) %>%
  filter (perc_rda > 30)

# where do m. gayi, m. australis, g. maculatus fit in ----
# have to go back to fishnutr_mu...
rda_groups <- readRDS("Data/RDAs_5groups.Rds")
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 

# have to deal with _mu names
# https://stackoverflow.com/questions/23413331/how-to-remove-last-n-characters-from-every-element-in-the-r-vector
library(stringr)

tmp <- fishnutr_mu %>%
        filter(species %in% c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus")) %>%
    pivot_longer (Selenium_mu:Vitamin_A_mu,
              names_to = "nutrient",
              values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup() %>%
  filter (nutrient %in% c("Calcium", "Vitamin_A", "Iron"))

# m gayi micronutrient density
tmp %>%
  filter (!nutrient %in% c("Selenium", "Protein")) %>%
  select (species, nutrient, perc_rda) %>%
  distinct() %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda))


# peru
sau_per <- sau_ds %>% filter (country == "Peru")

per_nutr_spp <- sau_per %>%
  rename (species = scientific_name) %>%
  group_by (country, species) %>%
  summarise (catch_mt = mean (tonnes_tot)) %>%
  ungroup() %>%
  left_join (sau_spp_nutr, by = "species") %>%
  filter (group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron")) %>%
  select (country, species, catch_mt, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  filter (catch_mt > 5000, Calcium > 10 | Vitamin_A > 10 | Iron > 10) %>%
  arrange (desc (catch_mt)) %>%
  left_join (upside_sm, by = c("country", "species"))

write.excel (per_nutr_spp)


per_nutr_spp$species[which (!per_nutr_spp$species %in% sau_peru$species)]
# "Trichiurus lepturus"       
per_nutr_spp$species[which (!per_nutr_spp$species %in% sau_peru_ssf$species)]

# indo

sau_indo <- sau_ds %>% filter (country == "Indonesia")

indo_nutr_spp <- sau_indo %>%
  rename (species = scientific_name) %>%
  group_by (country, species) %>%
  summarise (catch_mt = mean (tonnes_tot)) %>%
  ungroup() %>%
  left_join (sau_spp_nutr, by = "species") %>%
  filter (country == "Indonesia", group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Zinc")) %>%
  select (country, species, catch_mt, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  filter (catch_mt > 10000, Calcium > 15 | Vitamin_A > 15 | Zinc > 25) %>%
  arrange (desc (catch_mt)) %>%
  left_join (upside_sm, by = c("country", "species"))

write.excel (indo_nutr_spp)

# this gets 10 spp, using SAU data. probably okay

# need to only do ssf?

indo_nutr_spp %>%
  filter (species %in% sau_indo_ssf$species) %>%
  write.excel()

write.excel (indo_nutr_spp)

# just list that IS in ssf--still have 13
indo_nutr_spp$species[which (indo_nutr_spp$species %in% sau_indo$species)]
# "Trichiurus lepturus"       
indo_nutr_spp$species[which (indo_nutr_spp$species %in% sau_indo_ssf$species)]

# sierra leone
sl_nutr_spp <- ds_spp_nutr_content %>%
  filter (country == "Sierra Leone", group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc")) %>%
  select (species, catch_mt, nutrient, perc_rda) %>%
  pivot_wider (names_from = nutrient, values_from = perc_rda) %>%
  filter (catch_mt > 2000, Calcium > 10 | Vitamin_A > 10 | Iron > 10 | Zinc > 10) %>%
  arrange (desc (catch_mt))

write.excel (sl_nutr_spp)

sl_nutr_spp$species[which (!sl_nutr_spp$species %in% sau_sl$species)]
    
sl_nutr_spp$species[which (!sl_nutr_spp$species %in% sau_sl_ssf$species)]
# "Trachurus trachurus"   "Sardinella maderensis"

# for this, doesn't matter RDA? but would want amount_mt?

# looking at amounts per serving
ds_spp_nutr_content %>%
  select (country, species, catch_mt, nutrient, amount) %>%
  distinct() %>%
  left_join (ds_catch_props_baseline, by = c ("country", "species")) %>%
  group_by (country, nutrient) %>%
  slice_max (amount, n = 5) %>%
  select (country, nutrient, species, amount, prop_catch, rank_catch) %>%
  arrange (country, nutrient, desc (amount)) %>% View()

# important species in terms of density--one serving meets 25%, 30%? some cutoff of child rda ----

ds_spp_nutr_content %>%
  filter (country == "Sierra Leone", group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc"), perc_rda > 25) %>%
  left_join (ds_catch_props_baseline, by = c ("country", "species")) %>%
  arrange (nutrient, desc (perc_rda)) %>%
  select (nutrient, species, perc_rda, prop_catch, rank_catch) 

# important species in terms of total yield ----
ds_catch_nutr_yield_baseline %>%
  filter (country == "Sierra Leone",  nutrient %in% c("Calcium", "Vitamin_A", "Iron", "Zinc")) %>%
  select (nutrient, species, nutr_mt, amount) %>%
  group_by (nutrient) %>%
  slice_max (nutr_mt, n = 10) %>% 
  View()

# important species in terms of MEY upside ----
nutr_upside %>%
  filter (period == "2050-2060", country == "Sierra Leone") %>%
  group_by (rcp, nutrient) %>%
  slice_max (mey_diff_mt, n = 5) %>%
  arrange (rcp, nutrient, mey_diff_mt) %>%
  View()
  
nutr_upside %>%
  filter (period == "2050-2060", country == "Sierra Leone") %>%
  group_by (rcp, nutrient) %>%
  slice_max (mey_diff_child_rda, n = 5) %>%
  select (rcp, nutrient, species, mey_diff_child_rda) %>%
  View()
