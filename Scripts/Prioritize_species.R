# select most nutritious species 
# 3/25/2022
# JGM

library (tidyverse)

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
nutr_upside <- readRDS("Data/ds_nutr_upside.Rds")


# SAU baseline nutrient content by sector and enduse
sau_enduse_nutr_contribution <- readRDS("Data/SAU_nutr_content_sector_enduse.Rds")

# # two ways of looking at this. the species that are the most nutrient dense for that nutrient, and the species that are frequently caught and therefore providing a lot of that nutrient. 


# later could bring in which species will show the most gains under management reforms; which are the most climate vulnerable/fishing vulnerable from Maire et al. 


## biggest adaptive mgmt upsides ----
## Just start with biggest changes for now. species that show more than 10% difference?
nutr_upside_adapt_change <- nutr_upside %>%
  group_By (country, rcp, period, nutrient species)
  
nutr_upside %>%
  filter (country == "Indonesia", rcp == "RCP60", nutrient == "Calcium", period == "2050-2060") %>%
  arrange (desc (mey_diff_mt)) %>% View()

# maybe make a scatterplot where y axis is micronutrient density and x axis is % change by midcentury. size could be initial catch_mt
# can use ds_Spp_nutr for catch_mt and micronutrient density. need to make a % change upside ds

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")

ds_sm <- sample_n(ds_spp, 1000)
ds_sm2 <- ds_spp %>% 
  filter (country == "Indonesia", rcp == "RCP60", scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), species %in% c ("Trachurus murphyi", "Engraulis ringens", "Brama australis", "Engraulis japonicus"))


spp_mt_upside <- ds_spp %>%
  mutate (
    period = case_when (
      year %in% c(2025:2035) ~ "2025-2035",
      year %in% c(2050:2060) ~ "2050-2060",
      year %in% c(2090:2100) ~ "2090-2100"
    )) %>%
  filter (!is.na (period), scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), !is.na(catch_mt)) %>%
  group_by (country, rcp, period, species) %>%
  # double checked this method and seems sound
  summarise ( mey_diff_mt = mean(catch_mt[scenario == "Productivity Only"]) - mean(catch_mt[scenario == "No Adaptation"]),
            adapt_diff_mt = mean(catch_mt[scenario == "Full Adaptation"]) - mean(catch_mt[scenario == "No Adaptation"]))


upside_nonzero <- spp_mt_upside %>%
  filter (abs(mey_diff_mt) > 0 | abs(adapt_diff_mt) > 0)

saveRDS(upside_nonzero, file = "Data/nutricast_catch_upside.Rds")


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

chl_upside_spp <- spp_ch %>%
  filter (period == "2050-2060", rcp == "RCP60", country == "Chile", 
          adapt_diff_mt > 5000)
# 5 spp with mey_diff > 10k, 6 with > 5k (normanic; pretty high catch; medium nutrition)

ds_spp_nutr_content %>% 
  filter (species %in% chl_upside_spp$species, group == "Child") %>%
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


# Chile: Calcium and vitamin A. maybe iron?
# Indo: calcium and vitamin A, maybe zinc with golden
# Malawi: calcium, maybe vitamin A
# Peru: calcium, zinc
#Sierra leone: calcium, viatmin A, iron, zinc

# chile, do look at rda and amount_mt
chl_nutr_spp <- ds_spp_nutr_content %>%
  filter (country == "Chile", group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron")) %>%
  select (species, catch_mt, nutrient, perc_rda) %>%
  filter (catch_mt > 5000, perc_rda > 10)

write.excel (chl_nutr_spp)

  
ds_spp_nutr_content %>%
  filter (country == "Chile", group == "Child", nutrient %in% c("Calcium", "Vitamin_A", "Iron")) %>%
  select (species, catch_mt, nutrient, perc_rda) %>%
  filter (perc_rda > 30)

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
