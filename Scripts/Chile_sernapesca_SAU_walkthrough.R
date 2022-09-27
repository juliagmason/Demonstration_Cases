# Chile full walkthrough
# 9 26 2022
# JGM

library (tidyverse)


# I want to tell a better story. 

# another goal is to revisit the SAU data but cut out the questionable tyears? I did that already, but do some of the same figures and try presenting them again. 

# be able to compare SAU and official data
# be able to walk through graph of landings, then sector, end use, exports

# then nutrition

# priority spp
priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv")
chl_pri_spp <- priority_spp %>% filter (country == "Chile")

# cleaned and compiled landings data in clean_chile_sernapesca_landings
# use 2012 data onward to compare to SAU
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

chl_landings %>% 
  group_by (sector, year) %>%
  summarise (tot_cat = sum (catch_mt))
  
chl_landings %>%
  ggplot (aes (x = year, y = catch_mt/1000, fill = sector)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  labs (x = "", y = "Catch, 1000 tons")


# join to SAU categories and plot
sau_chl <- read.csv ("Data/SAU_EEZ_landings.csv") %>%
  filter (area_name =="Chile (mainland)") %>%
  rename (species = scientific_name, catch_mt = tonnes)

sau_chl_sm <- sau_chl %>%
  filter (between (year, 2012, 2018)) %>%
  mutate (sector = case_when (
    fishing_sector == "Industrial" ~ "Industrial_SAU",
  fishing_sector == "Artisanal" ~ "Artisanal_SAU"
  )) %>%
  select (species, catch_mt, year, sector)

sau_groups <- sau_chl %>%
  select (species, functional_group, commercial_group)
  

chl_compare <- rbind (chl_landings, sau_chl_sm)

chl_compare %>%
  ggplot (aes (x = year, y = catch_mt/1000, fill = sector)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  labs (x = "", y = "Catch, 1000 tons")

# separate df to join commercial and functional groups to chl landings species
sau_spp_groups <- sau_chl %>% 
  select (scientific_name, commercial_group, functional_group) %>%
  distinct () %>%
  rename (species = scientific_name)

chl_compare %>%
  left_join (sau_spp_groups, by = "species") %>%
  ggplot (aes (x = year, fill  = commercial_group, y = catch_mt/1000)) +
  geom_bar (stat = "identity") +
  facet_wrap (~sector, byrow = TRUE) +
  theme_bw() +
  labs (y = "Catch, 1000 tons", x = "")


# yes, these are quite different. large increasing trend in artisanal/decreasing industrial for SAU; more constant for sernapesca. a lot more blue perch/other for SAU than Sernapesca artisanal.

# summarise end use by sector and species ----
# don't trust 2016-2018
# not big changes in proportion, but maybe still worth using more recent years
sau_chl %>%
  filter (species %in% priority_spp$species, between (year, 2000, 2015)) %>%
  ggplot (aes (x = year, fill = end_use_type, y = catch_mt)) +
  geom_bar (stat = "identity") +
  facet_grid (species ~ fishing_sector, scales = "free_y") +
  theme_bw()

sau_chl_end_use <- sau_chl %>%
  filter (between (year, 2011, 2015)) %>%
  group_by (species, fishing_sector) %>%
  summarise (DHC = sum (catch_mt[end_use_type == "Direct human consumption"])/sum (catch_mt),
             Fishmeal = sum (catch_mt[end_use_type == "Fishmeal and fish oil"]) / sum (catch_mt),
             Discards = sum (catch_mt [end_use_type == "Discards"]) / sum (catch_mt)
            ) %>%
  rename (sector = fishing_sector)


sau_chl_end_use %>%
  filter (species %in% chl_pri_spp$species)

# join to official landings data to convert amounts
end_use_chl_landings <- chl_landings %>%
  filter (species %in% chl_pri_spp$species, between (year, 2017, 2021)) %>%
  group_by (species, sector) %>%
  summarise (mean_catch = mean (catch_mt, na.rm = TRUE)) %>%
  left_join (sau_chl_end_use, by = c("species", "sector")) %>%
  mutate (catch_loss_fishmeal = mean_catch * Fishmeal,
          catch_loss_discards = mean_catch * Discards)

source ("Scripts/Function_convert_catch_amt_children_fed.R")

tmurph_ind <- calc_children_fed_func("Trachurus murphyi", "Finfish", 372869)
tmurph_ind <- calc_children_fed_func("Trachurus murphyi", "Finfish", (444677 + 34109))

chl_land %>%
  #arrange (desc (catch_mt)) %>%
  
  left_join (sau_spp_groups, by = "species") %>%
  slice_max (n = 10, order_by = catch_mt)

# nutrient data and rda data ----
rda_groups <- readRDS("Data/RDAs_5groups.Rds")
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu"))

# nutr_long
fishnutr_long <- fishnutr_mu %>% 
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>% 
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient),
          perc_rda = amount/mean_rda * 100,
          # cap at 100
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda))



chl_land_nutr <- chl_landings %>%
  left_join (fishnutr_long, by = "species") %>%
   mutate (edible_meat_servings = catch_mt * 0.87 * 1000 * 1000 / 100 / 365, 
                 total_servings = amount * edible_meat_servings,
                 children_fed = total_servings / mean_rda)

  
chl_land_nutr %>%
  filter (species %in% chl_pri_spp)

# start with anchovy

chl_landings %>%
  filter (year > 2016) %>%
  filter (species == "Engraulis ringens") %>%
  ggplot (aes (x = year, y = catch_mt/1000)) +
  geom_bar (stat = "identity") +
  theme_bw() +
  labs (x = "", y = "Catch, 1000 tons" ) +
  ggtitle ("Anchovy catch in Chile's waters") 

# plot mean

mean_anch <- chl_landings %>%
  filter (year > 2016) %>%
  filter (species == "Engraulis ringens") %>%
  group_by (year) %>%
  summarise (tot = sum (catch_mt)) %>%
  ungroup () %>%
  pull (tot) %>% mean()


chl_landings %>%
  filter (year > 2016) %>%
  filter (species == "Engraulis ringens") %>%
  ggplot (aes (x = year, y = catch_mt/1000)) +
  geom_bar (stat = "identity") +
  theme_bw() +
  #stat_summary (fun = mean, aes (x = 1, yintercept = ..y..), geom = "hline", col = "red") +
  geom_hline (yintercept = mean_anch/1000, col = "red", lwd = 1.5) +
  labs (x = "", y = "Catch, 1000 tons" ) +
  ggtitle ("Anchovy catch in Chile's waters") 


chl_landings %>%
  filter (year > 2016) %>%
  filter (species == "Engraulis ringens") %>%
  ggplot (aes (x = year, y = catch_mt/1000, fill = sector)) +
  geom_bar (stat = "identity") +
  theme_bw() +
  labs (x = "", y = "Catch, 1000 tons" ) +
  ggtitle ("Anchovy catch in Chile's waters")


# anchovy nutrition
chl_land_nutr %>%
  filter (year > 2016, species == "Engraulis ringens") %>%
  group_by (year, nutrient) %>%
  summarise (tot_fed = sum (children_fed, na.rm = TRUE)) %>%
  ungroup () %>%
  group_by (nutrient ) %>%
  summarise (mean_fed = mean (tot_fed, na.rm = TRUE)) %>%
  ggplot (aes (x = nutrient, y = mean_fed/1000000)) +
  geom_bar (stat = "identity") +
  theme_bw() +
  labs (y = "Children's daily needs met (millions)")

# how much lost to foreign fishing? ----
# negligible

prop_foreign_sau <- sau_chl %>%
  filter (between (year, 2011, 2015), species %in% chl_pri_spp$species) %>%
  mutate (fishing_country = ifelse (fishing_entity == "Chile", "Domestic catch", "Foreign catch")) %>%
  group_by (year, species, fishing_country) %>%
  summarise (sum_tonnes = sum (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (species) %>%
  summarise (prop_for = sum (sum_tonnes[fishing_country == "Foreign catch"]) / sum (sum_tonnes),
             mean_for_catch = mean (sum_tonnes[fishing_country == "Foreign catch"], na.rm = TRUE))
  
  
# now split into industrial and artisanal
chl_land_nutr %>%
  filter (year > 2016) %>%
  filter (species == "Engraulis ringens") %>%
  group_by (sector, nutrient) %>%
  summarise (mean_fed = mean (children_fed, na.rm = TRUE)) %>%
  ggplot (aes (x = nutrient, y = mean_fed/1000000, fill = sector)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  labs (y = "Children's daily needs met (millions)")

# now by end use
chl_land_end_use <- chl_landings %>%
  filter (year > 2016) %>%
  left_join (sau_chl_end_use, by = c ("species", "sector")) %>%
  mutate (DHC_c = catch_mt * DHC,
          Fishmeal_c = catch_mt * Fishmeal,
          Discards_c = catch_mt * Discards) %>%
  pivot_longer (DHC_c:Discards_c,
                names_to = "end_use_type",
                values_to = "end_use_mt") %>%
  mutate (end_use_type = str_sub(end_use_type, end = -3))

chl_land_end_use %>%
  filter (species == "Engraulis ringens") %>%
  ggplot (aes (x = year, y = end_use_mt/1000, fill = end_use_type)) +
  geom_bar (stat = "identity") +
  facet_wrap (~sector) +
  theme_bw() +
  labs (x = "", y = "Catch, 1000 tons", fill = "End use")
  
 
# so actual nutrition, filter out end use
chl_anchov_end_use_nutr <- chl_land_end_use %>%
  filter (species == "Engraulis ringens") %>%
  group_by (species, sector, end_use_type) %>%
  summarise (mean_catch = mean (end_use_mt)) %>%
  ungroup () %>% 
  left_join (fishnutr_long, by = "species") %>%
  mutate (edible_meat_servings = mean_catch * 0.87 * 1000 * 1000 / 100 / 365, 
          total_servings = amount * edible_meat_servings,
          children_fed = total_servings / mean_rda)


chl_anchov_end_use_nutr %>%
  filter (end_use_type == "DHC") %>%
  ggplot (aes (x = nutrient, y = children_fed/1000000)) +
  geom_bar (stat = "identity") +
  facet_wrap (~sector) +
  theme_bw() +
  labs (y = "Children's daily needs met (millions)")
            