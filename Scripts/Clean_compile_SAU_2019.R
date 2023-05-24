# Clean and filter SAU 2019 landings
# updated 5/24/23 from SAU_explore
# JGM

# have this in one place and fix the peru anchovy issue
# In order to fix anchovy without fiddling with every single country, I'm simplifying foreign vs. domestic catch in sau_2019. I think this is fine, I can go back to sau_explore if I need country specific metrics

library (tidyverse)

# 2019 download doesn't include indonesia; too big
sau_2019_download <- read.csv("Data/SAU EEZ 2019.csv") %>%
  rename (species = scientific_name) %>%
  mutate (country = case_when(
    grepl("Indo", area_name) ~ "Indonesia",
    grepl ("Mex", area_name) ~ "Mexico",
    area_name == "Chile (mainland)" ~ "Chile",
    TRUE ~ area_name),
    
    # lump foreign and domestic 
    fleet = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")
    ) %>%

  group_by (country, species, year, fishing_sector, fleet, end_use_type) %>%
  summarise (tonnes = sum(tonnes))

# separate indo download
sau_indo <- read.csv("Data/SAU EEZ indonesia.csv") %>% 
  rename (species = scientific_name) %>%
  mutate (country = "Indonesia",
  # lump foreign and domestic 
  fleet = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")
  ) %>%
  group_by (country, species, year, fishing_sector, fleet, end_use_type) %>%
  summarise (tonnes = sum(tonnes))

# last five years ----
sau_2015_2019_full <- sau_2019_download %>%
  filter (between(year,2015,2019))

sau_2015_2019_indo <- sau_indo %>%
  filter (between(year,2015,2019))

sau_2015_2019 <- rbind (sau_2015_2019_full, sau_2015_2019_indo)

saveRDS(sau_2015_2019, file = "Data/SAU_2015_2019.Rds")


# 2019 only ----
sau_2019 <- sau_2019_download %>%
  filter (year == 2019)

sau_indo_2019 <- sau_indo %>% 
  filter (year == 2019) %>%
# replace blank with discards??
  mutate (end_use_type = ifelse (end_use_type == "", "Discards", end_use_type))

sau_2019 <- rbind (sau_2019, sau_indo_2019)


# fix Peru anchovy dhc issue. 2019 direct human consumption of anchovy is crazy high. 
# proportion of domestic catch going to fishmeal in 2018


# find overall 2019 totals 
peru_anchov_2019_totals <- sau_2019 %>%
  filter (country == "Peru", species == "Engraulis ringens") %>%
  group_by (fishing_sector, fleet) %>%
  summarise (tot = sum (tonnes))

# find 2018 proportions and multiply by 2019 totals
peru_anchov_2018_correction <- sau_2015_2019 %>%
  filter (country == "Peru", year == 2018, species == "Engraulis ringens") %>%
  group_by (fleet, fishing_sector) %>%
  mutate (prop = tonnes/sum(tonnes)) %>%
  select (-c(tonnes, year)) %>%
  # join to 2019 totals
  left_join (peru_anchov_2019_totals, by = c ("fishing_sector", "fleet")) %>%
  mutate (tonnes_corrected = prop * tot, .keep = "unused")

# # fill in artisanal and other? this seems dumb
fill_in_rows <- data.frame (
  country = "Peru",
  species = "Engraulis ringens",
  fishing_sector = c(rep("Artisanal", 2), rep("Industrial", 3)),
  fleet = c(rep("Domestic catch", 4), "Foreign catch"),
  end_use_type = c("Fishmeal and fish oil", "Other", "Direct human consumption", "Other", "Other"),
  tonnes_corrected = 0)

peru_anchov_corrected <- rbind (peru_anchov_2018_correction, fill_in_rows)

# correct in the main dataframe; can't figure out how to cut the peru anchovy and rbind
sau_anchov_join <- sau_2019 %>%
  left_join (peru_anchov_corrected, by = c ("country", "species", "fishing_sector", "fleet", "end_use_type")) %>%
  mutate (tonnes = case_when (
    country == "Peru" & species == "Engraulis ringens" ~ tonnes_corrected,
    TRUE ~ tonnes)) %>%
  # get rid of prop column
  select (-tonnes_corrected)
  


saveRDS (sau_anchov_join , file = "Data/SAU_2019.Rds")

# make species to taxa conversion df ----
c <- read.csv("Data/SAU EEZ 2019.csv")
indo <-  read.csv("Data/SAU EEZ indonesia.csv")

c <- rbind (c, indo)

# other fishes and inverts... fish except cephalopods, "other demersal invertebrates", jellyfish

# remaining: urchins, echinoderms, sea cucumbers
# Loxechinus albus
# Pyura chilensis
# Miscellaneous aquatic invertebrates
# Athyonidium chilensis -- sea cucumber
# Holothuriidae	
# Echinodermata
# Stichopus
other_sau <- c %>% filter (functional_group == "Other demersal invertebrates" & commercial_group == "Other fishes & inverts") %>% pull (scientific_name) %>% unique()

sau_2019_taxa <- c %>%
  select (scientific_name, commercial_group, functional_group) %>%
  distinct () %>%
  mutate (taxa = case_when (
    commercial_group == "Molluscs" & !functional_group %in% c("Cephalopods", "Jellyfish") ~ "Mollusc",
    functional_group == "Cephalopods" ~ "Cephalopod",
    commercial_group == "Crustaceans" ~ "Crustacean",
    functional_group == "Jellyfish" ~ "Other",
    scientific_name %in% other_sau   ~ "Other",
    TRUE ~ "Finfish"
  )) %>%
  rename (species = scientific_name)

saveRDS (sau_2019_taxa, file = "Data/SAU_2019_taxa.Rds")

