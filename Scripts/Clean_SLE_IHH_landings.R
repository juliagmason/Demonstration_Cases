# Clean and compile Sierra Leone IHH landings
# 3/8/23

# full data emailed 6/1/23

library (tidyverse)
library (readxl)
library (stringr) # for word()


sle_ssf <- read_excel("Data/IHH Siera Leone SSF and LSF data for paper.xlsx", sheet = "SieraLeone_SSF") %>%
#tons of variables, wide format. assuming catch is in metric tonnes
  select (
    fishery, catch_2018:catch_2013
  ) %>%
  rename (species = fishery) %>%
  pivot_longer (cols = starts_with("catch"),
                names_to = "year",
                names_prefix = "catch_",
                #names_sep = "_",
                values_to = "catch_mt") %>%
  mutate (country = "Sierra Leone",
          sector = "Small-scale")

sle_ind <- read_excel("Data/IHH Siera Leone SSF and LSF data for paper.xlsx", sheet = "SierraLeone_LSF") %>%
  select (
    Lowest_taxonomic_id, year, total_LSF_catch
  ) %>%
  rename (species = Lowest_taxonomic_id, catch_mt = total_LSF_catch) %>%
  mutate (country = "Sierra Leone",
          sector = "Large-scale")

sle_landings_ihh <- rbind (sle_ssf, sle_ind) %>%
  # change species to match SAU/fish nutrients
 
  mutate (species = case_when (
    # remove instances of "spp", just take the first word/characters before space
    grepl ("spp", species, ignore.case = TRUE) ~word(species, 1),
    # fix capital/spelling issue
    species == "Scomboromorus Tritor" ~ "Scomberomorus tritor",
    species == "Dentex Congoinsis" ~ "Dentex congoinsis",
    species == "Katswonus pelamis" ~ "Katsuwonus pelamis",
    species == "Galoides decadactylus" ~ "Galeoides decadactylus",
    species ==  "Illex coindeti" ~ "Illex coindetii",
    species == "Pseudupenaeus prayensis" ~ "Pseudupeneus prayensis",
    # A. alexandrinus not in fishnutr, assume the same
    species == "Alectis alexandrinus" ~ "Alectis alexandrina",
    TRUE ~ species),
    year = as.integer(year)
  )


# attempt to add commercial group
# start by matching with SAU
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

sle_landings_ihh <- sle_landings_ihh %>%
left_join (sau_2019_taxa, by = "species") %>%
  # copy from afcd_explore 
  mutate (
    commercial_group =  case_when (
      species %in% c("Octopus vulgaris", "Illex coindetii", "Sepia") ~ "Other fishes & inverts",
      species %in% c("Penaeus notialis",   "Penaeus kerathurus", "Callinectes","Panulirus", "Holthuispenaeopsis atlantica", "Parapeneopsis atlantica") ~ "Crustaceans",
      # maybe check these
      species %in% c("Arius", "Gerres", "Dentex congoinsis", "Diodon holocanthus","Dactylopterus volitans", "Priacanthus arenatus", "Pseudotolithus brachygnathus", "Albula vulpes", "Sphyraena afra", "Decapturus rhonsus") ~ "Other fishes & inverts",
      species == "Cymbium" ~ "Other fishes & inverts",
      TRUE ~ commercial_group
    ))
    

saveRDS(sle_landings_ihh, file = "Data/SLE_landings_IHH.Rds")  

# quick plot check
sle_landings_ihh %>%
  group_by (year, sector) %>%
  summarize (tot = sum (catch_mt, na.rm = TRUE)) %>%
  ggplot (aes (x = year, y = tot, fill = sector)) +
  geom_area(position = "stack")


## compare with sau ----
sau_2015_2019 <- readRDS("Data/SAU_2015_2019.Rds")

sau_sl <- sau_2015_2019 %>%
  filter (country == "Sierra Leone", species %in% c ("Ethmalosa fimbriata", "Engraulis encrasicolus", "Sardinella maderensis", "Sardinella aurita", "Sardinella"))

# sau has no industrial catch that is also from sierra leone 

sau_sl %>%
  filter (fishing_entity == "Sierra Leone") %>%
  group_by (year, species, fishing_sector) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>% View()
