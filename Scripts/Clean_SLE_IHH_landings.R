# Clean and compile Sierra Leone IHH landings
# 3/8/23

library (tidyverse)
library (readxl)

sle_ssf <- read_excel("Data/IHH Siera Leone disaggrated small pelagic data.xlsx", sheet = "SieraLeone_SSF_Lowest") %>%
#tons of variables, wide format. assuming catch is in metric tonnes
  select (
    country, fishery, catch_2018:catch_2013
  ) %>%
  rename (species = fishery) %>%
  pivot_longer (cols = starts_with("catch"),
                names_to = "year",
                names_prefix = "catch_",
                #names_sep = "_",
                values_to = "catch_mt") %>%
  mutate (sector = "Artisanal")

sle_ind <- read_excel("Data/IHH Siera Leone disaggrated small pelagic data.xlsx", sheet = "SieraLeone_LSF_Lowest") %>%
  select (
    country, Lowest_taxonomic_id, year, total_LSF_catch
  ) %>%
  rename (species = Lowest_taxonomic_id, catch_mt = total_LSF_catch) %>%
  mutate (sector = "Industrial")

sle_landings_ihh <- rbind (sle_ssf, sle_ind)

saveRDS(sle_landings_ihh, file = "Data/SLE_landings_IHH.Rds")  
