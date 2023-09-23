# ARTIS explore
# 8/15/22

# https://github.com/Seafood-Globalization-Lab/exploreARTIS

library (sf)
#devtools::install_github("Seafood-Globalization-Lab/exploreARTIS", dependencies = TRUE)
library(exploreARTIS)
library (tidyverse)

plot_bar(artis, bar_group = "exporter_iso3c")

colnames (artis)


# 9/20/23 looking at updated SAU species, not differentiated by aquaculture/marine, compare with previous dataset
exports_sau <- read_csv ("Data/20230830_edf_ARTIS_snet.csv") %>%
  mutate (species = str_to_sentence(sciname),
          prop_exp = exports_percent_of_prod
          /100,
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone"
          ), .keep = "unused") #%>%
  # # remove aquaculture and inland
  # filter (habitat == "marine", method == "capture")

exports_previous <- read_csv("Data/20230622_edf_ARTIS_full_spp.csv") %>%
  mutate (species = str_to_sentence(sciname),
          prop_exp = export_percent_production
          /100,
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone"
          ), .keep = "unused")


peru_previous_marine_only <- exports_previous %>%
  filter (country == "Peru", year == 2019, habitat == "marine", method == "capture") %>%
  arrange (species)

peru_new <- exports_sau %>% filter (year == 2019, country == "Peru")

# yes, these are quite different. different species and new one has way more production

# clip to SAU species 
sau_2019 <- readRDS("Data/SAU_2019.Rds")
sau_per <- sau_2019 %>% filter (country == "Peru")

peru_previous_sau_clip <- peru_previous_marine_only %>% filter (species %in% sau_per$species)
peru_new_sau_clip <- peru_new %>% filter (species %in% sau_per$species)

head (peru_previous_sau_clip)
head (peru_new_sau_clip)

peru_previous_sau_clip %>% arrange (desc((production))) %>% head (10)
peru_new_sau_clip %>% arrange (desc((production_t))) %>% head (10)

# new dataset says 77.5% of peru anchovy exported, previous was 89.9%

# are there aquaculture species in the sau data?! in the chl landings datA?
# Chile country specific 
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
# very minimal catch of O. tshawytcha