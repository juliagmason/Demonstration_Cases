###Check SAU/Free close species

# 4/22/22
# JGM

#For Chile: Brama australis--> Brama brama? Atlantic pomfret. different resilience and temperature...Just the one other fish in teh genus

#Merluccius gayi---M. australis?
  
#For Peru: 
# Sarda chiliensis do have nutrient info. In fishcast. So is M. gayi
# have Sarda orientalis, and sarda (atlantic)

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")


merluc <- ds_spp %>%
  filter (grepl ("Merluccius", species), country %in% c ("Chile", "Peru"), catch_mt > 0)


merluc %>%
  filter (country == "Chile") %>%
  ggplot() +
  geom_line (aes (x = year, y = catch_mt, col = species)) +
  facet_grid(scenario ~ rcp) +
  theme_bw()

merluc %>%
  filter (country == "Peru") %>%
  ggplot() +
  geom_line (aes (x = year, y = catch_mt, col = species)) +
  facet_grid(scenario ~ rcp) +
  theme_bw()
# no catch for peru

sarda <- ds_spp %>%
  filter (grepl ("Sarda", species))

sarda %>%
  #filter (country == "Chile") %>%
  filter (species == "Sarda orientalis", country %in% c ("Chile", "Peru")) %>%
  ggplot() +
  geom_line (aes (x = year, y = catch_mt, col = country)) +
  facet_grid(scenario ~ rcp) +
  theme_bw()

# no catch of sarda sarda in chile obv. Sarda doesn't show in nutricast so I don't know what to compare it to?