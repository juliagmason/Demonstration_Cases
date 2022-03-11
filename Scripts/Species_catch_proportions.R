# Smaller dataset with 2012 catch for comparing relative importance/contribution of species
# 3/11/22

library (tidyverse)

# Species specific projection data from Chris Free 2020 ----

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 


# https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
catch_props <- ds_spp %>% 
  filter (year == 2012, rcp == "RCP26", scenario == "No Adaptation", catch_mt > 0) %>%
  group_by (country, species) %>%
  summarize (tot_cat = sum(catch_mt)) %>%
  mutate (prop_catch = tot_cat / sum(tot_cat),
          n_spp = n(),
          rank_catch = dense_rank (desc (tot_cat)))

saveRDS (catch_props, file = "Data/ds_spp_catch_proportions_2012.Rds")


# quick explore of missing nutrients data
catch_props %>%
  filter (species %in% c("Sebastes levis", "Cynoponticus coniceps","Sebastes jordani"))

catch_props %>%
  group_by (country) %>%
  summarise (mean_prop = mean (prop_catch),
             min= min(prop_catch),
             max = max(prop_catch))
