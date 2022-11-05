## RNIs

# jgm
#10 10 2020
# rni.rds emailed by Rachel Zuercher on 5 oct 2022
library (tidyverse)

rni <- readRDS("Data/RNI.RDA.Rds")

rni_child_long <- rni %>%
  filter (Age %in% c("7-12 months", "1-3 years", "4-6 years")) %>%
  pivot_longer (-c(Sex, Age), 
                names_to = "nutrient", 
                values_to = "RNI") %>%
  group_by (nutrient) %>%
  summarise (RNI = mean (RNI)) %>%
  mutate (nutrient = sub("_.*", "", nutrient), 
          nutrient = case_when (nutrient == "Omega3" ~ "Omega_3",
                                nutrient == "VitaminA" ~ "Vitamin_A",
                                TRUE ~ nutrient)
  )

saveRDS (rni_child_long, file = "data/RNI_child.Rds")

          