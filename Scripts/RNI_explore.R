## RNIs

# jgm
#10 10 2020
# rni.rds emailed by Rachel Zuercher on 5 oct 2022
library (tidyverse)

rni <- readRDS("Data/RNI.RDA.Rds")

# this just uses adult lactatic female omega 3 AI values, so take those from nutricast dris
# DRIS data from Free nutrient_endowment --> data/ears/data
dris <- readRDS("Data/dietary_reference_intake_data.Rds")

# no RDA for omega 3, only AI, adequate intake. 
ai_omega <- dris %>%
  filter (grepl("Linolenic", nutrient)) %>% 
mutate (group = 
          case_when (
            age_range %in% c("6-12 mo",  "1-3 yr",  "4-8 yr") ~ "Child",
            !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "None" ~ "Females", 
            !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "Pregnancy" ~ "Pregnant", 
            !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Females"  & stage == "Lactation" ~ "Lactating", 
            !age_range %in% c("0-6 mo",   "6-12 mo",  "1-3 yr",  "4-8 yr") & sex == "Males" ~ "Males",
            TRUE ~ NA_character_
          )) %>%
  group_by (nutrient, group) %>% 
  summarise (unit = first(units),
             RNI = mean (value, na.rm = TRUE)) %>%
  filter (!is.na(group)) %>%
  # match nutrient names; assume linolenic is omega 3
  mutate (nutrient = "Omega_3")

omega_child <- ai_omega %>% filter (group == "Child") %>%
  select (nutrient, RNI)

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
  ) %>%
  
  # remove omega 3s and "fish"
  filter (!nutrient %in% c("fish", "Omega_3"))

# append omega 3 AI
rni_child_long <- rbind (rni_child_long, omega_child)

saveRDS (rni_child_long, file = "data/RNI_child.Rds")


# mean of adults?
rni_adult_long <- rni %>%
  filter (!Age %in% c("7-12 months", "1-3 years", "4-6 years", "pregnant_tri1",    "pregnant_tri2",    "pregnant_tri3", "lactating_0-3mo",  "lactating_3-6om",  "lactating_7-12mo")) %>%
  pivot_longer (-c(Sex, Age), 
                names_to = "nutrient", 
                values_to = "RNI") %>%
  group_by (nutrient) %>%
  summarise (RNI = mean (RNI)) %>%
  mutate (nutrient = sub("_.*", "", nutrient), 
          nutrient = case_when (nutrient == "Omega3" ~ "Omega_3",
                                nutrient == "VitaminA" ~ "Vitamin_A",
                                TRUE ~ nutrient)
  ) %>%
  filter (!nutrient == "fish")

saveRDS (rni_adult_long, file = "data/RNI_adult.Rds")  
