# Prevalance of inadequate intake for each country
# 6/8/23
# JGM

library (tidyverse)

pii <- read_csv ("Data/Beal_2017_SI_inadequate_micronutrients.csv")


peru_pii <- pii %>% 
  filter (Country %in% c("Peru", "Sierra Leone","Chile", "Indonesia", "Malawi"), Year == 2011, Micronutrient %in% c("Calcium", "Zinc", "Vitamin A", "Iron")) %>%
  select (Country, Fortification, Micronutrient, `Prevalence of Inadequate Intake`)  %>%
    #pivot_wider (Fortification)
  arrange (Country, Fortification desc (round(`Prevalence of Inadequate Intake`, 2))) %>% View()
