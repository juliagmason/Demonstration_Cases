# Compile nutrient data ----

# 3/29/23
library (tidyverse)
# dumb that I'm doing the fishnutr_long thing every time

# should compile with taxa column, then reconfigure function_convert to not take taxa, and put p_edible in the mutate with case_When

# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
# this is per 100g raw, edible portion
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")

# truncate to just summary predicted value. eventually will want range?
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu"))


fishnutr_long <- fishnutr_mu %>% 
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4), 
          taxa = "Finfish")


# d gigas data from Bianchi et al. 2022 supp table 2. Vita A is retinol equiv; omega 3 is n-3 fatty acids
d_gigas_nutr <- data.frame (
  species = "Dosidicus gigas", 
  nutrient = c ("Calcium", "Iron", "Omega_3", "Protein", "Selenium", "Vitamin_A", "Zinc"),
  amount = c(37.5, 3.3, 0.6, 16.4, 50.9, 0, 2.8), 
  taxa = "Cephalopod"
)


# afcd data for nonfish ----
# compiled/cleaned in AFCD_explore.R

# something weird with taxa! some are plural some not!!
nonfish_afcd_nutr <- readRDS("Data/nonfish_afcd_nutr_compiled.Rds") %>%
  select (species, nutrient, amount, taxa) %>%
  filter (species != "Dosidicus gigas") %>%
  mutate (taxa = case_when (
    taxa == "Cephalopods" ~ "Cephalopod",
    taxa == "Crustaceans" ~ "Crustacean",
    taxa == "Molluscs" ~ "Mollusc",
    TRUE ~ taxa
  ))

# also duplicates?
nonfish_afcd_nutr <- nonfish_afcd_nutr[!duplicated(nonfish_afcd_nutr), ]

# Malawi fish ----
# Per Abby email, average O. lidole and O. karongae for chambo. Also average M. inornata and c. inornata for Utaka, or maybe they're the same name?
# M. inornata in fishnutr but not C. inornata
chambo <- fishnutr_long %>%
  filter (species %in% c("Oreochromis lidole", "Oreochromis karongae")) %>%
  mutate (species = "Oreochromis lidole combined") %>%
  group_by (species, nutrient, taxa) %>%
  summarise (amount = mean(amount)) %>%
  select (species, nutrient, amount, taxa)


compiled_nutr <- rbind (fishnutr_long, d_gigas_nutr, nonfish_afcd_nutr, chambo)
saveRDS(compiled_nutr, file = "Data/species_nutrients_compiled.Rds")
