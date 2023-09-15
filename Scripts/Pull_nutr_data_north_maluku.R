# North Maluku CVA species nutrition graphs
# 8/1/23
# JGM

library (tidyverse)
library (googlesheets4)

# access species and catch from google sheet ----
# https://docs.google.com/spreadsheets/d/1XHXmREH76sOw025qiJ7KTqxNfRgl_91kmYZvLSc73x4/edit#gid=866480859
# id url code for North Maluku google sheet
gs_id <- "1XHXmREH76sOw025qiJ7KTqxNfRgl_91kmYZvLSc73x4"

# will ask for authorization, check all the boxes and give all permissions

spp_data <- read_sheet(ss = gs_id, sheet = "short_list_target") 

spp_list <- spp_data$species

# fish nutrition data ----
# this represents nutrient content in various units per 100g raw, edible portion
# these are outputs from a predictive trait based model, Hicks et al., 2019 http://www.nature.com/articles/s41586-019-1592-6
# obtained by running python script from  https://github.com/mamacneil/NutrientFishbase
# alternatively could download the Indonesia EEZ-level csv from https://fishbase.ca/Nutrients/NutrientSearch.php

fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
# truncate to just median summary nutr value and pivot longer
fishnutr_long <- fishnutr %>%
  select (species, ends_with ("_mu")) %>% 
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  # trim the _mu nutrient name endings
  mutate (nutrient = str_sub(nutrient, end = -4), 
          # not really relevant for this list which is just fish, but adding a "taxa" column for edible conversions
          taxa = "Finfish")

# any missing species?
spp_list[which (!spp_list %in% fishnutr_long$species)]
# [1] "Epinephelus radiatus" "Nemipterus nematopus"

# RNI data ----
# Recommended Nutrient Intakes (RNIs), set by the World Health Organization, represent daily intake values that would meet the nutrient requirements of 97.5% of the healthy population of a given age and sex. 
# We have been using RNIs for children (mean RNIs of age seven months to six years) since many of these nutrients are critical for childhood growth and development, so they represent a somewhat universal nutritionally vulnerable demographic group
# Compiled from Annex 1 of this WHO report: https://www.who.int/activities/establishing-global-nutrient-requirements
# We assumed 10% bioavailability for iron and moderate bioavailability for zinc. RNIs have not been set for Omega 3 fatty acids, so we used Adequate Intake values for 𝛼-Linolenic Acid for children aged 7 months to 8 years (Institute of Medicine, 2005: https://ods.od.nih.gov/HealthInformation/nutrientrecommendations.aspx). 
rni_child <- readRDS("Data/RNI_child.Rds")

# filter nutrition data for priority species, join to RNI data
NM_spp_nutr_rnis <- fishnutr_long %>%
  filter (species %in% spp_list) %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. 
  mutate (perc_rni = amount/RNI * 100,
          # shorten nutrient names so easier to see in graph
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup() 


# Plot as a big bar
# also calculate "micronutrient density" to order/rank overall nutrient contribution
# Maire et al, 2021 define micronutrient density as the sum of %RNI for nutrients of interest https://www.cell.com/current-biology/abstract/S0960-9822(21)00896-4
# for other EDF work, we've been omitting protein (focusing instead on micronutrients, although I think technically Omega 3s are not a micronutrient). Have also been omitting Selenium because it is only required in very trace amounts and is abundant in fish, so it tends to swamp any y axis.
  
# decide which nutrients you want to omit
omit_nutrients <- c("Protein", "Selenium")

NM_spp_nutr_rnis %>% 
  filter (!nutrient %in% omit_nutrients) %>%
  group_by (species) %>%
  mutate (micronutrient_density = sum (perc_rni),
          # shorten species name so legible on chart
          spp_short = ifelse (
            grepl(" ", species),
            paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
            species)
  ) %>%
  ungroup() %>%
  
  ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
  geom_col (position = "dodge") +
  #facet_wrap (~species) +
  theme_bw() +
  labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") +
  #ylim (c(0,100)) +
  ggtitle ("Nutrient content of selected species") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11, angle = 60, hjust = 1),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))


# Calculate potential nutrient provisioning from catch values ----

# function that takes a species name and catch amount (metric tons) and converts it to child RNI equivalents

calc_children_fed_func <- function (species_name, amount_mt) {
  #species name is latin name separated by a space, e.g. "Caesio cuning"
  # amount_mt is a number in metric tonnes, e.g., 1500
  
  catch_nutrients <- fishnutr_long %>%
    filter (species == species_name) %>%
    mutate (catch_mt = amount_mt,
            # convert from whole caught fish to edible portion. I lifted these from Chris Free's nutricast code; Roberts, 1998; Free et al., 2022. Leaving non-fish in here in case useful down the line
            p_edible = case_when (
              taxa == "Finfish" ~ 0.87,
              taxa == "Crustacean" ~ 0.36,
              taxa == "Mollusc" ~ 0.17,
              taxa == "Cephalopod" ~ 0.21,
              taxa == "Other" ~ 1,
              taxa == "Algae" ~ 1),
            # convert tons per year to 100g /day, proportion edible
            edible_servings = catch_mt * p_edible * 1000 * 1000 /100 / 365,
            nutrient_servings = edible_servings * amount) %>%
    # join to RNI data and divide total nutrient provisioning by RNI
    left_join (rni_child, by = "nutrient") %>%
    mutate (rni_equivalents = nutrient_servings / RNI) %>%
    select (nutrient, rni_equivalents)
  
  
}


NM_nutrient_provisioning <- spp_data %>%
  select (species, total_catch_kg) %>%
  mutate (
    # convert catch in kg to mt (although these values are very small?)
    catch_mt = total_catch_kg / 1000,
    # convert catch to rni_equivalents
    rni_equivalents = pmap (list (species_name = species, amount_mt = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 
