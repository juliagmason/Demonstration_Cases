# RDAs met from current catch
# 3/17/22
# JGM

# following hicks et al more closely
library (tidyverse)

ds_spp_nutr_content <- readRDS("Data/ds_spp_nutr_content_FishNutrientsGENuS_RDA_groups.Rds")

# Species specific projection data from Chris Free 2020 ----

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")

# population data ----

# downloaded from https://population.un.org/wpp/Download/Standard/CSV/
# Population by 5-year age groups.
# PopMale: Male population in the age group (thousands)
# PopFemale: Female population in the age group (thousands)
# PopTotal: Total population in the age group (thousands)

pop <- read_csv("Data/WPP2019_PopulationByAgeSex_Medium.csv")

# separate into broad adult male/female and child categories
pop_current <- pop %>%
  filter (Location %in% c("Sierra Leone", "Indonesia", "Peru", "Chile", "Malawi"), Time == 2020) %>%
  mutate (group = 
            case_when (
              AgeGrp %in% c("0-4", "5-9") ~ "Child",
              !AgeGrp %in% c("0-4", "5-9") ~ "Adult")
  ) %>%
  group_by (Location) %>%
  summarize (Pop_Males =  1000 * sum (PopMale[group == "Adult"]),
             Pop_Females = 1000 * sum(PopFemale[group == "Adult"]),
             Pop_Child = 1000 * sum(PopTotal[group == "Child"])) %>%
  pivot_longer (cols = starts_with("Pop"),
                names_prefix = "Pop_", 
                names_to = "group", 
                values_to = "population") %>%
  rename (country = Location)
  

rdas_met_current_catch <- ds_spp_nutr_content %>%
  filter (!is.na(amount)) %>%
  # have to convert catch_mt/yr into 100g servings/day. 1000 kg/mt * 1000 g/kg * 1 serving/100g * 1 yr/365 days
  mutate (catch_servings = catch_mt * 1000 * 1000 / 100 / 365, 
          spp_nutr = amount * catch_servings) %>%
  group_by (country, nutrient, group) %>%
  mutate (tot_nutr = sum (spp_nutr)) %>%
  select (country, nutrient, group, unit, mean_rda, tot_nutr) %>%
  distinct() %>%
  left_join (pop_current, by = c("country", "group")) %>%
  mutate (yield_per_cap = tot_nutr / population,
          rda_needs = mean_rda * population,
          rda_met = tot_nutr/rda_needs)


# really big amounts. sanity check
  
# https://thomasadventure.blog/posts/labels-ggplot2-bar-chart/
library (ggcharts)

rdas_met_current_catch %>%
  filter (!is.na(rda_met)) %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = group)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  geom_text (aes (label = round(rda_met, 1), y = rda_met + 0.5), position = position_dodge(width = 1)) +
  theme_bw() +
  labs (x = "", fill = "", y = "")
