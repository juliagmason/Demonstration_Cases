# RDAs met from current catch
# 3/17/22
# JGM

# following hicks et al more closely
library (tidyverse)


# RDAs data ----
# made in Species_level_nutrient_content
rda_groups <- readRDS("Data/RDAs_5groups.Rds")


# baseline catch data in servings ----
ds_catch_nutr_yield_baseline <- readRDS("Data/ds_catch_nutr_yield_baseline.Rds")

# projected nutritrient yield under management scenarios ----
# from Free data. Convert_catch_to_nutrients.R
ds_catch_nutr_yield_projected <- readRDS("Data/ds_catch_nutr_yield_projected.Rds")

# population data ----

# downloaded from https://population.un.org/wpp/Download/Standard/CSV/ on 3/17/2022
# Population by 5-year age groups.
# PopMale: Male population in the age group (thousands)
# PopFemale: Female population in the age group (thousands)
# PopTotal: Total population in the age group (thousands)

pop <- read_csv("Data/WPP2019_PopulationByAgeSex_Medium.csv")

# separate into broad adult male/female and child categories to  match RDAs
pop_current <- pop %>%
  filter (Location %in% c("Sierra Leone", "Indonesia", "Peru", "Chile", "Malawi", "Mexico"), Time == 2020) %>%
  mutate (group = 
            case_when (
              AgeGrp %in% c("0-4", "5-9") ~ "Child",
              !AgeGrp %in% c("0-4", "5-9") ~ "Adult")
  ) %>%
  group_by (Location) %>%
  summarize (Pop_Males =  1000 * sum (PopMale[group == "Adult"]),
             Pop_Females = 1000 * sum(PopFemale[group == "Adult"]),
             Pop_Child = 1000 * sum(PopTotal[group == "Child"])) %>%
             # also have total population that would eat fish--sum of males, females, and children maybe do this later?
             #Pop_Total = sum (Pop_Males, Pop_Females, Pop_Child)) %>%
  pivot_longer (cols = starts_with("Pop"),
                names_prefix = "Pop_", 
                names_to = "group", 
                values_to = "population") %>%
  rename (country = Location)


# future population in decadal time periods ----

pop_future <- pop %>%
  filter (Location %in% c("Sierra Leone", "Indonesia", "Peru", "Chile", "Malawi", "Mexico"), Time %in% c(2026:2035, 2051:2060, 2091:2100)) %>%
  mutate (group = 
            case_when (
              AgeGrp %in% c("0-4", "5-9") ~ "Child",
              !AgeGrp %in% c("0-4", "5-9") ~ "Adult")
  ) %>%
  group_by (Location, Time) %>%
  summarize (Pop_Males =  1000 * sum (PopMale[group == "Adult"]),
             Pop_Females = 1000 * sum(PopFemale[group == "Adult"]),
             Pop_Child = 1000 * sum(PopTotal[group == "Child"])) %>%
  ungroup() %>%
  mutate (
    period = case_when (
      Time %in% c(2026:2035) ~ "2026-2035",
      Time %in% c(2051:2060) ~ "2051-2060",
      Time %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  group_by (Location, period) %>%
  summarise (Pop_Males = mean (Pop_Males),
             Pop_Females = mean (Pop_Females),
             Pop_Child = mean (Pop_Child)) %>%
  pivot_longer (cols = starts_with("Pop"),
                names_prefix = "Pop_", 
                names_to = "group", 
                values_to = "population") %>%
  rename (country = Location)

saveRDS(pop_future, file = "Data/country_pop_projections.Rds")

# convert current catch to rdas met ----
 
rdas_met_current_catch <- ds_catch_nutr_yield_baseline %>%
  # calculate total nutrient servings per day in each country
  group_by (country, nutrient) %>%
  summarise (tot_nutr_servings = sum (nutr_servings, na.rm = TRUE)) %>%
  left_join (rda_groups, by = "nutrient") %>%
  left_join (pop_current, by = c("country", "group")) %>%
  filter (!is.na(population)) %>%
  mutate (yield_per_cap = tot_nutr_servings / population,
          rda_needs = mean_rda * population,
          rda_met = tot_nutr_servings/rda_needs) # this is equivalent to yield_per_cap/mean_rda


# also do this by species ----
rdas_met_current_catch_spp <- ds_catch_nutr_yield_baseline %>%
  select (country, species, major_group, nutrient, nutr_servings) %>%
  left_join (rda_groups, by = "nutrient") %>%
  left_join (pop_current, by = c("country", "group")) %>%
  filter (!is.na(population)) %>%
  mutate (yield_per_cap = nutr_servings / population,
          rda_needs = mean_rda * population,
          # proportion of population rda met for each group
          rda_met = nutr_servings/rda_needs)

saveRDS(rdas_met_current_catch_spp, file = "Data/ds_baseline_RDAs_met.Rds")

# convert future catch to rdas met----
ds_sm <-  sample_n(ds_catch_nutr_yield_projected, 5000)

rdas_met_projected_spp <- ds_catch_nutr_yield_projected %>%
  # summarize by period
  select (-c(catch_mt, amount, dens_units, pedible, meat_mt, nutr_mt, meat_servings)) %>%
  mutate (
    period = case_when (
      year %in% c(2026:2035) ~ "2026-2035",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, nutrient, species) %>%
    summarise (nutr_servings = mean (nutr_servings, na.rm = TRUE)) %>%
  
  left_join (rda_groups, by = "nutrient") %>%
  left_join (pop_future, by = c("country", "group", "period")) %>%
  filter (!is.na(population)) %>%
  mutate (yield_per_cap = nutr_servings / population,
          rda_needs = mean_rda * population,
          # proportion of population rda met for each group
          rda_met = nutr_servings/rda_needs)

saveRDS (rdas_met_projected_spp, file = "Data/ds_projected_RDAs_met.Rds")



# plot ----  
# https://thomasadventure.blog/posts/labels-ggplot2-bar-chart/
library (ggcharts) 
# to label proportion on stacked bar charts

rdas_met_current_catch %>%
  filter (!is.na(rda_met)) %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = group)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  geom_text (aes (label = round(rda_met, 1), y = rda_met + 0.5), position = position_dodge(width = 1)) +
  theme_bw() +
  labs (x = "", fill = "", y = "")


rdas_met_current_catch_spp %>%
  filter (!is.na(rda_met), species != "Engraulis ringens") %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = group)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  geom_text (aes (label = round(rda_met, 1), y = rda_met + 0.5), position = position_dodge(width = 1)) +
  theme_bw() +
  labs (x = "", fill = "", y = "")


# plot for overall population
rdas_met_current_catch_spp %>%
  filter (!is.na (nutr_servings)) %>%
  group_by (country, nutrient, species) %>%
  mutate (tot_pop = sum(population),
             tot_rda_needs = sum(rda_needs),
             tot_nutr_servings = first (nutr_servings),
             tot_rda_met = nutr_servings / tot_rda_needs) %>%
  # get rid of groups, otherwise triple counting
  select (country, species, major_group, nutrient, tot_rda_met) %>%
  distinct() %>% 
  ggplot (aes (y = tot_rda_met, x = nutrient, fill = major_group)) +
  geom_bar (stat = "identity") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free") +
  labs (y = "Proportion of population RDAs met") +
  ggtitle ("Proportion of population RDAs met by current catch, BAU")


rdas_met_current_catch_spp %>%
  filter (!is.na (nutr_servings)) %>%
  group_by (country, nutrient, species) %>%
  mutate (tot_pop = sum(population),
          tot_rda_needs = sum(rda_needs),
          tot_nutr_servings = first (nutr_servings),
          tot_rda_met = nutr_servings / tot_rda_needs,
          anchovy = ifelse (species == "Engraulis ringens", "Anchovy", "Other")) %>%
 ggplot (aes (y = tot_rda_met, x = nutrient, fill = anchovy)) +
  geom_bar (stat = "identity") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free")

# plot for overall population, future
t <- rdas_met_projected_spp %>%
  group_by (country, rcp, scenario, period, nutrient, species) %>%
  mutate (tot_pop = sum(population),
          tot_rda_needs = sum(rda_needs),
          tot_nutr_servings = first (nutr_servings),
          tot_rda_met = nutr_servings / tot_rda_needs) %>%
  # get rid of groups, otherwise triple counting
  select (country, rcp, scenario, period, species, nutrient, tot_rda_met) %>%
  distinct() 

# t %>% 
#   filter (country == "Chile") %>%
#   ggplot (aes (x = tot_rda_met, y = period, fill = scenario)) +
#   geom_bar (stat = "identity", position = "dodge") +
#   facet_grid (nutrient ~ rcp, scales = "free") +
#   theme_bw() +
#   labs(x="Percent of RDAs met from fisheries reforms", y="", fill = "Management\nscenario") +
#   ggtitle ("Proportion of population RDAs met under future management scenarios")


# flip so free scales has an effect
t %>% 
  filter (country == "Chile", species != "Engraulis ringens") %>%
  ggplot (aes (y = tot_rda_met, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (nutrient ~ rcp, scales = "free") +
  theme_bw() +
  labs(y="Percent of RDAs met from fisheries reforms", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of population RDAs met under future management scenarios")


# sanity check, why such big amounts?? ----

# nutricast uses nutr_demand with EARs, in mt
#from nutricast code --> calc_nutr_deficiencies/Step4_calculate_nutrient_demand_hist_proj.R
# supply required is in metric tons for the whole year
nutr_demand_free <- readRDS(file.path ("../nutrient_endowment/output/1960_2100_nutrient_demand_by_country.Rds"))
# CF used _50perc for calculations
# this is summed for the whole country, with projected population growth (50% estimate; this is where the 50% comes from)

x <- nutr_demand_free %>%
  filter (country %in% c("Sierra Leone", "Indonesia", "Peru", "Chile"),
          nutrient %in% c("Calcium", "Iron", "Zinc",  "Vitamin A", "Selenium"),
          year == 2020) %>%
  select (country, nutrient, supply_req_mt_yr_50perc)

z <- ds_catch_nutr_yield_baseline %>%
  group_by (country, nutrient) %>%
  summarise (yield_mt = sum (nutr_mt, na.rm = TRUE))

# free supply required is considerably bigger than my yield because he used an optimization function based on EARs to achieve 95% of population not nutrient deficient. but based on sd/cv of 0.25, not sure where that comes from; probably should use RDAs. 
t2 <- rdas_met_current_catch %>%
  # convert rda needs to mt
  mutate(
  dens_units = 
  case_when (
    nutrient %in% c("Protein", "Omega_3") ~ "g",
    nutrient %in% c("Vitamin_A", "Selenium") ~ "ug",
    TRUE ~ "mg"
  ),
  # rda needs are in native unit/day * 365 days/yr * 1kg / 1000 g * 1metric ton / 1000 kg
  #rda_mt = measurements::conv_unit(rda_needs, dens_units, "metric_ton") * 365
  rda_mt = case_when (
    nutrient %in% c ("Calcium", "Iron", "Zinc") ~ rda_needs * 365 / 1000 / 1000 / 1000,
    nutrient %in% c ("Selenium", "Vitamin_A") ~ rda_needs * 365 / 1000 / 1000 / 1000 / 1000,
    nutrient %in% c ("Omega_3", "Protein") ~ rda_needs * 365 / 1000 / 1000
  )
  ) %>%
  left_join (z, by = c ("country", "nutrient")) %>%
  left_join (x, by = c("country", "nutrient"))


# problem is unit conversion??
conv_unit (850 * 2428079, "mg", "metric_ton") * 365
conv_unit (0.7 * 2428079, "g", "metric_ton") * 365
0.7 * 2428079 * 365 / 1000 / 1000

# conv_unit not recycling properly when used in nutate; better to do by hand for now

# problem is also that Free method is for the entire population, so I would need to add up all my rda_mt to get there? it's still a lot smaller, but closer to the right order of magnitude. 
# check RDAs vs Ears
ears <- readRDS("Data/dietary_reference_intake_data.Rds") %>%
  # Reduce to EARs
  filter(dri_type=="Estimated Average Requirement (EAR)" & !is.na(value)) %>%
  select (nutrient, units, sex_stage, age_range, value) %>%
  group_by (nutrient, sex_stage) %>%
  summarise (ear_value = mean(value, na.rm = TRUE)) %>%
  mutate (group = 
            case_when (sex_stage == "Women (pregnant)" ~ "Pregnant",
                       sex_stage == "Women (lactating)" ~ "Lactating", 
                     sex_stage == "Women" ~ "Females",
                     sex_stage == "Men" ~ "Males",
                     sex_stage == "Children" ~ "Child"))

rda_groups %>%
  left_join (ears, by = c ("nutrient", "group")) %>% View()
# RDA is consistently larger than EAR; this makes sense. definitely something weird with EARs for protein. 