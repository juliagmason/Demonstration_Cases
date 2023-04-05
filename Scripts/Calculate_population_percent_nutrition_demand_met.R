# Plot nutrient contribution as percent population demand met
# 4/3/23
# JGM

library (tidyverse)

# revisiting nutricast code, demonstration_cases_SAU_SSF_explore
# https://github.com/cfree14/nutrient_endowment/blob/master/code/calc_nutr_deficiencies/Step4_calculate_nutrient_demand_hist_proj.R

# joins to nutrient data in shiny/v3/page3_Fig2c at the bottom

#pop_proj_req_supply <- readRDS("../nutrient_endowment/output/1960_2100_nutrient_demand_by_country.Rds")
# this is nutrient supply required (in metric tons) to meet projected and historical nutrient demand of 50 and 95% of the population, sensitive to age and sex. 
# based on EARs and excludes children
# this is per year, has been multiplied by 365

# full rnis, not just child
# rni.rds emailed by Rachel Zuercher on 5 oct 2022
rni <- readRDS("Data/RNI.RDA.Rds")

# updated WPP data
# https://population.un.org/wpp/Download/Standard/CSV/
# chose option population -1 January by 5 yr age groups, medium variant
# numbers are in 1000s
wpp_pop <- read_csv("Data/WPP2022_Population1JanuaryByAge5GroupSex_Medium.csv")


# Build age matching key----
# not perfect. take average RNI for 0-3 years to match 0-4
# for MOST micronutrients, level for 7-9 is higher than 4-6. so using 7-9 level for ages 5-9 is conservative.

# first do avg of rni for infants? probably a better way to do this
rni_merge_infants <- rni %>%
  mutate (age_infant = case_when (
    Age %in% c("0-6 months", "7-12 months","1-3 years") ~ "0-3",
    TRUE ~ Age
  )) %>% 
  group_by (Sex, age_infant) %>%
  summarise (across(Calcium_mg_day:Omega3_PUFA_g_day, ~mean(.x, na.rm = TRUE))) %>%
  # rename age range to match
  rename (age_range_rni = age_infant) %>%
  # pivot longer
  pivot_longer(Calcium_mg_day:Omega3_PUFA_g_day,
               names_to = "nutrient",
               values_to = "amount") %>%
  # fix names
  mutate (nutrient = sub("_.*", "", nutrient),
          nutrient = case_when (nutrient == "VitaminA" ~ "Vitamin_A",
                                nutrient == "Omega3" ~ "Omega_3",
                              TRUE ~ nutrient )) %>%
  filter (!nutrient == "fish")


age_range_key <- tibble(age_range_wpp=unique(wpp_pop$AgeGrp)) %>% 
  mutate(age_range_wpp=factor(age_range_wpp, levels = c ("0-4",
                                                         "5-9",
                                                         "10-14",
                                                         "15-19",
                                                         "20-24",
                                                         "25-29",
                                                         "30-34",
                                                         "35-39",
                                                         "40-44",
                                                         "45-49",
                                                         "50-54",
                                                         "55-59",
                                                         "60-64",
                                                         "65-69",
                                                         "70-74",
                                                         "75-79",
                                                         "80-84",
                                                         "85-89",
                                                         "90-94",
                                                         "95-99",
                                                         "100+")),
         age_range_rni = case_when(
           age_range_wpp == "0-4" ~ "0-3",
           age_range_wpp ==   "5-9" ~ "7-9 years",
           age_range_wpp %in% c("10-14", "15-19") ~ "10-18 years",
           age_range_wpp %in% c("20-24",
                                "25-29",
                                "30-34",
                                "35-39",
                                "40-44",
                                "45-49")~ "19-50 years",
           age_range_wpp %in% c("50-54",
                                "55-59",
                                "60-64") ~ "51-65 years",
           age_range_wpp %in% c("65-69",
                                "70-74",
                                "75-79",
                                "80-84",
                                "85-89",
                                "90-94",
                                "95-99",
                                "100+") ~  "65+"
  ))


# calculate annual nutrition demand ----
# reshape and join population data to 
wpp_long <- wpp_pop %>%
  filter (!is.na(ISO3_code)) %>%
  select (Location, ISO3_code, Time, AgeGrp, PopMale, PopFemale) %>%
  rename (country = Location, age_range_wpp = AgeGrp) %>%
  pivot_longer (PopMale:PopFemale,
                names_prefix = "Pop",
                names_to = "Sex",
                values_to = "Pop") %>%
  # cut to first letter to match rni
  mutate (Sex = substr(Sex, 1, 1)) %>%
  # join to age key
  left_join (age_range_key, by = "age_range_wpp") %>%
  # join to rni data
  left_join (rni_merge_infants, by = c ("Sex", "age_range_rni")) %>%
  # multiply population by amount needed. this is in native units so need to convert to tons
  mutate (scalar = case_when (
            nutrient %in% c("Protein", "Omega_3") ~ 1,
            nutrient %in% c("Calcium", "Zinc", "Iron") ~ 1/1000,
            nutrient %in% c("Vitamin_A", "Selenium") ~ 1/1e6),
          #multiply population by 1000
          nutr_annual_demand = Pop * 1000 * amount * scalar/1000/1000*365) 
          

saveRDS(wpp_long, "Data/annual_nutr_demand_rni_by_country_age_sex.Rds")

# also calculate total for each country
wpp_country_aggregate <- wpp_long %>%
  group_by (country, Time, nutrient) %>%
  summarise (tot_pop = sum (Pop),
             tot_nutr_annual_demand = sum (nutr_annual_demand))

saveRDS(wpp_country_aggregate, "Data/annual_nutr_demand_rni_by_country.Rds")


# relate landings to nutrition demand ----
# take current landings, convert to metric tons of nutrients, convert to proportion of current population

sau_2019 <- readRDS("Data/SAU_2019.Rds")
#Clean_Chile_Sernapesca_landings.R
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# nutrient data ----
# compiled in compile_species_nutrition_data.R
#this has Fishnutr, AFCD, and D. gigas
#this is amount in native units per 100g serving

compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")
# matched fishnutr data for missing species ----
# depends on country. Finfish only; potentially could do with nonfish, but might have them all through AFCD?
fish_taxamatch_nutr <- readRDS("Data/Matched_finfish_nutr.Rds") 


# convert catch to nutrient yield ----
#this will return the amount of edible nutrient yield in metric tons
convert_catch_to_nutr_tons <- function (species_name, catch_mt, country_name) {
  
  if (species_name %in% compiled_nutr$species) {
    nutr_content <- compiled_nutr %>% filter (species == species_name)
  } else  {
    nutr_content <- fish_taxamatch_nutr %>% filter (species == species_name, country == country_name)
  }
  
  catch_nutrients <- nutr_content %>%
    mutate (
      p_edible = case_when (
        taxa == "Finfish" ~ 0.87,
        taxa == "Crustacean" ~ 0.36,
        taxa == "Mollusc" ~ 0.17,
        # GENuS/nutricast is 0.21 for cephalopods. Using 0.67, Bianchi et al. 2022 value for D. gigas; only cephalopod in our priority species. They also have a blanket 0.7 value for cephalopods.  
        taxa == "Cephalopod" ~ 0.67,
        taxa == "Other" ~ 1),
      # have to put back into units
      scalar = case_when (
        nutrient %in% c("Protein", "Omega_3") ~ 1,
        nutrient %in% c("Calcium", "Zinc", "Iron") ~ 1/1000,
        nutrient %in% c("Vitamin_A", "Selenium") ~ 1/1e6
      ),
      # input (catch_mt) is in metric tons. amount is in units / 100g. so divide by 100 to account for serving size, and multiply by scalar to cancel out g
      nutr_tonnes = catch_mt * p_edible * amount * scalar / 100 ) %>%
    select (nutrient, nutr_tonnes)
  
  return (catch_nutrients)
}


# convert catch to proprotion of population demand met in a given time period
calculate_prop_demand_met <- function (country_name, year) {
  # year can be a range, e.g. 2051:2060
  
  if (country_name == "Chile") {
    
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      group_by (species) %>%
      summarise (catch_mt = sum (catch_mt)) %>%
      mutate (nutr_yield = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Chile"), convert_catch_to_nutr_tons)) %>%
      unnest(cols = c(nutr_yield),  names_repair = "check_unique") 
    
  } else {
    
    landings <- sau_2019 %>%
      filter(country == country_name) %>%
      group_by (species) %>%
      summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
      mutate (nutr_yield = pmap (list (species_name = species, catch_mt = catch_mt, country_name = country_name), convert_catch_to_nutr_tons)) %>%
      unnest(cols = c(nutr_yield),  names_repair = "check_unique")
  }
  
  prop_demand_met <- wpp_country_aggregate %>%
    filter (country == country_name, Time %in% year) %>%
    group_by (nutrient) %>%
    summarise (mean_annual_demand = mean(tot_nutr_annual_demand, na.rm = TRUE)) %>%
    left_join (sau_peru_nutr, by = "nutrient") %>%
    # this is proportion, not percent
    mutate (prop_demand_met = nutr_tonnes / mean_annual_demand)
  
  # this is proportion. multiply by 100 for percent
  return (prop_demand_met)
  
}

x <- calculate_prop_demand_met("Indonesia", 2022)

h <- calculate_prop_demand_met("Indonesia", 2051:2060)


# plot current landings ----

# plot by commercial group?
# bring back in sau_2019_taxa for groups
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

peru_demand_current <- pop_proj_req_supply %>%
  filter (country == "Peru", year == 2020) %>%
  mutate (nutrient = case_when (nutrient == "Vitamin A" ~ "Vitamin_A",
                                TRUE ~ nutrient))

a <- sau_2019 %>%
  filter(country == "Peru") %>%
  left_join (sau_2019_taxa, by = "species") %>%
  group_by (country, species, commercial_group) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Peru"), convert_catch_to_nutr_tons)) %>%
  unnest(cols = c(nutr_tonnes),  names_repair = "check_unique") %>%
  left_join (filter(wpp_country_aggregate, Time == 2019), by = c("nutrient", "country")) %>%
  mutate (prop_demand_met = nutr_tonnes / tot_nutr_annual_demand)

a %>%
  group_by (nutrient, commercial_group) %>%
  summarise (prop_demand_met = sum (prop_demand_met, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(nutrient, -prop_demand_met, na.rm = TRUE), y = prop_demand_met*100, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Proportion demand met\nMost recent year of landings, Peru") +
  labs (x = "", y = "% population RNIs met", fill = "Comm. group") 


b <- calculate_prop_demand_met("Peru", 2019) # seems to match


# plot nutricast upsides ----
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")

# averaged data for missing spp
catch_upside_relative_missing <- readRDS("Data/catch_upside_relative_repair_missing.Rds")

catch_upside_relative_repaired <- 
  rbind (catch_upside_relative, catch_upside_relative_missing)

# wpp population needs match periods up here?
wpp_nutricast_periods <- wpp_country_aggregate %>%
  mutate (
    period = case_when (
      Time %in% c(2051:2060) ~ "midcentury",
      Time %in% c(2091:2100) ~ "endcentury"
    )) %>%
  filter (!is.na(period)) %>%
  group_by (country, period, nutrient) %>%
  summarise (mean_annual_demand = mean(tot_nutr_annual_demand))
  


plot_nutr_upside_absolute_population <- function (country_name, Selenium = FALSE) {
  
  if (country_name == "Chile") {
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      group_by (species) %>%
      summarise (total_tonnes = sum (catch_mt)) %>%
      mutate (country = "Chile")
  } else {
    
    landings <- sau_2019 %>%
      filter (country == country_name) %>%
      group_by (country, species) %>%
      summarise (total_tonnes = sum (tonnes))
  }
  
  # fix colnames so can pivot_longer and break into period and scenario
  better_scenario_colnames <- gsub("ratio_", "", colnames(catch_upside_relative_repaired)[4:9])
  old_colnames <- colnames(catch_upside_relative_repaired)[4:9]
  
  upside_ratios_absolute <- landings %>% 
    left_join(catch_upside_relative_repaired, by = c ("country", "species")) %>%
    rename_with (~ better_scenario_colnames, all_of(old_colnames)) %>%
    mutate (# multiply ratio by current landings
      across(bau_midcentury:adapt_endcentury, ~.x * total_tonnes)) %>%
    
    select (country, rcp, species, bau_midcentury:adapt_endcentury) %>%
    pivot_longer(bau_midcentury:adapt_endcentury, 
                 names_to = c("scenario", "period"),
                 names_sep = "_",
                 values_to = "tonnes") %>%
    # get rid of non-matching species, NAs
    filter (!is.na (rcp)) %>%
    # convert to nutrients
    mutate (nutr_yield = pmap (list (species_name = species, catch_mt = tonnes, country_name = country), convert_catch_to_nutr_tons)) %>%
    unnest(cols = c(nutr_yield),  names_repair = "check_unique") 
  
  # fix levels
  upside_ratios_absolute$scenario <- factor(upside_ratios_absolute$scenario, levels = c ("bau", "mey", "adapt"))
  upside_ratios_absolute$period <- factor(upside_ratios_absolute$period, levels = c ("midcentury", "endcentury"))
  
  if (Selenium == TRUE) {
    upside_summary <- upside_ratios_absolute %>%
      group_by (country, rcp, scenario, period, nutrient) %>%
      summarise (total_yield = sum (nutr_tonnes, na.rm = TRUE)) %>%
      filter (!nutrient %in% c("Protein")) 
  } else {
    
    upside_summary <- upside_ratios_absolute %>%
      group_by (country, rcp, scenario, period, nutrient) %>%
      summarise (total_yield = sum (children_yield, na.rm = TRUE)) %>%
      filter (!nutrient %in% c("Protein", "Selenium")) 
  }
  
  # join to population needs
  
 prop_met <-  upside_summary %>%
    left_join (wpp_nutricast_periods, by = c ("country", "period", "nutrient")) %>%
    mutate (prop_met = total_yield/mean_annual_demand)
  
  prop_met %>%
    # preliminary plot
    ggplot (aes (x = reorder(nutrient, -total_yield), y = prop_met * 100, fill = scenario)) +
    geom_col (position = "dodge") +
    geom_hline (yintercept = 0, lty = 2) +
    facet_wrap (period ~ rcp, ncol =4) +
    theme_bw() +
    # roughly match colors from gaines et al
    scale_fill_manual (values = c ("firebrick", "mediumseagreen", "dodgerblue4")) +
    labs (y = "# Child RNIs met, millions", x = "", fill = "Management \nstrategy") +
    ggtitle ("Nutrition provisioning from climate-adaptive management")
}