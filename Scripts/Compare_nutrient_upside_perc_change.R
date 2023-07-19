# Calculate percent change nutricast
# 6/27/23
# JGM

library (tidyverse)
library (beepr)

# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")


# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# baseline catch from compiled data; from plot_contextual_landings.forecasts.R
full_baseline <- readRDS("Data/baseline_catch_sau_chl_ihh.Rds")


# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")

# averaged data for missing spp, scripts/check_SAU_nutricast_species
catch_upside_relative_missing <- readRDS("Data/catch_upside_relative_repair_missing.Rds")

catch_upside_relative_repaired <- 
  rbind (catch_upside_relative, catch_upside_relative_missing)


# multiply ratio by baseline
catch_upside_tonnes <- catch_upside_relative_repaired %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (across(where (is.double), ~. * bl_tonnes))

# remove "ratio" from columns to make it easier to pivot_longer
colnames (catch_upside_tonnes) <- gsub ("_ratio", "", colnames (catch_upside_tonnes))

catch_upside_tonnes_long <- catch_upside_tonnes %>%
  filter (-bl_tonnes) %>%
  pivot_longer(-c(country, rcp, species),
               names_to = c("scenario", "period"),
               names_sep = "_",
               values_to = "tonnes") %>%
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) 

# make separate one for bl_tonnes???!? doing this in a dumb way
bl_nutr <- full_baseline %>%
  mutate(rni_equivalents= pmap (list (species = species, amount = bl_tonnes, country_name = country), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  rename (baseline_rni_equivalents = rni_equivalents)

catch_upside_tonnes_long_compare <- catch_upside_tonnes_long %>%
  left_join (bl_nutr, by = c ("country", "species", "nutrient"))


# for Abby, make table of percent change for rcp 6.0
#not summarisizing so I can filter out anchovy

indo <- catch_upside_tonnes_long_compare %>%
  filter (country == "Indonesia", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

write.excel(indo)

chile <- catch_upside_tonnes_long_compare %>%
  filter (country == "Chile", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

write.excel(chile)

sl <- catch_upside_tonnes_long_compare %>%
  filter (country == "Sierra Leone", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

write.excel(sl)

peru <- catch_upside_tonnes_long_compare %>%
  filter (country == "Peru", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

write.excel(peru)


peru_no_anchov <- catch_upside_tonnes_long_compare %>%
  filter (country == "Peru", rcp == "RCP60", species != "Engraulis ringens") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

write.excel(peru_no_anchov)

peru_anchov <- catch_upside_tonnes_long_compare %>%
  filter (country == "Peru", rcp == "RCP60", species == "Engraulis ringens") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

write.excel(peru_anchov)
