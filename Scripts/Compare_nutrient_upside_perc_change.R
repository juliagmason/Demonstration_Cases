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


# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landing
# includes interpolated missing species, from calculate_projected_nutritional_upsides
catch_upside_relative <- readRDS("Data/catch_upside_relative_repaired.Rds")


# multiply ratio by baseline
catch_upside_tonnes <- catch_upside_relative %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (across(where (is.double), ~. * bl_tonnes))

# remove "ratio" from columns to make it easier to pivot_longer
colnames (catch_upside_tonnes) <- gsub ("_ratio", "", colnames (catch_upside_tonnes))

catch_upside_tonnes_long <- catch_upside_tonnes %>%
  select (-bl_tonnes) %>%
  pivot_longer(-c(country, rcp, species),
               names_to = c("scenario", "period"),
               names_sep = "_",
               values_to = "tonnes") %>%
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) 

# summarise by nutrient 
catch_upside_rni_summarize <- catch_upside_tonnes_long %>%
  group_by (country, rcp, scenario, period, nutrient) %>%
  summarise (tot_future_rni = sum (rni_equivalents, na.rm = TRUE))

# make separate one for bl_tonnes???!? doing this in a dumb way
# but need to clip to the species in the projection data
bl_nutr <- full_baseline %>%
  mutate(rni_equivalents= pmap (list (species = species, amount = bl_tonnes, country_name = country), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  rename (baseline_rni_equivalents = rni_equivalents)

bl_nutr_summarize <- bl_nutr %>%
  group_by (country, nutrient) %>%
  summarise (tot_baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE))

catch_upside_rni_compare <- catch_upside_rni_summarize %>%
  left_join (bl_nutr_summarize, by = c("country", "nutrient"))

catch_upside_tonnes_long_compare <- catch_upside_tonnes_long %>%
  left_join (bl_nutr, by = c ("country", "species", "nutrient"))




bl_nutr_summarize %>% filter (country == "Indonesia") # this is probably different, not clipped to species with projection values
catch_upside_rni_compare %>% filter (country == "Indonesia", rcp == "RCP60")


# Report results ----

# for Abby, make table of percent change for rcp 6.0
# Projected nutrient yield percent change sheet, https://docs.google.com/spreadsheets/d/1y7xqg8TaHLniVxs10k0b0ovfGtrtR42-RpjJQQ91pb0/edit#gid=0

#not summarizing so I can filter out anchovy

indo_perc_change <- catch_upside_tonnes_long_compare %>%
  filter (country == "Indonesia", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

write.excel(indo_perc_change)

indo26 <- catch_upside_tonnes_long_compare %>%
  filter (country == "Indonesia", rcp == "RCP26") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100)

catch_upside_tonnes_long_compare %>%
  filter (country == "Chile", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100) %>% write.excel()

catch_upside_tonnes_long_compare %>%
  filter (country == "Sierra Leone", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100) %>% write.excel()


catch_upside_tonnes_long_compare %>%
  filter (country == "Peru", rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (baseline_rni = sum (baseline_rni_equivalents, na.rm = TRUE), 
             future_rni = sum (rni_equivalents, na.rm = TRUE)) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni * 100) %>% write.excel()

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
