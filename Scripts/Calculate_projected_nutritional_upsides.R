# Calculate aggregate nutrient upsides
# 3/21/23
# JGM

# Calculate the ratio of projected catch to a baseline period for each year, under each scenario. translate that catch into nutrient yield

library (tidyverse)
library (stringr) # for wrangling species, family, genus names
#remotes::install_github("ropensci/rfishbase")
library (rfishbase) # for interpolating missing species
library (beepr) # calc_nutr_upside_tonnes_annual takes many minutes

# major update 3/27/23 is that I'm going to calculate children fed from current landings, and then multiply ratios for nutrient yield. doing this to preserve matched species. but maybe doesn't matter?
# 11/21/23 overhauling code--this is calculating, not plotting. bring in calculation of catch upsides from calculate_nutritional_upsides.R, which was misnamed, only calculated catch upsides there


# projection data from free et al. 2020. smaller version to reduce processing time, just has 3 management scenarios
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

# 2/1/23 moving code from regional team priority spp
# 9/16/23 update: changing baseline year to 2017-2021. Weird dynamics in the first years of the model are creating strange results

# calculate catch upside in terms of relation to baseline catch, by period----
# to be able to use with our updated landings data, instead I want to calculate what the catch in 2050 is relative to baseline under the different scenarios. 

catch_upside_relative <-  ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0) %>%
  mutate (
    # baseline and mid century and end century
    period = case_when (
      year %in% c(2017:2021) ~ "2017-2021",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100")) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, species) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, rcp, species) %>%
  reframe (bau_ratio_midcentury = catch_mt[scenario == "No Adaptation" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2017-2021"],
           bau_ratio_endcentury = catch_mt[scenario == "No Adaptation" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2017-2021"],
           mey_ratio_midcentury = catch_mt[scenario == "Productivity Only" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2017-2021"],
           mey_ratio_endcentury = catch_mt[scenario == "Productivity Only" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2017-2021"],
           adapt_ratio_midcentury = catch_mt[scenario == "Full Adaptation" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2017-2021"],
           adapt_ratio_endcentury = catch_mt[scenario == "Full Adaptation" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2017-2021"]
  )

saveRDS (catch_upside_relative, file = "Data/nutricast_upside_relative.Rds")

# calculate annual catch upside for full time series----

# probably can do this with brackets but just make a baseline value separately
ds_spp_baseline <- ds_spp %>%
  filter (scenario %in% "No Adaptation", catch_mt > 0, between (year, 2017, 2021)) %>%
  group_by (country, rcp, scenario, species) %>%
  summarise (baseline_catch = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup()

# then need to make 2 more fake ones with the other scenarios??
ds_spp_baseline_mey <- ds_spp_baseline %>%
  mutate (scenario = "Productivity Only")

ds_spp_baseline_adapt <- ds_spp_baseline %>%
  mutate (scenario = "Full Adaptation")

ds_spp_baseline <- rbind (ds_spp_baseline, ds_spp_baseline_mey, ds_spp_baseline_adapt)

catch_upside_relative_annual <-  ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0, year > 2021) %>%
  left_join (ds_spp_baseline, by = c("country", "rcp", "scenario", "species")) %>%
  mutate (catch_ratio = catch_mt / baseline_catch) %>%
  select (country, rcp, scenario, year, species, baseline_catch, catch_mt, catch_ratio)


saveRDS (catch_upside_relative_annual, file = "Data/nutricast_upside_relative_annual_ratio.Rds")

# repair missing species ----
# moving from check_SAU_nutricast_species.R
# these return JUST the missing species, so have to load both

# match to family, get family info from fb?
# species table just has family code. match from "families" table
famcodes_fb <- fb_tbl("families") %>%
  select ("FamCode", "Family")

famcodes_slb <- fb_tbl("families", "sealifebase") %>%
  select ("FamCode", "Family") 
# these have overlap with fb


nutricast_fams_fb <- fb_tbl ("species") %>%
  mutate(species = paste(Genus, Species)) %>%
  filter (species %in% catch_upside_relative$species) %>%
  left_join (famcodes_fb, by = "FamCode") %>%
  select (species, Genus, Family)

nutricast_fams_slb <- fb_tbl ("species", "sealifebase") %>%
  mutate(species = paste(Genus, Species)) %>%
  filter (species %in% catch_upside_relative$species) %>%
  left_join (famcodes_slb, by = "FamCode") %>%
  select (species, Genus, Family)

nutricast_fams <- rbind (nutricast_fams_fb, nutricast_fams_slb)

catch_upside_relative_fam <- catch_upside_relative %>%
  left_join (nutricast_fams, by = "species") %>%
  # grab Genus even if not in fishbase
  mutate (Genus = 
            case_when (is.na (Genus) ~ word(species, 1),
                       TRUE ~ Genus)
  )

# relate to country landings data
# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
# just grab species names
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")
sau_2019_country_spp <- readRDS("Data/SAU_2019.Rds") %>%
  ungroup() %>%
  select (country, species) %>%
  distinct()

chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
chl_landings_spp <- chl_landings %>%
  filter (year == 2021) %>%
  mutate (country = "Chile") %>%
  select (country, species) %>%
  distinct()

# 6/2/23 updating with SierraLeone IHH data
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")
sl_ihh_spp <- sl_landings %>%
  mutate (country == "Sierra Leone") %>%
  select (country, species) %>%
  distinct()

# Function to take landed species name and match to nutricast spp by genus or family  ----

match_nutricast_taxa <- function (species_name, country_name) {
  # send SAU or lande species name into nutricast df
  
  catch_upside_country <- catch_upside_relative_fam %>%
    filter (country == country_name) 
  
  # if identified to species level, clip to Genus
  if (grepl(" ", species_name)) {species_name = word(species_name, 1)}
  
  if (grepl ("ae", str_sub(species_name, -2, -1))) {
    match <- filter (catch_upside_country, Family == species_name)
    
    match_mean <- match %>%
      group_by (rcp, Family) %>%
      # take mean of all the ratio columns
      summarise (across(bau_ratio_midcentury:adapt_ratio_endcentury, mean, na.rm = TRUE)) %>%
      #rename (species = Family) %>%
      # remove to avoid name duplication
      select (-Family)
    
    
  } else {
    match <- filter (catch_upside_country, Genus == species_name)
    
    match_mean <- match %>%
      group_by (rcp, Genus) %>%
      # take mean of all the ratio columns
      summarise (across(bau_ratio_midcentury:adapt_ratio_endcentury, \(x) mean (x, na.rm = TRUE))) %>%
      #rename (species = Genus)
      select (-Genus)
    
  }
  
  
  return (tibble(match_mean))
  
}

# Function to grab missing species that don't match nutricast ----
check_missing_nutricast_spp <- function (country_name) {
  
  if (country_name == "Chile") {
    
    landed_spp <- chl_landings_spp 
  } else if (country_name == "Sierra Leone") {
    landed_spp <- sl_ihh_spp
  } else {
    
    landed_spp <- sau_2019_country_spp %>% filter (country == country_name)
  }
  
  
  nutricast_country <- catch_upside_relative %>% filter (country == country_name)
  
  landed_missing <- landed_spp %>%
    filter (!species %in% nutricast_country$species) %>%
    distinct()
  
  return (tibble(landed_missing))
  
}

nutricast_missing_spp <- map_dfr(as.list (c("Chile", "Peru", "Indonesia", "Sierra Leone")), check_missing_nutricast_spp)

nutricast_repair <- nutricast_missing_spp %>%
  mutate (nutricast = pmap(list(species_name = species, country_name = country), match_nutricast_taxa)
  ) %>% 
  unnest (cols = c(nutricast), names_repair = "check_unique")

#saveRDS(nutricast_repair, file = "Data/catch_upside_relative_repair_missing.Rds")

# join
catch_upside_repaired <- catch_upside_relative %>%
  #match columns from missing species
  select (country, species, rcp, bau_ratio_midcentury, bau_ratio_endcentury, mey_ratio_midcentury, mey_ratio_endcentury, adapt_ratio_midcentury, adapt_ratio_endcentury) %>%
  rbind (nutricast_repair)

saveRDS(catch_upside_repaired, file = "Data/catch_upside_relative_repaired.Rds")



# repair missing for annual ts ----
catch_upside_relative_annual <- readRDS ("Data/nutricast_upside_relative_annual_ratio.Rds")


nutricast_annual_fam <- catch_upside_relative_annual %>%
  left_join (nutricast_fams, by = "species") %>%
  # grab Genus even if not in fishbase
  mutate (Genus = 
            case_when (is.na (Genus) ~ word(species, 1),
                       TRUE ~ Genus)
  )

# for this one we only need to mutate one column, catch_ratio
match_nutricast_taxa_annual <- function (species_name, country_name) {
  # send SAU species name into nutricast df
  
  catch_upside_country <- nutricast_annual_fam %>%
    filter (country == country_name) 
  
  # if identified to species level, clip to Genus
  if (grepl(" ", species_name)) {species_name = word(species_name, 1)}
  
  # if identified at family level, choose family
  if (grepl ("ae", str_sub(species_name, -2, -1))) {
    match <- filter (catch_upside_country, Family == species_name)
    
    match_mean <- match %>%
      group_by (rcp, scenario, year, Family) %>%
      # take mean of all the ratio columns
      summarise (catch_ratio = mean (catch_ratio, na.rm = TRUE)) %>%
      #rename (species = Family) %>%
      # remove to avoid name duplication
      select (-Family)
    
    
  } else {
    match <- filter (catch_upside_country, Genus == species_name)
    
    match_mean <- match %>%
      group_by (rcp, scenario, year, Genus) %>%
      # take mean of all the ratio columns
      summarise (catch_ratio = mean (catch_ratio, na.rm = TRUE)) %>%
      #rename (species = Genus)
      select (-Genus)
    
  }
  
  
  return (tibble(match_mean))
  
}

nutricast_repair_annual <- nutricast_missing_spp %>%
  mutate (nutricast = pmap(list(species_name = species, country_name = country), match_nutricast_taxa_annual)
  ) %>% 
  unnest (cols = c(nutricast), names_repair = "check_unique")

#saveRDS(nutricast_repair_annual, file = "Data/nutricast_upside_relative_annual_repair_missing.Rds")

# join
catch_upside_annual_repaired <- catch_upside_relative_annual %>%
  #match columns from missing species
  select (country, species, rcp, scenario, year, catch_ratio) %>%
  rbind (nutricast_repair_annual)

saveRDS(catch_upside_annual_repaired, file = "Data/catch_upside_relative_annual_repaired.Rds")


########################################################################################################################################################################
# Relate ratios to landings data ----

# create a new baseline dataframe using country-specific landings

sau_2019 <- readRDS("Data/SAU_2019.Rds")

# just grab peru and indonesia
sau_baseline <- sau_2019 %>%
  filter (country %in% c("Peru", "Indonesia")) %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (tonnes))

# if not loaded above
# chl landings
# note: as of 6/6/23, have added commercial_group column but very preliminary, just lumped all the fish that weren't in SAU into "other"
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

chl_baseline <- chl_landings %>% 
  filter (year == 2021) %>%
  mutate (country = "Chile") %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (catch_mt)) 

# sl IHH
# year with data for both artisanal and industrial is 2017
sl_ihh_landings <- readRDS("Data/SLE_landings_IHH.Rds")

sl_baseline <- sl_ihh_landings %>% 
  filter (year == 2017) %>%
  mutate (country = "Sierra Leone") %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (catch_mt, na.rm = TRUE)) 

full_baseline <- rbind (sau_baseline, chl_baseline, sl_baseline)

saveRDS(full_baseline, file = "Data/baseline_catch_sau_chl_ihh.Rds")


# baseline catch from compiled data; from plot_contextual_landings.forecasts.R
full_baseline <- readRDS("Data/baseline_catch_sau_chl_ihh.Rds")

# catch upside values from above
catch_upside_annual <- readRDS ("Data/catch_upside_relative_annual_repaired.Rds")
catch_upside_relative <- readRDS("Data/catch_upside_relative_repaired.Rds")

# multiply ratio by baseline
# annual
catch_upside_ts <- catch_upside_annual %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (tonnes = catch_ratio * bl_tonnes)

# by period
catch_upside_tonnes <- catch_upside_relative %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (across(where (is.double), ~. * bl_tonnes))

########################################################################################################################################################################
# Convert to nutrients ----

# population nutrient needs data
#Calcualte_population_percent_nutrient_needs.R
wpp_country_aggregate <- readRDS("Data/annual_nutr_demand_rni_by_country.Rds")

# calculate mean demand by period
pop_demand_period <- wpp_country_aggregate %>%
  mutate (
    # baseline and mid century and end century, consistent with catch_upside_relative
    period = case_when (
      Time %in% c(2051:2060) ~ "midcentury",
      Time %in% c(2091:2100) ~ "endcentury")) %>%
  filter(!is.na(period)) %>%
  group_by (country, period, nutrient) %>%
  summarize (tot_pop = mean (tot_pop),
             mean_nutr_demand = mean (tot_nutr_annual_demand, na.rm = TRUE))

# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")

# function for converting catch in mt to tonnes of nutrient, for pop needs ----
# from calculate_population_percent_nutrition.R
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
        taxa == "Cephalopod" & species == "Dosidicus gigas" ~ 0.67,
        taxa == "Cephalopod" & species != "Dosidicus gigas" ~ 0.21,
        taxa == "Other" ~ 1,
        taxa == "Algae" ~ 1),
      # have to put back into units
      scalar = case_when (
        nutrient %in% c("Protein", "Omega_3") ~ 1,
        nutrient %in% c("Calcium", "Zinc", "Iron") ~ 1/1000,
        nutrient %in% c("Vitamin_A", "Selenium") ~ 1/1e6
      ),
      # input (catch_mt) is in metric tons. amount is in units / 100g. so divide by 100 to account for serving size, and multiply by scalar to cancel out g
      nutr_tonnes = catch_mt * p_edible * amount * scalar / 100 ) %>%
    select (nutr_tonnes)
  
  return (catch_nutrients)
}

# for periods, calculate rni equiv and nutr tonnes ----
# remove "ratio" from columns to make it easier to pivot_longer
colnames (catch_upside_tonnes) <- gsub ("_ratio", "", colnames (catch_upside_tonnes))

# make peru anchovy and peru not anchovy a different country

nutr_upside_rni_nutr_tonnes <- catch_upside_tonnes %>%
  select (-bl_tonnes) %>%
  pivot_longer(-c(country, rcp, species),
               names_to = c("scenario", "period"),
               names_sep = "_",
               values_to = "tonnes") %>%
  mutate(
    # make life easier by making peru anchovy and not anchovy different countries so calculates separately
    country = case_when (country == "Peru" & species == "Engraulis ringens" ~ "Peru_anchoveta",
                         TRUE ~ country),
    rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func), 
         nutr_tonnes = pmap (list (species_name = species, catch_mt = tonnes, country_name = country), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. 
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, rcp, scenario, period, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))); beep()

saveRDS(nutr_upside_rni_nutr_tonnes, file = "Data/nutrient_upside_rni_nutr_tonnes_period.Rds")

# differences for rcp 6.0
nutr_upside_diff <- nutr_upside_rni_nutr_tonnes %>% 
  filter (rcp == "RCP60", scenario != "mey") %>%
  group_by (country, period, nutrient) %>%
  summarize (rni_diff = rni_equivalents[scenario == "adapt"] - rni_equivalents[scenario == "bau"],
             nutr_tonnes_diff = nutr_tonnes[scenario == "adapt"] - nutr_tonnes[scenario == "bau"]) %>%
  # join to population need
  # ignore anchoveta for now...can just calculate from the excel sheet?
  left_join (pop_demand_period, by = c("country", "period", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes_diff / mean_nutr_demand * 100)

# also want to calculate baseline rnis/nutr tonnes, clipped to the species with nutricast
# can I just use the first baseline year??
# easier to use annual
catch_upside_annual_spp <- catch_upside_annual %>%
  select (country, species) %>%
  distinct()

baseline_nutricast_clip <- full_baseline %>%
  inner_join (catch_upside_annual_spp, by = c ("country", "species"))

baseline_rni_nutr_tonnes <- baseline_nutricast_clip %>%
  mutate(rni_equivalents = pmap (list (species = species, amount = bl_tonnes, country_name = country), calc_children_fed_func), 
         nutr_tonnes = pmap (list (species_name = species, catch_mt = bl_tonnes, country_name = country), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. 
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  # make peru anchoveta its own country
  mutate(country = case_when (country == "Peru" & species == "Engraulis ringens" ~ "Peru_anchoveta",
                         TRUE ~ country)) %>%
  group_by (country, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE)))
beep()

# combine these and write to table. want to see the differences, the baseline, and calculate the percent of differences

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

nutr_upside_diff_perc_baseline <- baseline_rni_nutr_tonnes %>%
  rename (baseline_rni = rni_equivalents, baseline_nutr_tonnes = nutr_tonnes) %>%
  left_join (nutr_upside_diff, by = c ("country", "nutrient")) %>%
  mutate (rni_prop_of_baseline = rni_diff / baseline_rni * 100)

nutr_upside_diff_perc_baseline %>% 
  arrange (country, period) %>%
  write.excel()


#########################################################################################################
# Calculate annual timeseries of nutrient yield ----

# function to convert annual to rni equiv ----
calc_nutr_upside_tonnes_annual <- function (country_name) {
  
  nutr_upside_annual <- catch_upside_ts %>%
    filter (country == country_name) %>%
    filter (!is.na (rcp)) %>%
    # convert to nutrients
    mutate (rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func)) %>%
    unnest(cols = c(rni_equivalents),  names_repair = "check_unique")
 
  # fix levels
  nutr_upside_annual$scenario <- factor(nutr_upside_annual$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  return (nutr_upside_annual)
  
}

# this takes several minutes
indo <-   calc_nutr_upside_tonnes_annual ("Indonesia"); beep()
saveRDS(indo, file = "Data/annual_nutr_upside_childRNI_Indonesia.Rds")

peru <-   calc_nutr_upside_tonnes_annual ("Peru"); beep()
saveRDS(peru, file = "Data/annual_nutr_upside_childRNI_Peru.Rds")

# split this into two here, think this will streamline things
peru <- readRDS("Data/annual_nutr_upside_childRNI_Peru.Rds")
peru_anchoveta <- peru %>%
  filter (species == "Engraulis ringens")
saveRDS(peru_anchoveta, file = "Data/annual_nutr_upside_childRNI_Peru_anchoveta.Rds")

peru_non_anchoveta <- peru %>%
  filter (species != "Engraulis ringens")
# NOTE--overriding previous rds object, now does NOT INCLUDE anchoveta
saveRDS(peru_non_anchoveta, file = "Data/annual_nutr_upside_childRNI_Peru.Rds")

sl <-   calc_nutr_upside_tonnes_annual ("Sierra Leone"); beep()
saveRDS(sl, file = "Data/annual_nutr_upside_childRNI_Sierra Leone.Rds")

chl <-   calc_nutr_upside_tonnes_annual ("Chile"); beep()
saveRDS(chl, file = "Data/annual_nutr_upside_childRNI_Chile.Rds")



################################################################################################################

# not sure we need this....
report_RNI_met_values <- function (country_name, anchovy = TRUE) {
  
  rni_nutr_ts <- readRDS(paste0("Data/annual_nutr_upside_childRNI_", country_name, ".Rds"))
  
  # aggregate by nutrient, rcp, scenario
  # for peru, default without anchovy
  if (country_name == "Peru") {
    
    if (anchovy == TRUE) {
      rni_nutr_agg_ts <-  rni_nutr_ts %>%
        filter (species == "Engraulis ringens") %>%
        group_by (country, year, nutrient, rcp, scenario) %>%
        summarise (tot_rni = sum (rni_equivalents, na.rm = TRUE))
    } else if (anchovy == FALSE) {
      rni_nutr_agg_ts <-  tonnes_nutr_ts %>%
        filter (species != "Engraulis ringens") %>%
        group_by (country, year, nutrient, rcp, scenario) %>%
        summarise (tot_rni = sum (rni_equivalents, na.rm = TRUE))
    }  # end country ifelse
  } else {  
    # if not peru, don't worry about anchovy
    rni_nutr_agg_ts <-  rni_nutr_ts %>%
      group_by (country, year, nutrient, rcp, scenario) %>%
      summarise (tot_rni = sum (rni_equivalents, na.rm = TRUE))
  }
  
  # mid and end century averages
  rni_period <- rni_nutr_agg_ts %>%
    mutate (
      period = case_when (year %in% 2051:2060 ~ "midcentury",
                        year %in% 2091:2100 ~ "endcentury")
    ) %>%
    # cut to periods of interest
  filter (!is.na (period), rcp == "RCP60") %>%
    # take mean per period
    group_by (country, rcp, nutrient, scenario, period) %>%
    summarise (mean_rni_equiv = mean (tot_rni, na.rm = TRUE))

}  

indo_annual_vals <- report_RNI_met_values("Indonesia") 
  
