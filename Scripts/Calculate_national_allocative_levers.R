# National allocative levers--Calculate RNIs 
# 9/23/23

# moving this from plot_sankey_allocative_levers.R to streamline

library (tidyverse)

# function for writing values to excel (For paper reporting)
# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# function for converting catch in mt to child RNI equivalents ----
source ("Scripts/Function_convert_catch_amt_children_fed.R")

# function for converting catch in mt to tonnes of nutrient, for pop needs
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
    select (nutrient, nutr_tonnes)
  
  return (catch_nutrients)
}

# landings/production data ----
#  SAU data
sau_2019 <- readRDS ("Data/SAU_2019.Rds")

# country landings data
#Clean_Chile_Sernapesca_landings.R
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# Sierra Leone IHH data
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

# exports ARTIS ----
# updated with 8/30 data using harmonized SAU names. This has 2011-2019.
# just use 2019?
# not taking five year mean, this is just 2019 data. so just need to fix countries and species names
# export percent production is in percentage points, convert
exports <- read_csv ("Data/20230830_edf_ARTIS_snet.csv") %>%
  mutate (species = str_to_sentence(sciname),
          prop_exp = exports_percent_of_prod
          /100,
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone"
          ), .keep = "unused") %>%
  # # remove aquaculture and inland
  # filter (habitat == "marine", method == "capture")
  filter (year == 2019)

# country nutrient demand data ----
# from calculate_population_percent_nutrition...R
wpp_country_aggregate <- readRDS("Data/annual_nutr_demand_rni_by_country.Rds")

######################################################################################
# build country-level dataframes----
# for each, I think I need two different dataframes--too complicated to deal with exports and artisanal/industrial
# one is "sector", foreign, artisanal, industrial. assume domestic is sum of artisanal and industrial.
# other is all domestic, split into exported and retained

# Sierra Leone ----

# use IHH landings and join to foreign catch from SAU
sl_landings_2017 <- sl_landings %>%
  filter (year == 2017, !is.na(catch_mt)) %>%
  select (country, species, sector, catch_mt) 

#subset just the foreign catch for SL
sl_foreign <- sau_2019 %>%
  ungroup () %>%
  filter (country == "Sierra Leone", fleet == "Foreign catch") %>%
  # all foreign catch is industrial catch. but what I'm going to do is name the sector "Foreign catch" so it's in one column. 
  # also have to group to combine end_use_types
  group_by (species) %>%
  #rename to match sl_landings--make sector column that says foreign
  summarise (country = "Sierra Leone",
             sector = "Foreign catch",
             catch_mt = sum (tonnes))

# join exports and foreign
sl_foreign_sector_vol <- sl_landings_2017 %>%
  rbind (sl_foreign) 

# calculate RNI equivalents and percent population needs met
sl_foreign_sector_nutr <- sl_foreign_sector_vol %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Sierra Leone"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, sector, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to Sierra leone population demand
  left_join (filter (wpp_country_aggregate, Time == 2017), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))

saveRDS(sl_foreign_sector_nutr, "Data/levers_RNI_pop_foreign_sector_SierraLeone.Rds")
             

# sierra leone exports
sl_export <- sl_landings_2017 %>%
  # aggregate small and large scale
  group_by (country, species) %>%
  summarise (catch_mt = sum(catch_mt, na.rm = TRUE)) %>%
  # join to ARTIS data
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (country, species, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "exports",
                values_to = "catch_mt") 

# calculate RNI equivalents and percent population needs met
sl_export_nutr <- sl_export %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Sierra Leone"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, exports, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to Sierra leone population demand
  left_join (filter (wpp_country_aggregate, Time == 2017), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))  

saveRDS(sl_export_nutr, "Data/levers_RNI_pop_export_SierraLeone.Rds")

# report values ----

# total current provisioning (domestic, IHH)
# I actually want this to be the "total nutrient bank" incl foreign
sl_foreign_sector_nutr %>%
  #filter (sector != "Foreign catch") %>%
  group_by (nutrient) %>%
  summarise (across( where(is.numeric), sum)) %>%
  write.excel()

# foreign vs. domestic catch 
sl_foreign_sector_nutr %>%
  mutate (foreign = ifelse (sector == "Foreign catch", "Foreign", "Domestic")) %>%
  group_by(nutrient, foreign) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = foreign,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{foreign}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Foreign = Foreign_rni_equivalents/(Domestic_rni_equivalents + Foreign_rni_equivalents)* 100) %>%
  write.excel()

# overall % foreign by volume
sl_foreign_sector_vol %>%
  summarise (perc_foreign = sum(catch_mt[sector == "Foreign catch"], na.rm = TRUE)/sum (catch_mt, na.rm = TRUE) * 100)


# artisanal vs industrial 
sl_foreign_sector_nutr %>%
  filter (sector != "Foreign catch") %>%
  group_by(nutrient, sector) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = sector,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{sector}_{.value}") %>%
  ungroup() %>%
  mutate (SSF_perc_rnis = `Small-scale_rni_equivalents`/(`Large-scale_rni_equivalents` + `Small-scale_rni_equivalents`)* 100,
          LSF_perc_rnis = `Large-scale_rni_equivalents`/(`Large-scale_rni_equivalents` + `Small-scale_rni_equivalents`)* 100) %>%
  write.excel()

# percent catch by volume
sl_foreign_sector_vol %>%
  filter (sector != "Foreign catch") %>%
  group_by (sector) %>%
  summarise (catch_volume = sum (catch_mt, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

# exports 
sl_export_nutr %>%
  pivot_wider (names_from = exports,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{exports}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Exported = Exported_rni_equivalents/(Retained_rni_equivalents + Exported_rni_equivalents)* 100) %>%
  select (-country) %>%
  write.excel()

# percent catch by volume
sl_export %>%
  group_by (exports) %>%
  summarise (catch_volume = sum (catch_mt, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

#####################################################################################3
# Chile ----

# 2021 landings from sernapesca
chl_noalgae <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  mutate (country = "Chile") 

chl_algae <- chl_landings %>%
  filter (year == 2021) %>%
  mutate (country = "Chile")

# foreign catch from SAU
chl_foreign <- sau_2019 %>%
  ungroup () %>%
  filter (country == "Chile", fleet == "Foreign catch") %>%
  # all foreign catch is industrial catch. but what I'm going to do is name the sector "Foreign catch" so it's in one column. 
  # also have to group to combine end_use_types
  group_by (country, species) %>%
  #rename to match chl_landings
  summarise (sector = "Foreign catch",
             catch_mt = sum (tonnes))

# bind chl no algae and foreign
chl_foreign_sector_vol <- chl_noalgae %>%
  select (country, species, sector, catch_mt) %>%
  rbind (chl_foreign)

# calculate RNI equivalents and percent population needs met
chl_foreign_sector_nutr <- chl_foreign_sector_vol %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Chile"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, sector, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to population demand
  left_join (filter (wpp_country_aggregate, Time == 2021), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))

saveRDS(chl_foreign_sector_nutr, "Data/levers_RNI_pop_foreign_sector_Chile.Rds")

# chile exports
chl_export <- chl_noalgae %>%
  # aggregate small and large scale
  group_by (country, species) %>%
  summarise (catch_mt = sum(catch_mt, na.rm = TRUE)) %>%
  # join to ARTIS data
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (country, species, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "exports",
                values_to = "catch_mt") 

# calculate RNI equivalents and percent population needs met
chl_export_nutr <- chl_export %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Chile"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, exports, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to Sierra leone population demand
  left_join (filter (wpp_country_aggregate, Time == 2021), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))  

saveRDS(chl_export_nutr, "Data/levers_RNI_pop_export_Chile.Rds")

# report values ----

# total current provisioning (sernapesca)
# without algae
chl_foreign_sector_nutr %>%
  #filter (sector != "Foreign catch") %>%
  group_by (nutrient) %>%
  summarise (across( where(is.numeric), sum)) %>%
  write.excel()

# with alage--i don't think we're plotting this ever, so just calculating here
chl_algae %>%
  group_by (country, species) %>%
  summarise (catch_mt = sum (catch_mt, na.rm = TRUE)) %>%
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Chile"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to population demand
  left_join (filter (wpp_country_aggregate, Time == 2021), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand)) %>%
  write.excel()

# foreign vs. domestic catch 
chl_foreign_sector_nutr %>%
  mutate (foreign = ifelse (sector == "Foreign catch", "Foreign", "Domestic")) %>%
  group_by(nutrient, foreign) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = foreign,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{foreign}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Foreign = Foreign_rni_equivalents/(Domestic_rni_equivalents + Foreign_rni_equivalents)* 100) %>%
  write.excel()

# overall % foreign by volume
chl_foreign_sector_vol %>%
  summarise (perc_foreign = sum(catch_mt[sector == "Foreign catch"], na.rm = TRUE)/sum (catch_mt, na.rm = TRUE) * 100) %>% write.excel()


# artisanal vs industrial 
chl_foreign_sector_nutr %>%
  filter (sector != "Foreign catch") %>%
  group_by(nutrient, sector) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = sector,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{sector}_{.value}") %>%
  ungroup() %>%
  mutate (SSF_perc_rnis = Artisanal_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100,
          LSF_perc_rnis = Industrial_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100) %>%
  write.excel()

# percent catch by volume
chl_foreign_sector_vol %>%
  filter (sector != "Foreign catch") %>%
  group_by (sector) %>%
  summarise (catch_volume = sum (catch_mt, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

# exports 
chl_export_nutr %>%
  pivot_wider (names_from = exports,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{exports}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Exported = Exported_rni_equivalents/(Retained_rni_equivalents + Exported_rni_equivalents)* 100) %>%
  select (-country) %>%
  write.excel()

# percent catch by volume
chl_export %>%
  group_by (exports) %>%
  summarise (catch_volume = sum (catch_mt, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

#####################################################################################3
# Indonesia ----

indo_foreign_sector_vol <- sau_2019 %>%
  filter (country == "Indonesia") %>%
  # make sector category with foreign, artisanal, industrial 
  mutate (sector = ifelse (fleet == "Foreign catch", "Foreign catch", fishing_sector)) %>%
  ungroup() %>%
  select (country, species, sector, tonnes)

# calculate RNI equivalents and percent population needs met
indo_foreign_sector_nutr <- indo_foreign_sector_vol %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Indonesia"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = tonnes, country_name = "Indonesia"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, tonnes)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, sector, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to population demand
  left_join (filter (wpp_country_aggregate, Time == 2019), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))

saveRDS(indo_foreign_sector_nutr, "Data/levers_RNI_pop_foreign_sector_Indo.Rds")

# indo exports
indo_export <- sau_2019 %>%
  filter (country == "Indonesia") %>%
  # aggregate small and large scale
  group_by (country, species) %>%
  summarise (catch_mt = sum(tonnes, na.rm = TRUE)) %>%
  # join to ARTIS data
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (country, species, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "exports",
                values_to = "catch_mt") 

# calculate RNI equivalents and percent population needs met
indo_export_nutr <- indo_export %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Indonesia"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Indonesia"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, exports, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to Sierra leone population demand
  left_join (filter (wpp_country_aggregate, Time == 2019), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))  

saveRDS(indo_export_nutr, "Data/levers_RNI_pop_export_Indo.Rds")

# report values ----

# total current provisioning 
indo_foreign_sector_nutr %>%
  #filter (sector != "Foreign catch") %>%
  group_by (nutrient) %>%
  summarise (across( where(is.numeric), sum)) %>%
  write.excel()


# foreign vs. domestic catch 
indo_foreign_sector_nutr %>%
  mutate (foreign = ifelse (sector == "Foreign catch", "Foreign", "Domestic")) %>%
  group_by(nutrient, foreign) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = foreign,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{foreign}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Foreign = Foreign_rni_equivalents/(Domestic_rni_equivalents + Foreign_rni_equivalents)* 100) %>%
  write.excel()

# overall % foreign by volume
indo_foreign_sector_vol %>%
  summarise (perc_foreign = sum(tonnes[sector == "Foreign catch"], na.rm = TRUE)/sum (tonnes, na.rm = TRUE) * 100) %>% write.excel()


# artisanal vs industrial 
indo_foreign_sector_nutr %>%
  filter (sector != "Foreign catch") %>%
  # group subsistence into artisanal
  mutate (sector = ifelse (sector %in% c("Subsistence", "Recreational"), "Artisanal", sector)) %>%
  group_by(nutrient, sector) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = sector,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{sector}_{.value}") %>%
  ungroup() %>%
  mutate (SSF_perc_rnis = Artisanal_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100,
          LSF_perc_rnis = Industrial_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100) %>%
  write.excel()

# percent catch by volume
indo_foreign_sector_vol %>%
  filter (sector != "Foreign catch") %>%
  # group subsistence into artisanal
  mutate (sector = ifelse (sector %in% c("Subsistence", "Recreational"), "Artisanal", sector)) %>%
  group_by (sector) %>%
  summarise (catch_volume = sum (tonnes, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

# exports 
indo_export_nutr %>%
  pivot_wider (names_from = exports,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{exports}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Exported = Exported_rni_equivalents/(Retained_rni_equivalents + Exported_rni_equivalents)* 100) %>%
  select (-country) %>%
  write.excel()

# percent catch by volume
indo_export %>%
  group_by (exports) %>%
  summarise (catch_volume = sum (catch_mt, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

#####################################################################################3
# Peru ----

# No anchovy ----
peru_foreign_sector_vol_noanchov <- sau_2019 %>%
  # filter to country
  filter (country == "Peru", species != "Engraulis ringens") %>%
  # make sector category with foreign, artisanal, industrial 
  mutate (sector = ifelse (fleet == "Foreign catch", "Foreign catch", fishing_sector)) %>%
  ungroup() %>%
  select (country, species, sector, tonnes)

# calculate RNI equivalents and percent population needs met
peru_foreign_sector_nutr_noanchov <- peru_foreign_sector_vol_noanchov %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = tonnes, country_name = "Peru"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, tonnes)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, sector, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to population demand
  left_join (filter (wpp_country_aggregate, Time == 2019), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))

saveRDS(peru_foreign_sector_nutr_noanchov, "Data/levers_RNI_pop_foreign_sector_Peru_noanchov.Rds")

# peru exports
peru_export_noanchov <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru", species != "Engraulis ringens") %>%
  # aggregate small and large scale
  group_by (country, species) %>%
  summarise (catch_mt = sum(tonnes, na.rm = TRUE)) %>%
  # join to ARTIS data
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (country, species, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "exports",
                values_to = "catch_mt") 

# calculate RNI equivalents and percent population needs met
peru_export_noanchov_nutr <- peru_export_noanchov %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Peru"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Peru"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, exports, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to Sierra leone population demand
  left_join (filter (wpp_country_aggregate, Time == 2019), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))  

saveRDS(peru_export_noanchov_nutr, "Data/levers_RNI_pop_export_Peru_noanchov.Rds")

# report values ----

# total current provisioning [ncluding foreign]
peru_foreign_sector_nutr_noanchov %>%
  #filter (sector != "Foreign catch") %>%
  group_by (nutrient) %>%
  summarise (across( where(is.numeric), sum)) %>%
  write.excel()


# foreign vs. domestic catch 
peru_foreign_sector_nutr_noanchov %>%
  mutate (foreign = ifelse (sector == "Foreign catch", "Foreign", "Domestic")) %>%
  group_by(nutrient, foreign) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = foreign,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{foreign}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Foreign = Foreign_rni_equivalents/(Domestic_rni_equivalents + Foreign_rni_equivalents)* 100) %>%
  write.excel()

# overall % foreign by volume
peru_foreign_sector_vol_noanchov %>%
  summarise (perc_foreign = sum(tonnes[sector == "Foreign catch"], na.rm = TRUE)/sum (tonnes, na.rm = TRUE) * 100) %>% write.excel()


# artisanal vs industrial 
peru_foreign_sector_nutr_noanchov %>%
  filter (sector != "Foreign catch") %>%
  # group subsistence into artisanal
  mutate (sector = ifelse (sector %in% c("Subsistence", "Recreational"), "Artisanal", sector)) %>%
  group_by(nutrient, sector) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = sector,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{sector}_{.value}") %>%
  ungroup() %>%
  mutate (SSF_perc_rnis = Artisanal_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100,
          LSF_perc_rnis = Industrial_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100) %>%
  write.excel()

# percent catch by volume
peru_foreign_sector_vol_noanchov %>%
  filter (sector != "Foreign catch") %>%
  # group subsistence into artisanal
  mutate (sector = ifelse (sector %in% c("Subsistence", "Recreational"), "Artisanal", sector)) %>%
  group_by (sector) %>%
  summarise (catch_volume = sum (tonnes, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

# exports 
peru_export_nutr_noanchov %>%
  pivot_wider (names_from = exports,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{exports}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Exported = Exported_rni_equivalents/(Retained_rni_equivalents + Exported_rni_equivalents)* 100) %>%
  select (-country) %>%
  write.excel()

# percent catch by volume
peru_export_noanchov %>%
  group_by (exports) %>%
  summarise (catch_volume = sum (catch_mt, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

#################################################################################
# Peru only anchovy ----
peru_foreign_sector_vol_anchov <- sau_2019 %>%
  # filter to country
  filter (country == "Peru", species == "Engraulis ringens") %>%
  # make sector category with foreign, artisanal, industrial 
  mutate (sector = ifelse (fleet == "Foreign catch", "Foreign catch", fishing_sector)) %>%
  ungroup() %>%
  select (country, species, sector, tonnes)

# calculate RNI equivalents and percent population needs met
peru_foreign_sector_nutr_anchov <- peru_foreign_sector_vol_anchov %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = tonnes, country_name = "Peru"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, tonnes)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, sector, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to population demand
  left_join (filter (wpp_country_aggregate, Time == 2019), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))

saveRDS(peru_foreign_sector_nutr_anchov, "Data/levers_RNI_pop_foreign_sector_Peru_anchov.Rds")

# peru exports
peru_export_anchov <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru", species == "Engraulis ringens") %>%
  # aggregate small and large scale
  group_by (country, species) %>%
  summarise (catch_mt = sum(tonnes, na.rm = TRUE)) %>%
  # join to ARTIS data
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (country, species, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "exports",
                values_to = "catch_mt") 

# calculate RNI equivalents and percent population needs met
peru_export_nutr_anchov <- peru_export_anchov %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Peru"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = catch_mt, country_name = "Peru"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, catch_mt)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, exports, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to population demand
  left_join (filter (wpp_country_aggregate, Time == 2019), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))  

saveRDS(peru_export_nutr_anchov, "Data/levers_RNI_pop_export_Peru_anchov.Rds")

# report values ----

# total current provisioning [ncluding foreign]
peru_foreign_sector_nutr_anchov %>%
  #filter (sector != "Foreign catch") %>%
  group_by (nutrient) %>%
  summarise (across( where(is.numeric), sum)) %>%
  write.excel()


# foreign vs. domestic catch 
peru_foreign_sector_nutr_anchov %>%
  mutate (foreign = ifelse (sector == "Foreign catch", "Foreign", "Domestic")) %>%
  group_by(nutrient, foreign) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = foreign,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{foreign}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Foreign = Foreign_rni_equivalents/(Domestic_rni_equivalents + Foreign_rni_equivalents)* 100) %>%
  write.excel()

# overall % foreign by volume
peru_foreign_sector_vol_anchov %>%
  summarise (perc_foreign = sum(tonnes[sector == "Foreign catch"], na.rm = TRUE)/sum (tonnes, na.rm = TRUE) * 100) %>% write.excel()


# artisanal vs industrial 
peru_foreign_sector_nutr_anchov %>%
  filter (sector != "Foreign catch") %>%
  group_by(nutrient, sector) %>%
  summarise (across( where(is.numeric), sum)) %>%
  pivot_wider (names_from = sector,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{sector}_{.value}") %>%
  ungroup() %>%
  mutate (SSF_perc_rnis = Artisanal_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100,
          LSF_perc_rnis = Industrial_rni_equivalents/(Industrial_rni_equivalents + Artisanal_rni_equivalents)* 100) %>%
  write.excel()

# percent catch by volume
peru_foreign_sector_vol_anchov %>%
  filter (sector != "Foreign catch") %>%
  group_by (sector) %>%
  summarise (catch_volume = sum (tonnes, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

# exports 
peru_export_nutr_anchov %>%
  pivot_wider (names_from = exports,
               values_from = c(rni_equivalents, perc_demand_met),
               names_glue = "{exports}_{.value}") %>%
  ungroup() %>%
  mutate (perc_rnis_Exported = Exported_rni_equivalents/(Retained_rni_equivalents + Exported_rni_equivalents)* 100) %>%
  select (-country) %>%
  write.excel()

# percent catch by volume
peru_export_anchov %>%
  group_by (exports) %>%
  summarise (catch_volume = sum (catch_mt, na.rm = TRUE)) %>%
  mutate (perc_volume = catch_volume / sum(catch_volume) * 100) %>%
  write.excel()

