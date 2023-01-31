# Regional team priority species

# 5/27/22, updated 7/26/22, jan 23
library (tidyverse)
#library (stringr)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")

# Priority species ----

# top 5-7 priority species identified by regional teams
# as of 8/4/22  have peru and chile, mexico (limited data avail). took indo spp from willow spreadsheet, but don't know where they came from
priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv") %>%
  # just change S. japonicus peruanus to S. japonicus; no nutrient or SAU or nutricast data
  mutate (species = case_when (species == "Scomber japonicus peruanus" ~ "Scomber japonicus",
                               TRUE ~ species)
          )


# join priority spp to nutrient data ----

# macgyver pota data for chile/peru
d_gigas_rbind <- data.frame (
  species = d_gigas_nutr$species,
  country = "Peru",
  comm_name = "Pota",
  rank = 1,
  taxa = "Cephalopod",
  nutrient = d_gigas_nutr$nutrient,
  amount = d_gigas_nutr$amount)


# also grab crab nutrients from afcd for indo
# picking somewhat randomly. note that there's a separate dha + epa for omegas, and different vitamin As
library (AFCD)

scylla_nutr <- afcd_sci %>% 
  filter (sciname == "Scylla serrata",
          nutrient_code_fao %in% c(
            "CA", "ZN", "FE", "SE", "Protein", "FAPU", "VITA")) %>%
  group_by(nutrient_code_fao) %>%
  summarise (amount = mean (value, na.rm = TRUE)) %>%
  mutate (nutrient =
            case_when (nutrient_code_fao == "CA" ~ "Calcium",
                       nutrient_code_fao == "FE" ~ "Iron",
                       nutrient_code_fao == "SE" ~ "Selenium", 
                       nutrient_code_fao == "ZN" ~ "Zinc",
                       nutrient_code_fao == "FAPU" ~ "Omega_3",
                       nutrient_code_fao == "VITA" ~ "Vitamin_A"),
          # make columns to match other df
          country = "Indonesia",
          comm_name = "Kepiting bakau",
          rank = 6,
          taxa = "Crustacean",
          species = "Scylla serrata"
          ) %>%
  # reorder
  select (species, country, comm_name, rank, taxa, nutrient, amount)
          
  


pri_spp_nutr <- fishnutr_mu %>%
  right_join (priority_spp, by = "species")  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>% 
  # join d_gigas
  filter (!species %in% c("Dosidicus gigas", "Scylla serrata")) %>%
  rbind (d_gigas_rbind) %>%
  rbind (scylla_nutr) %>%

  
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rni = amount/RNI * 100,
          perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup()


# Landings ----

# Chile country specific 
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# SAU data ----

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds")

# or mean of most recent 5 years *doesn't currently include indonesia* 
sau_2015_2019 <- readRDS("Data/SAU_2015_2019.Rds")


# climate projection data ----
# smaller, just rcp 60 and 85. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")


###########################################################################
## Print values----

# nutrition content ----
# remove protein and copy to excel
# this is species nutrition content in excel sheet; species_nutrition_info in google sheet

fishnutr_mu %>%
  right_join (priority_spp, by = "species")  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  arrange (country, rank, nutrient) %>%
  write.excel ()



# micronutrient density----

print_micronutrient_density <- function (country_name, n_spp) {
  pri_spp_nutr %>%
    filter (country == country_name, rank <= n_spp, 
            !nutrient %in% c("Protein", "Selenium")) %>%
    group_by (species) %>%
    summarise (micronutrient_density = sum (perc_rni)) 
}

print_micronutrient_density ("Chile", 10)
print_micronutrient_density ("Indonesia", 10)
print_micronutrient_density("Sierra Leone", 4)


# sammi spp
#child_rda <- rda_groups %>% filter (group == "Child")

sammi_spp_nutr <- fishnutr_mu %>%
  filter (species %in% sammi_spp) %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4), 
          units = case_when (
            nutrient %in% c("Calcium", "Iron", "Zinc") ~ "mg",
            nutrient %in% c("Selenium", "Vitamin_A") ~"ug",
            nutrient %in% c("Protein", "Omega_3") ~ "g"
          )) %>%
  left_join(rda_groups, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup() %>%
  select (species, nutrient, amount, units, group, perc_rda) %>%
  rename (unit = units)

write.csv (sammi_spp_nutr, file = "Data/nutrient_content_Mexico_spp_for_Sammi.csv", row.names = FALSE)

# landings ----

#print mean catch, last five years ----

# Chile
chl_landings %>%
  mutate (country = "Chile") %>%
  right_join (priority_spp, by = c ("country", "species")) %>%
  filter (between (year, 2017, 2021)) %>%
  
  group_by (species, rank, year) %>%
  # have to sum artisanal and industrial first
  summarise (catch = sum (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (species, rank) %>%
  summarise (catch = mean (catch, na.rm = TRUE)) %>%
  arrange (rank) %>%
  write.excel()


chl_landings %>%
  mutate (country = "Chile") %>%
  right_join (priority_spp, by = c ("country", "species")) %>%
  filter (year == 2021)%>%
  
  group_by (species, rank) %>%
  # have to sum artisanal and industrial first
  summarise (catch = sum (catch_mt, na.rm = TRUE)) %>%
  arrange (rank) %>%
  write.excel()



# SAU catch
# Deng Palomares suggested just using 2019 data; improved. Updated with just 2019 as of oct 2022
sau_country_catch_2019 <- 
  sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  group_by (country, species, rank) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  arrange (country, rank) %>%
  write.excel()

# tiny catches for SL...
sau_country_cleaned %>%
  filter (country == "Sierra Leone", species %in% priority_spp$species, between (year, 2000, 2015)) %>%
  ggplot (aes (x = year, y = tonnes, fill = species)) +
  geom_bar(stat = "identity")

# sau foreign vs domestic----
# print proportion foreign by country 
# use mean? or just 2019? look at both to compare
# skipping indonesia for now because don't have priority species

sau_2015_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
  group_by (country, species, rank, year, fishing_country) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  group_by (country, species, rank) %>%
  summarise (prop_for = sum (sum_tonnes[fishing_country == "Foreign catch"]) / sum (sum_tonnes),
             mean_for_catch = mean (sum_tonnes[fishing_country == "Foreign catch"], na.rm = TRUE)) %>%
  
  arrange (country, rank) %>%
  filter (!country == "Indonesia") %>%
  write.excel()

sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
  group_by (country, species, rank) %>%
  summarise (prop_for = sum (tonnes[fishing_country == "Foreign catch"]) / sum (tonnes),
             mean_for_catch = mean (tonnes[fishing_country == "Foreign catch"], na.rm = TRUE)) %>%
  
  arrange (country, rank) %>%
  filter (!country == "Indonesia") %>%
  write.excel()

# sau industrial vs artisanal----
# domestic only
# try 2015-2019 mean and 2019 only. Values are similar. However, for chile anchovy values are very different from 2011-2015, way more artisanal catch. this does match up with official landings though. 
sau_2015_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (fishing_entity == country, !country == "Indonesia") %>%
  group_by (country, species, rank, year, fishing_sector) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  group_by (country, species, rank) %>%
  summarise (prop_ssf = sum (sum_tonnes[fishing_sector == "Artisanal"]) / sum (sum_tonnes) ,
             prop_ind = sum (sum_tonnes[fishing_sector == "Industrial"]) / sum (sum_tonnes)) %>%
  arrange (country, rank) %>% 
  write.excel()

sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (fishing_entity == country, !country == "Indonesia") %>%
  group_by (country, species, rank) %>%
  summarise (prop_ssf = sum (tonnes[fishing_sector == "Artisanal"]) / sum (tonnes) ,
             prop_ind = sum (tonnes[fishing_sector == "Industrial"]) / sum (tonnes)) %>%
  arrange (country, rank) %>% 
  write.excel()


# chile, national landings data artisanal vs industrial ----
chl_landings %>%
  mutate (country = "Chile") %>%
  right_join (priority_spp, by = c ("country", "species")) %>%
  filter (between (year, 2017, 2021)) %>%
  
  group_by (species, rank) %>%
  summarise (prop_ssf = sum (catch_mt[sector == "Artisanal"]) / sum (catch_mt),
             prop_ind = sum (catch_mt[sector == "Industrial"]) / sum (catch_mt)) %>%
  arrange ( rank) %>%
  write.excel()

# end use SAU ----
#discards only show up in indonesia data
sau_2015_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (fishing_entity == country, !country == "Indonesia") %>%
  group_by (country, species, rank, year, end_use_type) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
  group_by (country, species, rank) %>%
  summarise (prop_dhc = sum (tonnes[end_use_type == "Direct human consumption"]) / sum (tonnes) ,
             prop_fmfo = sum (tonnes[end_use_type == "Fishmeal and fish oil"]) / sum (tonnes),
             prop_other = sum (tonnes [end_use_type == "Other"]) / sum (tonnes),
             prop_non_dhc = 1-prop_dhc) %>%
  arrange (country, rank) %>% 
  write.excel()

sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (fishing_entity == country, !country == "Indonesia") %>%
  group_by (country, species, rank) %>%
  summarise (prop_dhc = sum (tonnes[end_use_type == "Direct human consumption"]) / sum (tonnes) ,
             prop_fmfo = sum (tonnes[end_use_type == "Fishmeal and fish oil"]) / sum (tonnes),
             prop_other = sum (tonnes [end_use_type == "Other"]) / sum (tonnes),
             prop_non_dhc = 1-prop_dhc) %>%
  arrange (country, rank) %>% 
  write.excel()

# big jump in Peru anchovy dhc in 2019
sau_2015_2019 %>%
  filter (country == "Peru", species == "Engraulis ringens") %>%
  ggplot (aes (x = year, y = tonnes, fill = end_use_type)) +
  geom_col () +
  theme_bw() +
  ggtitle ("Peru anchovy landings, SAU")

sau_2015_2019 %>%
  filter (country == "Peru", species == "Engraulis ringens", year < 2019) %>%
  group_by (year) %>%
  summarise (prop_dhc = sum (tonnes[end_use_type == "Direct human consumption"]) / sum (tonnes) ,
             prop_fmfo = sum (tonnes[end_use_type == "Fishmeal and fish oil"]) / sum (tonnes),
             prop_other = sum (tonnes [end_use_type == "Other"]) / sum (tonnes),
             prop_non_dhc = 1-prop_dhc)

# use 2018 value?
sau_2015_2019 %>%
  filter (country == "Peru", species == "Engraulis ringens", year == 2018) %>%
  group_by (year) %>%
  summarise (prop_dhc = sum (tonnes[end_use_type == "Direct human consumption"]) / sum (tonnes) ,
             prop_fmfo = sum (tonnes[end_use_type == "Fishmeal and fish oil"]) / sum (tonnes),
             prop_other = sum (tonnes [end_use_type == "Other"]) / sum (tonnes),
             prop_non_dhc = 1-prop_dhc) %>%
  pull (prop_non_dhc) %>%
  write.excel()


# exports ARTIS ----
# emailed 10 19 2022
exports <- read_csv ("Data/20221019_edf_ARTIS_snet.csv")

# additional Indo species emailed 1/28/23
exports_indo <- read_csv("Data/20230125_edf_ARTIS_indo.csv")

exports <- rbind (exports, exports_indo)

# stringr for all lowercase species names
library (stringr)

# also have to rep
# most recent year is 2019
exports_5yr_mean <- exports %>%
  filter (between (year, 2015, 2019)) %>%
  group_by (exporter_iso3c, sciname) %>%
  summarise (mn_prop_exp = mean (exports_percent_of_prod, na.rm = TRUE)/100) %>%
  ungroup() %>%
  mutate (species = str_to_sentence(sciname),
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone"
          ), .keep = "unused") 


# upside BAU to MEY ----

# nutricast upside ---- [move to calculate_nutritional_upsides script??]

# for spreadsheet just calculate the tons
pri_spp_catch_upside <- ds_spp %>% 
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (
    period = case_when (
      year %in% c(2026:2035) ~ "2026-2035",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  filter (!is.na (catch_mt), !is.na (period), scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation")) %>%
  #take mean projected catch for the decade period
  group_by (country, rcp, period, species, scenario) %>%
  summarise (catch_mt = mean (catch_mt)) %>%
  ungroup() %>%
  # find difference among scenarios--absolute and percent diff
  group_by (country, rcp, period, species) %>%
  summarize (mey_diff_mt = catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"],
             mey_diff_percent = (catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100,
             adapt_diff_mt = catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"],
             adapt_diff_percent = (catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100) %>%
  ungroup()

# rejoin to rank
pri_spp_catch_upside %>%
  filter (period == "2051-2060", rcp == "RCP60") %>%
  left_join (priority_spp, by = c("country", "species")) %>%
  arrange (country, rank) %>%
  write.excel()


## nutritional upside inds fed

nutr_upside_excel <- nutr_upside %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (rcp == "RCP60", period == "2051-2060", !nutrient == "Protein")

# prop is proportion, so multiply by 100 for percent

nutr_upside_excel %>%
  ungroup() %>%
  select (country, species, nutrient, mey_diff_child_rda, mey_diff_rdas_prop, rank) %>%
  arrange (country, rank) %>%
  write.excel()


nutr_upside_excel %>%
  ungroup() %>%
  select (country, species, nutrient, adapt_diff_child_rda, rank) %>%
  arrange (country, rank) %>%
  write.excel()

# upside full adapt to no adapt percent changes---
catch_upside <- ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0) %>%
  mutate (
    period = case_when (
      year %in% c(2026:2035) ~ "2026-2035",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, species) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, rcp, period, species) %>%
  summarize (mey_diff_mt = catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"],
             mey_diff_percent = (catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100,
             adapt_diff_mt = catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"],
             adapt_diff_percent = (catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100)

catch_upside %>%
  filter (period == "2051-2060", rcp == "RCP60") %>%
  right_join (priority_spp, by = c ("species", "country")) %>% View()


# upside in terms of relation to baseline catch----
# to be able to use with our updated landings data, instead I want to calculate what the catch in 2050 is relative to baseline under the different scenarios. 

catch_upside_relative <-  ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0) %>%
  mutate (
    # baseline and mid century and end century
  period = case_when (
    year %in% c(2012:2021) ~ "2012-2021",
    year %in% c(2051:2060) ~ "2051-2060",
    year %in% c(2091:2100) ~ "2091-2100")) %>%
    filter (!is.na (period)) %>%
    group_by (country, rcp, scenario, period, species) %>%
    summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by (country, rcp, species) %>%
    summarize (bau_ratio_midcentury = catch_mt[scenario == "No Adaptation" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
               bau_ratio_endcentury = catch_mt[scenario == "No Adaptation" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
               mey_ratio_midcentury = catch_mt[scenario == "Productivity Only" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
               mey_ratio_endcentury = catch_mt[scenario == "Productivity Only" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
               adapt_ratio_midcentury = catch_mt[scenario == "Full Adaptation" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
               adapt_ratio_endcentury = catch_mt[scenario == "Full Adaptation" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"]
               )

catch_upside_relative %>%
  filter (rcp == "RCP60") %>%
  right_join (priority_spp, by = c("country", "species")) %>%
  filter (!country == "Indonesia") %>%
  arrange (country, rank) %>%
  write.excel()
            

# convert to children fed, mid century, mey for chile ----

# For each species, i would take the landings, then multiply by the ratio, then feed that number into the function

chl_landings_input <- chl_landings %>%
  filter (between (year, 2017, 2021)) %>%
  group_by (species) %>%
  summarise (catch = mean (catch_mt, na.rm = TRUE)) %>%
  mutate (country = "Chile") %>%
  inner_join (priority_spp, by = c ("country", "species")) %>%
  inner_join (catch_upside_relative, by = c ("country", "species")) %>%
  filter (rcp == "RCP60") %>%
  mutate (mey_diff_catch = mey_ratio_midcentury * catch - bau_ratio_midcentury * catch,
          adapt_diff_catch = adapt_ratio_midcentury * catch - bau_ratio_midcentury * catch)

chl_landings_input_ls <- list (
  species = chl_landings_input$species,
  taxa = "Finfish",
  amount_mt = chl_landings_input$mey_diff_catch
)

q <- pmap_dfr (chl_landings_input_ls, calc_children_fed_func)

p <- pmap_dfr (chl_landings_input_ls, calc_children_fed_func)



###################################################################
# Plots----


# mega plot of policy levers. ----
#I need to recreate the quant sheet and then pivot long, so there's a "variable" column and a "children fed" column

#could have facets with species, or also have facets with variable and species on x axis

# start with just SAU data--> foreign, non-DHC, and exports
# messy hack, but replace Peru anchovy dhc value with 2018 value. in 2018, all artisanal was dhc and all industrial is fmfo
peru_anchov_dhc <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2018, species == "Engraulis ringens") %>%
  group_by (country, species, year) %>%
  summarise (prop_non_dhc = sum(tonnes[end_use_type == "Fishmeal and fish oil" & fishing_entity == "Peru"])/sum(tonnes[fishing_entity == "Peru"])) # 0.955

peru_anchov_total_2019 <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2019, species == "Engraulis ringens") %>%
  pull (tonnes) %>% sum()


  
t <- sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
  group_by (country, species, taxa) %>%
  summarise (total_domestic_catch = sum (tonnes[fishing_country == "Domestic catch"]),
             foreign_catch = sum (tonnes[fishing_country == "Foreign catch"]),
             domestic_non_dhc = sum (tonnes[end_use_type != "Direct human consumption" & fishing_country == "Domestic catch"])) %>%
  ungroup () %>%
  # mutate hack, fix peru anchovy. multiply total domestic anchov production * proportion non dhc from 2018
  mutate (domestic_non_dhc = ifelse (country == "Peru" & species == "Engraulis ringens", peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,  domestic_non_dhc)) %>%
  # join to exports
  left_join (exports_5yr_mean, by = c ("species", "country")) %>%
  mutate (export_volume = total_domestic_catch * mn_prop_exp, .keep = "unused") %>%
  # pivot longer
  pivot_longer (foreign_catch:export_volume,
                names_to = "lever",
                values_to = "mt")
# need to replace NA with 0?

# convert to list for calculate nutrients function
sau_lever_ls <-  list (
    species = t$species,
    taxa = t$taxa,
    amount_mt = t$mt)

sau_levers_nutr <- pmap_dfr (sau_lever_ls, calc_children_fed_func)

# just select one nutrient?
sau_levers_calcium <- sau_levers_nutr %>%
  filter (nutrient == "Calcium") %>%
  select (species, nutrient, catch_mt, children_fed) %>%
  rename (mt = catch_mt)

# bring back to ds
r <- t %>%
  left_join (sau_levers_calcium, by = c("species", "mt"))

plot_supply_chain_levers <- function (country_name) {
  
  p <- r %>%
    right_join(priority_spp, by = c("country", "species")) %>%
    filter (country == country_name)
    
  
  q <- p %>%
    ggplot (aes (y = children_fed, x = lever, fill = lever)) +
    geom_col () +
    facet_wrap( ~ comm_name, scales = "free_y") +
    theme_bw() +
    scale_x_discrete(labels = c ("Non-DHC", "Exports", "Foreign catch")) +
    labs (y = "Children's RNIs forgone", x = "Policy lever") +
    ggtitle (paste0("Calcium losses, ", country_name)) +
    theme (plot.title = element_text (size = 18),
          axis.text = element_text (size = 12),
          strip.text = element_text (size = 14),
          axis.title = element_text (size = 16))
    
  

  
}

png ("Figures/Foreign_DHC_Export_losses_Chile_comm_names.png", width = 12, height = 7, unit = "in", res = 300)  
print(plot_supply_chain_levers("Chile"))
dev.off()

png ("Figures/Foreign_DHC_Export_losses_Peru_comm_names.png", width = 12, height = 7, unit = "in", res = 300)  
print(plot_supply_chain_levers("Peru"))
dev.off()

png ("Figures/Foreign_DHC_Export_losses_Mex_comm_names.png", width = 12, height = 7, unit = "in", res = 300)  
print(plot_supply_chain_levers("Mexico"))
dev.off()

png ("Figures/Foreign_DHC_Export_losses_Indo_comm_names.png", width = 12, height = 7, unit = "in", res = 300)  
print(plot_supply_chain_levers("Indonesia"))
dev.off()

countries <- c("Chile", "Peru", "Mexico", "Sierra Leone")

pdf (file = "Figures/Foreign_DHC_Export_losses.pdf", width = 12, height = 8)

  lapply (countries, plot_supply_chain_levers)

dev.off()


# plot stacked?
png ("Figures/Foreign_DHC_Export_losses_Chile_stacked.png", width = 12, height = 8, unit = "in", res = 300)  
r %>%
  left_join (priority_spp, by = c ("country", "species")) %>%
  filter (country == "Chile", children_fed> 0) %>%
  ggplot (aes (y = children_fed/1000000, x = fct_rev(reorder(comm_name, children_fed)), fill = lever)) +
  geom_col (position = "stack") +
  #facet_wrap( ~ species, scales = "free_y") +
  theme_bw() +
  #scale_x_discrete(labels = c ("Non-DHC", "Exports", "Foreign catch")) +
  labs (y = "Children's RNIs forgone, millions", x = "", fill = "Policy lever") +
  ggtitle ("Calcium losses, Chile") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 18),
         strip.text = element_text (size = 14),
         axis.title = element_text (size = 14))
         #legend.position = "none")
dev.off()

png ("Figures/Foreign_DHC_Export_losses_Peru_stacked.png", width = 12, height = 8, unit = "in", res = 300)  
r %>%
  left_join (priority_spp, by = c ("country", "species")) %>%
  filter (country == "Peru", children_fed> 0) %>%
  ggplot (aes (y = children_fed/1000000, x = fct_rev(reorder(comm_name, children_fed)), fill = lever)) +
  geom_col (position = "stack") +
  #facet_wrap( ~ species, scales = "free_y") +
  theme_bw() +
  #scale_x_discrete(labels = c ("Non-DHC", "Exports", "Foreign catch")) +
  labs (y = "Children's RNIs forgone, millions", x = "", fill = "Policy lever") +
  ggtitle ("Calcium losses, Peru") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 18),
         strip.text = element_text (size = 14),
         axis.title = element_text (size = 14))
#legend.position = "none")
dev.off()
  
png ("Figures/Foreign_DHC_Export_losses_Peru_stacked_no_anchov.png", width = 12, height = 8, unit = "in", res = 300)  
r %>%
  left_join (priority_spp, by = c ("country", "species")) %>%
  filter (country == "Peru", children_fed> 0, comm_name != "Anchoveta") %>%
  ggplot (aes (y = children_fed/1000000, x = fct_rev(reorder(comm_name, children_fed)), fill = lever)) +
  geom_col (position = "stack") +
  #facet_wrap( ~ species, scales = "free_y") +
  theme_bw() +
  #scale_x_discrete(labels = c ("Non-DHC", "Exports", "Foreign catch")) +
  labs (y = "Children's RNIs forgone, millions", x = "", fill = "Policy lever") +
  ggtitle ("Calcium losses, Peru") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 18),
         strip.text = element_text (size = 14),
         axis.title = element_text (size = 14))
#legend.position = "none")
dev.off()

# plot with species as x axis? I don't think this is as helpful
plot_supply_chain_levers_spp <- function (country_name) {
  
  p <- r %>%
    filter (country == country_name)
  
  
  q <- p %>%
    ggplot (aes (y = children_fed, x = species)) +
    geom_col () +
    facet_wrap( ~ lever, scales = "free_y") +
    theme_bw() +
    #scale_x_discrete(labels = c ("Non-DHC", "Exports", "Foreign catch")) +
    labs (y = "Children's RNIs forgone", x = "Policy lever") +
    ggtitle (paste0("Nutrient losses, ", country_name)) +
    theme (plot.title = element_text (size = 18),
           axis.text = element_text (size = 12),
           strip.text = element_text (size = 14),
           axis.title = element_text (size = 14))
  
  
  
  
}

## plot dhc industrial vs. artisanal ----

# hack peru again --> all years all artisanal is 0 and all industrial is 1
# so need industrial catch 2019
peru_anchov_total_2019_ind <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2019, fishing_sector == "Industrial", species == "Engraulis ringens") %>%
  pull (tonnes) %>% sum()



sector_dhc <- sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
  filter (!country == "Indonesia", fishing_country =="Domestic catch", fishing_sector %in% c("Artisanal", "Industrial")) %>%
  group_by (country, species, taxa, fishing_sector) %>%
  summarise (non_dhc = sum (tonnes[end_use_type != "Direct human consumption"])) %>%
  ungroup () %>%
  # fix peru
  mutate (non_dhc = case_when (
          country == "Peru" & species == "Engraulis ringens" & fishing_sector == "Industrial" ~ peru_anchov_total_2019_ind, 
          country == "Peru" & species == "Engraulis ringens" & fishing_sector == "Artisanal" ~ 0,
        TRUE ~ non_dhc
          )
  ) 

sector_dhc_ls <- list (
  species = sector_dhc$species,
  taxa = sector_dhc$taxa,
  amount_mt = sector_dhc$non_dhc)
  
sector_dhc_nutr <- pmap_dfr (sector_dhc_ls, calc_children_fed_func)

# just select one nutrient?
sector_dhc_calcium <- sector_dhc_nutr %>%
  filter (nutrient == "Calcium") %>%
  select (species, nutrient, catch_mt, children_fed) %>%
  rename (non_dhc = catch_mt)

# bring back to ds
sector_dhc_calcium_join <- sector_dhc %>%
  left_join (sector_dhc_calcium, by = c("species", "non_dhc"))

plot_sector_dhc <- function (country_name) {
  
  p <- sector_dhc_calcium_join %>%
    filter (country == country_name)
  
  
  q <- p %>%
    ggplot (aes (y = children_fed, x = fishing_sector)) +
    geom_col () +
    facet_wrap( ~ species, scales = "free_y") +
    theme_bw() +
    labs (y = "Children's RNIs forgone", x = "") +
    ggtitle (paste0("Calcium losses from non-DHC use, ", country_name)) +
    theme (plot.title = element_text (size = 18),
           axis.text = element_text (size = 12),
           strip.text = element_text (size = 14),
           axis.title = element_text (size = 14))
  
  

  
}

plot_sector_dhc("Peru")
countries <- c("Chile", "Peru", "Mexico", "Sierra Leone")

pdf (file = "Figures/DHC_sector_losses.pdf", width = 12, height = 8)

lapply (countries, plot_sector_dhc)

dev.off()

# plot as stacked instead, children fed, not just losses ----

sector_dhc_full <- sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
  filter (!country == "Indonesia", fishing_country =="Domestic catch", fishing_sector %in% c("Artisanal", "Industrial")) %>%
  group_by (country, species, comm_name, taxa, fishing_sector) %>%
  summarise (non_dhc = sum (tonnes[end_use_type != "Direct human consumption"]),
             dhc = sum (tonnes[end_use_type == "Direct human consumption"])) %>%
  ungroup () %>%
  # fix peru
  mutate (non_dhc = case_when (
    country == "Peru" & species == "Engraulis ringens" & fishing_sector == "Industrial" ~ peru_anchov_total_2019_ind, 
    country == "Peru" & species == "Engraulis ringens" & fishing_sector == "Artisanal" ~ 0,
    TRUE ~ non_dhc),
    dhc = case_when (
      country == "Peru" & species == "Engraulis ringens" & fishing_sector == "Industrial" ~ 0,
      TRUE ~ dhc)
  ) %>%
  pivot_longer(c(non_dhc, dhc),
               names_to = "end_use",
               values_to = "catch_mt")

sector_dhc_full_ls <- list (
  species = sector_dhc_full$species,
  taxa = sector_dhc_full$taxa,
  amount_mt = sector_dhc_full$catch_mt)

sector_dhc_full_nutr <- pmap_dfr (sector_dhc_full_ls, calc_children_fed_func)

# bring back to ds
sector_dhc_calcium_join_full <- sector_dhc_full %>%
  left_join (filter (sector_dhc_full_nutr, nutrient == "Calcium"), by = c("species", "catch_mt"))
sector_dhc_calcium_join_full$end_use <- factor (sector_dhc_calcium_join_full$end_use, levels = c ("non_dhc", "dhc"))

png ("Figures/Chile_sector_calcium_dhc.png", width = 11, height = 8, unit = "in", res = 300)
sector_dhc_calcium_join_full %>%
  filter (country == "Chile") %>%
  ggplot (aes (x = fishing_sector, y = children_fed/1000000, fill = end_use)) +
  geom_col () +
  ggtitle ("Calcium provisioning by sector, Chile") +
  facet_wrap (~comm_name, scales = "free_y") +
  theme_bw() +
  labs (x = "", y = "Children's RNIs met (millions)") +
  theme (plot.title = element_text (size = 24),
         axis.text = element_text (size = 18),
         strip.text = element_text (size = 16),
         axis.title = element_text (size = 20),
         legend.position = "none")
dev.off()


png ("Figures/Peru_sector_calcium_dhc.png", width = 11, height = 8, unit = "in", res = 300)
sector_dhc_calcium_join_full %>%
  filter (country == "Peru") %>%
  ggplot (aes (x = fishing_sector, y = children_fed/1000000, fill = end_use)) +
  geom_col () +
  facet_wrap (~comm_name, scales = "free_y") +
  ggtitle ("Calcium provisioning by sector, Peru") +
  labs (x = "", y = "Children's RNIs met (millions)") +
  theme_bw() +
  theme (plot.title = element_text (size = 24),
         axis.text = element_text (size = 18),
         strip.text = element_text (size = 16),
         axis.title = element_text (size = 20),
         legend.position = "none")
dev.off()


# plot nutrient upsides MEY and BEY bar graphs ----

# also need to combine the landings data
chl_pri_21 <- priority_spp %>%
  filter (country == "Chile") %>%
  left_join (chl_landings, by = "species") %>%
  # take only 2021?
  filter (year == 2021) %>%
  group_by (country, species) %>%
  summarise (total_tonnes = sum (catch_mt)) %>%
  ungroup() %>%
  select (country, species, total_tonnes)

# disaggregate small scale vs. industrial; not looking at this right now

# start with DOMESTIC only
pri_spp_landings_sau_dom <- sau_2019 %>%
  filter (fishing_entity == country) %>%
  right_join(priority_spp, by = c ("country", "species")) %>%
  filter (!country %in% c("Indonesia", "Chile")) %>%
  group_by(country, species) %>%
  summarise (total_tonnes = sum(tonnes)) %>%
  ungroup()


compiled_landings_dom <- rbind (chl_pri_21, pri_spp_landings_sau_dom)

# just calcium content and p_edible 

p_edible <- data.frame (
  taxa = c ("Finfish", "Mollusc", "Cephalopod"), 
  p_edible = c (0.87, 0.17, 0.67)
)

calcium_amts <-fishnutr_long %>%
  filter (nutrient == "Calcium") %>%
  right_join (priority_spp, by = "species") %>%
  left_join (p_edible, by = "taxa") %>%
  select (-c(country, comm_name, rank)) %>%
  distinct()

# manually do d.gigas?
calcium_amts$amount[which (calcium_amts$species == "Dosidicus gigas")] <- d_gigas_nutr$amount[which (d_gigas_nutr$nutrient == "Calcium")]

# attach to upside
upside_ratios_pri_spp <- catch_upside_relative %>%
  right_join (compiled_landings_dom, by = c("country", "species")) %>%
  left_join (calcium_amts, by = "species") %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
          #convert to upside, subtract 
          mey_midcentury = mey_ratio_midcentury - bau_ratio_midcentury,
          mey_endcentury = mey_ratio_endcentury - bau_ratio_endcentury,
          adapt_midcentury = adapt_ratio_midcentury - bau_ratio_midcentury,
          adapt_endcentury = adapt_ratio_endcentury - bau_ratio_endcentury,
          # convert to calcium amount and then RNIs met
          across (mey_midcentury:adapt_endcentury, 
                  ~.x * p_edible * amount * 1000 * 1000 /100 / 365 / 500 ) # 500 is RNI
          ) 

write.excel (upside_ratios_pri_spp)



# make sure this checks out with excel sheet
catch_upside_relative %>%
  right_join (compiled_landings_dom, by = c("country", "species")) %>%
  write.excel()

# similar but not perfect, some might be bc foreign fishing

v <- upside_ratios_pri_spp %>%
  select (country, rcp, species, mey_midcentury:adapt_endcentury) %>%
  pivot_longer(mey_midcentury:adapt_endcentury, 
               names_to = "upside",
               values_to = "child_rni") 

v$upside <- factor(v$upside, levels = c ("mey_midcentury", "mey_endcentury", "adapt_midcentury", "adapt_endcentury"))

plot_upside_bar <- function (country_name) {
  v %>%
    filter (country == country_name, !is.na(rcp)) %>%
    ggplot (aes (x = upside, y = child_rni)) +
    geom_col () +
    facet_grid (species ~ rcp, scales = "free_y") +
    theme_bw() +
    geom_hline (yintercept = 0, lty = 2) +
    labs (y = "Additional child RNIs met", x = "") +
    ggtitle (paste0("Calcium upside, ", country_name)) +
    theme (plot.title = element_text (size = 18),
           axis.text = element_text (size = 12),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 14),
           strip.text.y = element_text (size = 12),
           axis.title = element_text (size = 14)) 
} 

plot_upside_bar("Peru")

pdf (file = "Figures/upside_calcium_bar.pdf", width = 14, height = 12)

lapply (countries, plot_upside_bar)

dev.off()

# simpler bar graphs for LAC presentation ----
png ("Figures/Peru_calcium_upside_LAC_pres.png", width = 10, height = 6, units= "in", res = 300)
v %>%
  filter (country == "Peru", rcp %in% c("RCP26", "RCP60"), upside == "adapt_midcentury") %>%
  right_join(filter (priority_spp, country == "Peru"), by = c("country", "species")) %>%
  filter (!is.na(rcp)) %>%
  ggplot (aes (x = rcp, y = child_rni)) +
  geom_col () +
  facet_wrap (~comm_name, scales = "free_y") +
  theme_bw() +
  geom_hline (yintercept = 0, lty = 2) +
  labs (y = "Additional child RNIs met", x = "") +
  ggtitle ("Calcium upside, Peru") +
  theme (plot.title = element_text (size = 24),
         axis.text = element_text (size = 14),
         #axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 16),
         axis.title = element_text (size = 18)) 
  dev.off()
  
  
  png ("Figures/Chile_calcium_upside_LAC_pres.png", width = 10, height = 6, units= "in", res = 300)
  v %>%
    filter (country == "Chile", rcp %in% c("RCP26", "RCP60"), upside == "adapt_midcentury") %>%
    right_join(filter (priority_spp, country == "Chile"), by = c("country", "species")) %>%
    filter (!is.na(rcp)) %>%
    ggplot (aes (x = rcp, y = child_rni)) +
    geom_col () +
    facet_wrap (~comm_name, scales = "free_y") +
    theme_bw() +
    #geom_hline (yintercept = 0, lty = 2) +
    labs (y = "Additional child RNIs met", x = "") +
    ggtitle ("Calcium upside, Chile") +
    theme (plot.title = element_text (size = 24),
           axis.text = element_text (size = 14),
           #axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 16),
           axis.title = element_text (size = 18)) 
  dev.off()
  
  png ("Figures/Mexico_calcium_upside_LAC_pres.png", width = 10, height = 6, units= "in", res = 300)
  v %>%
    filter (country == "Mexico", rcp %in% c("RCP26", "RCP60"), upside == "adapt_midcentury") %>%
    right_join(filter (priority_spp, country == "Mexico"), by = c("country", "species")) %>%
    filter (!is.na(rcp)) %>%
    ggplot (aes (x = rcp, y = child_rni)) +
    geom_col () +
    facet_wrap (~comm_name, scales = "free_y") +
    theme_bw() +
    #geom_hline (yintercept = 0, lty = 2) +
    labs (y = "Additional child RNIs met", x = "") +
    ggtitle ("Calcium upside, Mexico") +
    theme (plot.title = element_text (size = 24),
           axis.text = element_text (size = 14),
           #axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 16),
           axis.title = element_text (size = 18)) 
  dev.off()
  
  
  # for mexico, have to do grouper from raw nutricast data
  mex_group <- pri_spp_catch_upside %>%
    filter (country == "Mexico", species == "Epinephelus morio", period == "2051-2060") 
  
  mex_group_input_ls <- list (
    species = "Epinephelus morio",
    taxa = "Finfish",
    amount_mt = mex_group$adapt_diff_mt
  )
  
  mex_group_fed <- pmap_dfr (mex_group_input_ls, calc_children_fed_func)
  
  png ("Figures/Mexico_calcium_upside_LAC_pres_Grouper.png", width = 5, height = 5, units= "in", res = 300)
  mex_group %>%
    rename (catch_mt = adapt_diff_mt) %>%
    left_join (mex_group_fed, by = "catch_mt") %>%
    filter (nutrient == "Calcium", rcp %in% c("RCP26", "RCP60")) %>%
    ggplot (aes (x = rcp, y = children_fed)) +
    geom_col() +
    theme_bw( ) +
    geom_hline (yintercept = 0, lty = 2) +
    labs (y = "Additional child RNIs met", x = "") +
    theme (plot.title = element_text (size = 24),
           axis.text = element_text (size = 14),
           #axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 16),
           axis.title = element_text (size = 18)) 
  
  dev.off()
  
#################################################################################

# plot micronutrient density vs. % loss nutricast ----
t <- ds_spp %>%
  right_join (priority_spp, by = c("country", "species")) %>%
  filter (scenario %in% c("No Adaptation"), catch_mt > 0) %>%
  mutate (
    period = case_when (
      year %in% c(2012:2021) ~ "2012-2021",
      year %in% c(2026:2035) ~ "2026-2035",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, period, species) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, rcp, species) %>%
  summarise (perc_ratio_midcentury = (catch_mt[period == "2051-2060"] - catch_mt[period == "2012-2021"])/catch_mt[period == "2012-2021"],
             perc_diff_endcentury = (catch_mt[period == "2091-2100"] - catch_mt[period == "2012-2021"])/catch_mt[period == "2012-2021"])


mic_dens <- pri_spp_nutr %>%
  filter ( 
          !nutrient %in% c("Protein", "Selenium")) %>%
  group_by (country, species) %>%
  summarise (micronutrient_density = sum (perc_rda)) 


mic_dens %>%
  left_join (t, by = c ("country", "species")) %>%
  filter (rcp == "RCP60") %>%
  ggplot (aes (x = perc_diff_midcentury, y = micronutrient_density, col = country)) +
  geom_point () +
  geom_text (aes(label = species)) +
  theme_bw() +
  geom_hline (yintercept = 0, lty = 2) +
  geom_vline (xintercept = 0, lty = 2) +
  xlim (c(-1, 0.005))

# plot mic dens vs. percent gain adapt diff ----
x <- catch_upside %>%
  right_join (priority_spp, by = c ("country", "species")) %>%
  right_join (mic_dens, by = c("species", "country")) %>%
  filter (rcp == "RCP60", period == "2051-2060") %>%
  ggplot (aes (x = adapt_diff_percent, y = micronutrient_density, col = country)) +
  geom_point () +
  geom_text (aes(label = species)) +
  theme_bw() +
  geom_hline (yintercept = 0, lty = 2) +
  geom_vline (xintercept = 0, lty = 2) #+
  #xlim (c(-1, 0.005))
  



# show nutr content as bar graph ----
plot_priority_spp_nutr_bar <- function (country_name, n_spp) {
  
  # filter priority species
 country_spp <- priority_spp %>%
    filter (country == country_name, rank <= n_spp)
  
  
  p <- pri_spp_nutr %>%
    # bring back common names, filter to country
    #right_join (country_spp, by = "species") %>%
    filter (country == country_name, species %in% country_spp$species, !nutrient %in% c("Protein", "Selenium")) %>%
    ggplot () +
    geom_bar (aes (x = nutrient, y = perc_rni), stat = "identity") +
    facet_wrap (~comm_name) +
    ggtitle (paste0("Daily recommended nutrient intake for children met from 100g serving \n", country_name, " priority species")) +
    theme_bw() +
    labs (x = "", y = "Percent daily needs met") +
    theme (axis.title = element_text (size = 14),
           axis.text = element_text(size =10),
           strip.text = element_text (size = 14),
           plot.title = element_text (size = 14))
  
 # png (paste0("Figures/", country_name, "_pri_spp_nutr_bar.png"), res = 300, width = 8, height = 5, units = "in")
  print (p)
  #dev.off()
  
  
}

plot_priority_spp_nutr_bar(country_name = "Chile", n_spp = 5)
plot_priority_spp_nutr_bar(country_name = "Peru", n_spp = 5)

# Can I have dodged colorful bars all on one axis
# order by overall nutrient density 
png ("Figures/Chl_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
pri_spp_nutr %>%
  filter (nutrient != "Protein") %>%
  group_by (country, species) %>%
  mutate (micronutrient_density = sum (perc_rni)) %>%
  ungroup() %>%
  filter (country == "Chile") %>%
  ggplot (aes (x = reorder(comm_name, -micronutrient_density), fill = nutrient, y = perc_rni)) +
  geom_col (position = "dodge") +
  #geom_text (aes(label = round(micronutrient_density, 1))) +
  theme_bw() +
  labs (x = "", y = "") +
  theme (legend.position = "none", 
         axis.text = element_text (size = 16))
dev.off()

png ("Figures/Peru_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
pri_spp_nutr %>%
  filter (nutrient != "Protein") %>%
  group_by (country, species) %>%
  mutate (micronutrient_density = sum (perc_rni)) %>%
  ungroup() %>%
  filter (country == "Peru") %>%
  ggplot (aes (x = reorder(comm_name, -micronutrient_density), fill = nutrient, y = perc_rni)) +
  geom_col (position = "dodge") +
  #geom_text (aes(label = round(micronutrient_density, 1))) +
  theme_bw() +
  labs (x = "", y = "") +
  theme (legend.position = "none", 
         axis.text = element_text (size = 16))
dev.off()


png ("Figures/Mex_pri_spp_nutr_dodge_bar.png", width = 10, height = 5, units = "in", res = 300)
pri_spp_nutr %>%
  filter (nutrient != "Protein") %>%
  group_by (country, species) %>%
  mutate (micronutrient_density = sum (perc_rni)) %>%
  ungroup() %>%
  filter (country == "Mexico") %>%
  ggplot (aes (x = reorder(comm_name, -micronutrient_density), fill = nutrient, y = perc_rni)) +
  geom_col (position = "dodge") +
  theme_bw() +
  labs (x = "", y = "") +
  theme (legend.position = "none", 
         axis.text = element_text (size = 16))
dev.off()




# initial plot, landings and needs met of one species ----
# anchoveta

# plot initial landings figure function ----
plot_sau_landings_one_spp <- function (country_name, species_name, year) {
  
  common_name <- filter (priority_spp, country == country_name, species == species_name) %>%
    pull (comm_name)
  
  
  landings <- sau_country_cleaned %>%
    filter (country == country_name, species == species_name, between (year, 2010, 2019)) 
  
  p_title <- paste0 (common_name, " catch in ", country_name, "'s EEZ \n Sea Around Us project data")
  
  p_filename <- paste (country_name, common_name, "landings", sep = "_")
  
  p <- ggplot (landings,
               aes (x = year, y = tonnes/1000, fill = species)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual (values = c("dodgerblue3")) +
    labs (x = "", y = "Catch, thousand tons") +
    ggtitle (p_title) +
    theme (
      
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 12),
     plot.title = element_text (size = 14),
      legend.position = "none"
    )
  
  
  png (paste0("Figures/", p_filename, ".png"), res = 300, width = 4, height = 5, units = "in")
  print (p)
  dev.off()
  
  #plot updated 2019 sau data, just peru----
  pri_spp_per <- pri_spp_nutr %>% 
    filter (country == "Peru")
  
png ("Figures/Peru_SAU_2019_landings_pri_spp.png", res = 300, width = 6, height = 5, units = "in")  
  sau_2019 %>%
    right_join(pri_spp_per) %>%
    filter (country == "Peru", fishing_entity == "Peru") %>%
    ggplot (aes (x = reorder(comm_name, -tonnes), y = tonnes/1000000)) +
    geom_col () +
    theme_bw() +
    labs (x = "", y = "") +
    theme (axis.text.x = element_text (size = 13),
           axis.text.y = element_text (size = 15))
  dev.off()
    
  
  # plot by foreign, dhc...this is too complicated 
  # have to use hack from mega lever
  t <- sau_2019 %>%
    right_join (priority_spp, by = c ("country",  "species")) %>%
    mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
    filter (!country == "Indonesia") %>%
    group_by (country, species, taxa) %>%
    summarise (total_domestic_catch = sum (tonnes[fishing_country == "Domestic catch"]),
               foreign_catch = sum (tonnes[fishing_country == "Foreign catch"]),
               domestic_non_dhc = sum (tonnes[end_use_type != "Direct human consumption" & fishing_country == "Domestic catch"])) %>%
    ungroup () %>%
    # mutate hack, fix peru anchovy. multiply total domestic anchov production * proportion non dhc from 2018
    mutate (domestic_non_dhc = ifelse (country == "Peru" & species == "Engraulis ringens", peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,  domestic_non_dhc)) %>%
    # join to exports
    left_join (exports_5yr_mean, by = c ("species", "country")) %>%
    mutate (export_volume = total_domestic_catch * mn_prop_exp, .keep = "unused") %>%
    # pivot longer
    pivot_longer (foreign_catch:export_volume,
                  names_to = "lever",
                  values_to = "mt")
  
  
  png ("Figures/Peru_SAU_2019_landings_pri_spp.png", res = 300, width = 6.5, height = 5, units = "in")  
sau_2019 %>%

    filter (country == "Peru") %>%
    mutate (fishing_country = ifelse (fishing_entity == "Peru", "Domestic catch", "Foreign catch")) %>%
    group_by (country, species) %>%
    summarise (total_domestic_catch = sum (tonnes[fishing_country == "Domestic catch"]),
               foreign_catch = sum (tonnes[fishing_country == "Foreign catch"]),
               domestic_non_dhc = sum (tonnes[end_use_type != "Direct human consumption" & fishing_country == "Domestic catch"])) %>%
    ungroup () %>%
    # mutate hack, fix peru anchovy. multiply total domestic anchov production * proportion non dhc from 2018
    mutate (domestic_non_dhc = ifelse (country == "Peru" & species == "Engraulis ringens", peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,  domestic_non_dhc)) %>%
    # join to exports
    left_join (exports_5yr_mean, by = c ("species", "country")) %>%
    mutate (export_volume = total_domestic_catch * mn_prop_exp, .keep = "unused") %>%
    # pivot longer
    pivot_longer (foreign_catch:export_volume,
                  names_to = "lever",
                  values_to = "mt") %>%
   right_join(filter (priority_spp, country == "Peru")) %>%
  
    ggplot (aes (x = reorder(comm_name, -mt), y = mt/1000000, fill = lever)) +
    geom_col () +
    theme_bw() +
    labs (x = "", y = "") +
    theme (axis.text.x = element_text (size = 13),
           axis.text.y = element_text (size = 15))
  dev.off()
  

  # plot nutrient needs met----
  
  # just pota for Peru, 2019
  pota_catch <- sau_2019 %>% filter (country == "Peru", fishing_entity == "Peru", species == "Dosidicus gigas") %>%
    group_by (species) %>% summarise (tonnes = sum (tonnes))
  
  pota_2019 <- calc_children_fed_func(taxa = "Cephalopod", species = "Dosidicus gigas", amount = pota_catch$tonnes)
  
  png ("Figures/Peru_SAU_2019_pota_children_fed.png", res = 300, width = 5.5, height = 5, units = "in")  
  pota_2019 %>%
    filter (nutrient != "Protein") %>%
    ggplot (aes (x = nutrient, y = children_fed / 1000000, fill = nutrient)) +
    geom_col() +
    theme_bw() +
    labs (x = "", y = "") +
    theme (axis.text.x = element_blank (),
           axis.text.y = element_text (size = 15),
           legend.position = "none")
 
  dev.off()
  
  #indonesia
  mack_catch <- sau_2019 %>%
    filter (country == "Indonesia", 
            fishing_entity == "Indonesia",
            species == "Rastrelliger brachysoma")%>%
    group_by (species) %>% summarise (tonnes = sum (tonnes))
  
  mack_2019_children_fed <- calc_children_fed_func(taxa = "Finfish", species = "Rastrelliger brachysoma", amount =  mack_catch$tonnes)
  
  png ("Figures/Indo_SAU_2019_R_brach_children_fed.png", res = 300, width = 5.5, height = 5, units = "in")  
  mack_2019_children_fed %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    ggplot (aes (x = nutrient, y = children_fed / 1000000, fill = nutrient)) +
    geom_col() +
    theme_bw() +
    labs (x = "", y = "") +
    theme (axis.text.x = element_blank (),
           axis.text.y = element_text (size = 15),
           legend.position = "none")
  
  dev.off()
  # plot overall needs met for priority spp, each country ----
  

  peru_pri_spp_catch <- sau_2019 %>%
    filter (country == "Peru", fishing_entity == "Peru") %>%
    group_by (species) %>%
    summarise (tonnes = sum (tonnes)) %>%
    right_join (filter (priority_spp, country == "Peru")) %>%
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
    unnest(cols = c(children_fed),  names_repair = "check_unique")
  
  
  png ("Figures/Peru_pri_spp_nutr_bank.png", res = 300, width = 11, height = 6, units = "in") 
  peru_pri_spp_catch %>%
    filter (nutrient != "Protein") %>%
    ggplot (aes (x = nutrient, y = children_fed/1000000, fill = nutrient)) +
    geom_col() +
    facet_wrap (~comm_name, scales = "free_y") +
    theme_bw () +
    labs (x = "", y = "") +
    theme (axis.text.x = element_blank(),
           legend.position = "none",
           axis.text.y = element_text (size = 14),
           strip.text = element_text (size = 14))
  dev.off()
  
  
  
  mex_pri_spp_catch <- sau_2019 %>%
    filter (country == "Mexico", fishing_entity == "Mexico") %>%
    group_by (species) %>%
    summarise (tonnes = sum (tonnes)) %>%
    right_join (filter (priority_spp, country == "Mexico")) %>%
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
    unnest()
  
  
  png ("Figures/Mexico_pri_spp_nutr_bank.png", res = 300, width = 11, height = 6, units = "in") 
  mex_pri_spp_catch  %>%
    filter (nutrient != "Protein") %>%
    ggplot (aes (x = nutrient, y = children_fed/1000, fill = nutrient)) +
    geom_col() +
    facet_wrap (~comm_name, scales = "free_y") +
    theme_bw () +
    labs (x = "", y = "") +
    theme (axis.text.x = element_blank(),
           legend.position = "none",
           axis.text.y = element_text (size = 14),
           strip.text = element_text (size = 14))
  dev.off()
  
  # chile use landings data
  chl_pri_spp_catch <- chl_landings %>%
    filter (year == 2021) %>%
    group_by (species) %>%
    summarise (catch_mt = sum (catch_mt)) %>%
    right_join (filter (priority_spp, country == "Chile")) %>%
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
    unnest()
  
  png ("Figures/Chile_pri_spp_nutr_bank.png", res = 300, width = 11, height = 6, units = "in") 
  chl_pri_spp_catch  %>%
    filter (nutrient != "Protein") %>%
    ggplot (aes (x = nutrient, y = children_fed/1000000, fill = nutrient)) +
    geom_col() +
    facet_wrap (~comm_name, scales = "free_y") +
    theme_bw () +
    labs (x = "", y = "") +
    theme (axis.text.x = element_blank(),
           legend.position = "none",
           axis.text.y = element_text (size = 14),
           strip.text = element_text (size = 14))
  dev.off()
  
  
 ################################## 
   
  nutr_needs <- sau_nutr %>%
    filter (country == country_name, species == species_name, year == year) %>%
    group_by (species, nutrient) %>%
    summarise (tot_servings = sum (nutr_servings)) %>%
    left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
    mutate (needs_met = tot_servings / mean_rda, 
            nutrient = case_when (nutrient == "Vitamin_A" ~ "Vit A",
                                  nutrient == "Omega_3" ~ "Omega 3",
                                  TRUE ~ nutrient)) %>%
    filter (!nutrient %in% c("Protein", "Selenium"))
  
  ymax <- filter (ylim_sau, country == country_name, species == species_name)
  
  
  nutr_p <- ggplot (nutr_needs, aes (x = nutrient, y = needs_met/1000000, fill = species)) +
    geom_bar (stat = "identity", position = "dodge") +
    theme_bw() +
    labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
    ggtitle (paste0("# children's daily intake needs met,\n ", common_name, ", ", year)) +
    ylim(c(0, ymax$max)) +
    scale_fill_manual (values = c("dodgerblue3")) +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 12),
     plot.title = element_text (size = 14),
      legend.position = "none"
    )
  
  p_filename <- paste (country_name, common_name, "needs_met", sep = "_")
  
  #png (paste0("Figures/", p_filename, ".png"), res = 300, width = 4, height = 5, units = "in")
  print (nutr_p)
  #dev.off()
  
}

plot_sau_landings_one_spp(country_name = "Peru", species_name = "Trachurus murphyi", year = 2019)
plot_sau_landings_one_spp(country_name = "Chile", species_name = "Trachurus murphyi", year = 2015)



# show foreign vs domestic catch----

plot_sau_landings_domestic_one_spp <- function (country_name, species_name, year) {
  
  common_name <- filter (priority_spp, country == country_name, species == species_name) %>%
    pull (comm_name)
  
  
  landings_country <- sau_country_cleaned %>%
    filter (country == country_name, species == species_name, between (year, 2000, 2015)) %>%
    mutate (fishing_country = ifelse (fishing_entity == country_name, "Domestic catch", "Foreign catch"))
  
  landings_country$fishing_country <- factor (landings_country$fishing_country, levels = c("Foreign catch", "Domestic catch"))
  
  p_title <- paste0 (common_name, " catch in ", country_name, "'s EEZ \n Sea Around Us project data")
  
  p_filename <- paste (country_name, common_name, "landings_domestic", sep = "_")
  
  p_dom <- landings_country %>%
  ggplot (aes (x = year, y = tonnes/1000, fill = fishing_country)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual (values = c("gray70", "dodgerblue3")) +
    labs (x = "", y = "Catch, thousand tons", fill = "") +
    ggtitle (p_title) +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 12),
     plot.title = element_text (size = 14),
    )
  
  png (paste0("Figures/", country_name, "_", common_name, "_landings_domestic.png"),res = 300, width = 5, height = 5, units = "in")
  
  print (p_dom)
  
  dev.off()
  
  # plot nutrient needs met
  nutr_needs <- sau_nutr %>%
    filter (country == country_name, species == species_name, year == year, fishing_entity == country_name) %>%
    group_by (species, nutrient) %>%
    summarise (tot_servings = sum (nutr_servings)) %>%
    left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
    mutate (needs_met = tot_servings / mean_rda, 
            nutrient = case_when (nutrient == "Vitamin_A" ~ "Vit A",
                                  nutrient == "Omega_3" ~ "Omega 3",
                                  TRUE ~ nutrient)) %>%
    filter (!nutrient %in% c("Protein", "Selenium"))
  
  # set ymax to be consistent
  ymax <- filter (ylim_sau, country == country_name, species == species_name)
  
  p_dom_nutr <- ggplot (nutr_needs, aes (x = nutrient, y = needs_met/1000000, fill = species)) +
    geom_bar (stat = "identity", position = "dodge") +
    theme_bw() +
    labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
    ggtitle (paste0("# children's daily intake needs met,\n ", common_name, ", ", year, ", Domestic catch")) +
    ylim(c(0, ymax$max)) +
    scale_fill_manual (values = c("dodgerblue3")) +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 12),
     plot.title = element_text (size = 14),
      legend.position = "none"
    )
  
  p_filename <- paste (country_name, common_name, "needs_met_domestic", sep = "_")
  
  png (paste0("Figures/", p_filename, ".png"), res = 300, width = 4, height = 5, units = "in")
  
  print (p_dom_nutr)
  
  dev.off()
  
}

plot_sau_landings_domestic_one_spp ("Chile", "Trachurus murphyi", 2015)



# plot nutritional upside projections ----
proj_inds_fed_sector <- readRDS("Data/Nutricast_by_SAU_sector_inds_fed.Rds")

plot_nutr_upside_one_spp <- function (country_name, species_name, climate_scenario) {
  
  common_name <- filter (priority_spp, country == country_name, species == species_name) %>%
    pull (comm_name)
  
  spp_ts <- ds_spp %>%
    filter (country == country_name, species == species_name, rcp == climate_scenario, scenario %in% c("No Adaptation", "Full Adaptation"), year > 2025)
  
  p_title <- paste0 ("Projected catch, ", common_name, ", \nClimate scenario: ", climate_scenario)
  
  
  p_ts <- spp_ts %>%
    ggplot (aes (x = year, y = catch_mt/1000, col = scenario)) +
    geom_line(lwd = 1.5) +
    theme_bw() +
    labs (x = "", y = "Catch, 1000 tonnes", fill = "Management\n scenario") +
    ggtitle (p_title) +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 12),
     plot.title = element_text (size = 14),
     legend.title = element_blank()
      #legend.position = "none"
    )
  
  #png (paste0("Figures/", country_name, "_", common_name, "_nutricast_ts.png"), res = 300, width = 4, height = 5, units = "in")
  
  print (p_ts)
  
  dev.off()
  
  # additional nutr needs met
  addl_needs <- proj_inds_fed_sector %>%
    filter (country == country_name, species == species_name, rcp == climate_scenario,
            group == "Child", scenario %in% c("No Adaptation", "Full Adaptation"), period == "2050-2060", 
            !nutrient %in% c("Protein", "Selenium")) %>%
    # dumb but group by sector and calculate difference
    mutate (tot_fed = inds_fed_industrial + inds_fed_ssf) %>%
    group_by (nutrient) %>%
    summarise (diff_fed = tot_fed[scenario == "Full Adaptation"] - tot_fed[scenario == "No Adaptation"])
  
  nutr_upside_bar <- addl_needs %>%
    ggplot (aes (x = nutrient, y = diff_fed/1000, fill = nutrient)) +
    geom_bar (stat = "identity", position = "dodge") +
    scale_fill_manual (values = rep("dodgerblue3", 5)) +
    theme_bw() +
    labs (x = "Nutrient", y = "Additional children fed, thousands", fill = "Management\n scenario") +
    ggtitle (paste0("Additional nutrient needs met by \nimplementing adaptive management,\n", common_name, ", 2050-2060, ", climate_scenario)) +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 12),
     plot.title = element_text (size = 14),
     legend.position = "none"

    )
  
  png (paste0("Figures/", country_name, "_", common_name, "_nutrient_upside.png"), res = 300, width = 4, height = 5, units = "in")
  
  print (nutr_upside_bar) 
  
  dev.off()
}
  
plot_nutr_upside_one_spp ("Chile", "Trachurus murphyi", "RCP60")

# just plot upside for priority species, by country ----
addl_needs <- proj_inds_fed_sector %>%
  right_join (priority_spp, by = c("species", "country")) %>%
  filter (rcp == "RCP60",
          group == "Child", scenario %in% c("No Adaptation", "Full Adaptation"), period == "2050-2060", 
          !nutrient %in% c("Protein", "Selenium")) %>%
  # dumb but group by sector and calculate difference
  mutate (tot_fed = inds_fed_industrial + inds_fed_ssf) %>%
  group_by (nutrient) %>%
  mutate (diff_fed = tot_fed[scenario == "Full Adaptation"] - tot_fed[scenario == "No Adaptation"])

nutr_upside_bar <- addl_needs %>%
  ggplot (aes (x = nutrient, y = diff_fed/1000, fill = nutrient)) +
  geom_bar (stat = "identity", position = "dodge") +
  scale_fill_manual (values = rep("dodgerblue3", 5)) +
  theme_bw() +
  labs (x = "Nutrient", y = "Additional children fed, thousands", fill = "Management\n scenario") +
  ggtitle (paste0("Additional nutrient needs met by \nimplementing adaptive management,\n", common_name, ", 2050-2060, ", climate_scenario)) +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
    plot.title = element_text (size = 14),
    legend.position = "none"
    
  )






# nutr upside, multispecies ----
plot_upside_multispecies <- function (country_name, n_spp, climate_scenario) {
  
  country_spp <- priority_spp %>%
    filter (country == country_name, rank <= n_spp)
  
  # additional nutr needs met
  addl_needs <- proj_inds_fed_sector %>%
    right_join (country_spp, by = c ("country", "species")) %>%
    filter (rcp == climate_scenario,
            group == "Child", scenario %in% c("No Adaptation", "Full Adaptation"), period == "2050-2060", 
            !nutrient %in% c("Protein", "Selenium")) %>%
    # dumb but group by sector and calculate difference
    mutate (tot_fed = inds_fed_industrial + inds_fed_ssf,
            nutrient = case_when (nutrient == "Calcium" ~ "Cal",
                                  nutrient == "Omega 3" ~ "Om3",
                                  TRUE ~ nutrient)) %>%
    group_by (comm_name, nutrient) %>%
    summarise (diff_fed = tot_fed[scenario == "Full Adaptation"] - tot_fed[scenario == "No Adaptation"])
  
  
  nutr_upside_bar <- addl_needs %>%
    ggplot (aes (x = nutrient, y = diff_fed/1000, fill = nutrient)) +
    geom_bar (stat = "identity", position = "dodge") +
    scale_fill_manual (values = rep("dodgerblue3", 5)) +
    theme_bw() +
    labs (x = "Nutrient", y = "Additional children fed, thousands", fill = "Management\n scenario") +
    ggtitle (paste0("Additional nutrient needs met by implementing adaptive management,\n", country_name, ", 2050-2060, ", climate_scenario)) +
    facet_wrap (~comm_name, scales = "free_y") +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 10),
     plot.title = element_text (size = 14),
      legend.position = "none"
    )
  
  png (paste0("Figures/", country_name, "_nutrient_upside_multispecies.png"), res = 300, width = 8, height = 5, units = "in")
  
  print (nutr_upside_bar) 
  
  dev.off()
  
}

plot_upside_multispecies("Chile", n_spp = 5, climate_scenario = "RCP60")
plot_upside_multispecies("Peru", n_spp = 5, climate_scenario = "RCP60")

# stacked graph with inds fed for all priority species, foreign/domestic/exported ----
# don't have export data yet
plot_stack_foreign_export_multi <- function (country_name, n_spp, year) {
  
  country_spp <- priority_spp %>%
    filter (country == country_name, rank <= n_spp)
  
  # filter nutrient content by sector
  nutr_sector_sau <- sau_nutr %>%
    right_join (country_spp, by = c("species", "country")) %>%
                  filter (year == year) %>%
                  
    # filter (country == country_name, species %in% country_spp$species, between (year, 2000, 2015)) %>%
    mutate (fishing_country = ifelse (fishing_entity == country_name, "Domestic catch", "Foreign catch")) %>%
    # calculate number inds fed
    group_by (comm_name, nutrient, fishing_country) %>%
    summarise (tot_servings = sum (nutr_servings)) %>%
    left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
    mutate (needs_met = tot_servings / mean_rda, 
            nutrient = case_when (nutrient == "Calcium" ~ "Cal",
                                  nutrient == "Vitamin_A" ~ "Vit A",
                                  nutrient == "Omega_3" ~ "Om3",
                                  TRUE ~ nutrient)) %>%
    filter (!nutrient %in% c("Protein", "Selenium"))
  
  # set levels
  nutr_sector_sau$fishing_country <- factor (nutr_sector_sau$fishing_country, levels = c("Foreign catch", "Domestic catch"))
  
  
  # plot
  p_stack <- nutr_sector_sau %>%
    ggplot (aes (x = nutrient, y = needs_met/1000000, fill = fishing_country)) +
    geom_bar (stat = "identity", position = "dodge") +
    theme_bw() +
    #ylim(c(0, 3.25)) +
    labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
    ggtitle (paste0("# children's daily intake needs met,\n", country_name, ", ", year)) +
    scale_fill_manual (values = c("gray70", "dodgerblue3")) +
    facet_wrap (~comm_name, scales = "free_y") +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 10),
     plot.title = element_text (size = 14),
      # legend.position = "none"
    )
  
  png (paste0("Figures/", country_name, "_priority_nutr_domestic_stacked.png"), res = 300, width = 8, height = 5, units = "in")
  
  print (p_stack)
  
  dev.off()
  
}


plot_stack_foreign_export_multi ("Chile", n_spp = 5, year = 2015)
plot_stack_foreign_export_multi ("Peru", n_spp = 5, year = 2015)


# stacked graph with inds fed for all priority species, industrial/artisanal ----
plot_stack_sector_multi <- function (country_name, n_spp, year) {
  
  country_spp <- priority_spp %>%
    filter (country == country_name, rank <= n_spp)
  
  # filter nutrient content by sector
  nutr_sector_sau <- sau_nutr %>%
    right_join (country_spp, by = c("species", "country")) %>%
    filter (year == year, fishing_sector %in% c("Industrial", "Artisanal")) %>%
    # calculate number inds fed
    group_by (comm_name, nutrient, fishing_sector) %>%
    summarise (tot_servings = sum (nutr_servings)) %>%
    left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
    mutate (needs_met = tot_servings / mean_rda, 
            nutrient = case_when (nutrient == "Calcium" ~ "Cal",
              nutrient == "Vitamin_A" ~ "Vit A",
                                  nutrient == "Omega_3" ~ "Om3",
                                  TRUE ~ nutrient)) %>%
    filter (!nutrient %in% c("Protein", "Selenium"))

  
  
  # plot
  p_stack <- nutr_sector_sau %>%
    ggplot (aes (x = nutrient, y = needs_met/1000000, fill = fishing_sector)) +
    geom_bar (stat = "identity", position = "dodge") +
    theme_bw() +
    #ylim(c(0, 3.25)) +
    labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
    ggtitle (paste0("# children's daily intake needs met,\n", country_name, ", ", year)) +
    scale_fill_manual (values = c("gray70", "dodgerblue3")) +
    facet_wrap (~comm_name, scales = "free_y") +
    theme (
      axis.title = element_text (size = 14),
      axis.text = element_text (size = 10),
     plot.title = element_text (size = 14),
      #legend.position = "none"
    )
  
  png (paste0("Figures/", country_name, "_priority_nutr_sector_stacked.png"), res = 300, width = 8, height = 5, units = "in")
  
  print (p_stack)
  
  dev.off()
  
}

plot_stack_sector_multi ("Chile", n_spp = 5, year = 2015)
plot_stack_sector_multi ("Peru", n_spp = 5, year = 2015)


#####################################################
# indonesia team ----
# focus on skipjack tuna
sau_indo <- sau %>%
  filter (grepl("Indo", area_name), between (year, 2000, 2015)) %>%
  mutate (country = "Indonesia") %>%
  rename (species = scientific_name) %>%
  group_by (country, species, year, fishing_sector, fishing_entity, end_use_type) %>%
  summarise (tonnes = sum(tonnes),
             landed_value = sum(landed_value))

indo_spp_nutr <-  fishnutr_mu %>%
  filter (species %in% sau_indo$species)  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup()

spp_nutr_density <- indo_spp_nutr %>% 
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  select (species, nutrient, perc_rda) %>%
  distinct() %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda)) %>%
  filter (species %in% sau_indo$species)

# skipjack landings
png ("Figures/Indo_skipjack_landings.png", res = 300, width = 4, height = 5, units = "in")  
sau_indo %>%
  filter (species == "Katsuwonus pelamis") %>%
  ggplot (aes (x = year, y = tonnes/1000, fill = species)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual (values = c("dodgerblue3")) +
  labs (x = "", y = "Catch, thousand tons") +
  ggtitle ("Skipjack tuna catch in Indonesia's EEZ \n Sea Around Us project data") +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# foreign vs. domestic
skipjack_country <- sau_indo %>%
  filter (species == "Katsuwonus pelamis") %>%
  mutate (fishing_country = ifelse (fishing_entity == "Indonesia", "Domestic catch", "Foreign catch"))
skipjack_country$fishing_country <- factor (skipjack_country$fishing_country, levels = c("Foreign catch", "Domestic catch"))

png ("Figures/Indo_skipjack_landings_domestic.png", res = 300, width = 5, height = 5, units = "in")  
skipjack_country %>%
  filter (species == "Katsuwonus pelamis") %>%
  ggplot (aes (x = year, y = tonnes/1000, fill = fishing_country)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual (values = c("gray70", "dodgerblue3")) +
  labs (x = "", y = "Catch, thousand tons", fill = "") +
  ggtitle ("Skipjack tuna catch in Indonesia's EEZ \n Sea Around Us project data") +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
  )
dev.off()

# dummy variable for exports
# rbinom not grouping by year. 

set.seed(52)
skipjack_export_dummy1 <- skipjack_country %>%
  filter (fishing_country == "Domestic catch") %>%
  ungroup() %>%
  group_by (year) %>%
  #rowwise() %>%
  mutate (export_dummy = rbinom (n = length(year), size = 1, prob = 0.75)) %>%
  ungroup() %>%
  mutate(
    export = case_when (
      fishing_country == "Domestic catch" & export_dummy == 1 ~ "Exported",
      fishing_country == "Domestic catch" & export_dummy == 0 ~ "Kept",
      fishing_country == "Foreign catch" ~ "Foreign catch"
    )
  )

skipjack_export_dummy1$export <- factor (skipjack_export_dummy1$export, levels = c("Foreign catch", "Exported", "Kept"))


# brute force
skipjack_domestic_yrs <- skipjack_country %>%
  filter (fishing_country == "Domestic catch") %>%
  group_by (year) %>%
  summarise (N = n())

skipjack_domestic_rbinom <- sapply (skipjack_domestic_yrs$N, rbinom, size = 1, prob = 0.75, simplify = TRUE)
skipjac_domestic_rbinom_v <- unlist (skipjack_domestic_rbinom)

skipjack_export <- skipjack_country %>%
  arrange (fishing_country, year)

skipjack_export$export_dummy <- c(rep (1, length(which(skipjack_country$fishing_country == "Foreign catch"))),
                                       skipjac_domestic_rbinom_v)

skipjack_export <- skipjack_export %>%
  mutate(
    export = case_when (
      fishing_country == "Domestic catch" & export_dummy == 1 ~ "Exported",
      fishing_country == "Domestic catch" & export_dummy == 0 ~ "Kept",
      fishing_country == "Foreign catch" ~ "Foreign catch"
    )
    
  )


skipjack_export$export <- factor (skipjack_export$export, levels = c("Foreign catch", "Exported", "Kept"))

# I want to multiply the TONNES by 0.75, not select observations!!
         
png ("Figures/Indo_skipjack_landings_domestic_dummy_exports.png", res = 300, width = 5, height = 5, units = "in") 
t <- skipjack_country %>%
  group_by (year, fishing_country) %>%
  summarise (tot_tonnes = sum (tonnes)) %>%
  pivot_wider (names_from = fishing_country, values_from = tot_tonnes) %>%
  mutate (Exported = 0.75 * `Domestic catch`,
          Kept = 0.25 * `Domestic catch`) %>%
  select (-`Domestic catch`) %>%
  pivot_longer (-year, names_to = "Export", 
                values_to = "tonnes")

t$Export <- factor (t$Export, levels = c("Foreign catch", "Exported", "Kept"))
  
png ("Figures/Indo_skipjack_landings_domestic_dummy_exports.png", res = 300, width = 5, height = 5, units = "in")
  ggplot (t, aes (x = year, y = tonnes/1000, fill = Export)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual (values = c("gray70", "gray40", "dodgerblue3")) +
  labs (x = "", y = "Catch, thousand tons", fill = "") +
  ggtitle ("Skipjack tuna catch in Indonesia's EEZ \n Sea Around Us project data") +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
  )
dev.off()


# skipjack as nutrients, just 2015
png ("Figures/Indo_skipjack_nutr.png", res = 300, width = 4, height = 5, units = "in")
sau_nutr %>%
  filter (country %in% c("Indonesia"), species == "Katsuwonus pelamis", year == 2015) %>%
  group_by (species, nutrient) %>%
  summarise (tot_servings = sum (nutr_servings)) %>%
  left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (needs_met = tot_servings / mean_rda) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  
  ggplot (aes (x = nutrient, y = needs_met/1000000, fill = species)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
  ggtitle ("# children's daily intake needs met, 2015") +
  ylim(c(0, 3.25)) +
  scale_fill_manual (values = c("dodgerblue3")) +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# domestic skipjack as nutrients, just 2015
png ("Figures/Indo_skipjack_nutr_domestic.png", res = 300, width = 4, height = 5, units = "in")
sau_nutr %>%
  filter (country %in% c("Indonesia"), species == "Katsuwonus pelamis", year == 2015, fishing_entity == "Indonesia") %>%
  group_by (nutrient, species) %>%
  summarise (tot_servings = sum (nutr_servings)) %>%
  left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (needs_met = tot_servings / mean_rda) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  
  ggplot (aes (x = nutrient, y = needs_met/1000000, fill = species)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  ylim(c(0, 3.25)) +
  labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
  ggtitle ("# children's daily intake needs met, 2015\n Domestic catch") +
  scale_fill_manual (values = c("dodgerblue3")) +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# fake domestic/kept skipjack as nutrients
png ("Figures/Indo_skipjack_nutr_domestic_fake_kept.png", res = 300, width = 4, height = 5, units = "in")
sau_nutr %>%
  filter (country %in% c("Indonesia"), species == "Katsuwonus pelamis", year == 2015, fishing_entity == "Indonesia") %>%
  group_by (nutrient, species) %>%
  summarise (tot_servings = sum (nutr_servings)) %>%
  left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (needs_met = tot_servings / mean_rda) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  
  ggplot (aes (x = nutrient, y = needs_met/1000000 * 0.75, fill = species)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  ylim(c(0, 3.25)) +
  labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
  ggtitle ("# children's daily intake needs met, 2015\n Domestic catch not exported") +
  scale_fill_manual (values = c("dodgerblue3")) +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# stacked bar
# doing more dumb stuff here, have to re-join with export df
# adding--divide by 100 in here. I think Chris did that in his head but making me doubt everything
calc_nutr_supply_mt <- function(meat_mt, nutr_dens, nutr_dens_units){
  
  # Convert meat to grams
  # "Mg" is metric tons
  meat_g <- measurements::conv_unit(meat_mt, "Mg", "g")
  
  # Calculate amount of nutrient in density units. divide by 100 because density units are per 100g
  nutrient_q <- meat_g *  nutr_dens / 100
  
  # Calculate amount of nutrient in metric tons
  nutrient_mt <- measurements::conv_unit(nutrient_q, nutr_dens_units, "Mg")
  
  # Return
  return(nutrient_mt)
  
}

skipjack_nutr <- sau_nutr %>% 
  filter (species == "Katsuwonus pelamis") %>%
  select (species, nutrient, amount, pedible, dens_units) %>%
  distinct()

skipjack_stacked <- t %>%
  mutate (species = "Katsuwonus pelamis") %>%
  left_join (skipjack_nutr, by = "species") %>%
  mutate (meat_mt = tonnes * pedible,
          
          
          #available nutrient in mt
          #nutr_mt = mapply(calc_nutr_supply_mt, meat_mt, amount, dens_units)
          nutr_mt = pmap (list (meat_mt = meat_mt, nutr_dens = amount, nutr_dens_units = dens_units), calc_nutr_supply_mt),
          
          # edible meat in 100g servings/day
          # meat in metric tons/yr *1000 kg/ton * 1000g/kg * 1 serving /100 g * 1 yr/365 days
          meat_servings = meat_mt * 1000 * 1000 / 100 / 365,
          
          # nutrient content in servings/day
          nutr_servings = meat_servings * amount) %>%


  left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (needs_met = nutr_servings / mean_rda) %>%
  filter (!nutrient %in% c("Protein", "Selenium"))

skipjack_stacked$Export <- factor (skipjack_stacked$Export, levels = c("Foreign catch", "Exported", "Kept"))  

  png ("Figures/Indo_skipjack_nutr_domestic_stacked.png", res = 300, width = 4, height = 5, units = "in")
  skipjack_stacked %>%
    filter (year == 2015) %>%
    ggplot (aes (x = nutrient, y = needs_met/1000000, fill = Export)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  #ylim(c(0, 3.25)) +
  labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
  ggtitle ("# children's daily intake needs met, 2015") +
  scale_fill_manual (values = c("gray70", "gray40", "dodgerblue3")) +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# nutricast time series
indo_kp <- ds_spp %>%
  filter (country == "Indonesia", species == "Katsuwonus pelamis", rcp == "RCP60", scenario %in% c("No Adaptation", "Full Adaptation"))

png ("Figures/Indo_skipjack_nutricast_ts.png", res = 300, width = 4, height = 5, units = "in")
indo_kp %>%
  filter (year > 2025) %>%
  ggplot (aes (x = year, y = catch_mt/1000, col = scenario)) +
  geom_line(lwd = 1.5) +
  theme_bw() +
  labs (x = "", y = "Catch, 1000 tonnes", fill = "Management\n scenario") +
  ggtitle ("Projected skipjack tuna catch\nClimate scenario: RCP 6.0") +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

png ("Figures/Indo_skipjack_nutrient_upside.png", res = 300, width = 4, height = 5, units = "in")
proj_inds_fed_sector %>%
  filter (group == "Child", country =="Indonesia", rcp == "RCP60", scenario %in% c("No Adaptation", "Full Adaptation"), period == "2050-2060", !nutrient %in% c("Protein", "Selenium"), species =="Katsuwonus pelamis") %>%
  # dumb but group by sector and calculate difference
  mutate (tot_fed = inds_fed_industrial + inds_fed_ssf) %>%
  group_by (nutrient) %>%
  summarise (diff_fed = tot_fed[scenario == "Full Adaptation"] - tot_fed[scenario == "No Adaptation"]) %>%
  ggplot (aes (x = nutrient, y = diff_fed/1000, fill = nutrient)) +
  geom_bar (stat = "identity", position = "dodge") +
  scale_fill_manual (values = rep("dodgerblue3", 5)) +
  theme_bw() +
  labs (x = "Nutrient", y = "Additional children fed, thousands", fill = "Management\n scenario") +
  ggtitle ("Additional nutrient needs met by \nimplementing adaptive management,\nSkipjack tuna, 2050-2060, RCP 6.0") +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 12),
   plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()
  

# Chile team ----
nutricast_chile <- ds_spp %>% 
  filter (country == "Chile")

nutricast_chile_baseline <- nutricast_chile %>%
  filter (year %in% c(2012:2020), catch_mt > 0, rcp == "RCP26", scenario == "No Adaptation") %>%
  group_by (species) %>%
  summarise (baseline_catch = mean (catch_mt, na.rm = TRUE))


upside_nonzero <- readRDS("Data/nutricast_catch_upside.Rds")

chl_upside <- upside_nonzero %>%
  filter (country == "Chile", period == "2050-2060", rcp == "RCP60")


chl_spp_key <- read.csv ("Data/Chile_spp_names_key.csv")
# only has fishes; colnames not cooperating
colnames (chl_spp_key)[1] <- "common_name"


# SAU data
sau_chl <- sau %>%
  filter (area_name == "Chile (mainland)", between (year, 2000, 2015)) %>%
  mutate (country = "Chile")














# BILGE ----
  # landings data from sergio, 2021
  chl_landings_artesanal <- read.csv ("Data/2021_030202_desembarque_artesanal_por_region_Chile.csv", skip = 5, header = T) %>%
    select (ESPECIE, Total) %>%
    rename (common_name = ESPECIE) %>%
    left_join (chl_spp_key, by = "common_name") %>%
    as_tibble()
  
  View (chl_landings_artesanal)
  
  chl_landings_data_compare <- chl_landings_artesanal %>%
    filter (!grepl("TOTAL", common_name)) %>%
    arrange (desc (Total)) %>%
    mutate (species = case_when (
      species == "Merluccius gayi" ~ "Merluccius gayi gayi", 
      common_name == "Jibia O Calamar Rojo" ~ "Dosidicus gigas",
      common_name == "Erizo" ~ "Loxechinus albus",
      TRUE ~ species),
      Nutrient_data = ifelse (species %in% fishnutr_mu$species, "Yes", "No"),
      Nutricast_data = ifelse (species %in% nutricast_chile$species, "Yes", "No")) %>%
    left_join (nutricast_chile_baseline, by = "species") %>%
    left_join (chl_upside, by = "species")
  
  write.excel (chl_landings_data_compare)
  
  # radar plots of their desired species nutrient content----
  library (ggradar)
  
  
  # filter nutrient content data
  nutr_radar_plot <- chl_pri_nutr %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    select (species, nutrient, perc_rda) %>%
    pivot_wider (
      names_from = nutrient,
      values_from = perc_rda) %>%
    # radar plot can't deal with NA and non-fish don't have selenium
    replace_na (list (Selenium = 0)) 
  
  # set levels so colors stay the same
  nutr_radar_plot$species <- factor (nutr_radar_plot$species, levels = c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus"))
  
  
  png ("Figures/Chl_radar_reg_tm_spp.png", res = 300, width = 8, height = 5, units = "in")  
  ggradar(nutr_radar_plot,
          grid.min = 0, grid.max = 100, 
          group.point.size = 2,
          group.line.width = 1,
          legend.text.size = 8,
          axis.label.size = 4,
          grid.label.size = 4,
          legend.position = "right") +
    ggtitle ("Child's daily nutrition needs from one serving") +
    theme (plot.title = element_text (size = 14))
  dev.off()
  
  # compare to anchovy and jurel
  
  # filter nutrient content data
  nutr_radar_plot_anchov <- chl_spp_nutr %>%
    filter (species %in% c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"), !nutrient %in% c("Protein", "Selenium")) %>%
    select (species, nutrient, perc_rda) %>%
    pivot_wider (
      names_from = nutrient,
      values_from = perc_rda) %>%
    # radar plot can't deal with NA and non-fish don't have selenium
    replace_na (list (Selenium = 0)) 
  
  # set levels so colors stay the same
  nutr_radar_plot_anchov$species <- factor (nutr_radar_plot_anchov$species, levels = c ("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"))
  
  
  png ("Figures/Chl_radar_reg_tm_spp_anchov_ref.png", res = 300, width = 8, height = 5, units = "in")  
  ggradar(nutr_radar_plot_anchov,
          grid.min = 0, grid.max = 100, 
          group.point.size = 2,
          group.line.width = 1,
          legend.text.size = 8,
          axis.label.size = 4,
          grid.label.size = 4,
          legend.position = "right") +
    ggtitle ("Child's daily nutrition needs from one serving") +
    theme (plot.title = element_text (size = 14))
  dev.off()
  
  # display nutr density
  chl_spp_nutr %>%
    filter (species %in% c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"), !nutrient %in% c("Protein", "Selenium")) %>%
    select (species, nutrient, perc_rda) %>%
    distinct() %>%
    group_by (species) %>%
    summarise (micronutrient_density = sum (perc_rda))
  
  
  # overall chl sau----
  # boxplot of micronutrient density of export, industrial, end use?
  # show avg nutr content of each too?
  sau_chl %>%
  