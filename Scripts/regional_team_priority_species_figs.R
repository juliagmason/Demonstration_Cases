# Regional team priority species
# 5/27/22, updated 7/26/22
library (tidyverse)
#library (stringr)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# country nutrient deficiencies from nutricast ----
# do children to match rda
golden_mic_def <- readRDS("../nutrient_endowment/output/nutr_deficiencies_by_cntry_sex_age_2011.Rds") 

golden_mic_def %>%
  filter (sex == "Children", nutrient %in% c ("Vitamin A", "Calcium", "Iron", "Zinc"), country %in% pri_spp_nutr$country) %>% 
  arrange (country, nutrient) %>%
  write.excel()


# no data for sierra leone, use guinea
golden_mic_def %>%
  filter (sex == "Children", nutrient %in% c ("Vitamin A", "Calcium", "Iron", "Zinc", "Vitamin B6"), country %in% c("Guinea", "Malawi")) %>% 
  arrange (country, nutrient) %>%
  write.excel()


# Priority species ----

# top 5-7 priority species identified by regional teams
# as of 8/4/22  have peru and chile, mexico (limited data avail). took indo spp from willow spreadsheet, but don't know where they came from
priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv")

# 8/10/22 mexico request from sammi lin
sammi_spp <- c("Thunnus thynnus", "Diplodus vulgaris", "Galeorhinus galeus", "Argyrosomus regius", "Dicentrarchus labrax", "Sphoeroides annulatus", "Mugil cephalus", "Scomberomorus sierra", "Sardina pilchardus")

# nutrient data and rda data ----
rda_groups <- readRDS("Data/RDAs_5groups.Rds")
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu"))

# join priority spp to nutrient data
pri_spp_nutr <- fishnutr_mu %>%
  right_join (priority_spp, by = "species")  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>% 
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda),
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup()



# SAU data ----
# also just want top ssf spp for each country
sau <- read.csv ("Data/SAU_EEZ_landings.csv")

sau_mex <- read.csv("Data/SAU_mexico_landings.csv")

# clean by country
sau_country_cleaned <- sau %>%
  rbind (sau_mex) %>%
  mutate (country = case_when(
          grepl("Indo", area_name) ~ "Indonesia",
          grepl ("Mex", area_name) ~ "Mexico",
          area_name == "Chile (mainland)" ~ "Chile",
          TRUE ~ area_name)
          ) %>%
  rename (species = scientific_name) %>%
  group_by (country, species, year, fishing_sector, fishing_entity, end_use_type) %>%
  summarise (tonnes = sum(tonnes),
             landed_value = sum(landed_value))

# percent SSF for priority species 
          
          

# full sau nutr data by sector and end use
sau_nutr <- readRDS("Data/SAU_nutr_content_sector_enduse.Rds")

# translate to inds fed
sau_nutr_inds_fed <- sau_nutr %>%
  group_by (species, country, nutrient) %>%
  summarise (tot_servings = sum (nutr_servings)) %>%
  left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (needs_met = tot_servings / mean_rda, 
          nutrient = case_when (nutrient == "Vitamin_A" ~ "Vit A",
                                nutrient == "Omega_3" ~ "Omega 3",
                                TRUE ~ nutrient)) %>%
  filter (!nutrient %in% c("Protein", "Selenium"))

# calculate upper value for each one?
ylim_sau <- sau_nutr_inds_fed %>%
  right_join (priority_spp, by = c("country", "species")) %>%
  # keep species so I can plot whichever species
  group_by (country, species) %>%
  summarise (max = max (needs_met/1000000))

# climate projection data ----
# smaller, just rcp 60 and 85. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

###########################################################################
## Check data availability ----
# nutrition
pri_spp_nutr %>% filter (is.na(amount)) %>% select (country, comm_name, species) %>% distinct()

# sau
t <- sau_country_cleaned %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (between (year, 2010, 2018)) %>% 
  group_by (country, species, year) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  group_by (country, species) %>%
  summarise (mean_tonnes = mean (sum_tonnes, na.rm = TRUE))


# climate projections--which ones DONT have data
cc <- ds_spp %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (is.na (catch_mt)) %>%
  select (country, species) %>% distinct() %>% arrange (country)




###########################################################################
## Print values----

# nutrition content ----
# remove protein and copy to excel
pri_spp_nutr %>%
  filter (!nutrient == "Protein") %>%
  arrange (country, rank, nutrient) %>%
  write.excel()

# micronutrient density----

print_micronutrient_density <- function (country_name, n_spp) {
  pri_spp_nutr %>%
    filter (country == country_name, rank <= n_spp, 
            !nutrient %in% c("Protein", "Selenium")) %>%
    group_by (species) %>%
    summarise (micronutrient_density = sum (perc_rda)) 
}

print_micronutrient_density ("Chile", 10)
print_micronutrient_density ("Indonesia", 10)
print_micronutrient_density("Sierra Leone", 4)

# not using these anymore

# print nutrient % ----
pri_spp_nutr %>%
  select (country, species, nutrient, perc_rda) %>% 
  filter (!nutrient %in% c("Protein")) %>%
  arrange (country, species, desc(perc_rda))

# print raw values, raw nutrient content ----
pri_spp_nutr_values <- fishnutr_mu %>%
  right_join (priority_spp, by = "species")  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>% 
  select (country, species, nutrient, amount) %>% 
  filter (!nutrient %in% c("Protein")) %>%
  group_by (country, species) %>%
  slice_max (amount, n = 4)


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


# SAU print catch ----
sau_country_catch <- 
  sau_country_cleaned %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (year == 2015) %>%
  group_by (country, species, rank) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  arrange (country, rank) %>%
  write.excel()

# tiny catches for SL...
sau_country_cleaned %>%
  filter (country == "Sierra Leone", species %in% priority_spp$species, between (year, 2000, 2015)) %>%
  ggplot (aes (x = year, y = tonnes, fill = species)) +
  geom_bar(stat = "identity")

# sau industrial vs artisanal----
sau_country_cleaned %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (between (year, 2000, 2015)) %>%
  group_by (country, species, rank, year, fishing_sector) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  group_by (country, species, rank) %>%
  summarise (prop_ssf = sum (sum_tonnes[fishing_sector == "Artisanal"]) / sum (sum_tonnes) * 100,
             prop_ind = sum (sum_tonnes[fishing_sector == "Industrial"]) / sum (sum_tonnes) * 100) %>%
  arrange (country, rank) %>%
  write.excel()

# this seems like it really reduced SSF catch...yes, much more in recent years
sau_country_cleaned %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (between (year, 2000, 2015)) %>%
  filter (country == "Chile") %>%
  ggplot (aes (x = year, y = tonnes, fill = fishing_sector)) +
  geom_bar (stat = "identity")


# sau foreign vs domestic----
# print proportion foreign by country ----
sau_country_cleaned %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (between (year, 2000, 2015)) %>%
  mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
  group_by (country, species, rank, year, fishing_country) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  group_by (country, species, rank) %>%
  summarise (prop_for = sum (sum_tonnes[fishing_country == "Foreign catch"]) / sum (sum_tonnes),
             prop_dom = sum (sum_tonnes[fishing_country == "Domestic catch"]) / sum (sum_tonnes)) %>%
  arrange (country, rank) %>%
  write.excel()



# upside BAU to MEY ----

# nutricast upside ----

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
  filter (period == "2050-2060", rcp == "RCP60") %>%
  right_join (priority_spp, by = c ("species", "country")) %>% View()
            


###################################################################
# Plots----

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
    geom_bar (aes (x = nutrient, y = perc_rda), stat = "identity") +
    facet_wrap (~comm_name) +
    ggtitle (paste0("Daily recommended nutrient intake for children met from 100g serving \n", country_name, " priority species")) +
    theme_bw() +
    labs (x = "", y = "Percent daily needs met") +
    theme (axis.title = element_text (size = 14),
           axis.text = element_text(size =10),
           strip.text = element_text (size = 14),
           plot.title = element_text (size = 14))
  
  png (paste0("Figures/", country_name, "_pri_spp_nutr_bar.png"), res = 300, width = 8, height = 5, units = "in")
  print (p)
  dev.off()
  
  
}

plot_priority_spp_nutr_bar(country_name = "Chile", n_spp = 5)
plot_priority_spp_nutr_bar(country_name = "Peru", n_spp = 5)


# initial plot, landings and needs met of one species ----
# anchoveta

# plot initial landings figure function ----
plot_sau_landings_one_spp <- function (country_name, species_name, year) {
  
  common_name <- filter (priority_spp, country == country_name, species == species_name) %>%
    pull (comm_name)
  
  
  landings <- sau_country_cleaned %>%
    filter (country == country_name, species == species_name, between (year, 2000, 2015)) 
  
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
  
  # plot nutrient needs met
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
  
  png (paste0("Figures/", p_filename, ".png"), res = 300, width = 4, height = 5, units = "in")
  print (nutr_p)
  dev.off()
  
}

plot_sau_landings_one_spp(country_name = "Chile", species_name = "Engraulis ringens", year = 2015)
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
  
  png (paste0("Figures/", country_name, "_", common_name, "_nutricast_ts.png"), res = 300, width = 4, height = 5, units = "in")
  
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
  