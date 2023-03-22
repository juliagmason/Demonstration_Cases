# Plot RNIs met of recent landings
# 3/2/23 from regional_team_priority_species_Figs

library (tidyverse)
library (stringr)

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

# SAU landings data ----

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R

# IF using priority species:
# sau_2019 <- readRDS("Data/SAU_2019.Rds") %>%
#   # alter species names for Indonesia. Assume Lutjanus is L. gibbus; Epinephelus is E. coioides
#   mutate (species = case_when (
#     country == "Indonesia" & species == "Lutjanus" ~ "Lutjanus gibbus",
#     country == "Indonesia" & species == "Epinephelus" ~ "Epinephelus coioides",
#     TRUE ~ species
#   ))

# if using aggregate:
sau_2019 <- readRDS("Data/SAU_2019.Rds")

sau_2019_taxa <- readRDS ("Data/sau_2019_taxa.Rds")

######################################
# Chile ----

# country specific landings data ----

#Clean_Chile_Sernapesca_landings.R
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# plot landings volume by taxa?
chl_landings %>%
  filter (year == 2021) %>%
  ggplot (aes (x = reorder(taxa, -catch_mt, na.rm = TRUE), y = catch_mt/1000000, fill = sector)) +
  geom_col () +
  theme_bw() +
  ggtitle ("Official landings, 2021, Chile") +
  labs (x = "", y = "Catch, million metric tonnes", fill = "Fishing sector") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))

# aggregate landings chl ----
chl_landings %>%
  filter (year == 2021) %>%
  group_by (species, taxa) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = taxa)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Child RNIs met from most recent year of landings, Chile") +
  labs (x = "", y = "Child RNIs met, millions") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))

# priority species ----
chl_pri_spp_catch <- chl_landings %>%
  filter (year == 2021) %>%
  group_by (species) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  right_join (filter (priority_spp, country == "Chile")) %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") 

png ("Figures/Chile_pri_spp_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
chl_pri_spp_catch %>%
  mutate(
    spp_short = ifelse (
      grepl(" ", species),
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species) 
  ) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = reorder(spp_short, -catch_mt), y = children_fed/1000000, fill = nutrient)) +
  geom_col(position = "dodge") +
  theme_bw() +
  ggtitle ("Child RNIs met from most recent year of landings, Chile") +
  labs (x = "", y = "Child RNIs met, millions", fill = "Nutrient") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

####################################
# sierra leone ----

# country specific IHH data

#Clean_SLE_IHH_landings.R
sle_landings <- readRDS("Data/SLE_landings_IHH.Rds")

sle_pri_spp_catch <- sle_landings %>%
  filter (year == 2017) %>%
  group_by (species) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  mutate (taxa = "Finfish") %>%
  # convert to children fed
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") 


png ("Figures/SLE_IHH_pri_spp_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  

sle_pri_spp_catch %>%
  mutate(
    spp_short = ifelse (
      grepl(" ", species),
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species) 
  ) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = reorder(spp_short, -catch_mt), y = children_fed/1000000, fill = nutrient)) +
  geom_col(position = "dodge") +
  theme_bw() +
  ggtitle ("Child RNIs met from most recent year of landings, Sierra Leone") +
  labs (x = "", y = "Child RNIs met, millions", fill = "Nutrient") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

# split by sector ind vs artisanal ----
png ("Figures/SLE_IHH_pri_spp_landings_RNIs_met_sector.png", width = 10, height = 5, units = "in", res = 300) 
sle_landings %>%
  filter (year == 2017) %>%
  group_by (species, sector) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  mutate (taxa = "Finfish") %>%
  #right_join (filter (priority_spp, country == "Chile")) %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique")  %>%
  mutate(
    spp_short = ifelse (
      grepl(" ", species),
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species) 
  ) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = reorder(spp_short, -catch_mt), y = children_fed/1000000, fill = nutrient)) +
  geom_col(position = "dodge") +
  theme_bw() +
  ggtitle ("Child RNIs met from most recent year of landings, Sierra Leone") +
  labs (x = "", y = "Child RNIs met, millions", fill = "Nutrient") +
  facet_wrap (~sector) +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

# Malawi values from google sheet ----
mwi_nutr <- read.csv("Data/MWI_spp_nutr.csv") %>%
  mutate (nutrient = gsub (" ", "_", nutrient),
          nutrient = ifelse (nutrient == "Vit_A", "Vitamin_A", nutrient))

mwi_catch <- data.frame (
  species = c ("Oreochromis karongae", "Engraulicyprus sardella"),
  catch_mt = c (3930.67, 156717.13),
  taxa = "Finfish"
) %>%
  left_join(mwi_nutr, by = "species") %>%
  mutate (    # convert tons per year to 100g /day, proportion edible is 0.87 for finfish
    edible_servings = catch_mt * 0.87 * 1000 * 1000 /100 / 365,
    nutrient_servings = edible_servings * amount) %>%
   left_join (rni_child, by = "nutrient") %>%
    mutate (children_fed = nutrient_servings / RNI) %>%
    select (species, catch_mt, nutrient, children_fed)

png ("Figures/MWI_pri_spp_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
mwi_catch %>%
  mutate(
    spp_short =
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2])) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = reorder(spp_short, -catch_mt), y = children_fed/1000000, fill = nutrient)) +
  geom_col(position = "dodge") +
  theme_bw() +
  ggtitle ("Child RNIs met from most recent year of landings, Malawi") +
  labs (x = "", y = "Child RNIs met, millions", fill = "Nutrient") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

# Sau data ----

# plot aggregate landings ----
plot_sau_rnis_met <- function (country_name) {
  
sau_2019 %>%
    filter(country == country_name) %>%
    left_join(sau_2019_taxa, by = "species") %>%
    group_by (species, taxa, commercial_group) %>%
    summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
    unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
   filter (!nutrient %in% c("Protein", "Selenium")) %>%
    
    ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = commercial_group)) +
    geom_col() +
    theme_bw() +
    ggtitle (paste0("Child RNIs met from most recent year of landings, ", country_name)) +
    labs (x = "", y = "Child RNIs met, millions", fill = "Nutrient") +

    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 14),
      plot.title = element_text (size = 18),
      legend.position = "none")
  
}

png ("Figures/Indo_aggregate_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Indonesia")
)
dev.off()

# Peru  ----
png ("Figures/Peru_aggregate_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Peru")
)
dev.off()

png ("Figures/Peru_aggregate_landings_RNIs_met_commgroup.png", width = 6, height = 4, units = "in", res = 300) 
sau_2019 %>%
  filter(country == "Peru") %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (species, taxa, commercial_group) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
  filter (!nutrient %in% c("Protein")) %>%
  
  ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Child RNIs met from 2019 landings, Peru") +
  labs (x = "", y = "Child RNIs met, millions", fill = "Group") +
  
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11, angle = 60, hjust =1),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

# plot RNI provision by sector ----
png ("Figures/Peru_aggregate_landings_RNIs_met_sector.png", width = 5, height = 5, units = "in", res = 300)

sau_2019 %>%
  filter(country == "Peru", fishing_sector %in% c("Artisanal", "Industrial")) %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (species, taxa, fishing_sector) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  
  ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = fishing_sector)) +
  geom_col(position = "dodge") +
  theme_bw() +
  ggtitle ("Child RNIs met from 2019 landings, Peru") +
  labs (x = "", y = "Child RNIs met, millions", fill = "Fishing\nsector") +
  
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

# compare with just dhc
# have to do weird hack situation
# fix peru 2019 anchovy issue
# messy hack, but replace Peru anchovy dhc value with 2018 value. in 2018, all artisanal was dhc and all industrial is fmfo
peru_anchov_dhc <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2018, species == "Engraulis ringens") %>%
  group_by (country, species, year) %>%
  summarise (prop_non_dhc = sum(tonnes[end_use_type == "Fishmeal and fish oil" & fishing_entity == "Peru"])/sum(tonnes[fishing_entity == "Peru"])) # 0.955

peru_anchov_total_2019 <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2019, species == "Engraulis ringens") %>%
  pull (tonnes) %>% sum()

png ("Figures/Peru_aggregate_landings_RNIs_met_sector_DHC.png", width = 5, height = 5, units = "in", res = 300)

sau_2019 %>%
  filter(country == "Peru", fishing_sector %in% c("Artisanal", "Industrial")) %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (species, taxa, fishing_sector, end_use_type) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (catch_mt = case_when (
    country_name == "Peru" & species == "Engraulis ringens" & end_use_type == "Fishmeal and fish oil" ~ peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,
    country_name == "Peru" & species == "Engraulis ringens" & end_use_type == "Direct human consumption" ~ (1 - peru_anchov_dhc$prop_non_dhc) * peru_anchov_total_2019,
    TRUE ~ catch_mt )
  ) %>%
  filter (end_use_type == "Direct human consumption") %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  
  ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = fishing_sector)) +
  geom_col(position = "dodge") +
  theme_bw() +
  ggtitle ("Child RNIs met from 2019 landings, Peru\nDirect human consumption") +
  labs (x = "", y = "Child RNIs met, millions", fill = "Fishing\nsector") +
  
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()


# plot overall catch by comm_group ----
png ("Figures/Peru_SAU_catch_commgroup.png", width = 5, height = 5, units = "in", res = 300)
sau_2019 %>%
  filter(country == "Peru") %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (commercial_group) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
  ggplot (aes (y = tonnes/1000000, x = reorder(commercial_group, -tonnes, na.rm = TRUE), fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Peru aggregate catch for 2019, SAU") +
  labs (x = "", y = "Catch, million metric tonnes", fill = "Group") +
  
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11, angle = 60, hjust = 1),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18),
    legend.position = "none")
dev.off ()



# Sierra Leone  ----
png ("Figures/SL_aggregate_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Sierra Leone")
)
dev.off()

# plot species specific ----

plot_sau_rnis_met_spp <- function (country_name) {
  
 sau_2019 %>%
    filter(country == country_name) %>%
    inner_join (priority_spp, by = c ("country", "species")) %>%
    group_by (species, taxa) %>%
    summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
    unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
    mutate(
      spp_short = ifelse (
        grepl(" ", species),
        paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
        species) 
    ) %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    ggplot (aes (x = reorder(spp_short, -catch_mt), y = children_fed/1000000, fill = nutrient)) +
    geom_col(position = "dodge") +
    theme_bw() +
    ggtitle (paste0("Child RNIs met from most recent year of landings, ", country_name)) +
    labs (x = "", y = "Child RNIs met, millions", fill = "Nutrient") +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 14),
      plot.title = element_text (size = 18))
  
}

# Indo  ----


png ("Figures/Indo_pri_spp_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
print(
plot_sau_rnis_met_spp("Indonesia")
)
dev.off()

# Peru  ----
png ("Figures/Peru_pri_spp_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Peru")
)
dev.off()

# Sierra Leone  ----
png ("Figures/SL_pri_spp_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Sierra Leone")
)
dev.off()

# facet all SAU countries ----

multicountry_nutr_bank_recent_yr <- sau_2019 %>%
  filter (!country %in% c("Chile", "Mexico")) %>%
  group_by (country, species) %>%
  summarise (tonnes = sum (tonnes)) %>%
  right_join (priority_spp, by = c("country", "species")) %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
  
  unnest(cols = c(children_fed)) %>%
  rename (catch_mt = tonnes) %>%
  rbind (chl_pri_spp_catch)

png ("Figures/Facet_pri_spp_RNIs_met.png", width = 12, height = 12, units = "in", res = 300)  
multicountry_nutr_bank_recent_yr %>%
  mutate(
    spp_short = ifelse (
      grepl(" ", species),
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species) 
  ) %>%
  filter (!country == "Mexico", !nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = spp_short, y = children_fed/1000000, fill = nutrient)) +
  geom_col(position = "dodge") +
  facet_wrap (~country, scales = "free", ncol = 1) +
  theme_bw() +
  labs (x = "", y = "Child RNIs met, millions \nLandings, most recent year", fill = "Nutrient") +
  theme ( 
    axis.text.y = element_text (size = 12),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()


# plot industrial vs. artisanal facet, SAU data ----

# dumb peru anchovy situation
sau_2015_2019 <- readRDS("Data/SAU_2015_2019.Rds")

# slight tweak, not just looking at domestic, looking overall (value is 0.949 instead of 0.955)
# only fishmeal and dhc, no "other"
peru_anchov_dhc <- sau_2015_2019 %>%
  filter (country == "Peru",  year == 2018, species == "Engraulis ringens") %>%
  group_by (species) %>%
  summarise (prop_fishmeal = sum(tonnes[end_use_type ==  "Fishmeal and fish oil"])/sum(tonnes))

peru_anchov_total_2019 <- sau_2015_2019 %>%
  filter (country == "Peru", year == 2019, species == "Engraulis ringens") %>%
  pull (tonnes) %>% sum()

# https://stackoverflow.com/questions/24569154/use-variable-names-in-functions-of-dplyr

plot_sau_rnis_met_category <- function (country_name, category, restrict = FALSE) {
  # categories: fishing_sector, end_use_type, fishing_country

  # remove subsistence and recreational if too cluttered
  if (category == "fishing_sector") {
    if (restrict == TRUE) {
      input_landings <- sau_2019 %>%
        filter(country == country_name, !fishing_sector %in% c ("Subsistence", "Recreational")) %>%
        inner_join (priority_spp, by = c ("country", "species")) %>%
        group_by (species, taxa, !!as.symbol(category)) %>%
        summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
        filter (catch_mt > 0)
    }
 else { input_landings <- sau_2019 %>%
      filter (country == country_name) %>%
      inner_join (priority_spp, by = c ("country", "species")) %>%
      group_by (species, taxa, !!as.symbol(category)) %>%
      summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
      filter (catch_mt > 0)
  } 
    }# end fishing_sector if
  
  # for Peru end use, have to use dumb hack to fix weird dhc value
  if (category == "end_use_type") {
    if (country_name == "Peru") {
    input_landings <- sau_2019 %>%
      filter (country == country_name) %>%
      inner_join (priority_spp, by = c ("country", "species")) %>%
      group_by (species, taxa, !!as.symbol(category)) %>%
      summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
      mutate (catch_mt = case_when(
        species == "Engraulis ringens" & end_use_type == "Fishmeal and fish oil" ~ peru_anchov_dhc$prop_fishmeal * peru_anchov_total_2019,
        species == "Engraulis ringens" & end_use_type == "Direct human consumption" ~ (1- peru_anchov_dhc$prop_fishmeal) * peru_anchov_total_2019,
        # there will be a tiny bit of overage bc there's 1926 tons of "other" and 0 in 2018. ignoring for now
        TRUE ~ catch_mt
      )) %>%
      filter (catch_mt > 0)
    } # end peru if 
    else {
      input_landings <- sau_2019 %>%
        filter (country == country_name) %>%
        inner_join (priority_spp, by = c ("country", "species")) %>%
        group_by (species, taxa, !!as.symbol(category)) %>%
        summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
        filter (catch_mt > 0)
    }
  } # end end_use if
  
  if (category == "fishing_country") {
    input_landings <- sau_2019 %>%
      filter (country == country_name) %>%
      inner_join (priority_spp, by = c ("country", "species")) %>%
      mutate (fishing_country = ifelse (fishing_entity == country_name, "Domestic catch", "Foreign catch")) %>%
      group_by (species, taxa, !!as.symbol(category)) %>%
      summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
      filter (catch_mt > 0)
  }
  
   input_landings %>%
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
    unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
    mutate(
      spp_short = ifelse (
        grepl(" ", species),
        paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
        species) 
    ) %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    ggplot (aes (x = reorder(spp_short, -catch_mt), y = children_fed/1000000, fill = nutrient)) +
    geom_col(position = "dodge") +
    facet_wrap (as.formula(paste("~", category))) +
    theme_bw() +
    ggtitle (paste0("Child RNIs met from most recent year of landings, ", country_name)) +
    labs (x = "", y = "Child RNIs met, millions", fill = "Nutrient") +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 14),
      plot.title = element_text (size = 18))
  
}

plot_sau_rnis_met_category(country_name = "Sierra Leone", category = "fishing_sector")
plot_sau_rnis_met_category(country_name = "Peru", category = "end_use_type")
plot_sau_rnis_met_category(country_name = "Sierra Leone", category = "fishing_country")
