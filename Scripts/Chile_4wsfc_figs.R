# Chile figures for 4WSFC
# 10/11/22
# JGM

library (tidyverse)

# function for converting catch in mt to children fed
source ("Scripts/Function_convert_catch_amt_children_fed.R")

# official landings data last five years
chl_landings <- readRDS("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# SAU data, domestic
sau <- read.csv ("Data/SAU_EEZ_landings.csv")

chl_sau <- sau %>% 
  filter (area_name == "Chile (mainland)", year > 2000) %>%
  mutate (country =  "Chile") %>%
  rename (species = scientific_name) %>%
  group_by (country, species, year, fishing_sector, fishing_entity, end_use_type) %>%
  summarise (tonnes = sum(tonnes),
             landed_value = sum(landed_value))

chl_sau_dom <- chl_sau %>% filter (fishing_entity == "Chile")

chl_sau_dom %>% ggplot (aes (x = year, y = tonnes/1000)) +
  geom_bar (stat = "identity") +
  facet_wrap (~fishing_sector)

# trying to diagnose barplot dodge issue, still happening after r restart
chl_sau_dom %>% ggplot (aes (x = year, y = tonnes/1000, fill = fishing_sector)) +
  geom_col (stat = "identity", position = "dodge") 

# check overlap btw species
unique (chl_landings$species[which (!chl_landings$species %in% chl_sau_dom$species)])

png ("Figures/4WSFC_Chl_landings_sector.png", width = 12, height = 7, units = "in", bg = "transparent", res = 300)
chl_landings %>%
  filter (year > 2016) %>%
  ggplot (aes (x = year, y = catch_mt/1000, fill = sector)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text = element_text (size = 28)) +
  labs (x = "", y = "", fill = "")
dev.off()

# ??? this doesn't reflect the values?? something weird happening with position = "dodge??
chl_landings %>%
  filter (year > 2016) %>%
  group_by (sector, year) %>%
  summarise (tot_cat = sum(catch_mt, na.rm = TRUE)) %>%
  ggplot (aes (x = year, y = tot_cat/1000, col = sector)) +
  geom_point () +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text = element_text (size = 28)) +
  labs (x = "", y = "", fill = "")


png ("Figures/4WSFC_Chl_landings_sector_mean.png", width = 5, height = 5, units = "in", bg = "transparent", res = 300)
chl_landings %>%
  filter (year > 2016) %>%
  group_by (sector, year) %>%
  summarise (tot_cat = sum (catch_mt, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by (sector) %>%
  summarise (mean_catch = mean (tot_cat)) %>%
  ggplot (aes (x = sector, y = mean_catch/1000, fill = sector)) +
  geom_col () +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text = element_text (size = 20)) +
  labs (x = "", y = "", fill = "")
dev.off()

# then show top species ----
top_spp <- chl_landings %>%
  filter (year > 2016) %>%
  group_by (species) %>%
  summarise (tot_cat = sum (catch_mt, na.rm = TRUE)) %>%
  slice_max (tot_cat, n = 5)

chl_landings_top_spp <- chl_landings %>%
  filter (year > 2016) %>%
  mutate (species = ifelse (species %in% top_spp$species, species, "Other")) %>%
  group_by (sector, year, species) %>%
  summarise (tot_cat = sum (catch_mt, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by (sector, species) %>%
  summarise (mean_catch = mean (tot_cat))

chl_landings_top_spp$species <- factor (chl_landings_top_spp$species, levels =c ("Engraulis ringens", "Trachurus murphyi", "Strangomera bentincki", "Scomber japonicus", "Dosidicus gigas", "Other"))


png ("Figures/4WSFC_Chl_landings_sector_mean_spp.png", width = 5, height = 5, units = "in", bg = "transparent", res = 300)
chl_landings_top_spp %>%
  ggplot (aes (x = sector, y = mean_catch/1000, fill = species)) +
  geom_col () +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18),
         axis.text = element_text (size = 20)) +
  labs (x = "", y = "", fill = "")
dev.off()


# show total SSF and industrial as edible yield of nutrients in mt ----
# maybe I can just get at it from children fed metric. 
library (measurements)

chl_landings_recent <- chl_landings %>% 
  filter (year > 2016) %>%
  mutate (taxa = ifelse (species == "Dosidicus gigas", "Cephalopods", "Finfish"))

chl_landings_nutr_input_ls <- list (
  species = chl_landings_recent$species,
  taxa = chl_landings_recent$taxa,
  amount_mt = chl_landings_recent$catch_mt
)

chl_landings_nutr <- pmap_dfr (chl_landings_nutr_input_ls, calc_children_fed_func)

# hmmm...how to get back to artisanal and year? just left join seemed to go okay but definitely suspect

chl_nutr_sector <- chl_landings_nutr %>%
  left_join (chl_landings_recent, by = c ("species", "catch_mt")) %>%
  # convert to total MT. add density units and
  mutate (dens_units = 
            case_when (
              nutrient %in% c("Protein", "Omega_3") ~ "g",
              nutrient %in% c("Vitamin_A", "Selenium") ~ "ug",
              TRUE ~ "mg"
            ),
          nutrient_mt = mapply (conv_unit, nutrient_servings, from = dens_units, to = "Mg"))

png ("Figures/4WSFC_Chl_sector_overall.png", width = 14, height = 7, units = "in", bg = "transparent", res = 300)
chl_nutr_sector %>%
  filter (!nutrient == "Protein") %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Omega 3",
                                nutrient == "Vitamin_A" ~ "Vitamin A", 
                                TRUE ~ nutrient)) %>%
  ggplot (aes (x = year, y = nutrient_mt, fill = sector)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~nutrient, scales = "free_y") +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text.x = element_text (size = 16),
         axis.text.y = element_text (size = 20)) +
  labs (x = "", y = "", fill = "")
dev.off()

# do this as a mean so can get away from the dodge issue
chl_landings_mean <- chl_landings %>%
  group_by (sector, year, species) %>%
  summarise (tot_cat = sum (catch_mt, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by (sector, species) %>%
  summarise (catch_mt = mean (tot_cat)) %>%
  mutate (taxa = ifelse (species == "Dosidicus gigas", "Cephalopods", "Finfish"))

mean_chl_landings_nutr_input_ls <- list (
  species = chl_landings_mean$species,
  taxa = chl_landings_mean$taxa,
  amount_mt = chl_landings_mean$catch_mt
)

chl_landings_nutr_mean <- pmap_dfr (mean_chl_landings_nutr_input_ls, calc_children_fed_func)

# bring back sector and fix nutrient labels
chl_landings_nutr_mean <- chl_landings_nutr_mean %>%
  left_join (chl_landings_mean, by = c ("species", "catch_mt")) %>%
  # convert to total MT. add density units and
  mutate (dens_units = 
            case_when (
              nutrient %in% c("Protein", "Omega_3") ~ "g",
              nutrient %in% c("Vitamin_A", "Selenium") ~ "ug",
              TRUE ~ "mg"
            ),
          nutrient_mt = mapply (conv_unit, nutrient_servings, from = dens_units, to = "Mg"))

# check actual values
chl_landings_nutr_mean %>%
  group_by (sector, nutrient) %>%
  summarise (sum = sum(nutrient_mt, na.rm = TRUE))

png ("Figures/4WSFC_Chl_sector_mean_nutr_overall.png", width = 6, height = 7, units = "in", bg = "transparent", res = 300)
chl_landings_nutr_mean %>%
  filter (!nutrient == "Protein") %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Omega 3",
                                nutrient == "Vitamin_A" ~ "Vitamin A", 
                                TRUE ~ nutrient)) %>%
  ggplot (aes (x = sector, y = nutrient_mt, fill = sector)) +
  geom_col (stat = "identity") +
  facet_wrap (~nutrient, scales = "free_y", ncol= 2) +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text.x = element_text (size = 16),
         axis.text.y = element_text (size = 20)) +
  labs (x = "", y = "", fill = "")
dev.off()

# then plot as children fed ----
png ("Figures/4WSFC_Chl_sector_mean_nutr_overall_children_fed.png", width = 6, height = 7, units = "in", bg = "transparent", res = 300)
chl_landings_nutr_mean %>%
  filter (!nutrient == "Protein") %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Omega 3",
                                nutrient == "Vitamin_A" ~ "Vitamin A", 
                                TRUE ~ nutrient)) %>%
  ggplot (aes (x = sector, y = children_fed/1000000, fill = sector)) +
  geom_col (stat = "identity") +
  facet_wrap (~nutrient, scales = "free_y", ncol= 2) +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text.x = element_text (size = 16),
         axis.text.y = element_text (size = 20)) +
  labs (x = "", y = "", fill = "")
dev.off()


# children fed, end use  ----
# take end proportions for each species from sau
chl_sau_enduse_prop <- chl_sau_dom %>%
  group_by (species, fishing_sector) %>%
  summarise (DHC = sum (tonnes[end_use_type == "Direct human consumption"]/sum(tonnes)),
             Fishmeal = sum (tonnes[end_use_type == "Fishmeal and fish oil"]/sum(tonnes)),
             Discards = sum (tonnes[end_use_type == "Discards"]/sum(tonnes))) %>%
  rename (sector = fishing_sector)

# demonstration figure--same side by side landings with end use
chl_landings_enduse <- chl_landings_mean %>%
  left_join (chl_sau_enduse_prop, by = c("species", "sector")) %>%
  mutate (across (DHC:Discards, ~ .x * catch_mt, 
  )) %>%
  pivot_longer (-c(sector:taxa), names_to = "end_use", values_to = "catch_enduse")

chl_landings_enduse$end_use <- factor (chl_landings_enduse$end_use, levels = c ("Fishmeal", "Discards", "DHC"))

png ("Figures/4WSFC_Chl_landings_sector_mean_enduse.png", width = 6, height = 6, units = "in", bg = "transparent", res = 300)
chl_landings_enduse %>%
  ggplot (aes (x = sector, y = catch_enduse/1000, fill = end_use)) +
  geom_col() +
  theme_bw() +
  theme (legend.position = "none",
                   strip.text = element_text (size = 18), 
                   axis.text = element_text (size = 16)) +
  labs (x = "", y = "", fill = "")
dev.off()

# now just DHC just mean nutrients to show gap ----
# make sector and enduse with children fed
chl_nutr_sector_enduse <- chl_landings_nutr_mean %>%
  filter (!nutrient == "Protein") %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Omega 3",
                                nutrient == "Vitamin_A" ~ "Vitamin A",
                                TRUE ~ nutrient)) %>%
  select (species, children_fed, sector, nutrient) %>%
  left_join (chl_sau_enduse_prop, by = c ("species", "sector")) %>%
  mutate (across (DHC:Discards, ~ .x * children_fed, 
  )) %>%
  pivot_longer (-c(species:nutrient), names_to = "end_use", values_to = "children_fed_enduse")

chl_nutr_sector_enduse$end_use <- factor (chl_nutr_sector_enduse$end_use, levels = c ("Fishmeal", "Discards", "DHC"))

# check the actual values
chl_nutr_sector_enduse %>% 
  group_by (sector, nutrient, end_use) %>%
  summarise (sum_fed = sum (children_fed_enduse, na.rm = TRUE)) %>% View()

png ("Figures/4WSFC_Chl_sector_mean_nutr_DHC.png", width = 6, height = 7, units = "in", bg = "transparent", res = 300)
chl_nutr_sector_enduse %>%
  filter ( end_use == "DHC") %>%
  ggplot (aes (x = sector, y = children_fed_enduse/1000000, fill = sector)) +
  geom_col (stat = "identity") +
  facet_wrap (~nutrient, scales = "free_y", ncol= 2) +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text.x = element_text (size = 16),
         axis.text.y = element_text (size = 20)) +
  labs (x = "", y = "", fill = "")
dev.off()

# zoom in on calcium, figures to illustrate gap

png ("Figures/4WSFC_Chl_sector_mean_nutr_Ca.png", width = 7, height = 9, units = "in", bg = "transparent", res = 300)
chl_nutr_sector_enduse %>%
  filter ( nutrient == "Calcium") %>%
  ggplot (aes (x = sector, y = children_fed_enduse/1000000, fill = sector)) +
  geom_col (stat = "identity") +
  facet_wrap (~nutrient) +
  ylim (c(0, 6)) +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 24), 
         axis.text = element_text (size = 28)) +
  labs (x = "", y = "", fill = "")
dev.off()

png ("Figures/4WSFC_Chl_sector_mean_nutr_DHC_Ca.png", width = 7, height = 9, units = "in", bg = "transparent", res = 300)
chl_nutr_sector_enduse %>%
  filter ( end_use == "DHC", nutrient == "Calcium") %>%
  ggplot (aes (x = sector, y = children_fed_enduse/1000000, fill = sector)) +
  geom_col (stat = "identity") +
  facet_wrap (~nutrient) +
  ylim (c(0, 6))+
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 24), 
         axis.text = element_text (size = 28)) +
  labs (x = "", y = "", fill = "")
dev.off()


##### 
# plot one nutricast example ----
# climate projection data ----
# smaller, just rcp 60 and 85. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

# mauybe n. crockeri
png ("Figures/4WSFC_Chl_nutricast_ts_bacalodillo.png", width = 7, height = 7, units = "in", bg = "transparent", res = 300)
ds_spp %>%
  filter (country == "Chile", species == "Normanichthys crockeri", 
          rcp == "RCP85", year > 2030,  scenario %in% c("No Adaptation", "Full Adaptation"))  %>%
  ggplot (aes (x = year, y = catch_mt/1000, col = scenario)) +
  geom_line(lwd = 1.5) +
  theme_bw() +
  labs (x = "", y = "") +
  ylim (5, 17) + 
  scale_y_continuous (breaks =c(6, 9, 12, 15)) +
  scale_color_manual(values = c ("dodgerblue3", "darkred")) +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 24),
    plot.title = element_text (size = 14),
    legend.title = element_blank(),
    legend.position = "none"
  )
dev.off()

# what is the actual upside?
mote_upside_mt <- ds_spp %>%
  filter (country == "Chile", species == "Normanichthys crockeri", 
          rcp == "RCP85", year > 2030,  scenario %in% c("No Adaptation", "Full Adaptation"))  %>%
  mutate (
    period = case_when (

      year %in% c(2051:2060) ~ "2051-2060"
    )) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, species) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, rcp, period, species) %>%
  summarize (
             adapt_diff_mt = catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"])

# plot nutritional upside as nutrition bargraph
mote_fed <- calc_children_fed_func("Normanichthys crockeri", taxa = "Finfish", amount_mt = mote_upside_mt$adapt_diff_mt)


png ("Figures/4WSFC_Chl_bacalodillo_upside_fed.png", width = 7, height = 5, units = "in", bg = "transparent", res = 300)
mote_fed %>%
  filter (!nutrient == "Protein") %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Om 3",
                                nutrient == "Vitamin_A" ~ "Vit A", 
                                nutrient == "Calcium" ~ "Cal",
                                nutrient == "Selenium" ~ "Sel",
                                TRUE ~ nutrient)) %>% 
ggplot ( aes (x = nutrient, y = children_fed/1000000)) +
  geom_col () +
  theme_bw() +
  labs (x = "", y = "") +
  theme (
    axis.title = element_text (size = 14),
    axis.text = element_text (size = 24),
    plot.title = element_text (size = 14),
    legend.title = element_blank(),
    legend.position = "none"
  )
dev.off()

####################################################3333
### same with Peru, just children fed, end use ssf vs ind----
# SAU data, mean of last five years
# sau taxa key
sau_taxa <- sau %>%
  select (scientific_name, functional_group) %>% distinct() %>%
  rename (species = scientific_name)

peru_sau <- sau %>% 
  filter (area_name == "Peru", year > 2013) %>%
  rename (species = scientific_name, country = area_name) %>%
  group_by (species, fishing_sector, fishing_entity, end_use_type) %>%
  summarise (tonnes = mean(tonnes, na.rm = TRUE))

peru_sau_dom <- peru_sau %>% filter (fishing_entity == "Peru") %>%
  rename (catch_mt = tonnes, 
          sector = fishing_sector)
  

peru_dom_nutr <- 
  peru_sau_dom %>%
  #rejoin to sau
  left_join(sau_taxa, by ="species") %>%
  # conform to nutrient taxa
  mutate (taxa = case_when (
    functional_group %in% c("Shrimps", "Lobsters, crabs") ~ "Crustacean",
    functional_group == "Cephalopods" ~ "Cephalopod",
    # not exact but close enought
    functional_group %in% c ("Other demersal invertebrates") ~ "Mollusc",
    functional_group %in% c("Jellyfish") ~ "NA",
    TRUE ~ "Finfish")
    ) 
peru_nutr_ls <- list (species_name = peru_dom_nutr$species, 
                      taxa = peru_dom_nutr$taxa, 
                      amount_mt =peru_dom_nutr$tonnes)  


peru_sau_nutr_mean <- pmap_dfr (peru_nutr_ls, calc_children_fed_func)
# join back to sector
# bring back sector and fix nutrient labels
peru_sau_nutr_mean <-  peru_sau_nutr_mean %>%
  left_join (peru_sau_dom, by = c ("species", "catch_mt"))

# plot as children fed----
png ("Figures/4WSFC_Peru_sector_mean_nutr_overall_children_fed.png", width = 7, height = 9, units = "in", bg = "transparent", res = 300)
peru_sau_nutr_mean %>%
  filter (!nutrient == "Protein", !sector %in% c("Subsistence", "Recreational")) %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Omega 3",
                                nutrient == "Vitamin_A" ~ "Vitamin A", 
                                TRUE ~ nutrient)) %>%
  ggplot (aes (x = sector, y = children_fed/1000000, fill = sector)) +
  geom_col (stat = "identity") +
  facet_wrap (~nutrient, scales = "free_y", ncol= 2) +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text = element_text (size = 16)) +
  labs (x = "", y = "", fill = "")
dev.off()

# plot DHC children fed ----

# TO DO: figure out faceted y-max
png ("Figures/4WSFC_Peru_sector_mean_nutr_overall_children_fed_DHC.png", width = 7, height = 9, units = "in", bg = "transparent", res = 300)
peru_sau_nutr_mean %>%
  filter (!nutrient == "Protein", !sector %in% c("Subsistence", "Recreational"), 
          end_use_type == "Direct human consumption") %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Omega 3",
                                nutrient == "Vitamin_A" ~ "Vitamin A", 
                                TRUE ~ nutrient)) %>%
  ggplot (aes (x = sector, y = children_fed/1000000, fill = sector)) +
  geom_col (stat = "identity") +
  facet_wrap (~nutrient, scales = "free_y", ncol= 2) +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text = element_text (size = 16)) +
  labs (x = "", y = "", fill = "")
dev.off()


##########################################################333
# dodge plot with years
png ("Figures/4WSFC_Chl_sector_DHC.png", width = 14, height = 7, units = "in", bg = "transparent", res = 300)
chl_nutr_sector %>%
  filter (!nutrient == "Protein") %>%
  mutate (nutrient = case_when (nutrient == "Omega_3" ~ "Omega 3",
                                nutrient == "Vitamin_A" ~ "Vitamin A",
                                TRUE ~ nutrient)) %>%
  left_join (chl_sau_enduse_prop, by = c ("species", "sector")) %>%
  mutate (dhc_amt = DHC * nutrient_mt) %>%
  ggplot (aes (x = year, y = dhc_amt, fill = sector))+
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~nutrient, scales = "free_y") +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text = element_text (size = 16)) +
  labs (x = "", y = "", fill = "")
dev.off()



# this reflects real values
chl_nutr_sector_enduse %>%
  ggplot (aes (x = year, y = children_fed_enduse/1000000, fill = end_use)) +
  geom_bar (stat = "identity") +
  facet_grid (nutrient ~ sector, scales = "free_y") +
  theme_bw()

# it reduces when I use position = "dodge"
chl_nutr_sector_enduse %>%
  filter (end_use == "DHC") %>% 
  ggplot (aes (x = year, y = children_fed_enduse/1000000, fill = nutrient)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (~sector, scales = "free_y") +
  theme_bw()

png ("Figures/4WSFC_Chl_sector_DHC_children.png", width = 14, height = 7, units = "in", bg = "transparent", res = 300)
chl_nutr_sector_enduse %>%
  filter (end_use == "DHC") %>%
  ggplot (aes (x = year, y = children_fed_enduse, fill = sector))+
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~nutrient, scales = "free_y") +
  theme_bw() +
  theme (legend.position = "none",
         strip.text = element_text (size = 18), 
         axis.text = element_text (size = 16)) +
  labs (x = "", y = "", fill = "")
dev.off()

# make dummy graph with grayed out bars?


  
t %>%
ggplot (aes (x = sector, y = children_fed_enduse/1000000, fill = end_use)) +
  geom_bar (stat = "identity") +
  facet_wrap (~nutrient, scales = "free_y")

t %>%
  filter (end_use == "prop_dhc") %>%
  ggplot (aes (x = sector, y = children_fed_enduse/1000000, fill = end_use)) +
  geom_bar (stat = "identity") +
  facet_wrap (~nutrient, scales = "free_y")

# why 6 million, when only 3 in the previous version
y %>% filter (nutrient == "Omega 3") %>% 
  group_by (sector, year) %>%
  summarise (tot_dhc_fed = sum (dhc_fed, na.rm = TRUE))

# FOR NOW, only non-fish is d. gigas. 


chl_sau %>%
  ggplot (aes (x = year, y = tonnes, fill = fishing_entity)) +
  geom_bar (stat = "identity")
