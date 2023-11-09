# Plot RNIs met of recent landings
# 3/2/23 from regional_team_priority_species_Figs

library (tidyverse)
library (stringr)

# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")


# country-specific landings data ----

#Clean_Chile_Sernapesca_landings.R
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# Clean_Malawi_landings.R
mal_landings <- readRDS("Data/Malawi_landings_cleaned.Rds")

# Sierra Leone IHH data
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

# SAU landings data ----
sau_2019 <- readRDS("Data/SAU_2019.Rds")

# join to commercial group
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

# cut to 8 commercial groups, get rid of scorpionfishies and flatfishes
sau_2019_taxa_8 <- sau_2019_taxa %>%
  mutate (commercial_group = case_when (
    commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
    TRUE ~ commercial_group
  ))

# note, m. gayi is cod-like


######################################


# function plot aggregate landings RNIS met ----
plot_sau_rnis_met <- function (country_name, Selenium = FALSE) {
  
  if (country_name == "Chile") {
    
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      # cut to 8 commercial groups
      mutate (commercial_group = case_when (
        commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes", "Salmon, smelts, etc") ~ "Other fishes & inverts",
        TRUE ~ commercial_group
      )) %>%
      group_by (species, commercial_group) %>%
      summarise (catch_mt = sum (catch_mt)) %>%
      mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
      unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 
    
    # set levels to deal with algae? maybe don't have to?
    #landings$commercial_group <- factor(landings$commercial_group, levels = c ("Algae", "Cephalopod", "Crustacean", "Finfish", "Mollusc", "Other"))
    
  } else if (country_name == "Sierra Leone") {
    landings <- sl_landings %>%
      filter (year == 2017) %>%
      # cut to 8 commercial groups
      mutate (commercial_group = case_when (
        commercial_group %in% c("Flatfishes", "Scorpionfishes") ~ "Other fishes & inverts",
        TRUE ~ commercial_group
      )) %>%
      group_by (species, commercial_group) %>%
      summarise (catch_mt = sum (catch_mt)) %>%
      mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
      unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 
    
    # standardize color scale, dark1
    landings$commercial_group <- factor (landings$commercial_group, levels = c("Anchovies", "Crustaceans","Herring-likes", "Molluscs", "Other fishes & inverts", "Perch-likes","Sharks & rays", "Tuna & billfishes") )
    
    
  } else {

    
    landings <- sau_2019 %>%
      left_join (sau_2019_taxa_8, by = "species") %>%
      filter(country == country_name) %>%
      group_by (species, commercial_group) %>%
      summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
      mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = country_name), calc_children_fed_func)) %>%
      unnest(cols = c(rni_equivalents),  names_repair = "check_unique")
    
    # standardize color scale, dark1
    landings$commercial_group <- factor (landings$commercial_group, levels = c("Anchovies", "Crustaceans","Herring-likes", "Molluscs", "Other fishes & inverts", "Perch-likes","Sharks & rays", "Tuna & billfishes") )
    
    
  }
  
 
  # not working?? omit_nutrients <- ifelse (Selenium == TRUE, c("Protein", "Selenium"), "Protein")
  if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}
  
  landings %>%
    
    filter (!nutrient %in% omit_nutrients) %>%
   # just have alphabetical so nutrients are always in the same order
    ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
    geom_col() +
    theme_bw() +
    labs (x = "", y = "Child RNI equivalents, millions", fill = "Comm. group") 
  
}


# indonesia ----

i <-  plot_sau_rnis_met("Indonesia")
png ("Figures/Indo_aggregate_landings_RNIs_met.png", width = 4, height = 4, units = "in", res = 300)  
print(
  i +
    scale_fill_brewer(palette = "Dark2") +
    guides (fill = "none") +
    # abbreviate nutrient names
    scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
    ggtitle ("Potential nutrient provisioning\n2019 reconstructed catch") +
    theme (axis.text = element_text (size = 11),
           axis.title = element_text (size = 13),
           plot.title = element_text (size = 16))
)
dev.off()


# Peru  ----
png ("Figures/Peru_aggregate_landings_RNIs_met.png", width = 4, height = 4, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Peru") +
    scale_fill_brewer(palette = "Dark2") +
    ggtitle ("Potential nutrient provisioning\n2019 reconstructed catch") +
    guides (fill = "none") +
    scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
    theme (axis.text = element_text (size = 11),
           axis.title = element_text (size = 13),
           plot.title = element_text (size = 16))
)
dev.off()

# Peru, without anchovy----
# not sure how to facet, just do one without anchovy
peru_no_anchovy <- sau_2019 %>%
  left_join (sau_2019_taxa_8, by = "species") %>%
  filter(country == "Peru") %>%
  group_by (species, commercial_group) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Peru"), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique")

# standardize color scale, dark1
peru_no_anchovy$commercial_group <- factor (peru_no_anchovy$commercial_group, levels = c("Anchovies", "Crustaceans","Herring-likes", "Molluscs", "Other fishes & inverts", "Perch-likes","Sharks & rays", "Tuna & billfishes") )

png ("Figures/Peru_aggregate_landings_RNIs_met_noanchov.png", width = 4, height = 4, units = "in", res = 300)

peru_no_anchovy %>%
  
  filter (species != "Engraulis ringens", !nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Comm. group")  +
  scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
  ggtitle ("Potential nutrient provisioning,\nAnchoveta omitted\n2019 reconstructed catch") +
  guides (fill = "none") +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 13),
         plot.title = element_text (size = 16))

dev.off()

# Sierra Leone  ----

library(scales)
show_col(brewer_pal(palette = "Dark2")(8))
show_col(brewer_pal(palette = "GnBu")(10))

png ("Figures/SL_aggregate_landings_RNIs_met_IHH.png", width = 4, height = 4, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Sierra Leone")  +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570b3", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
    scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
    ggtitle ("Potential nutrient provisioning, 2017 landings") +
    guides (fill = "none") +
    theme (axis.text = element_text (size = 11),
           axis.title = element_text (size = 13),
           plot.title = element_text (size = 16))
)

dev.off()

# SL plot industrial vs. artisanal----
sl_nutr_sector <- sl_landings %>%
  filter (year == 2017) %>%
  mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 

sl_nutr_sector$sector <- factor (sl_nutr_sector$sector, levels = c ("Small-scale", "Large-scale"))



png ("Figures/SL_aggregate_landings_RNIs_met_IHH_sector.png", width = 6, height = 4, units = "in", res = 300)  
sl_nutr_sector %>%
  mutate (commercial_group = ifelse (commercial_group == "Flatfishes", "Other fishes & inverts", commercial_group)) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
  facet_wrap (~sector) +
  
  geom_col() +
  theme_bw() +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570b3", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  ggtitle ("Potential nutrient provisioning, 2017 landings") +
  labs (x = "", y = "Child RNI equivalents, millions") +
  guides (fill = "none") +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 13),
         strip.text = element_text(size = 13),
         plot.title = element_text (size = 16))
  labs (x = "", y = "Child RNI equivalents, millions") 
dev.off()

# Chl  ----

c <- plot_sau_rnis_met("Chile")

# wider to accomodate legend
png ("Figures/Chl_aggregate_landings_RNIs_met.png", width = 5.5, height = 4, units = "in", res = 300)  
print(
  c  +
    ggtitle ("Potential nutrient provisioning, 2021 landings") +
    #guides (fill = "none") +
    # have to add a color for algae
    scale_fill_manual(values = c("#4EB3D3", "#1B9E77", "#D95F02", "#7570b3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
    theme (axis.text = element_text (size = 10),
           axis.title = element_text (size = 10),
           plot.title = element_text(size = 12)) 
)
dev.off()

# remove algae

landings <- chl_landings %>%
  filter (year == 2021, !commercial_group == "Algae") %>%
  # cut to 8 commercial groups
  mutate (commercial_group = case_when (
    commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes", "Salmon, smelts, etc") ~ "Other fishes & inverts",
    TRUE ~ commercial_group
  )) %>%
  group_by (species, commercial_group) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 

png ("Figures/Chl_aggregate_landings_RNIs_met_no_algae.png", width = 4, height = 4, units = "in", res = 300)  
landings %>%
  
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Comm. group") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle ("Potential nutrient provisioning,\nAlgae omitted; 2021 landings") +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
  guides (fill = "none") +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 13),
         plot.title = element_text (size = 16))
dev.off()

#version for ppt
png ("Figures/Chl_aggregate_landings_RNIs_met_no_algae_ppt.png", width = 4, height = 3, units = "in", res = 300)  
landings %>%
  
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  labs (x = "", y = "", fill = "Comm. group") +
  scale_fill_brewer(palette = "Dark2") +
  #ggtitle ("Potential nutrient provisioning, Algae omitted\n2021 landings") +
  guides (fill = "none") +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 13),
         plot.title = element_text (size = 16))
dev.off()


# Malawi----
# plot separately
# ssf has 2018 but industrial most recent is 2017
# Clean_Malawi_landings.R
mal_top <- readRDS("Data/malawi_landings_top.Rds")

mal_nutr <- mal_top %>%
  filter (Year == 2017) %>%
  mutate (rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Malawi"), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 

# keep colors consistent, 
show_col(brewer_pal(palette = "Dark2")(6))

png ("Figures/Mal_aggregate_landings_RNIs_met.png", width = 4, height = 4, units = "in", res = 300)  
mal_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = comm_name)) +
  #ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = commercial_group)) +
  geom_col() +
  scale_fill_manual(values = c("#D95F02", "#7570B3", "#e7298a", "#66A61E", "#E6AB02")) +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calc", "Iron", "Omg3", "VitA", "Zinc" )) +
  theme_bw() +
  ggtitle (paste0("Potential nutrient provisioning,\n2017 landings")) +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Species") +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 13),
         plot.title = element_text (size = 16)) +
  guides (fill = "none")

dev.off()

# facet by sector? industrial is negligible
mal_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = comm_name)) +
  facet_wrap (~sector) +
 
  geom_col() +
  theme_bw() +
  ggtitle (paste0("Child RNI equivalents, Malawi\nMost recent year of landings")) +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Species") 

# results reporting, make table of relative contribution to catch vs. RNIs for each commercial group ----
# i know there is a better way to do this!! ahhhhh!!
mal_tot_tonnes <- mal_top %>% filter (Year == 2017) %>% summarise (tot_tonnes = sum (tonnes))
mal_nutr_rnis <- mal_nutr %>%
  group_by (nutrient) %>% summarise (tot_rnis = sum (rni_equivalents))

mal_nutr %>%
  group_by(comm_name, nutrient) %>%
  summarise (tonnes = sum (tonnes),
             rnis = sum (rni_equivalents)) %>%
  left_join (mal_nutr_rnis, by = "nutrient") %>%
  mutate (prop_catch = tonnes/mal_tot_tonnes$tot_tonnes,
          prop_rnis = rnis / tot_rnis) %>% write.excel()

sl_tot_tonnes <- sl_landings %>% filter (year == 2017) %>% summarise (tot_tonnes = sum (catch_mt, na.rm = TRUE))
sl_nutr_rnis <- landings %>% group_by (nutrient) %>% summarise (tot_rnis = sum (rni_equivalents, na.rm = TRUE))

landings %>%
  group_by (commercial_group, nutrient) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE),
             rnis = sum (rni_equivalents, na.rm = TRUE)) %>%
  left_join (sl_nutr_rnis, by = "nutrient") %>%
  mutate (prop_catch = tonnes/sl_tot_tonnes$tot_tonnes,
          prop_rnis = rnis / tot_rnis) %>% write.excel()

chl_tot_tonnes <- chl_landings %>% filter (year == 2021) %>% summarise (tot_tonnes = sum (catch_mt, na.rm = TRUE))
chl_nutr_rnis <- landings %>% group_by (nutrient) %>% summarise (tot_rnis = sum (rni_equivalents, na.rm = TRUE))
landings %>%
  group_by (commercial_group, nutrient) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE),
             rnis = sum (rni_equivalents, na.rm = TRUE)) %>%
  left_join (chl_nutr_rnis, by = "nutrient") %>%
  mutate (prop_catch = tonnes/chl_tot_tonnes$tot_tonnes,
          prop_rnis = rnis / tot_rnis) %>% write.excel()

chl_tot_tonnes <- chl_landings %>% filter (year == 2021, commercial_group != "Algae") %>% summarise (tot_tonnes = sum (catch_mt, na.rm = TRUE))
chl_nutr_rnis <- landings %>% filter (commercial_group != "Algae") %>% group_by (nutrient) %>% summarise (tot_rnis = sum (rni_equivalents, na.rm = TRUE))
landings %>%
  filter (commercial_group != "Algae") %>%
  group_by (commercial_group, nutrient) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE),
             rnis = sum (rni_equivalents, na.rm = TRUE)) %>%
  left_join (chl_nutr_rnis, by = "nutrient") %>%
  mutate (prop_catch = tonnes/chl_tot_tonnes$tot_tonnes,
          prop_rnis = rnis / tot_rnis) %>% write.excel()

peru_tot_tonnes <- landings %>% ungroup() %>% summarise (tot_tonnes = sum (catch_mt, na.rm = TRUE))
peru_nutr_rnis <- landings %>% group_by (nutrient) %>% summarise (tot_rnis = sum (rni_equivalents, na.rm = TRUE))
landings %>%
  group_by (commercial_group, nutrient) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE),
             rnis = sum (rni_equivalents, na.rm = TRUE)) %>%
  left_join (peru_nutr_rnis, by = "nutrient") %>%
  mutate (prop_catch = tonnes/peru_tot_tonnes$tot_tonnes,
          prop_rnis = rnis / tot_rnis) %>% write.excel()


indo_tot_tonnes <- landings %>% ungroup() %>% summarise (tot_tonnes = sum (catch_mt, na.rm = TRUE))
indo_nutr_rnis <- landings %>% group_by (nutrient) %>% summarise (tot_rnis = sum (rni_equivalents, na.rm = TRUE))
landings %>%
  group_by (commercial_group, nutrient) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE),
             rnis = sum (rni_equivalents, na.rm = TRUE)) %>%
  left_join (indo_nutr_rnis, by = "nutrient") %>%
  mutate (prop_catch = tonnes/indo_tot_tonnes$tot_tonnes,
          prop_rnis = rnis / tot_rnis) %>% write.excel()
