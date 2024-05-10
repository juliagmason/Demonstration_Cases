# Plot Fig XB total nutrient yield
# 11 17 23
# JGM

# copying code from Plot_RNIs_met_landings.R

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

# function plot aggregate landings RNIS met ----
plot_sau_rnis_met <- function (country_name, Selenium = FALSE) {
  
  if (country_name == "Chile") {
    
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

# save for illustrator
plot_sau_rnis_met("Indonesia") +
  scale_fill_brewer(palette = "Dark2") +
  guides (fill = "none") +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calcium", "Iron", "Omega 3", "Vit. A", "Zinc" )) +
  ggtitle ("Total nutrient yield") +
  labs (y = "Child RNI equiv., millions") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         plot.title = element_text (size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/FigXB_TNY_Indo.eps", width = 74, height = 60, units = "mm")

# Peru ----
plot_sau_rnis_met("Peru") +
  scale_fill_brewer(palette = "Dark2") +
  guides (fill = "none") +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calcium", "Iron", "Omega 3", "Vit. A", "Zinc" )) +
  ggtitle ("Total nutrient yield") +
  labs (y = "Child RNI equiv., millions") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         plot.title = element_text (size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/FigXB_TNY_Peru.eps", width = 74, height = 60, units = "mm")

# Sierra Leone ----
# library(scales)
# show_col(brewer_pal(palette = "Dark2")(8))
# show_col(brewer_pal(palette = "GnBu")(10))

plot_sau_rnis_met("Sierra Leone")  +
  guides (fill = "none") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570b3", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  scale_x_discrete (labels = c ("Calcium", "Iron", "Omega 3", "Vit. A", "Zinc" )) +
  ggtitle ("Total nutrient yield") +
  labs (y = "Child RNI equiv., millions") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         plot.title = element_text (size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm'))
ggsave ("Figures/FigXB_TNY_SL.eps", width = 74, height = 60, units = "mm")

# Chile ----
plot_sau_rnis_met("Chile") +
  scale_fill_brewer(palette = "Dark2") +
  guides (fill = "none") +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calcium", "Iron", "Omega 3", "Vit. A", "Zinc" )) +
  ggtitle ("Total nutrient yield") +
  labs (y = "Child RNI equiv., millions") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         plot.title = element_text (size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/FigXB_TNY_Chile.eps", width = 74, height = 60, units = "mm")

# supplemental including algae
png ("Figures/FigSX_Chile_TNY_algae.png", width = 7, height = 4, units = "in", res = 300)
chl_landings %>%
  filter (year == 2021) %>%
  # cut to 8 commercial groups
  mutate (commercial_group = case_when (
    commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes", "Salmon, smelts, etc") ~ "Other fishes & inverts",
    TRUE ~ commercial_group
  )) %>%
  group_by (species, commercial_group) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  # just have alphabetical so nutrients are always in the same order
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Commercial group") +
  ggtitle ("Potential nutrient provisioning, 2021 landings") +
  # have to add a color for algae
  scale_fill_manual(values = c("#4EB3D3", "#1B9E77", "#D95F02", "#7570b3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 11),
         legend.title = element_text (size = 12),
         legend.key.size = unit (3.5, "mm"),
         legend.margin=margin(1,1,1,2),
         legend.box.margin=margin(-10,-10,-10,-10),
         plot.title = element_text(size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm')) 

dev.off()
  

# Malawi ----
# plot separately
# ssf has 2018 but industrial most recent is 2017
# Clean_Malawi_landings.R
mal_top <- readRDS("Data/malawi_landings_top.Rds")

mal_nutr <- mal_top %>%
  filter (Year == 2017) %>%
  mutate (rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Malawi"), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 

# keep colors consistent, 
#show_col(brewer_pal(palette = "Dark2")(6))

mal_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = comm_name)) +
  guides (fill = "none") +
  geom_col() +
  scale_fill_manual(values = c("#D95F02", "#7570B3", "#e7298a", "#66A61E", "#E6AB02")) +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calcium", "Iron", "Omega 3", "Vit. A", "Zinc" )) +
  ggtitle ("Total nutrient yield") +
  labs (y = "Child RNI equiv., millions") +
  theme_bw() +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Comm. name") + 
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         plot.title = element_text (size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/FigXB_TNY_Malawi.eps", width = 74, height = 60, units = "mm")
