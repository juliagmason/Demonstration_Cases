# Plot sankeys allocative levers
#5 24 23
# JGM

## FIRST HAVE TO FIX sau 2019 and anchovy situation!

#https://corybrunson.github.io/ggalluvial/

library (tidyverse)
library (ggalluvial)


# function for converting catch in mt to children fed ----
source ("Scripts/Function_convert_catch_amt_children_fed.R")


# Start with separate charts for exports and SAU data
sau_2019 <- readRDS ("Data/SAU_2019.Rds")


# 4 axis: SAU artisanal/industrial, foreign/domestic, end use----
plot_full_sau_sankey <- function (country_name) {
  
  ds <- sau_2019 %>%
    # filter to country, remove recreational and subsistence
    filter (country == country_name) %>%
    filter (fishing_sector %in% c("Industrial", "Artisanal")) %>%
    #calculate nutrient yield
    mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country_name), calc_children_fed_func)) %>%
    unnest (cols = c(rni_equivalents)) %>%
    
    # group by nutrient, this makes it slightly cleaner
    group_by (fishing_sector, fleet, end_use_type, nutrient) %>%
    summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))
  
  
  # plot
  p <- ds %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    ggplot (aes (axis1 = nutrient,
                 axis2 = fishing_sector,
                 axis3 = fleet,
                 axis4 = end_use_type,
                 y = rni_equivalents/1000000)) +
    scale_x_discrete (limits = c ("nutrient", "fishing sector", "fleet", "end use type"), expand = c(.2, .05)) +
    labs(y = "RNI equivalents, millions", x = "Allocation levers") +
    geom_flow(aes(fill = nutrient)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    ggtitle(paste0("Nutrient flows, ", country_name)) +
    theme (axis.text = element_text (size = 14),
           axis.title = element_text (size = 16),
           legend.position = "none")
  
  return (p)
  
}

q <- plot_full_sau_sankey("Chile")

p <- plot_full_sau_sankey("Peru")
png("Figures/Sankey_Peru_full_4axis.png", width = 10, height = 8, units = "in", res = 300)
print (p)
dev.off()

# 3 axis: domestic/foreign and end use ----
# not sector, just domestic/foreign and consumption
# issue with indonesia end_use_type

# Adding labels seems too hard, code seems to have changed
# do this in illustrator if ppl decide it's important
#   # https://stackoverflow.com/questions/57745314/how-to-add-value-labels-on-the-flows-item-of-a-alluvial-sankey-plot-on-r-ggallu

# but geom_flow seems to be better than geom_alluvial 
# https://cheatography.com/seleven/cheat-sheets/ggalluvial/

plot_sau_fleet_enduse_sankey <- function (country_name) {
  
  ds <- sau_2019 %>%
    # filter to country, remove recreational and subsistence
    filter (country == country_name) %>%
    group_by (species, fleet, end_use_type) %>%
    summarise (tonnes = sum (tonnes)) %>%
    #calculate nutrient yield
    mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country_name), calc_children_fed_func)) %>%
    unnest (cols = c(rni_equivalents)) %>%
    
    # group by nutrient, this makes it slightly cleaner
    group_by (fleet, end_use_type, nutrient) %>%
    summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))
  
  
  # plot
  

  p <- ds %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    ggplot (aes (axis1 = nutrient,
                 axis2 = fleet,
                 axis3 = end_use_type,
                 y = rni_equivalents/1000000)) +
    scale_x_discrete (limits = c ("nutrient", "fleet", "end use type"), expand = c(.15, .05)) +
    labs(y = "RNI equivalents, millions", x = "Allocation levers") +
    geom_flow(aes(fill = nutrient)) +
    geom_stratum(aes(fill = nutrient)) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
    #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
    theme_minimal() +
    ggtitle(paste0("Nutrient flows, ", country_name)) +
    theme (axis.text = element_text (size = 14),
           axis.title = element_text (size = 16),
           legend.position = "none")
  
  return (p)
  
}

sl3 <- plot_sau_fleet_enduse_sankey("Sierra Leone")
png("Figures/Sankey_SL_3axis.png", width = 8, height = 6, units = "in", res = 300)
print (sl3)
dev.off()

p3 <- plot_sau_fleet_enduse_sankey("Peru")
png("Figures/Sankey_Peru_3axis.png", width = 8, height = 6, units = "in", res = 300)
print (p3)
dev.off()


i3 <- plot_sau_fleet_enduse_sankey("Indonesia")
png("Figures/Sankey_Indo_3axis.png", width = 8, height = 6, units = "in", res = 300)
print (i3)
dev.off()


c3 <- plot_sau_fleet_enduse_sankey("Chile")
png("Figures/Sankey_Chl_3axis.png", width = 8, height = 6, units = "in", res = 300)
print (c3)
dev.off()

######
# bespoke SAU sankeys ----

# Sierra leone ----
#dhc is not a problem. maybe look at artisanal vs. industrial and domestic vs foreign?
# in SAU, ALL domestic catch is artisanal/subsistence and all industrial is foreign. boring
ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Sierra Leone") %>%
  group_by (species, fleet) %>%
  summarise (tonnes = sum (tonnes)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (fleet, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

#ds$fishing_sector <- factor (ds$fishing_sector, levels = c ("Artisanal", "Subsistence", "Industrial"))

p <- ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = fleet,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "fleet"), expand = c(.15, .15)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Sierra Leone")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

png("Figures/Sankey_SL_Dom_label.png", width = 8, height = 6, units = "in", res = 300)
print (p)
dev.off()

 #indonesia ----
# indo has very little foreing fishing, very little fishmeal/fishoil. 
# one area might be discards, one might be artisanal vs. industiral?
ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Indonesia", !end_use_type %in% c("Fishmeal and fish oil", "Other"), !fishing_sector == "Subsistence") %>%
  group_by (species, fishing_sector, end_use_type) %>%
  summarise (tonnes = sum (tonnes)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Indonesia"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (fishing_sector, end_use_type, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

#ds$fishing_sector <- factor (ds$fishing_sector, levels = c ("Artisanal", "Subsistence", "Industrial"))

p <- ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = fishing_sector,
               axis3 = end_use_type,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "fishing_sector", "end_use_type"), expand = c(.15, .15)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Indonesia")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

png("Figures/Sankey_Indo_sector_discards.png", width = 8, height = 6, units = "in", res = 300)
print (p)
dev.off()

# Chile ----
# foreign catch negligible

ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Chile", !end_use_type %in% c("Other")) %>%
  group_by (species, fishing_sector, end_use_type) %>%
  summarise (tonnes = sum (tonnes)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (fishing_sector, end_use_type, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

#ds$fishing_sector <- factor (ds$fishing_sector, levels = c ("Artisanal", "Subsistence", "Industrial"))

p <- ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = fishing_sector,
               axis3 = end_use_type,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "fishing_sector", "end_use_type"), expand = c(.15, .15)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Chile")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

png("Figures/Sankey_Chile_sector_enduse.png", width = 8, height = 6, units = "in", res = 300)
print (p)
dev.off()

##############################################################
# just domestic and exports ----

# exports ARTIS ----
# placehelder for now; need full exports
# emailed 10 19 2022
exports <- read_csv ("Data/20221019_edf_ARTIS_snet.csv")

# additional Indo species emailed 1/28/23
exports_indo <- read_csv("Data/20230125_edf_ARTIS_indo.csv")

exports <- rbind (exports, exports_indo)

# take a five year mean of proportion exported
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



plot_export_sankey <- function (country_name) {
  
  ds <- sau_2019 %>%
    # filter to country, remove recreational and subsistence
    filter (country == country_name, fleet == "Domestic catch") %>%
    group_by (country, species) %>%
    summarise (tonnes = sum (tonnes)) %>%
    left_join (exports_5yr_mean, by = c("species", "country")) %>%
    replace_na (list(mn_prop_exp = 0)) %>%
    # mutate, calculate exports 
    mutate (Exported = tonnes * mn_prop_exp,
            Kept = tonnes * (1-mn_prop_exp), .keep = "unused") %>%
    # pivot longer
    pivot_longer (Exported:Kept, 
                  names_to = "Exports",
                  values_to = "tonnes") %>%
    #calculate nutrient yield
    mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country_name), calc_children_fed_func)) %>%
    unnest (cols = c(rni_equivalents)) %>%
    
    # group by nutrient, this makes it slightly cleaner
    group_by (Exports, nutrient) %>%
    summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))
  
  
  # plot
  p <- ds %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    ggplot (aes (axis1 = nutrient,
                 axis2 = Exports,
                 y = rni_equivalents/1000000,
                 fill = nutrient)) +
    scale_x_discrete (limits = c ("nutrient", "Exports"), expand = c(.2, .05)) +
    labs(y = "RNI equivalents, millions", x = "Allocation levers") +
    geom_alluvium() +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    #geom_text(stat = "stratum", aes(label = after_stat(round(prop, 2)))) +
    theme_minimal() +
    ggtitle(paste0("Nutrient flows, ", country_name, " Domestic catch")) +
    theme (axis.text = element_text (size = 14),
           axis.title = element_text (size = 16),
           legend.position = "none")
  
  return (p)
  
}

pex <- plot_export_sankey("Peru")
png("Figures/Sankey_Peru_exports.png", width = 10, height = 8, units = "in", res = 300)
print (pex)
dev.off()
