# Plot sankeys allocative levers
#5 24 23
# JGM


#https://corybrunson.github.io/ggalluvial/

library (tidyverse)
library (ggalluvial)

# function for writing values to excel (For paper reporting)
# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# function for converting catch in mt to children fed ----
source ("Scripts/Function_convert_catch_amt_children_fed.R")


#  SAU data
sau_2019 <- readRDS ("Data/SAU_2019.Rds")

# country landings data
#Clean_Chile_Sernapesca_landings.R
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# Sierra Leone IHH data
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

# exports ARTIS ----
# full data emailed 6/23/2023
# not taking five year mean, this is just 2019 data. so just need to fix countries and species names
# export percent production is in percentage points, convert
exports <- read_csv ("Data/20230622_edf_ARTIS_full_spp.csv") %>%
  mutate (species = str_to_sentence(sciname),
          prop_exp = export_percent_production/100,
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone"
          ), .keep = "unused") %>%
  # remove aquaculture and inland
  filter (habitat == "marine", method == "capture")

# Sierra Leone ----

# actually not defensible to break up large and small scale. do domestic/foreign and exports
sl_landings_2017_dom <- sl_landings %>%
  filter (year == 2017, !is.na(catch_mt)) %>%
  select (country, species, catch_mt) %>%
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (sector = "Domestic catch",
          Exported = catch_mt * prop_exp,
          Kept = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (species, sector, Exported, Kept) %>%
  # pivot longer
  pivot_longer (Exported:Kept, 
                names_to = "exports",
                values_to = "tonnes") 

#subset just the foreign catch for SL
sl_foreign <- sau_2019 %>%
  ungroup () %>%
  filter (country == "Sierra Leone", fleet == "Foreign catch") %>%
  # all foreign catch is industrial catch. but what I'm going to do is name the sector "Foreign catch" so it's in one column. 
  # also have to group to combine end_use_types
  group_by (species) %>%
  #rename to match sl_landings--make sector column that says foreign, and exports column that ALSO says foreign??
  summarise (sector = "Foreign catch",
             exports = NA,
             tonnes = sum (tonnes))
  
# join and calculate nutrient yield

ihh_ds <- sl_landings_2017_dom %>%
  rbind (sl_foreign) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (sector, exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

# I'm not sure why this works, but this makes the foreign catch export flow disappear!
ihh_ds$sector <- factor (ihh_ds$sector, levels = c ("Foreign catch", "Domestic catch"))
ihh_ds$exports <- factor(ihh_ds$exports, levels = c ("Exported","Kept",  "Foreign catch"))

png("Figures/Sankey_SL_foreign_exports.png", width = 8, height = 6, units = "in", res = 300)
ihh_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Sierra Leone") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# report values?
ihh_ds %>%
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum(rni_equivalents)) %>%
  write.excel()

ihh_ds %>%
  group_by (exports, nutrient) %>%
  summarise (rni_equivalents = sum(rni_equivalents)) %>%
  write.excel()


############################################################
# try to do large scale and small scale as domestic, add SAU foreign. Then have domestic go into exports.
sl_landings_2017 <- sl_landings %>%
  filter (year == 2017, !is.na(catch_mt)) %>%
  select (country, species, sector, catch_mt) 

# make sep ds for exports
sl_exports <- sl_landings_2017 %>%
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = catch_mt * prop_exp,
                 Kept = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (species, sector, Exported, Kept) %>%
  # pivot longer
  pivot_longer (Exported:Kept, 
              names_to = "exports",
              values_to = "tonnes") 

#subset just the foreign catch for SL
sl_foreign <- sau_2019 %>%
  ungroup () %>%
  filter (country == "Sierra Leone", fleet == "Foreign catch") %>%
  # all foreign catch is industrial catch. but what I'm going to do is name the sector "Foreign catch" so it's in one column. 
  # also have to group to combine end_use_types
  group_by (species) %>%
  #rename to match sl_landings--make sector column that says foreign, and exports column that ALSO says foreign??
  summarise (sector = "Foreign catch",
             exports = NA,
             tonnes = sum (tonnes))



# join exports and foreign, then calculate nutrient yield
ihh_ds <- sl_exports %>%
  rbind (sl_foreign) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (sector, exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

# set levels
# ihh_ds$sector <- factor(ihh_ds$sector, levels = c ("Small-scale", "Large-scale", "Foreign catch"))

# I'm not sure why this works, but this makes the foreign catch export flow disappear!
ihh_ds$exports <- factor(ihh_ds$exports, levels = c ("Exported","Kept",  "Foreign catch"))

png("Figures/Sankey_SL_sector_exports.png", width = 8, height = 6, units = "in", res = 300)
ihh_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Sierra Leone") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# report large scale vs small scale values
ihh_ds %>%
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents)) %>%
  write.excel()

# Indonesia--just exports ----

plot_export_sankey <- function (country_name) {
  
  ds <- sau_2019 %>%
    # filter to country, remove recreational and subsistence
    filter (country == country_name, fleet == "Domestic catch") %>%
    group_by (country, species) %>%
    summarise (tonnes = sum (tonnes)) %>%
    left_join (exports, by = c("species", "country")) %>%
    replace_na (list(prop_exp = 0)) %>%
    # mutate, calculate exports 
    mutate (Exported = tonnes * prop_exp,
            Kept = tonnes * (1-prop_exp), .keep = "unused") %>%
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
           plot.title = element_text (size = 18),
           legend.position = "none")
  
  return (p)
  
}


iex <- plot_export_sankey("Indonesia")
png("Figures/Sankey_Indo_exports.png", width = 8, height = 6, units = "in", res = 300)
print (iex)
dev.off()

# Peru ----
# 3 axis domestic foreign/dhc vs fishmeal, with domestic broken into industrial/artisanal. 
# do for just anchovy and then everything but anchovy. 

peru_sau <- sau_2019 %>% 

  filter (country == "Peru", !end_use_type == "Other") %>%
  # remove rec/subsistence 
  # separate into foreign/artisanal/industrial
  mutate (fishing_sector =
            case_when (fishing_sector %in% c("Subsistence","Recreational") ~ "Artisanal",
                       fleet == "Foreign catch" ~ "Foreign catch",
                       TRUE ~ fishing_sector)
          ) %>%
  group_by (country, species, fishing_sector, end_use_type) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
  rename (sector = fishing_sector,
          end_use = end_use_type)

  

# join to exports -- if using
peru_exports <- peru_sau %>%
  # just domestic
  #filter (fishing_sector != "Foreign catch") %>%
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = ifelse (sector == "Foreign catch", NA, tonnes * prop_exp),
          Kept = ifelse (sector == "Foreign catch", NA, tonnes * (1-prop_exp)),
          Foreign_catch = ifelse (sector == "Foreign catch", tonnes, NA)
          ) %>%
  # cut unnecessary columns
  select (species, sector, end_use, Exported, Kept, Foreign_catch) %>%
  # pivot longer
  pivot_longer (Exported:Foreign_catch, 
                names_to = "exports",
                values_to = "tonnes") 

# calculate mean percent of non-anchovy exports to report in results
exports %>%
  filter (country == "Peru", species != "Engraulis ringens") %>%
  summarise (mean_kept = 1- mean (prop_exp))

# calculate nutrient content
peru_nutr_ds <- peru_exports %>%
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) 
  
# don't group by nutrient yet so can remove anchovy
  
peru_nutr_ds$sector <-factor (peru_nutr_ds$sector, levels = c("Foreign catch", "Artisanal", "Industrial")) 
peru_nutr_ds$end_use <-factor (peru_nutr_ds$end_use, levels = c("Direct human consumption", "Fishmeal and fish oil"))

# end use sankey without anchovy----
png("Figures/Sankey_Peru_sector_end_use_NOanchov.png", width = 8, height = 6, units = "in", res = 300)
peru_nutr_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          species != "Engraulis ringens") %>%
  
  # group by nutrient to clean
  group_by (sector, end_use, exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = end_use,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "end_use"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Peru; Anchovy removed") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# end use sankey with just anchovy----
png("Figures/Sankey_Peru_sector_enduse_anchov.png", width = 8, height = 6, units = "in", res = 300)
peru_nutr_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          species == "Engraulis ringens") %>%
  
  # group by nutrient to clean
  group_by (sector, end_use, exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = end_use,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "end_use"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Peru; Anchovy only") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

dev.off()


# exports sankey, no anchovy ----
peru_nutr_ds$exports <- factor(peru_nutr_ds$exports, levels = c ("Exported","Kept",  "Foreign catch"))

png("Figures/Sankey_Peru_sector_exports_NOanchov.png", width = 8, height = 6, units = "in", res = 300)
peru_nutr_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          species != "Engraulis ringens") %>%
  
  # group by nutrient to clean
  group_by (sector, end_use, exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Peru; Anchovy removed") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# exports just anchovy ----
png("Figures/Sankey_Peru_sector_exports_anchov.png", width = 8, height = 6, units = "in", res = 300)
peru_nutr_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          species == "Engraulis ringens") %>%
  
  # group by nutrient to clean
  group_by (sector, end_use, exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Peru; Anchovy removed") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# exports only no anchov ----
png("Figures/Sankey_Peru_exports_NOanchov.png", width = 8, height = 6, units = "in", res = 300)
peru_nutr_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          species != "Engraulis ringens", 
          !sector == "Foreign catch") %>%
  
  # group by nutrient to clean
  group_by (exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Peru; Domestic catch, Anchovy removed") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# exports, just anchov ----
png("Figures/Sankey_Peru_exports_anchov.png", width = 8, height = 6, units = "in", res = 300)
peru_nutr_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          species == "Engraulis ringens", 
          !sector == "Foreign catch") %>%
  
  # group by nutrient to clean
  group_by (exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Peru; Domestic catch, Anchovy only") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# Chile ----
# same as sierra leone, industrial/artisanal and then exports??
chl_ds <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  mutate (country = "Chile") %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = catch_mt * prop_exp,
          Kept = catch_mt * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Kept, 
                names_to = "exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (sector, exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

png("Figures/Sankey_Chl_sector_exports.png", width = 8, height = 6, units = "in", res = 300)
chl_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Chile") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

dev.off()



# just sector ----
png("Figures/Sankey_Chl_sector.png", width = 8, height = 6, units = "in", res = 300)
chl_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
              
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Chile") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

dev.off()

# report values
chl_sector_nutr <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))



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

plot_full_sau_sankey("Indonesia")
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

# peru3 without anchovy
ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru", species != "Engraulis ringens") %>%
  group_by (species, fleet, end_use_type) %>%
  summarise (tonnes = sum (tonnes)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (fleet, end_use_type, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

png("Figures/Sankey_Peru_3axis_noanchov.png", width = 8, height = 6, units = "in", res = 300)
ds %>%
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
  ggtitle(paste0("Nutrient flows, Peru\nAnchovy removed")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
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
# maybe foreign fishing shows up with selenium?
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
  filter (!nutrient %in% c("Protein")) %>%
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

# peru ----

ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru", !end_use_type %in% c("Other"), fishing_sector %in% c("Artisanal", "Industrial")) %>%
  group_by (species, fishing_sector, end_use_type) %>%
  summarise (tonnes = sum (tonnes)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func)) %>%
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
  ggtitle(paste0("Nutrient flows, Peru")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

png("Figures/Sankey_Peru_sector_enduse.png", width = 8, height = 6, units = "in", res = 300)
print (p)
dev.off()


# Chile ----
# foreign catch negligible, look at end use by sector (use SAU data)

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
# full data emailed 6/23/2023
# not taking five year mean, this is just 2019 data. so just need to fix countries and species names
# export percent production is in percentage points, convert
exports <- read_csv ("Data/20230622_edf_ARTIS_full_spp.csv") %>%
  mutate (species = str_to_sentence(sciname),
          prop_exp = export_percent_production/100,
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone"
          ), .keep = "unused") %>%
  # remove aquaculture and inland
  filter (habitat == "marine", method == "capture")

plot_export_sankey <- function (country_name) {
  
  ds <- sau_2019 %>%
    # filter to country, remove recreational and subsistence
    filter (country == country_name, fleet == "Domestic catch") %>%
    group_by (country, species) %>%
    summarise (tonnes = sum (tonnes)) %>%
    left_join (exports, by = c("species", "country")) %>%
    replace_na (list(prop_exp = 0)) %>%
    # mutate, calculate exports 
    mutate (Exported = tonnes * prop_exp,
            Kept = tonnes * (1-prop_exp), .keep = "unused") %>%
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
           plot.title = element_text (size = 18),
           legend.position = "none")
  
  return (p)
  
}


pex <- plot_export_sankey("Peru")
png("Figures/Sankey_Peru_exports.png", width = 8, height = 6, units = "in", res = 300)
print (pex)
dev.off()

# plot peru without anchovy
peru_noanchov_ex <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru", fleet == "Domestic catch", species != "Engraulis ringens") %>%
  group_by (country, species) %>%
  summarise (tonnes = sum (tonnes)) %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = tonnes * prop_exp,
          Kept = tonnes * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Kept, 
                names_to = "Exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (Exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) 

png("Figures/Sankey_Peru_exports_noanchov.png", width = 8, height = 6, units = "in", res = 300)
peru_noanchov_ex %>% 
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
  ggtitle(paste0("Nutrient flows, Peru Domestic catch\n Anchovy removed")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

iex <- plot_export_sankey("Indonesia")
png("Figures/Sankey_Indo_exports.png", width = 8, height = 6, units = "in", res = 300)
print (iex)
dev.off()

chex <- plot_export_sankey("Chile")
png("Figures/Sankey_Chl_exports.png", width = 8, height = 6, units = "in", res = 300)
print (chex)
dev.off()

# artis with country landings data----
# for chile might as well use landings data?
#Clean_Chile_Sernapesca_landings.R
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

ds <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  mutate (country = "Chile") %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = catch_mt * prop_exp,
          Kept = catch_mt * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Kept, 
                names_to = "Exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (Exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

# report values
ds %>% write.excel()
# plot
png("Figures/Sankey_Chl_exports_sernapesca.png", width = 8, height = 6, units = "in", res = 300)
ds %>%
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
  ggtitle(paste0("Nutrient flows, Chile Domestic catch\nOfficial country landings, Algae removed")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()


# sierra leone IHH
slex <- plot_export_sankey("Sierra Leone")
png("Figures/Sankey_SL_exports.png", width = 8, height = 6, units = "in", res = 300)
print (slex)
dev.off()


# with landings data
# Sierra Leone IHH data
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

ihh_ds_temp <- sl_landings %>%
filter (year == 2017) %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = catch_mt * prop_exp,
          Kept = catch_mt * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Kept, 
                names_to = "Exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (Exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

png("Figures/Sankey_SL_exports_IHH.png", width = 8, height = 6, units = "in", res = 300)
ihh_ds_temp %>%
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
  ggtitle(paste0("Nutrient flows, Sierra Leone Domestic catch\nIHH data")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

