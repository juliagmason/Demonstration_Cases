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

# Focal figures ----

# Sierra Leone: foreign and exports ----

# actually not defensible to break up large and small scale. do domestic/foreign and exports
sl_landings_2017_dom <- sl_landings %>%
  filter (year == 2017, !is.na(catch_mt)) %>%
  select (country, species, catch_mt) %>%
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (sector = "Domestic catch",
          Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (species, sector, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "exports",
                values_to = "tonnes") 

#subset just the foreign catch for SL from SAU
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
ihh_ds$exports <- factor(ihh_ds$exports, levels = c ("Exported","Retained",  "Foreign catch"))

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

# report values

# foreign vs. domestic
ihh_ds %>%
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum(rni_equivalents)) %>%
  write.excel()

# exports vs Retained
ihh_ds %>%
  group_by (exports, nutrient) %>%
  summarise (rni_equivalents = sum(rni_equivalents)) %>%
  write.excel()

# large scale vs small scale values
sl_landings %>%
  filter (year == 2017, !is.na(catch_mt)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  # group by small-scale vs. large scale
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  write.excel()
 

# Indonesia: just exports ----

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
            Retained = tonnes * (1-prop_exp), .keep = "unused") %>%
    # pivot longer
    pivot_longer (Exported:Retained, 
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

# report values 
indo_ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == country_name) %>%
  group_by (country, species) %>%
  summarise (tonnes = sum (tonnes)) %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = tonnes * prop_exp,
          Retained = tonnes * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "Exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country_name), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (Exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))


# Peru -- exports, anchovy removed/no anchov ----

# peru without anchovy
peru_noanchov_ex <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru", fleet == "Domestic catch", species != "Engraulis ringens") %>%
  group_by (country, species) %>%
  summarise (tonnes = sum (tonnes)) %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = tonnes * prop_exp,
          Retained = tonnes * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
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
  ggtitle(paste0("Nutrient flows, Peru Domestic catch\n Anchoveta removed")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# peru only anchovy
peru_anchov_ex <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru", fleet == "Domestic catch", species == "Engraulis ringens") %>%
  group_by (country, species) %>%
  summarise (tonnes = sum (tonnes)) %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = tonnes * prop_exp,
          Retained = tonnes * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "Exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (Exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) 

png("Figures/Sankey_Peru_exports_anchov.png", width = 8, height = 6, units = "in", res = 300)
peru_anchov_ex %>% 
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
  ggtitle(paste0("Nutrient flows, Peru Domestic catch\n Anchoveta only")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()



# calculate mean percent of non-anchovy exports to report in results
exports %>%
  filter (country == "Peru", species != "Engraulis ringens") %>%
  summarise (mean_Retained = 1- mean (prop_exp))


# Chile: artisanal vs industrial ----
# can't link exports and sector
chl_ds_sector <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  mutate (country = "Chile") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

# plot just sector
png("Figures/Sankey_Chl_sector.png", width = 8, height = 6, units = "in", res = 300)
chl_ds_sector %>%
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
chl_ds_sector %>% 
  #filter (!nutrient %in% c("Protein", "Selenium")) %>%
  write.excel()



############################################################

# SUPPLEMENTAL FIGURES ----

# Chile ----

# just exports but remove algae, use sernapesca ----
chl_ds <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  mutate (country = "Chile") %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "Exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (Exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))


# plot
png("Figures/Sankey_Chl_exports_sernapesca_noAlgae.png", width = 8, height = 6, units = "in", res = 300)
chl_ds %>%
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

# report values
chl_ds %>% write.excel()

# foreign/industrial/artisanal ----
#subset just the foreign catch from SAU
chl_foreign <- sau_2019 %>%
  ungroup () %>%
  filter (country == "Chile", fleet == "Foreign catch") %>%
  # all foreign catch is industrial catch. but what I'm going to do is name the sector "Foreign catch" so it's in one column. 
  # also have to group to combine end_use_types
  group_by (species) %>%
  #rename to match chl_landings
  summarise (sector = "Foreign catch",
             catch_mt = sum (tonnes))

# rbind and calculate nutrient yield
chl_foreign_ds <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  select (species, sector, catch_mt) %>%
  rbind (chl_foreign) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  # group by nutrient
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

#set levels
chl_foreign_ds$sector <- factor (chl_foreign_ds$sector, levels = c("Foreign catch", "Artisanal", "Industrial"))

png("Figures/Sankey_ChL_foreign_sector.png", width = 8, height = 6, units = "in", res = 300)
chl_foreign_ds %>%
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
  ggtitle("Nutrient flows, Chile\nOfficial country landings and SAU foreign catch, Algae removed") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# report proportional contribution of sectors to nutrients ----
chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>% 
  group_by (sector) %>% summarise (catch = sum(catch_mt, na.rm = TRUE)) %>% write.excel()
# SSF catches 87.9%, lsf catches 12.1% by volume

chl_foreign_ds %>%
  filter (sector != "Foreign catch") %>%
  # group by nutrient, this makes it slightly cleaner
  group_by (nutrient) %>%
  summarise (prop_ssf = sum (rni_equivalents[which (sector == "Artisanal")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100,
             prop_lsf = sum (rni_equivalents[which (sector == "Industrial")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100) %>% write.excel()


# Indonesia ----
indo_ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Indonesia") %>%
  filter (fishing_sector %in% c("Industrial", "Artisanal")) %>%
  # label foreign catch as fishing_sector category
  mutate (fishing_sector = ifelse (fleet == "Foreign catch", "Foreign catch", fishing_sector)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Indonesia"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (fishing_sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

#set levels
indo_ds$fishing_sector <- factor (indo_ds$fishing_sector, levels = c("Foreign catch", "Artisanal", "Industrial"))

# plot
png("Figures/Sankey_Indo_foreign_sector.png", width = 8, height = 6, units = "in", res = 300)
print(indo_ds %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = fishing_sector,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "fishing sector"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Indonesia\nSAU data") +
    theme (axis.text = element_text (size = 14),
           axis.title = element_text (size = 16),
           plot.title = element_text (size = 18),
           legend.position = "none")
)
dev.off()

# report proportional contribution of sectors to nutrients ----
sau_2019 %>%
  filter (country == "Indonesia", fishing_sector %in% c("Industrial", "Artisanal"), fleet == "Domestic catch") %>% 
  group_by (fishing_sector) %>% summarise (catch = sum(tonnes, na.rm = TRUE)) %>% write.excel()
# SSF catches 87.9%, lsf catches 12.1% by volume

indo_ds %>%
  filter (fishing_sector != "Foreign catch") %>%
  # group by nutrient, this makes it slightly cleaner
  group_by (nutrient) %>%
  summarise (prop_ssf = sum (rni_equivalents[which (fishing_sector == "Artisanal")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100,
             prop_lsf = sum (rni_equivalents[which (fishing_sector == "Industrial")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100) %>% write.excel()


# Peru ----

# foreign artisanal industrial ----
peru_ds <- sau_2019 %>%
  # filter to country, remove recreational and subsistence
  filter (country == "Peru") %>%
  filter (fishing_sector %in% c("Industrial", "Artisanal")) %>%
  # label foreign catch as fishing_sector category
  mutate (fishing_sector = ifelse (fleet == "Foreign catch", "Foreign catch", fishing_sector)) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Indonesia"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) 

#set levels
peru_ds$fishing_sector <- factor (peru_ds$fishing_sector, levels = c("Foreign catch", "Artisanal", "Industrial"))

# no anchov

png("Figures/Sankey_Peru_foreign_sector_noanchov.png", width = 8, height = 6, units = "in", res = 300)
print(peru_ds %>%
        filter (species != "Engraulis ringens") %>%
        # group by nutrient, this makes it slightly cleaner
        group_by (fishing_sector, nutrient) %>%
        summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
        filter (!nutrient %in% c("Protein", "Selenium")) %>%
        ggplot (aes (axis1 = nutrient,
                     axis2 = fishing_sector,
                     y = rni_equivalents/1000000)) +
        scale_x_discrete (limits = c ("nutrient", "fishing sector"), expand = c(.2, .05)) +
        labs(y = "RNI equivalents, millions", x = "Allocation levers") +
        geom_flow(aes(fill = nutrient)) +
        geom_stratum(aes(fill = nutrient)) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
        theme_minimal() +
        ggtitle("Nutrient flows, Peru; Anchoveta removed\nSAU data") +
        theme (axis.text = element_text (size = 14),
               axis.title = element_text (size = 16),
               plot.title = element_text (size = 18),
               legend.position = "none")
)
dev.off()

png("Figures/Sankey_Peru_foreign_sector_anchov.png", width = 8, height = 6, units = "in", res = 300)
print(peru_ds %>%
        filter (species == "Engraulis ringens") %>%
        # group by nutrient, this makes it slightly cleaner
        group_by (fishing_sector, nutrient) %>%
        summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
        filter (!nutrient %in% c("Protein", "Selenium")) %>%
        ggplot (aes (axis1 = nutrient,
                     axis2 = fishing_sector,
                     y = rni_equivalents/1000000)) +
        scale_x_discrete (limits = c ("nutrient", "fishing sector"), expand = c(.2, .05)) +
        labs(y = "RNI equivalents, millions", x = "Allocation levers") +
        geom_flow(aes(fill = nutrient)) +
        geom_stratum(aes(fill = nutrient)) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
        theme_minimal() +
        ggtitle("Nutrient flows, Peru; Anchoveta only\nSAU data") +
        theme (axis.text = element_text (size = 14),
               axis.title = element_text (size = 16),
               plot.title = element_text (size = 18),
               legend.position = "none")
)
dev.off()

# report proportional contribution of sectors to nutrients ----
sau_2019 %>%
  filter (country == "Peru", fishing_sector %in% c("Industrial", "Artisanal"), fleet == "Domestic catch", species != "Engraulis ringens") %>% 
  group_by (fishing_sector) %>% summarise (catch = sum(tonnes, na.rm = TRUE)) %>% write.excel()
# SSF catches 87.9%, lsf catches 12.1% by volume

peru_ds %>%
  filter (species != "Engraulis ringens", fishing_sector != "Foreign catch") %>%
  # group by nutrient, this makes it slightly cleaner
  group_by (fishing_sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  group_by (nutrient) %>%
  summarise (prop_ssf = sum (rni_equivalents[which (fishing_sector == "Artisanal")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100,
             prop_lsf = sum (rni_equivalents[which (fishing_sector == "Industrial")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100) %>% write.excel()


# Sierra Leone ----
# exports 

ihh_ds_exports <- sl_landings %>%
  filter (year == 2017) %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "Exports",
                values_to = "tonnes") %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (Exports, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

png("Figures/Sankey_SL_exports_IHH.png", width = 8, height = 6, units = "in", res = 300)
ihh_ds_exports %>%
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

# foreign and large/small sector ----
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

# join exports and foreign, then calculate nutrient yield
ihh_ds_sector <- sl_landings_2017 %>%
  rbind (sl_foreign) %>%
  #calculate nutrient yield
  mutate(rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents)) %>%
  
  # group by nutrient, this makes it slightly cleaner
  group_by (sector, nutrient) %>%
  summarise (rni_equivalents = sum (rni_equivalents, na.rm = TRUE))

# set levels
ihh_ds_sector$sector <- factor(ihh_ds_sector$sector, levels = c ("Foreign catch", "Small-scale", "Large-scale"))


png("Figures/Sankey_SL_foreign_sector.png", width = 8, height = 6, units = "in", res = 300)
ihh_ds_sector %>%
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
  ggtitle("Nutrient flows, Sierra Leone\nIHH data") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()


# report proportional contribution of sectors to nutrients ----
# Abby question: Do SSF or LSF provide any nutrient in particular in disproportionate amounts (compared to volume)? 
sl_sector_prop <- sl_landings %>% summarise (prop_ssf = sum (catch_mt[which (sector == "Small-scale")], na.rm = TRUE)/sum (catch_mt, na.rm = TRUE) * 100,
                                             prop_lsf = sum (catch_mt[which (sector == "Large-scale")], na.rm = TRUE)/sum (catch_mt, na.rm = TRUE) * 100)

sl_landings %>% filter (year == 2017) %>% group_by (sector) %>% summarise (catch = sum(catch_mt, na.rm = TRUE)) %>% write.excel()
# SSF catches 87.9%, lsf catches 12.1% by volume

ihh_ds_sector %>%
  filter (sector != "Foreign catch") %>%
  group_by (nutrient) %>%
  summarise (prop_ssf = sum (rni_equivalents[which (sector == "Small-scale")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100,
             prop_lsf = sum (rni_equivalents[which (sector == "Large-scale")], na.rm = TRUE)/sum (rni_equivalents, na.rm = TRUE) * 100)
  


#######################################################################################################
# Reject figures ----
# Additional Sierra Leone options ----

# just exports SAU ----
slex <- plot_export_sankey("Sierra Leone")
png("Figures/Sankey_SL_exports.png", width = 8, height = 6, units = "in", res = 300)
print (slex)
dev.off()


# foreign vs domestic, broken into large and small scale ----
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
                 Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (species, sector, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
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
ihh_ds$exports <- factor(ihh_ds$exports, levels = c ("Exported","Retained",  "Foreign catch"))

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

# Additional Indo options ----

# discards? by sector ----
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



# Additional Peru options ----

# just exports----
pex <- plot_export_sankey("Peru")
png("Figures/Sankey_Peru_exports.png", width = 8, height = 6, units = "in", res = 300)
print (pex)
dev.off()

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
          Retained = ifelse (sector == "Foreign catch", NA, tonnes * (1-prop_exp)),
          Foreign_catch = ifelse (sector == "Foreign catch", tonnes, NA)
          ) %>%
  # cut unnecessary columns
  select (species, sector, end_use, Exported, Retained, Foreign_catch) %>%
  # pivot longer
  pivot_longer (Exported:Foreign_catch, 
                names_to = "exports",
                values_to = "tonnes") 

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

# end use by sector overall ----
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


# Chile additional options ----

# just exports ----

#SAU
chex <- plot_export_sankey("Chile")
png("Figures/Sankey_Chl_exports.png", width = 8, height = 6, units = "in", res = 300)
print (chex)
dev.off()



# same as sierra leone, industrial/artisanal and then exports ----
chl_ds <- chl_landings %>%
  filter (year == 2021, chl_taxa != "Algae") %>%
  mutate (country = "Chile") %>%
  left_join (exports, by = c("species", "country")) %>%
  replace_na (list(prop_exp = 0)) %>%
  # mutate, calculate exports 
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp), .keep = "unused") %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
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


i3 <- plot_sau_fleet_enduse_sankey("Indonesia")
png("Figures/Sankey_Indo_3axis.png", width = 8, height = 6, units = "in", res = 300)
print (i3)
dev.off()


c3 <- plot_sau_fleet_enduse_sankey("Chile")
png("Figures/Sankey_Chl_3axis.png", width = 8, height = 6, units = "in", res = 300)
print (c3)
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
