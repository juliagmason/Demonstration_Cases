# Plot sankeys allocative levers
#5 24 23
# JGM


#https://corybrunson.github.io/ggalluvial/

library (tidyverse)
library (ggalluvial)



# special focal figures ----

# Sierra Leone: foreign and exports ----
# ohh, actually do need to combine for this one....try to work from saved nutr sets
sl_export_nutr <- readRDS("Data/levers_RNI_pop_export_SierraLeone.Rds")
sl_foreign_sector_nutr <- readRDS("Data/levers_RNI_pop_foreign_sector_SierraLeone.Rds")

# add a column to each dataframe and rbind
sl_export_nutr_comb <- sl_export_nutr %>%
  mutate (sector = "Domestic catch") %>%
  select (country, sector, exports, nutrient, rni_equivalents, perc_demand_met)

sl_foreign_export_nutr_comb <- sl_foreign_sector_nutr %>%
  # just take foreign catch
  filter (sector == "Foreign catch") %>%
  # make exports column
  mutate (exports = NA) %>%
  group_by (country, sector, exports, nutrient) %>% #summarize (rni_equiv = sum (rni_equivalents))
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # rbind to exports--this represents all domestic
  rbind (sl_export_nutr_comb)

# set levels
# I'm not sure why this works, but this makes the foreign catch export flow disappear!
sl_foreign_export_nutr_comb$sector <- factor (sl_foreign_export_nutr_comb$sector, levels = c ("Foreign catch", "Domestic catch"))
sl_foreign_export_nutr_comb$exports <- factor(sl_foreign_export_nutr_comb$exports, levels = c ("Exported","Retained",  "Foreign catch"))

png("Figures/Sankey_SL_foreign_exports.png", width = 8, height = 6, units = "in", res = 300)
sl_foreign_export_nutr_comb %>%
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

# chile just artisanal vs. industrial
chl_foreign_sector_nutr <- readRDS("Data/levers_RNI_pop_foreign_sector_Chile.Rds")

png("Figures/Sankey_Chl_sector.png", width = 8, height = 6, units = "in", res = 300)
chl_foreign_sector_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium"), sector != "Foreign catch") %>%
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


####################################################################################
# Standard sankeys: foreign/artisanal/industrial and exports for each country


# Indonesia ----
indo_foreign_sector_nutr <- readRDS("Data/levers_RNI_pop_foreign_sector_Indo.Rds") %>%
  # group subsistence into artisanal
  mutate (sector = ifelse (sector =="Subsistence", "Artisanal", sector))

#set levels
indo_foreign_sector_nutr$sector <- factor (indo_foreign_sector_nutr$sector, levels = c("Foreign catch", "Artisanal", "Industrial"))

# plot
png("Figures/Sankey_Indo_foreign_sector.png", width = 8, height = 6, units = "in", res = 300)
print(indo_foreign_sector_nutr %>%
        filter (!nutrient %in% c("Protein", "Selenium")) %>%
        ggplot (aes (axis1 = nutrient,
                     axis2 = sector,
                     y = rni_equivalents/1000000)) +
        scale_x_discrete (limits = c ("nutrient", "sector"), expand = c(.2, .05)) +
        labs(y = "RNI equivalents, millions", x = "Allocation levers") +
        geom_flow(aes(fill = nutrient)) +
        geom_stratum(aes(fill = nutrient)) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
        theme_minimal() +
        ggtitle("Nutrient flows, Indonesia\nSAU data") +
        theme (axis.text = element_text (size = 14),
               axis.title = element_text (size = 16),
               plot.title = element_text (size = 18),
               legend.position = "none")
)
dev.off()

# exports
indo_export_nutr <- readRDS("Data/levers_RNI_pop_export_Indo.Rds")

# plot
png("Figures/Sankey_Indo_exports.png", width = 8, height = 6, units = "in", res = 300)
indo_export_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000,
               fill = nutrient)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_alluvium() +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text(stat = "stratum", aes(label = after_stat(round(prop, 2)))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Indonesia Domestic catch\nSAU data")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()


# Peru no anchovy ----

# foreign artisanal industrial
peru_foreign_sector_nutr_noanchov <- readRDS("Data/levers_RNI_pop_foreign_sector_Peru_noanchov.Rds") %>%
  # group subsistence into artisanal
  mutate (sector = ifelse (sector %in% c("Subsistence", "Recreational"), "Artisanal", sector))

#set levels
peru_foreign_sector_nutr_noanchov$sector <- factor (peru_foreign_sector_nutr_noanchov$sector, levels = c("Foreign catch", "Artisanal", "Industrial"))

png("Figures/Sankey_Peru_foreign_sector_noanchov.png", width = 8, height = 6, units = "in", res = 300)
print(peru_foreign_sector_nutr_noanchov %>%
        filter (!nutrient %in% c("Protein", "Selenium")) %>%
        ggplot (aes (axis1 = nutrient,
                     axis2 = sector,
                     y = rni_equivalents/1000000)) +
        scale_x_discrete (limits = c ("nutrient", "sector"), expand = c(.2, .05)) +
        labs(y = "RNI equivalents, millions", x = "Allocation levers") +
        geom_flow(aes(fill = nutrient)) +
        geom_stratum(aes(fill = nutrient)) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
        theme_minimal() +
        ggtitle("Nutrient flows, Peru; Anchoveta removed\nSAU data") +
        theme (axis.text = element_text (size = 14),
               axis.title = element_text (size = 16),
               plot.title = element_text (size = 18),
               legend.position = "none")
)
dev.off()

peru_export_nutr_noanchov <- readRDS("Data/levers_RNI_pop_export_Peru_noanchov.Rds")

png("Figures/Sankey_Peru_exports_noanchov.png", width = 8, height = 6, units = "in", res = 300)
peru_export_nutr_noanchov  %>% 
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000,
               fill = nutrient)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_alluvium() +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text(stat = "stratum", aes(label = after_stat(round(prop, 2)))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Peru Domestic catch\n Anchoveta removed")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# peru only anchovy ----

# foreign artisanal industrial
peru_foreign_sector_nutr_anchov <- readRDS("Data/levers_RNI_pop_foreign_sector_Peru_anchov.Rds")

#set levels
peru_foreign_sector_nutr_anchov$sector <- factor (peru_foreign_sector_nutr_anchov$sector, levels = c("Foreign catch", "Artisanal", "Industrial"))

png("Figures/Sankey_Peru_foreign_sector_anchov.png", width = 8, height = 6, units = "in", res = 300)
print(peru_foreign_sector_nutr_anchov %>%
        filter (!nutrient %in% c("Protein", "Selenium")) %>%
        ggplot (aes (axis1 = nutrient,
                     axis2 = sector,
                     y = rni_equivalents/1000000)) +
        scale_x_discrete (limits = c ("nutrient", "sector"), expand = c(.2, .05)) +
        labs(y = "RNI equivalents, millions", x = "Allocation levers") +
        geom_flow(aes(fill = nutrient)) +
        geom_stratum(aes(fill = nutrient)) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
        theme_minimal() +
        ggtitle("Nutrient flows, Peru; Anchoveta only\nSAU data") +
        theme (axis.text = element_text (size = 14),
               axis.title = element_text (size = 16),
               plot.title = element_text (size = 18),
               legend.position = "none")
)
dev.off()

# exports
peru_export_nutr_anchov <- readRDS("Data/levers_RNI_pop_export_Peru_anchov.Rds")

png("Figures/Sankey_Peru_exports_anchov.png", width = 8, height = 6, units = "in", res = 300)
peru_export_nutr_anchov %>% 
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000,
               fill = nutrient)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_alluvium() +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text(stat = "stratum", aes(label = after_stat(round(prop, 2)))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Peru Domestic catch\nAnchoveta only")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()


# Chile ----

# exports, remove algae,
chl_export_nutr <- readRDS("Data/levers_RNI_pop_export_Chile.Rds")

# plot
png("Figures/Sankey_Chl_exports_sernapesca_noAlgae.png", width = 8, height = 6, units = "in", res = 300)
chl_export_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000,
               fill = nutrient)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_alluvium() +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text(stat = "stratum", aes(label = after_stat(round(prop, 2)))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Chile Domestic catch\nOfficial country landings, Algae removed")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()


# foreign/industrial/artisanal

chl_foreign_sector_nutr <- readRDS("Data/levers_RNI_pop_foreign_sector_Chile.Rds")

#set levels
chl_foreign_sector_nutr$sector <- factor (chl_foreign_sector_nutr$sector, levels = c("Foreign catch", "Artisanal", "Industrial"))

png("Figures/Sankey_ChL_foreign_sector.png", width = 8, height = 6, units = "in", res = 300)
chl_foreign_sector_nutr %>%
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


# Sierra Leone ----
# exports 

sl_export_nutr <- readRDS("Data/levers_RNI_pop_export_SierraLeone.Rds")

png("Figures/Sankey_SL_exports_IHH.png", width = 8, height = 6, units = "in", res = 300)
sl_export_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000,
               fill = nutrient)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_alluvium() +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text(stat = "stratum", aes(label = after_stat(round(prop, 2)))) +
  theme_minimal() +
  ggtitle(paste0("Nutrient flows, Sierra Leone Domestic catch\nIHH data")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

# foreign and large/small sector 
sl_foreign_sector <- readRDS("Data/levers_RNI_pop_foreign_sector_SierraLeone.Rds")

# set levels
sl_foreign_sector$sector <- factor(sl_foreign_sector$sector, levels = c ("Foreign catch", "Small-scale", "Large-scale"))

png("Figures/Sankey_SL_foreign_sector.png", width = 8, height = 6, units = "in", res = 300)
sl_foreign_sector %>%
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



###########################################################################################
# Reject figures


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
