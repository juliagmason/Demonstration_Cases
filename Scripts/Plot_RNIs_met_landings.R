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

# join to commercial group
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")


# Priority species ----

# # top 5-7 priority species identified by regional teams
# # as of 8/4/22  have peru and chile, mexico (limited data avail). took indo spp from willow spreadsheet, but don't know where they came from
# priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv") %>%
#   # just change S. japonicus peruanus to S. japonicus; no nutrient or SAU or nutricast data
#   mutate (species = case_when (species == "Scomber japonicus peruanus" ~ "Scomber japonicus",
#                                TRUE ~ species)
#   )
# 


######################################


# plot aggregate landings ----
plot_sau_rnis_met <- function (country_name, Selenium = FALSE) {
  
  if (country_name == "Chile") {
    
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      rename (commercial_group = taxa) %>%
      group_by (species, commercial_group) %>%
      summarise (catch_mt = sum (catch_mt)) %>%
      mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
      unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 
    
    #landings$commercial_group <- factor(landings$commercial_group, levels = c ("Algae", "Cephalopod", "Crustacean", "Finfish", "Mollusc", "Other"))
    
  } else {
    
    landings <- sau_2019 %>%
      left_join (sau_2019_taxa, by = "species") %>%
      filter(country == country_name) %>%
      group_by (species, commercial_group) %>%
      summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
      mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = country_name), calc_children_fed_func)) %>%
      unnest(cols = c(rni_equivalents),  names_repair = "check_unique")
    
  }
  
  # not working?? omit_nutrients <- ifelse (Selenium == TRUE, c("Protein", "Selenium"), "Protein")
  if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}
  
  landings %>%
    
    filter (!nutrient %in% omit_nutrients) %>%
    # #set algae to zero?
    # mutate (rni_equivalents = ifelse (commercial_group == "Algae", 0, rni_equivalents)) %>%
    
    # just have alphabetical so nutrients are always in the same order
    ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
    #ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = commercial_group)) +
    geom_col() +
    theme_bw() +
    ggtitle (paste0("Child RNI equivalents, ", country_name, "\nMost recent year of landings")) +
    labs (x = "", y = "Child RNI equivalents, millions", fill = "Comm. group") 
  
}

i <-  plot_sau_rnis_met("Indonesia")
png ("Figures/Indo_aggregate_landings_RNIs_met.png", width = 5, height = 4, units = "in", res = 300)  
print(
  i +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 10),
      legend.title = element_text (size = 12),
      plot.title = element_text (size = 18),
      legend.position = "none"
      #legend.position = c(0.8, 0.6))
    ))
dev.off()

# what are the perch-likes contributing so much in indo? R. brachysoma, decapturus, Selaroides leptolepis
sau_2019 %>%
  filter (country == "Indonesia") %>%
  left_join(sau_2019_taxa, by = "species") %>%
  arrange (desc (tonnes)) %>%
  View()

# Peru  ----
png ("Figures/Peru_aggregate_landings_RNIs_met.png", width = 5, height = 4, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Peru") +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 10),
      legend.title = element_text (size = 12),
      plot.title = element_text (size = 18),
      legend.position = "none"
    )
)
dev.off()


# Sierra Leone  ----
png ("Figures/SL_aggregate_landings_RNIs_met.png", width = 5, height = 4, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Sierra Leone")  +
    theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 10),
    legend.title = element_text (size = 12),
    plot.title = element_text (size = 18),
    legend.position = "none"
  )
)
dev.off()



# Chl  ----
#png ("Figures/Chl_aggregate_landings_RNIs_met_no_algae.png", width = 5, height = 4, units = "in", res = 300)  
png ("Figures/Chl_aggregate_landings_RNIs_met.png", width = 5, height = 4, units = "in", res = 300)  
print(
  plot_sau_rnis_met("Chile")  +
    theme (
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 10),
      legend.title = element_text (size = 12),
      plot.title = element_text (size = 18),
      legend.position = "none"
    )
)
dev.off()

# plot overall catch by comm_group ----
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

png ("Figures/Peru_SAU_catch_commgroup.png", width = 5, height = 5, units = "in", res = 300)
sau_2019 %>%
  filter(country == "Peru") %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (commercial_group) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
  ggplot (aes (y = tonnes/1000000, x = reorder(commercial_group, -tonnes, na.rm = TRUE), fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Peru aggregate catch, 2019, SAU") +
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

png ("Figures/Indo_SAU_catch_commgroup.png", width = 5, height = 5, units = "in", res = 300)
sau_2019 %>%
  filter(country == "Indonesia") %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (commercial_group) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
  ggplot (aes (y = tonnes/1000000, x = reorder(commercial_group, -tonnes, na.rm = TRUE), fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Indonesia aggregate catch, 2019, SAU") +
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

png ("Figures/SL_SAU_catch_commgroup.png", width = 5, height = 5, units = "in", res = 300)
sau_2019 %>%
  filter(country == "Sierra Leone") %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (commercial_group) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
  ggplot (aes (y = tonnes/1000000, x = reorder(commercial_group, -tonnes, na.rm = TRUE), fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  ggtitle ("Sierra Leone aggregate catch, 2019, SAU") +
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


# also plot time series for dicastery presentation
sau_2015_2019 <- readRDS("Data/SAU_2015_2019.Rds")

sl <- sau_2015_2019 %>%
  filter (country == "Sierra Leone") %>%
  left_join (sau_2019_taxa, by = "species") %>%
  # high-level groups
  mutate (group = case_when (
    commercial_group %in% c("Anchovies", "Herring-likes") ~ "Anchovies and sardines",
    commercial_group == "Tuna & billfishes" ~ "Tunas",
    commercial_group == "Crustaceans" ~ "Crustaceans",
    TRUE ~ "Other"
  ))

sl$group <- factor (sl$group, levels = c ("Anchovies and sardines", "Tunas", "Crustaceans", "Other"))

png ("F")
sl %>%
  ggplot (aes (x = year, y = tonnes/1000, fill = group)) +
    geom_col() +
    theme_bw() + 
  labs (x = "", fill = "", y = "Catch, 1000 tonnes") +
  theme (axis.text = element_text (size = 18),
         axis.title = element_text (size = 24),
         legend.text = element_text (size = 18)
         )
  



png ("Figures/Chl_aggregate_catch_taxa.png", width = 5, height = 4, units = "in", res = 300)
chl_landings %>%
  filter (year == 2021) %>%
  group_by (taxa) %>%
  summarise (catch_mt = sum (catch_mt, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(taxa, -catch_mt, na.rm = TRUE), y = catch_mt/1000000, fill = taxa)) +
  geom_col () +
  theme_bw() +
  ggtitle ("Official landings, 2021, Chile") +
  labs (x = "", y = "Catch, million metric tonnes", fill = "Taxa") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18),
    legend.position = "none")
dev.off()


# plot RNI provision by sector ----


plot_sau_rnis_met_sector <- function (country_name, Selenium = FALSE) {
    
    landings_s <- sau_2019 %>%
      filter(country == country_name, fishing_entity == country_name, fishing_sector %in% c("Artisanal", "Industrial")) %>%
      group_by (species, fishing_sector, end_use_type) %>%
      summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
      #honestly...for this one, maybe just replace "dhc" with "fishmeal" for industrial.
      mutate (end_use_type = case_when (
        country_name == "Peru" & species == "Engraulis ringens" & fishing_sector == "Industrial" & end_use_type == "Direct human consumption" ~ "Fishmeal and fish oil",
        TRUE ~ end_use_type),
        children_fed = pmap (list (species = species, amount = catch_mt, country_name = "Peru"), calc_children_fed_func)
      ) %>%
      unnest(cols = c(children_fed),  names_repair = "check_unique")
    
  
  # not working?? omit_nutrients <- ifelse (Selenium == TRUE, c("Protein", "Selenium"), "Protein")
  if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}
  

  landings_s %>%  filter (!nutrient %in% omit_nutrients) %>%
  
  ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = fishing_sector)) +
  geom_col(position = "dodge") +
  facet_wrap (~end_use_type, scales = "free_y", ncol = 1) +
  theme_bw() +
  ggtitle (paste0("Child RNIs met from 2019 landings, ", country_name)) +
  labs (x = "", y = "Child RNIs met, millions", fill = "Fishing\nsector")
  
}
 
  
png ("Figures/Peru_aggregate_landings_RNIs_met_sector.png", width = 6, height = 5, units = "in", res = 300) 
plot_sau_rnis_met_sector("Peru") +  
theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

png ("Figures/Indo_aggregate_landings_RNIs_met_sector.png", width = 6, height = 5, units = "in", res = 300) 
plot_sau_rnis_met_sector("Indonesia") +  
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

png ("Figures/SL_aggregate_landings_RNIs_met_sector.png", width = 6, height = 5, units = "in", res = 300) 
plot_sau_rnis_met_sector("Sierra Leone") +  
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()



# chile by sector ----
sau_2015_2019 <- readRDS("Data/SAU_2015_2019.Rds")

chl_sau_end_use_ratios <- 
  sau_2015_2019 %>%
  filter (country == "Chile") %>%
  group_by (fishing_sector, species, year) %>%
  summarise (prop_dhc = sum(tonnes[end_use_type == "Direct human consumption"])/sum (tonnes),
             prop_fmfo = sum(tonnes[end_use_type == "Fishmeal and fish oil"])/sum (tonnes),
             prop_other = sum(tonnes[end_use_type == "Other"])/sum (tonnes)
  ) %>%
  ungroup() %>%
  group_by (fishing_sector, species) %>%
  summarise (across(prop_dhc:prop_other, mean)) %>%
  rename (sector = fishing_sector)

x <- chl_landings %>%
  filter (year == 2021) %>%
  left_join (chl_sau_end_use_ratios, by = c ("species", "sector")) %>%
  pivot_longer (prop_dhc:prop_other,
                names_prefix = "prop_",
                names_to = "end_use_type",
                values_to = "prop") %>%
  mutate (catch_end_use = catch_mt * prop) %>%
  mutate (children_fed = pmap (list (species = species, amount = catch_end_use, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(children_fed)) %>%
  rename (mt = catch_end_use)

#x$end_use_type <- factor (x$end_use_type, levels = c ("dhc","fmfo", "other", ))

# Labels for facet wrap
end_use_labs <-  c ("Fishmeal and fish oil", "Other", "Direct human consumption")
names(end_use_labs) <- c ("fmfo", "other", "dhc")



png ("Figures/Chile_aggregate_landings_RNIs_met_sector.png", width = 6, height = 5, units = "in", res = 300)

x %>%  filter (!nutrient %in% c("Selenium", "Protein")) %>%
  
  ggplot (aes (x = reorder(nutrient, -children_fed, na.rm = TRUE), y = children_fed/1000000, fill = sector)) +
  geom_col(position = "dodge") +
  facet_wrap (~end_use_type, scales = "free_y", ncol = 1, 
              labeller = labeller(end_use_type = end_use_labs)) +
  theme_bw() +
  ggtitle ("Child RNIs met from 2019 landings, Chile") +
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



##############################################################
# plot species specific, priority species ----

plot_sau_rnis_met_spp <- function (country_name) {
  
  sau_2019 %>%
    filter(country == country_name) %>%
    inner_join (priority_spp, by = c ("country", "species")) %>%
    group_by (species, taxa) %>%
    summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
    mutate (children_fed = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
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



# chl priority species ----
chl_pri_spp_catch <- chl_landings %>%
  filter (year == 2021) %>%
  group_by (species) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  right_join (filter (priority_spp, country == "Chile")) %>%
  mutate (children_fed = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
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
# sierra leone IHH ----

# country specific IHH data

#Clean_SLE_IHH_landings.R
sle_landings <- readRDS("Data/SLE_landings_IHH.Rds")

sle_pri_spp_catch <- sle_landings %>%
  filter (year == 2017) %>%
  group_by (species) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  mutate (taxa = "Finfish") %>%
  # convert to children fed
  mutate (children_fed = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
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
  mutate (children_fed = pmap (list (species = species, amount = catch_mt, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
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





# facet all SAU countries ----

multicountry_nutr_bank_recent_yr <- sau_2019 %>%
  filter (!country %in% c("Chile", "Mexico")) %>%
  group_by (country, species) %>%
  summarise (tonnes = sum (tonnes)) %>%
  right_join (priority_spp, by = c("country", "species")) %>%
  mutate (children_fed = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func)) %>%
  
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
    mutate (children_fed = pmap (list (species = species, amount = catch_mt, country = country_name), calc_children_fed_func)) %>%
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
