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

# Landings data ----

# Chile country specific 
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# chile use landings data
chl_pri_spp_catch <- chl_landings %>%
  filter (year == 2021) %>%
  group_by (species) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  right_join (filter (priority_spp, country == "Chile")) %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") 

# SAU data ----

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds") %>%
  # alter species names for Indonesia. Assume Lutjanus is L. gibbus; Epinephelus is E. coioides
  mutate (species = case_when (
    country == "Indonesia" & species == "Lutjanus" ~ "Lutjanus gibbus",
    country == "Indonesia" & species == "Epinephelus" ~ "Epinephelus coioides",
    TRUE ~ species
  ))


# Chile ----
png ("Figures/Chile_pri_spp_landings_RNIs_met.png", width = 10, height = 5, units = "in", res = 300)  
chl_pri_spp_catch %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
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
    axis.text.y = element_text (size = 12),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
dev.off()

# Sau data ----
plot_sau_rnis_met <- function (country_name) {
  
  sau_2019 %>%
    filter(country == country_name) %>%
    inner_join (priority_spp, by = c ("country", "species")) %>%
    group_by (species, taxa) %>%
    summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
    unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
    mutate(
      spp_short = ifelse (
        species != "Stolephorus",
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
      axis.text.y = element_text (size = 12),
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
plot_sau_rnis_met("Indonesia")
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

# facet all countries ----

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
      species != "Stolephorus",
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
