# Plot allocative levers
# 3/2/23 moving from regional team pri spp figs

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

# for sierra leone, also add just "Sardinella
priority_spp <- priority_spp %>%
  rbind (data.frame (
    country = "Sierra Leone",
    comm_name = NA,
    species = "Sardinella",
    rank = NA,
    taxa = "Finfish"
  ))

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds") %>%
  # alter species names for Indonesia. Assume Lutjanus is L. gibbus; Epinephelus is E. coioides
  mutate (species = case_when (
    country == "Indonesia" & species == "Lutjanus" ~ "Lutjanus gibbus",
    country == "Indonesia" & species == "Epinephelus" ~ "Epinephelus coioides",
    TRUE ~ species
  ))

# or mean of most recent 5 years *doesn't currently include indonesia* 
sau_2015_2019 <- readRDS("Data/SAU_2015_2019.Rds")


# fix peru 2019 anchovy issue
# messy hack, but replace Peru anchovy dhc value with 2018 value. in 2018, all artisanal was dhc and all industrial is fmfo
peru_anchov_dhc <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2018, species == "Engraulis ringens") %>%
  group_by (country, species, year) %>%
  summarise (prop_non_dhc = sum(tonnes[end_use_type == "Fishmeal and fish oil" & fishing_entity == "Peru"])/sum(tonnes[fishing_entity == "Peru"])) # 0.955

peru_anchov_total_2019 <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2019, species == "Engraulis ringens") %>%
  pull (tonnes) %>% sum()

# exports ARTIS ----
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

# combine ----

supply_chain_levers <- sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (fishing_country = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")) %>%
  group_by (country, species, taxa) %>%
  summarise (total_domestic_catch = sum (tonnes[fishing_country == "Domestic catch"]),
             foreign_catch = sum (tonnes[fishing_country == "Foreign catch"]),
             domestic_non_dhc = sum (tonnes[end_use_type != "Direct human consumption" & fishing_country == "Domestic catch"])) %>%
  ungroup () %>%
  # mutate hack, fix peru anchovy. multiply total domestic anchov production * proportion non dhc from 2018
  mutate (domestic_non_dhc = ifelse (country == "Peru" & species == "Engraulis ringens", peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,  domestic_non_dhc)) %>%
  # join to exports
  left_join (exports_5yr_mean, by = c ("species", "country")) %>%
  mutate (export_volume = total_domestic_catch * mn_prop_exp, .keep = "unused") %>%
  # pivot longer
  pivot_longer (foreign_catch:export_volume,
                names_to = "lever",
                values_to = "catch_mt") %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = catch_mt), calc_children_fed_func)) %>%
  unnest (cols = c(children_fed)) %>%
  rename (mt = catch_mt)

# plot function ----
plot_supply_chain_levers <- function (country_name) {
  
  p <- supply_chain_levers %>%
    right_join(priority_spp, by = c("country", "species")) %>%
    filter (country == country_name, nutrient == "Calcium")
  
  
  q <- p %>%
    mutate(
      spp_short = ifelse (
        species != "Stolephorus",
        paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
        species)
    ) %>%
  filter (children_fed> 0, nutrient == "Calcium", country == country_name) %>%
  ggplot (aes (y = children_fed/1000000, x = fct_rev(reorder(spp_short, children_fed)), fill = lever)) +
  geom_col (position = "stack") +
  theme_bw() +
  scale_fill_grey() +
  labs (y = "Children's RNIs forgone, millions", x = "", fill = "Policy lever") +
  ggtitle (paste0("Allocative losses, Calcium, ", country_name)) +
  theme ( 
    axis.text.y = element_text (size = 12),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
  
  print (q)
  
}

png ("Figures/Peru_policy_levers_Calcium.png", width = 12, height = 5, unit = "in", res = 300)
plot_supply_chain_levers("Peru")
dev.off()

png ("Figures/Chile_policy_levers_Calcium.png", width = 12, height = 5, unit = "in", res = 300)
plot_supply_chain_levers("Chile")
dev.off()

png ("Figures/Indo_policy_levers_Calcium.png", width = 12, height = 5, unit = "in", res = 300)
plot_supply_chain_levers("Indonesia")
dev.off()

png ("Figures/SL_policy_levers_Calcium.png", width = 12, height = 5, unit = "in", res = 300)
plot_supply_chain_levers("Sierra Leone")
dev.off()


# plot facet stacked policy lever losses ----

png ("Figures/Facet_policy_levers_Calcium.png", width = 12, height = 10, unit = "in", res = 300)  
supply_chain_levers %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  filter (children_fed> 0, nutrient == "Calcium", country != "Mexico") %>%
  ggplot (aes (y = children_fed/1000000, x = fct_rev(reorder(spp_short, children_fed)), fill = lever)) +
  geom_col (position = "stack") +
  facet_wrap( ~ country, scales = "free", ncol = 1) +
  theme_bw() +
  scale_fill_grey() +
  labs (y = "Children's RNIs forgone, millions", x = "", fill = "Policy lever") +
  ggtitle ("Allocative losses, Calcium") +
  theme ( 
    axis.text.y = element_text (size = 12),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 14),
    plot.title = element_text (size = 18))
#legend.position = "none")
dev.off()
