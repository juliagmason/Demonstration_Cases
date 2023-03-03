## Plot nutricast relative upsides
# 3/2/23 from regional_team_priority_spp_figs

library (tidyverse)
library (stringr)

# catch upside relative, from calculate_nutritional_upsides.r
# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")
# having trouble with this not reading, unknown input format??


# top 5-7 priority species identified by regional teams
# as of 8/4/22  have peru and chile, mexico (limited data avail). took indo spp from willow spreadsheet, but don't know where they came from
priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv") %>%
  # just change S. japonicus peruanus to S. japonicus; no nutrient or SAU or nutricast data
  mutate (species = case_when (species == "Scomber japonicus peruanus" ~ "Scomber japonicus",
                               TRUE ~ species)
  )


# landings data ----
# Chile country specific 
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# SAU data 

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds") %>%
  # alter species names for Indonesia. Assume Lutjanus is L. gibbus; Epinephelus is E. coioides
  mutate (species = case_when (
    country == "Indonesia" & species == "Lutjanus" ~ "Lutjanus gibbus",
    country == "Indonesia" & species == "Epinephelus" ~ "Epinephelus coioides",
    TRUE ~ species
  ))

# special ds for indo spp to match nutricast
indo_spp_ds <- c(priority_spp$species[which(priority_spp$country == "Indonesia")], "Epinephelus tauvina", "Engraulis japonicus", "Encrasicholina punctifer")


# current catch tons and match species. Just find overall tons for priority species and then append those values to the nutricast species
sau_tonnes_indo <- sau_2019 %>%
  filter(country == "Indonesia", 
         species %in% priority_spp$species[which(priority_spp$country == "Indonesia")]) %>%
  group_by (species) %>%
  summarise (total_tonnes = sum (tonnes))

# merge Epinephelus species,and use Stolephorus as the other anchovy spp
sau_match_nutricast <- data.frame (
  species = c("Epinephelus tauvina", "Engraulis japonicus", "Encrasicholina punctifer"), 
  total_tonnes = c(sau_tonnes_indo$total_tonnes[which(sau_tonnes_indo$species == "Epinephelus coioides")],
                   sau_tonnes_indo$total_tonnes[which(sau_tonnes_indo$species == "Stolephorus")],
                   sau_tonnes_indo$total_tonnes[which(sau_tonnes_indo$species == "Stolephorus")]
  )
) 

sau_tonnes_indo <- sau_tonnes_indo %>%
  rbind (sau_match_nutricast)

# join and plot----

# doing by country anyway, don't need to compile

# Indonesia ----
upside_ratios_indo <- catch_upside_relative %>%
  filter (country == "Indonesia", species %in% indo_spp_ds) %>%
  #left_join (calcium_indo, by = "species") %>%
  left_join (sau_tonnes_indo, by = "species") %>% 
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%

  select (country, rcp, species, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") 

upside_ratios_indo$upside <- factor(upside_ratios_indo$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))


png ("Figures/Indo_nutricast_upside_overall.png", width = 10, height = 8, units= "in", res = 300)

upside_ratios_indo %>%
  filter (rcp %in% c("RCP26", "RCP85")) %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  ggplot (aes (x = upside, y = tonnes, fill = rcp)) +
  geom_col (position = "dodge") +
  facet_wrap (~spp_short, scales = "free_y", ncol = 3) +
  geom_hline (yintercept = 0, lty = 2) +
  theme_bw() +
  labs (y = "Production upside, tonnes", x = "", fill = "RCP") +
  ggtitle ("Production upside from climate-adaptive management, Indonesia") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 14),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 
dev.off()

# Chile ----
upside_ratios_chl <- chl_landings %>%
  filter (year == 2021) %>%
  group_by (species) %>%
  summarise (total_tonnes = sum (catch_mt)) %>%
  mutate (country = "Chile") %>%
  inner_join(priority_spp, by = c ("country", "species")) %>%
  left_join(catch_upside_relative, by = c ("country", "species")) %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (country, rcp, species, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") 

upside_ratios_chl$upside <- factor(upside_ratios_chl$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))

png ("Figures/Chile_nutricast_upside_overall.png", width = 10, height = 8, units= "in", res = 300)

upside_ratios_chl %>%
  filter (rcp %in% c("RCP26", "RCP85")) %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  ggplot (aes (x = upside, y = tonnes, fill = rcp)) +
  geom_col (position = "dodge") +
  facet_wrap (~spp_short, scales = "free_y", ncol = 3) +
  geom_hline (yintercept = 0, lty = 2) +
  theme_bw() +
  labs (y = "Production upside, tonnes", x = "", fill = "RCP") +
  ggtitle ("Production upside from climate-adaptive management, Chile") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 14),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 
dev.off()

# Peru ----

upside_ratios_peru <- sau_2019 %>%
  filter(country == "Peru", 
         species %in% priority_spp$species[which(priority_spp$country == "Peru")]) %>%
  group_by (country, species) %>%
  summarise (total_tonnes = sum (tonnes)) %>%
  ungroup() %>%
  left_join (catch_upside_relative, by = c("country", "species")) %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (country, rcp, species, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") 

upside_ratios_peru$upside <- factor(upside_ratios_peru$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))

png ("Figures/Peru_nutricast_upside_overall.png", width = 10, height = 8, units= "in", res = 300)

upside_ratios_peru %>%
  filter (rcp %in% c("RCP26", "RCP85")) %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  ggplot (aes (x = upside, y = tonnes, fill = rcp)) +
  geom_col (position = "dodge") +
  facet_wrap (~spp_short, scales = "free_y", ncol = 3) +
  geom_hline (yintercept = 0, lty = 2) +
  theme_bw() +
  labs (y = "Production upside, tonnes", x = "", fill = "RCP") +
  ggtitle ("Production upside from climate-adaptive management, Peru") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 14),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 
dev.off()


# Sierra Leone ----

upside_ratios_SL <- sau_2019 %>%
  filter(country == "Sierra Leone", 
         species %in% priority_spp$species[which(priority_spp$country == "Sierra Leone")]) %>%
  group_by (country, species) %>%
  summarise (total_tonnes = sum (tonnes)) %>%
  ungroup() %>%
  left_join (catch_upside_relative, by = c("country", "species")) %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (country, rcp, species, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") 

upside_ratios_SL$upside <- factor(upside_ratios_SL$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))

png ("Figures/SL_nutricast_upside_overall.png", width = 9, height = 8, units= "in", res = 300)

upside_ratios_SL %>%
  filter (rcp %in% c("RCP26", "RCP85")) %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  ggplot (aes (x = upside, y = tonnes, fill = rcp)) +
  geom_col (position = "dodge") +
  facet_wrap (~spp_short, scales = "free_y", ncol = 2) +
  geom_hline (yintercept = 0, lty = 2) +
  theme_bw() +
  labs (y = "Production upside, tonnes", x = "", fill = "RCP") +
  ggtitle ("Production upside from climate-adaptive management, Sierra Leone") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 14),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 
dev.off()

##########################################################################33
# plot in terms of RNIs ----
# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")

plot_nutricast_upside_rni_sau_countries_function <- function (country_name, nutrient_name, divide_value) {
  divide_label <- case_when (
    divide_value ==1 ~ "",
    divide_value == 1000 ~ "(thousands)",
    divide_value == 1000000 ~ "(millions)"
  )
  
upsides <- sau_2019 %>%
  filter(country == country_name) %>%
  inner_join (priority_spp, by = c ("country", "species")) %>%
  group_by (country, species, taxa) %>%
  summarise (total_tonnes = sum (tonnes)) %>%
  ungroup() %>%
  left_join (catch_upside_relative, by = c("country", "species")) %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (country, rcp, species, taxa, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") 

upsides$upside <- factor(upsides$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))
    
upsides %>%
    filter (nutrient == nutrient_name, rcp %in% c("RCP26", "RCP85")) %>%
    mutate(
      spp_short = ifelse (
        species != "Stolephorus",
        paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
        species)
    ) %>%
    ggplot (aes (x = upside, y = children_fed/divide_value, fill = rcp)) +
    geom_col (position = "dodge") +
    facet_wrap (~spp_short, scales = "free_y", ncol = 2) +
    geom_hline (yintercept = 0, lty = 2) +
    theme_bw() +
    labs (y = paste0("Additional child RNIs met ", divide_label), x = "", fill = "RCP") +
    ggtitle (paste0(nutrient_name, " upside from climate-adaptive management,\n", country_name)) +
    theme (plot.title = element_text (size = 18),
           axis.text = element_text (size = 12),
           axis.text.x = element_text (angle = 60, hjust = 1),
           axis.title = element_text (size = 14),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 12)) 
    
  
  
}

png ("Figures/Peru_nutricast_upside_Cal.png", width = 10, height = 8, units= "in", res = 300)
plot_nutricast_upside_rni_sau_countries_function (country_name = "Peru", nutrient_name = "Calcium", divide_value = 1000)
dev.off()

png ("Figures/SL_nutricast_upside_Cal.png", width = 10, height = 8, units= "in", res = 300)
plot_nutricast_upside_rni_sau_countries_function (country_name = "Sierra Leone", nutrient_name = "Calcium", divide_value = 1)
dev.off()

# indonesia ----
# have to do different indo anyway...
upside_ratios_indo_nutr <- catch_upside_relative %>%
  left_join (priority_spp, by = c ("country", "species")) %>%
  filter (country == "Indonesia", species %in% indo_spp_ds) %>%
  replace_na(list(taxa = "Finfish")) %>%
  left_join (sau_tonnes_indo, by = "species") %>% 
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (rcp, species, taxa, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") 

upside_ratios_indo_nutr$upside <- factor(upside_ratios_indo_nutr$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))

png ("Figures/Indo_nutricast_upside_Cal.png", width = 10, height = 8, units= "in", res = 300)
upside_ratios_indo_nutr %>%
  filter (nutrient == "Calcium", rcp %in% c("RCP26", "RCP85")) %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  ggplot (aes (x = upside, y = children_fed/1000, fill = rcp)) +
  geom_col (position = "dodge") +
  facet_wrap (~spp_short, scales = "free_y", ncol = 3) +
  geom_hline (yintercept = 0, lty = 2) +
  theme_bw() +
  labs (y = "Additional child RNIs met (thousands)", x = "", fill = "RCP") +
  ggtitle ("Calcium upside from climate-adaptive management,\nIndonesia") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 

dev.off()

# chile ----
upside_ratios_chl_nutr <- chl_landings %>%
  filter (year == 2021) %>%
  group_by (species) %>%
  summarise (total_tonnes = sum (catch_mt)) %>%
  mutate (country = "Chile") %>%
  inner_join(priority_spp, by = c ("country", "species")) %>%
  left_join(catch_upside_relative, by = c ("country", "species")) %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (rcp, species, taxa, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") %>%
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") 

upside_ratios_chl_nutr$upside <- factor(upside_ratios_chl_nutr$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))

png ("Figures/Chile_nutricast_upside_Cal.png", width = 10, height = 8, units= "in", res = 300)

upside_ratios_chl_nutr %>%
  filter (nutrient == "Calcium", rcp %in% c("RCP26", "RCP85")) %>%
  mutate(
    spp_short = ifelse (
      species != "Stolephorus",
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  ggplot (aes (x = upside, y = children_fed/1000, fill = rcp)) +
  geom_col (position = "dodge") +
  facet_wrap (~spp_short, scales = "free_y", ncol = 2) +
  geom_hline (yintercept = 0, lty = 2) +
  theme_bw() +
  labs (y = "Additional child RNIs met (thousands)", x = "", fill = "RCP") +
  ggtitle ("Calcium upside from climate-adaptive management,\nChile") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 

dev.off()

