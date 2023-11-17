# Plot Figure XA, recent and projected total landings for each country
# 11 17 2023
# JGM

# moving code from Plot_contextual_landings_forecasts.R

# For each case study, we want to provide an overall contextual figure that shows the past trend of landings and projected landings out to 2100
# color by commercial group

# we also want to show some discrepancies among the datasets? so there will be stacked area plots that have the species for which we have projections. there will be a line showing the total yield from our data. and maybe a line showing different total yield from other data

library (tidyverse)

# landings data

# full sau, at least ten years ----
download_2019_full <- read.csv("Data/SAU EEZ 2019.csv")%>% # this doesn't have indonesia
  mutate (country = case_when (grepl ("Mex", area_name) ~ "Mexico",
                               grepl ("Chile", area_name) ~ "Chile",
                               TRUE ~ area_name))
#         ) %>%
# # remove Chile and Sierra Leone. really should only be Peru now. 
# filter (country == "Peru")


indo_2019_download_full <- read.csv("Data/SAU EEZ indonesia.csv") %>%
  mutate (country = "Indonesia") 

sau_full <- rbind (download_2019_full, indo_2019_download_full)

sau_20yr <- sau_full %>%
  filter (between (year, 2000, 2019)) %>%
  rename (species = scientific_name)


# chl landings

# note: as of 6/6/23, have added commercial_group column but very preliminary, just lumped all the fish that weren't in SAU into "other"
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
 

# sl IHH
# year with data for both artisanal and industrial is 2017
sl_ihh_landings <- readRDS("Data/SLE_landings_IHH.Rds")


# bring in forecast data----
# baseline -- most recent year of data from each of the data sources. from plot_Contextual_landings_forecasts.R
full_baseline <- readRDS("Data/baseline_catch_sau_chl_ihh.Rds")

# ratio of baseline (2012-2021) to future catch for each year/scenario calculate_nutritional_upsides.R
catch_upside_annual <- readRDS ("Data/nutricast_upside_relative_annual_ratio.Rds")

# repaired missing species, this is in a slightly different format (check_sau_nutricast_species.R)
catch_upside_annual_missing <- readRDS("Data/nutricast_upside_relative_annual_repair_missing.Rds")

catch_upside_ts <- catch_upside_annual %>%
  select (country, species, rcp, scenario, year, catch_ratio) %>%
  rbind(catch_upside_annual_missing) %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (tonnes = catch_ratio * bl_tonnes)

# clip sau data to nutricast ----
nutricast_spp <- catch_upside_ts %>%
  select (country, species) %>%
  distinct()

sau_20yr_nutricast_clip <- nutricast_spp %>%
  left_join (sau_20yr, by = c ("country", "species"))

# make aggregated ts to simplify
sau_20yr_agg <- sau_20yr %>%
  group_by(country, year) %>%
  summarise (tonnes = sum (tonnes)) 

upside_ts_bau_agg <- catch_upside_ts %>%
  filter (scenario == "No Adaptation") %>%
  group_by (country, rcp, year) %>%
  summarise (tonnes = sum (tonnes))

# categorize by SAU commercial group ----
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

# cut to 8 commercial groups, get rid of scorpionfishies and flatfishes
sau_2019_taxa_8 <- sau_2019_taxa %>%
  mutate (commercial_group = case_when (
    commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
    TRUE ~ commercial_group
  ))

# aggregate future projections by comm group to simplify
upside_ts_bau_agg_comm_group <- catch_upside_ts %>%
  filter (scenario == "No Adaptation") %>%
  inner_join (sau_2019_taxa_8, by = "species") %>%
  group_by (country, rcp, year, commercial_group) %>%
  summarise (tonnes = sum (tonnes))

# Indonesia ----
sau_20yr_nutricast_clip %>%
  filter (country == "Indonesia") %>%
  # cut to 8 comm groups
  mutate (commercial_group = case_when (
    commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
    TRUE ~ commercial_group
  )) %>%
  group_by(year, commercial_group) %>%
  summarise (tonnes = sum (tonnes)) %>%
  
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_area(aes (fill = commercial_group), position = "stack") +
    
    # add future time series, plot by commercial group, only two scenarios
    geom_area (data = filter(upside_ts_bau_agg_comm_group, country == "Indonesia", rcp == "RCP60"), aes (fill = commercial_group), position = "stack") +
    
    # add line of aggregated past landings to show what's missing
    geom_line (data = filter (sau_20yr_agg, country == "Indonesia")) +
    #guides (fill = "none") +
    labs (y ="Catch, million tonnes", x = "", fill = "Commercial group")+
  theme_bw() +
    theme (axis.text = element_text (size = 11),
           axis.title = element_text (size = 12),
           legend.text = element_text (size = 11),
           legend.title = element_text (size = 12),
           legend.key.size = unit (3.5, "mm"),
           legend.margin=margin(1,1,1,2),
           legend.box.margin=margin(-10,-10,-10,-10),
           plot.title = element_text(size = 13),
           plot.margin=unit(c(1,1,1,1), 'mm')) +
   
    
    # qualitative color scale for species, Dark1
    scale_fill_brewer(palette = "Dark2") +
    ggtitle ("Indonesia: Recent and projected total landings (RCP 6.0)")
  
  # png (paste0("Figures/contextual_agg_catch_area_comm_group_2scen", country_name,".png"), width = 6, height = 3, units = "in", res = 300)
  # print (p_plot)
  # dev.off()
  
  ggsave ("Figures/FigXA_contextual_Indo.eps", width = 174, height = 60, units = "mm")

  # Peru ----

  sau_20yr_nutricast_clip %>%
    filter (country == "Peru") %>%
    # cut to 8 comm groups
    mutate (commercial_group = case_when (
      commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
      TRUE ~ commercial_group
    )) %>%
    group_by(year, commercial_group) %>%
    summarise (tonnes = sum (tonnes)) %>%
    
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_area(aes (fill = commercial_group), position = "stack") +
    
    # add future time series, plot by commercial group, only two scenarios
    geom_area (data = filter(upside_ts_bau_agg_comm_group, country == "Peru", rcp == "RCP60"), aes (fill = commercial_group), position = "stack") +
    
    # add line of aggregated past landings to show what's missing
    geom_line (data = filter (sau_20yr_agg, country == "Peru")) +
    #guides (fill = "none") +
    labs (y ="Catch, million tonnes", x = "", fill = "Commercial group")+
    theme_bw() +
    theme (axis.text = element_text (size = 11),
           axis.title = element_text (size = 12),
           legend.text = element_text (size = 11),
           legend.title = element_text (size = 12),
           legend.key.size = unit (3.5, "mm"),
           legend.margin=margin(1,1,1,2),
           legend.box.margin=margin(-10,-10,-10,-10),
           plot.title = element_text(size = 13),
           plot.margin=unit(c(1,1,1,1), 'mm')) +
    
    
    # qualitative color scale for species, Dark1
    scale_fill_brewer(palette = "Dark2") +
    ggtitle ("Peru: Recent and projected total landings (RCP 6.0)")

  
  ggsave ("Figures/FigXA_contextual_Peru.eps", width = 174, height = 60, units = "mm")
