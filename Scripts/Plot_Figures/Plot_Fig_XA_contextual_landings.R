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
# baseline -- most recent year of data from each of the data sources. from calculate_projected_nutritional_upsides.R
full_baseline <- readRDS("Data/baseline_catch_sau_chl_ihh.Rds")

# ratio of baseline (2012-2021) to future catch for each year/scenario calculate_projected_nutritional_upsides.R
catch_upside_annual <- readRDS ("Data/catch_upside_relative_annual_repaired.Rds")

catch_upside_ts <- catch_upside_annual %>%
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
    geom_area (data = filter(upside_ts_bau_agg_comm_group, country == "Indonesia", rcp == "RCP60", year <= 2060), aes (fill = commercial_group), position = "stack") +
    
    # add line of aggregated past landings to show what's missing
    geom_line (data = filter (sau_20yr_agg, country == "Indonesia"), lty = 2) +
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
    geom_area (data = filter(upside_ts_bau_agg_comm_group, country == "Peru", rcp == "RCP60", year <= 2060), aes (fill = commercial_group), position = "stack") +
    
    # add line of aggregated past landings to show what's missing
    geom_line (data = filter (sau_20yr_agg, country == "Peru"), lty = 2) +
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
  
  # Chile ----
  chl_landings_agg_clip_commgroup <- chl_landings %>%
    filter (!commercial_group == "Algae") %>%
    mutate (country = "Chile") %>%
    # clips to nutricast spp
    right_join (nutricast_spp, by = c("country", "species")) %>%
    
    # condense to 8 commercial groups
    # cut to 8 comm groups
    mutate (commercial_group = case_when (
      commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
      TRUE ~ commercial_group
    )) %>%
    group_by (year, commercial_group) %>%
    summarise (tonnes = sum (catch_mt))%>%
    filter (!is.na(year)) %>%
    ungroup()%>%
    mutate (year = as.integer(year))
  
  # data frame of species and taxa/commercial group to match to nutricast data
  chl_groups <- 
    chl_landings %>%
    select (species, commercial_group) %>%
    mutate (commercial_group = case_when (
      commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
      TRUE ~ commercial_group)) %>%
    distinct ()
  
  # chile specific nutricast data, add taxa
  upside_chile_groups <- catch_upside_ts %>%
    filter (scenario == "No Adaptation", country == "Chile") %>%
    inner_join (chl_groups, by = "species") %>%
    group_by (country, rcp, year, commercial_group) %>%
    summarise (tonnes = sum (tonnes))
  
  # line of aggregated landings
  chl_landings_agg <- chl_landings %>%
    group_by (year) %>%
    summarise (tonnes = sum (catch_mt)) %>% 
    ungroup() %>%
    mutate (year = as.integer(year))
  
  # line of SAU landings?
  sau_chl_landings <- sau_20yr_agg %>% 
    filter (country == "Chile", year >= min(chl_landings$year))

  chl_landings_agg_clip_commgroup %>%
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_area(aes (fill = commercial_group), position = "stack") +
    # add future landings
    geom_area (data = filter (upside_chile_groups, rcp == "RCP60", year <= 2060), aes (fill = commercial_group), position = "stack") +
    # add line of aggregated past landings to show what's missing
    geom_line (data = chl_landings_agg ) +
    # add line to show SAU landings
    geom_line (data = sau_chl_landings, lty = 2) +
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
    
    ggtitle (paste0("Recent and projected total landings, ", "Chile")) 
  
ggsave ("Figures/FigXA_contextual_Chile.eps", width = 174, height = 60, units = "mm") 



# which perch likes are accounting for steady rise?
chl_landings_check <- chl_landings %>%
     filter (!commercial_group == "Algae") %>%
       mutate (country = "Chile") %>%
       # clips to nutricast spp
       right_join (nutricast_spp, by = c("country", "species")) %>%
       
       # condense to 8 commercial groups
       # cut to 8 comm groups
       mutate (commercial_group = case_when (
           commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
          TRUE ~ commercial_group
         )) %>%
  group_by (species, year, commercial_group) %>%
  summarise (catch_mt = sum (catch_mt, na.rm = TRUE)) %>% ungroup()

chl_landings_check %>%
  filter (commercial_group == "Perch-likes", catch_mt > 1000) %>% 
  ggplot (aes (x = year, y = catch_mt/1000000, col = species, group = species)) +
  geom_line ()

chl_landings_check %>% filter (species == "Engraulis ringens") %>%
  ggplot (aes (x = year, y = catch_mt/1000000, col = species, group = species)) +
  geom_line ()

# Sierra Leone ----
  show_col(brewer_pal(palette = "Dark2")(8))

# check which species are the dominant herring-likes
sl_ihh_landings %>%
  filter (year == 2017) %>% 
  mutate (country = "Sierra Leone") %>%
  #clip to nutricast
  right_join (nutricast_spp, by = c("country", "species")) %>%
  mutate (catch_prop = catch_mt / sum (catch_mt)) %>%
  arrange (desc(catch_prop)) %>% View()
  
  sl_landings_agg_clip_commgroup <- sl_ihh_landings %>%
    mutate (country = "Sierra Leone") %>%
    #clip to nutricast
    right_join (nutricast_spp, by = c("country", "species")) %>%
    #rename (commercial_group = taxa) %>%
    group_by (year, commercial_group) %>%
    summarise (tonnes = sum (catch_mt, na.rm = TRUE))

  sl_groups <- 
    sl_ihh_landings %>%
    select (species, commercial_group) %>%
    distinct () 
  
  # SL specific nutricast data, add taxa
  upside_sl_groups <- catch_upside_ts %>%
    filter (scenario == "No Adaptation", country == "Sierra Leone") %>%
    inner_join (sl_groups, by = "species") %>%
    group_by (country, rcp, year, commercial_group) %>%
    summarise (tonnes = sum (tonnes))
  
  
  # full line of landings
  ihh_agg_landings <- sl_ihh_landings %>%
    #filter (sector == "Artisanal") %>%
    group_by (year) %>%
    summarise (tonnes = sum (catch_mt, na.rm = TRUE)) %>%
    # take out 0 at 2018, confusing
    filter (year < 2018)
  
  # line of SAU landings?
  sau_sl_landings <- sau_20yr_agg %>% 
    filter (country == "Sierra Leone", year >= min(ihh_agg_landings$year))
  
  
  # trick colors
  library(scales)
  #show_col(hue_pal()(8))
  
  # plot past landings
  sl_landings_agg_clip_commgroup %>%
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_area(aes(fill = commercial_group), position = "stack") +
    # add future landings
    geom_area (data = filter (upside_sl_groups, rcp == "RCP60", year <= 2060), aes(fill = commercial_group), position = "stack") +
    
    # add line of aggregated past landings to show what's missing
    geom_line (data = ihh_agg_landings ) +
    geom_line (data = sau_sl_landings, lty = 2) +
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
    scale_fill_manual(values = c("#1B9E77", "#7570b3", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
    ggtitle ("Sierra Leone: Recent and projected total landings (RCP 6.0)")

  ggsave ("Figures/FigXA_contextual_SierraLeone.eps", width = 174, height = 60, units = "mm") 

  # Malawi ----

# different format bc no forecast data

  # landings with just the top 5 species for clarity
mal_top <- readRDS("Data/malawi_landings_top.Rds")
mal_top$sector <- factor (mal_top$sector, levels = c("Small-scale", "Large-scale"))
  
  
# trick color scale so we keep species consistent
library (scales)
show_col(brewer_pal(palette = "Dark2")(6))
  
  
  
mal_top %>%
    group_by (comm_name, sector, Year) %>%
    summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
    ggplot (aes (x= Year, y = tonnes/1000,fill = comm_name)) +
    geom_area(position = "stack") +
    facet_wrap (~sector, scales = "free_y", nrow = 1) +
    #scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual(values = c("#D95F02", "#7570b3", "#e7298a", "#66A61E", "#E6AB02")) +
    theme_bw() +
    labs (y = "Catch, thousand tonnes", fill = "Species", x = "") +
    ggtitle("Malawi: Recent landings by sector") +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 11),
         legend.title = element_text (size = 12),
         legend.key.size = unit (3.5, "mm"),
         legend.margin=margin(1,1,1,2),
         legend.box.margin=margin(-10,-10,-10,-10),
         plot.title = element_text(size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm')) 

ggsave ("Figures/FigXA_contextual_Malawi.eps", width = 174, height = 60, units = "mm") 
