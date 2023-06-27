# Summary figure with current and projected landings
# 5/22/23
# JGM

# Context figure, just with current and projected landings (multiplied by current)

library (tidyverse)

# full sau, at least ten years ----
download_2019_full <- read.csv("Data/SAU EEZ 2019.csv")%>% # this doesn't have indonesia
  mutate (country = case_when (grepl ("Mex", area_name) ~ "Mexico",
                               grepl ("Chile", area_name) ~ "Chile",
                               TRUE ~ area_name)
          ) %>%
  # remove Chile and Sierra Leone. really should only be Peru now. 
  filter (country == "Peru")


indo_2019_download_full <- read.csv("Data/SAU EEZ indonesia.csv") %>%
  mutate (country = "Indonesia") 

sau_full <- rbind (download_2019_full, indo_2019_download_full)

sau_10yr <- sau_full %>%
  filter (between (year, 2010, 2019)) %>%
  rename (species = scientific_name)

sau_baseline <- sau_full %>%
  rename (species = scientific_name) %>%
  filter (year == 2019) %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (tonnes))

# chl landings

# note: as of 6/6/23, have added commercial_group column but very preliminary, just lumped all the fish that weren't in SAU into "other"
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

chl_baseline <- chl_landings %>% 
  filter (year == 2021) %>%
  mutate (country = "Chile") %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (catch_mt)) 

# sl IHH
# year with data for both artisanal and industrial is 2017
sl_ihh_landings <- readRDS("Data/SLE_landings_IHH.Rds")

sl_baseline <- sl_ihh_landings %>% 
  filter (year == 2017) %>%
  mutate (country = "Sierra Leone") %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (catch_mt, na.rm = TRUE)) 


# should remove SL
full_baseline <- rbind (sau_baseline, chl_baseline, sl_baseline)

saveRDS(full_baseline, file = "Data/baseline_catch_sau_chl_ihh.Rds")

full_baseline <- readRDS("DAta/baseline_catch_sau_chl_ihh.Rds")

# catch upside ----
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

# clip sau data to nutricast
nutricast_spp <- catch_upside_ts %>%
  select (country, species) %>%
  distinct()

sau_10yr_nutricast_clip <- nutricast_spp %>%
  left_join (sau_10yr, by = c ("country", "species"))

# make aggregated ts to simplify
sau_10yr_agg <- sau_10yr %>%
  group_by(country, year) %>%
  summarise (tonnes = sum (tonnes)) 

upside_ts_bau_agg <- catch_upside_ts %>%
  filter (scenario == "No Adaptation") %>%
  group_by (country, rcp, year) %>%
  summarise (tonnes = sum (tonnes))


# quick single line graph, aggregated species ----

# sau_10yr_nutricast_clip %>%
#   group_by(country, year) %>%
#   summarise (tonnes = sum (tonnes)) %>% 
#   ggplot (aes (x = year, y = tonnes/1000000)) +
#   geom_line() +
#   geom_line (data = upside_ts_bau_agg, aes(x = year, y = tonnes/1000000, col = rcp)) +
#   geom_line (data = sau_10yr_agg, aes (x = year, y = tonnes/1000000), lty = 2) +
#   facet_wrap (~country, scales = "free_y") +
#   theme_bw()

# country by country
chl_landings_agg <- chl_landings %>%
  group_by (year) %>%
  summarise (tonnes = sum (catch_mt)) %>% 
  ungroup() %>%
  mutate (year = as.integer(year))

chl_landings_agg_clip <- chl_landings %>%
  mutate (country = "Chile") %>%
  right_join (nutricast_spp, by = c("country", "species")) %>%
  group_by (year) %>%
  summarise (tonnes = sum (catch_mt))%>%
  filter (!is.na(year)) %>%
  ungroup()%>%
  mutate (year = as.integer(year))

upside_chile <- filter(upside_ts_bau_agg, country == "Chile") %>% ungroup()

png ("Figures/contextual_agg_catch_line_Chile.png", width = 6, height = 4, units = "in", res = 300)
chl_landings_agg_clip %>% 
  ggplot (aes (x = year, y = tonnes/1000000)) +
  geom_line(group = 1, lty = 2,) +
  geom_line (data = upside_chile, aes(x = year, y = tonnes/1000000, col = rcp)) +
  geom_line (data = chl_landings_agg, aes (x = year, y = tonnes/1000000),  group  =1) +
  theme_bw() +
  ggtitle ("Recent and projected total landings, Chile") +
  labs (y ="Catch, million tonnes", x = "", col = "Climate\nscenario")+
  theme (axis.text = element_text (size = 10),
         axis.title = element_text (size = 14)) 
dev.off()


sau_countries <- c("Indonesia", "Peru", "Sierra Leone")

for (country_name in sau_countries) {
    nutricast_clip <- sau_10yr_nutricast_clip %>%
      filter (country == country_name) %>%
      group_by(year) %>%
      summarise (tonnes = sum (tonnes))

 p <-  sau_10yr_nutricast_clip %>%
   filter (country == country_name) %>%
   group_by(year) %>%
   summarise (tonnes = sum (tonnes)) %>%
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_line(lty = 2) +
    geom_line (data = filter(upside_ts_bau_agg, country == country_name), aes(x = year, y = tonnes/1000000, col = rcp)) +
    geom_line (data = filter (sau_10yr_agg, country == country_name), aes (x = year, y = tonnes/1000000)) +
   labs (y ="Catch, million tonnes", x = "", col = "Climate\nscenario")+
   theme (axis.text = element_text (size = 10),
          axis.title = element_text (size = 14),
          strip.text = element_text (size = 14),
          plot.title = element_text(size = 16)) +
    theme_bw() +
   ggtitle (paste0("Recent and projected total landings, ", country_name))
    
png (paste0("Figures/contextual_agg_catch_line_", country_name,".png"), width = 6, height = 4, units = "in", res = 300)
  print (p)
  dev.off()
}


######
# by commercial group ----
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


for (country_name in sau_countries) {
  
  # take past time series, clipped to nutricast species, plot by commercial group
  p <-  sau_10yr_nutricast_clip %>%
    filter (country == country_name) %>%
    # cut to 8 comm groups
    mutate (commercial_group = case_when (
      commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
      TRUE ~ commercial_group
    )) %>%
    group_by(year, commercial_group) %>%
    summarise (tonnes = sum (tonnes))
  
  p_plot <- p %>%
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_area(aes (fill = commercial_group), position = "stack") +
    
    # add future time series, plot by commercial group, only two scenarios
    geom_area (data = filter(upside_ts_bau_agg_comm_group, country == country_name, rcp %in% c("RCP26", "RCP60")), aes (fill = commercial_group), position = "stack") +
    
    # add line of aggregated past landings to show what's missing
    geom_line (data = filter (sau_10yr_agg, country == country_name)) +
    facet_wrap (~rcp) +
    guides (fill = "none") +
    labs (y ="Catch, million tonnes", x = "", fill = "Commercial group")+
    theme (axis.text = element_text (size = 10),
           axis.title = element_text (size = 14),
           strip.text = element_text (size = 14),
           plot.title = element_text(size = 16)) +
    theme_bw() +
    
    # qualitative color scale for species, Dark1
    scale_fill_brewer(palette = "Dark2") +
    ggtitle (paste0("Recent and projected total landings, ", country_name))
  
  png (paste0("Figures/contextual_agg_catch_area_comm_group_2scen", country_name,".png"), width = 6, height = 3, units = "in", res = 300)
  print (p_plot)
  dev.off()
}


# Chile comm group ----

# try to trick color scale, match with other chile graphs. 6 colors instead of SAU 8. 
library(scales)
show_col(brewer_pal(palette = "Dark2")(6))

# past landings
chl_landings_agg_clip_commgroup <- chl_landings %>%
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

png ("Figures/contextual_agg_catch_area_comm_group_Chile.png", width = 6, height = 3, units = "in", res = 300)
# plot past landings
chl_landings_agg_clip_commgroup %>%
  ggplot (aes (x = year, y = tonnes/1000000)) +
  geom_area(aes (fill = commercial_group), position = "stack") +
  # add future landings
  geom_area (data = filter (upside_chile_groups, rcp %in% c("RCP26", "RCP60")), aes (fill = commercial_group), position = "stack") +
  # add line of aggregated past landings to show what's missing
  geom_line (data = chl_landings_agg ) +
  facet_wrap (~rcp) +
  labs (y ="Catch, million tonnes", x = "", col = "Climate\nscenario")+
  theme (axis.text = element_text (size = 10),
         axis.title = element_text (size = 14),
         strip.text = element_text (size = 14),
         plot.title = element_text(size = 16)) +
  theme_bw() +
  # qualitative color scale for species, Dark1
  scale_fill_brewer(palette = "Dark2") +
  # remove legend
  guides (fill = "none") +
  ggtitle (paste0("Recent and projected total landings, ", "Chile"))

dev.off()

# sierra leone ihh data comm_group ----
show_col(brewer_pal(palette = "Dark2")(8))

sl_landings_agg_clip_commgroup <- sl_ihh_landings %>%
 # filter (sector == "Artisanal") %>%
  mutate (country = "Sierra Leone") %>%

  #clip to nutricast
  right_join (nutricast_spp, by = c("country", "species")) %>%
  #rename (commercial_group = taxa) %>%
  group_by (year, commercial_group) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE))

# # trial plot
# sl_landings_agg_clip_commgroup %>%
#   ggplot (aes (x = year, y = tonnes/1000000, fill = commercial_group)) +
#   geom_area(position = "stack")

#  specific nutricast data, add taxa
# data frame of species and taxa/commercial group
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


# trick colors
library(scales)
#show_col(hue_pal()(8))

png ("Figures/contextual_agg_catch_area_comm_group_Sierra_Leone_IHH_all.png", width = 6, height = 3, units = "in", res = 300)
# plot past landings
sl_landings_agg_clip_commgroup %>%
  ggplot (aes (x = year, y = tonnes/1000000)) +
  geom_area(aes(fill = commercial_group), position = "stack") +
  # add future landings
  geom_area (data = filter (upside_sl_groups, rcp %in% c("RCP26", "RCP60")), aes(fill = commercial_group), position = "stack") +
  
  # add line of aggregated past landings to show what's missing
  geom_line (data = ihh_agg_landings ) +
  facet_wrap (~rcp) +
  labs (y ="Catch, million tonnes", x = "")+
  guides (fill = "none") +
 
  theme_bw() +
  scale_fill_manual(values = c("#1B9E77", "#7570b3", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  theme (axis.text = element_text (size = 10),
        axis.title = element_text (size = 14),
        strip.text = element_text (size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle ("Recent and projected total landings, Sierra Leone")

dev.off()

# old code: plot overall catch by comm_group bar ----
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
