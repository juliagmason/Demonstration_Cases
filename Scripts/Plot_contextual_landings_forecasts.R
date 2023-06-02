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
             plot.title = element_text(size = 18)) +
    theme_bw() +
   ggtitle (paste0("Recent and projected total landings, ", country_name))
    
png (paste0("Figures/contextual_agg_catch_line_", country_name,".png"), width = 6, height = 4, units = "in", res = 300)
  print (p)
  dev.off()
}


######
# by commercial group ----
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

# aggregate future projections by comm group to simplify
upside_ts_bau_agg_comm_group <- catch_upside_ts %>%
  filter (scenario == "No Adaptation") %>%
  inner_join (sau_2019_taxa, by = "species") %>%
  group_by (country, rcp, year, commercial_group) %>%
  summarise (tonnes = sum (tonnes))


for (country_name in sau_countries) {
  
  # take past time series, clipped to nutricast species, plot by commercial group
  p <-  sau_10yr_nutricast_clip %>%
    filter (country == country_name) %>%
    group_by(year, commercial_group) %>%
    summarise (tonnes = sum (tonnes)) %>%
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
           plot.title = element_text(size = 18),
           legend.position = "none") +
    theme_bw() +
    ggtitle (paste0("Recent and projected total landings, ", country_name))
  
  png (paste0("Figures/contextual_agg_catch_area_comm_group_2scen", country_name,".png"), width = 6, height = 4, units = "in", res = 300)
  print (p)
  dev.off()
}


# Chile comm group ----

# try to trick color scale, match with other chile graphs. 6 colors instead of SAU 8. 
library(scales)
show_col(hue_pal()(6))

# past landings
chl_landings_agg_clip_commgroup <- chl_landings %>%
  mutate (country = "Chile") %>%
  # clips to nutricast spp
  right_join (nutricast_spp, by = c("country", "species")) %>%
  rename (commercial_group = taxa) %>%
  group_by (year, commercial_group) %>%
  summarise (tonnes = sum (catch_mt))%>%
  filter (!is.na(year)) %>%
  ungroup()%>%
  mutate (year = as.integer(year))

# data frame of species and taxa/commercial group
chl_groups <- 
  chl_landings %>%
  select (species, taxa) %>%
  distinct () %>%
  rename (commercial_group = taxa)

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

png ("Figures/contextual_agg_catch_area_comm_group_Chile.png", width = 6, height = 4, units = "in", res = 300)
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
         plot.title = element_text(size = 18)) +
  theme_bw() +
  scale_fill_manual(values = c("#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")) +
  ggtitle (paste0("Recent and projected total landings, ", "Chile"))

dev.off()

# sierra leone ihh data comm_group ----


sl_landings_agg_clip_commgroup <- sl_ihh_landings %>%
  filter (sector == "Artisanal") %>%
  mutate (country = "Sierra Leone") %>%

  #clip to nutricast
  right_join (nutricast_spp, by = c("country", "species")) %>%
  #rename (commercial_group = taxa) %>%
  group_by (year, commercial_group) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE))

# trial plot
sl_landings_agg_clip_commgroup %>%
  ggplot (aes (x = year, y = tonnes/1000000, fill = commercial_group)) +
  geom_area(position = "stack")

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
  filter (sector == "Artisanal") %>%
  group_by (year) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE)) %>%
  # take out 0 at 2018, confusing
  filter (year < 2018)


# trick colors
library(scales)
show_col(hue_pal()(8))

png ("Figures/contextual_agg_catch_area_comm_group_Sierra_Leone_IHH_SSF.png", width = 6, height = 4, units = "in", res = 300)
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
 
  theme_bw() +
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC", "gray50")) +
  theme (axis.text = element_text (size = 10),
        axis.title = element_text (size = 14),
        plot.title = element_text(size = 18),
        legend.position = "none") +
  ggtitle (paste0("Recent and projected total landings, Sierra Leone\n IHH SSF"))
dev.off()
