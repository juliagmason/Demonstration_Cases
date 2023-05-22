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
          )
indo_2019_download_full <- read.csv("Data/SAU EEZ indonesia.csv") %>%
  mutate (country = "Indonesia") 

sau_full <- rbind (download_2019_full, indo_2019_download_full)

sau_10yr <- sau_full %>%
  filter (between (year, 2010, 2019)) %>%
  rename (species = scientific_name)

sau_baseline <- sau_full %>%
  rename (species = scientific_name) %>%
  filter (year == 2019, !country == "Chile") %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (tonnes))

# chl landings
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

full_baseline <- chl_landings %>%
  filter (year == 2021) %>%
  mutate (country = "Chile") %>%
  group_by (country, species) %>%
  summarise (bl_tonnes = sum (catch_mt)) %>%
  rbind (sau_baseline)

# catch upside ----
# ratio of baseline (2012-2021) to future catch for each year/scenario calculate_nutritional_upsides.R
catch_upside_annual <- readRDS ("Data/nutricast_upside_relative_annual_ratio.Rds")

# repaired missing species, this is in a slightly different format (check_sau_nutricast_species.R)
catch_upside_annual_repaired <- readRDS("Data/nutricast_upside_relative_annual_repair_missing.Rds")

catch_upside_ts <- catch_upside_annual %>%
  select (country, species, rcp, scenario, year, catch_ratio) %>%
  rbind(catch_upside_annual_repaired) %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (tonnes = catch_ratio * bl_tonnes)

# quick single line graph, aggregated species ----

# need to clip sau data to nutricast
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

sau_10yr_nutricast_clip %>%
  group_by(country, year) %>%
  summarise (tonnes = sum (tonnes)) %>% 
  ggplot (aes (x = year, y = tonnes/1000000)) +
  geom_line() +
  geom_line (data = upside_ts_bau_agg, aes(x = year, y = tonnes/1000000, col = rcp)) +
  geom_line (data = sau_10yr_agg, aes (x = year, y = tonnes/1000000), lty = 2) +
  facet_wrap (~country, scales = "free_y") +
  theme_bw()

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
  geom_line(group = 1) +
  geom_line (data = upside_chile, aes(x = year, y = tonnes/1000000, col = rcp)) +
  geom_line (data = chl_landings_agg, aes (x = year, y = tonnes/1000000), lty = 2, group  =1) +
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
    
    upside <- upside_ts_bau_agg  %>% filter(country == country_name)
    
    total_sau <- filter(sau_10yr_agg, country == country_name)
  
 p <-  sau_10yr_nutricast_clip %>%
   filter (country == country_name) %>%
   group_by(year) %>%
   summarise (tonnes = sum (tonnes)) %>%
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_line() +
    geom_line (data = filter(upside_ts_bau_agg, country == country_name), aes(x = year, y = tonnes/1000000, col = rcp)) +
    geom_line (data = filter (sau_10yr_agg, country == country_name), aes (x = year, y = tonnes/1000000), lty = 2) +
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