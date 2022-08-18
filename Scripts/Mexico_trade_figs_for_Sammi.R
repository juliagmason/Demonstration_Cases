# Mexico export data explore
# 8/18/22
# request from Sammi Lin; she pulled FishSTatJ data

# snooping on ARTIS github

# trade data by volume
mex_vol <- read.csv("Data/Mex_FAO_exports_sammi_tidy.csv")

# sankey https://github.com/Seafood-Globalization-Lab/exploreARTIS/blob/main/R/plot_sankey.R
library (networkD3)
prop_flow_cutoff <- 0.03

imports <- mex_vol %>%
  filter (Trade_flow == "Imports") %>%
  group_by (Partner_country) %>%
  summarize (total_q = sum (X2020)) %>%
  ungroup() %>%
  mutate(total = sum(total_q, na.rm=TRUE)) %>%
  mutate(prop = total_q / total) %>%
  mutate(source_country_name = case_when(
    prop < prop_flow_cutoff ~ "Other",
    TRUE ~ Partner_country
  )) %>%
  group_by (source_country_name) %>%
  summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
  mutate (target_country_name = "Mexico") %>%
  select (source_country_name, target_country_name, total_q)

exports <- mex_vol %>%
  filter (Trade_flow == "Exports") %>%
  group_by (Partner_country) %>%
  summarize (total_q = sum (X2020)) %>%
  ungroup() %>%
  mutate(total = sum(total_q, na.rm=TRUE)) %>%
  mutate(prop = total_q / total) %>%
  mutate(target_country_name = case_when(
    prop < prop_flow_cutoff ~ "Other",
    TRUE ~ Partner_country
  )) %>%
  group_by (target_country_name) %>%
  summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
  mutate (source_country_name = "Mexico") %>%
  select (source_country_name, target_country_name, total_q)


trade_flows <- imports %>%
  bind_rows(exports)

#Creating nodes dataframe
nodes <- data.frame(
  name = unique(c(trade_flows$source_country_name, trade_flows$target_country_name))
) %>% 
  mutate(label = name)

# Mapping source and target names to respective row in the nodes dataframe, 0-indexed
trade_flows <- trade_flows %>%
  mutate(source_id = match(source_country_name[1], nodes$name[1]) -1,
         target_id = match(target_country_name, nodes$name) -1)

sankeyNetwork(Links = trade_flows, Nodes = nodes, Source = "source_id", Target = "target_id",
              Value = "total_q", NodeID = "label")

# looks very weird, lose other for imports?

# chord ---
library (circlize)

trade_flows_chord <- imports %>%
  bind_rows(exports) %>%
  mutate (target_country_name = 
            case_when (target_country_name == "Korea, Republic of" ~ "Korea",
                       target_country_name == "United States of America" ~ "USA", 
                       TRUE ~ target_country_name
            ),
          source_country_name = 
            case_when (source_country_name == "United States of America" ~ "USA", 
                       TRUE ~ source_country_name)
  )

set.seed(15)
png ("Figures/Mex_FAO_chord_Sammi.png", res = 300, width = 8, height = 8, units = "in")



chordDiagram(trade_flows_chord, 
             annotationTrack = c("name", "grid"),
             directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.target.prop = FALSE,
             diffHeight = 0,
             link.sort = TRUE )


dev.off()

# map --> wait on this. need iso3 codes. 
# https://github.com/Seafood-Globalization-Lab/exploreARTIS/blob/main/R/plot_map.R
library (tidyverse)
library (viridis)
library (rnaturalearth)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(iso_a3 != "ATA")

# Change projection
PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
#world <- st_transform(world, PROJ)

trade_map <- ggplot(world) +
  geom_sf(size = 0.1) 
# FIX IT: Projection is not being applied for some reason
#coord_sf(crs = PROJ) 