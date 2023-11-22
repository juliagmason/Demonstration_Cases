# Plot sankeys allocative levers
#5 24 23
# JGM


#https://corybrunson.github.io/ggalluvial/

library (tidyverse)
library (ggalluvial)

# Sierra Leone: foreign and exports ----
# ohh, actually do need to combine for this one....try to work from saved nutr sets
sl_export_nutr <- readRDS("Data/levers_RNI_pop_export_SierraLeone.Rds")
sl_foreign_sector_nutr <- readRDS("Data/levers_RNI_pop_foreign_sector_SierraLeone.Rds")

# add a column to each dataframe and rbind
sl_export_nutr_comb <- sl_export_nutr %>%
  mutate (sector = "Domestic catch") %>%
  select (country, sector, exports, nutrient, rni_equivalents, perc_demand_met)

sl_foreign_export_nutr_comb <- sl_foreign_sector_nutr %>%
  # just take foreign catch
  filter (sector == "Foreign catch") %>%
  # make exports column
  mutate (exports = NA) %>%
  group_by (country, sector, exports, nutrient) %>% #summarize (rni_equiv = sum (rni_equivalents))
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # rbind to exports--this represents all domestic
  rbind (sl_export_nutr_comb)

# set levels
# I'm not sure why this works, but this makes the foreign catch export flow disappear!
sl_foreign_export_nutr_comb$sector <- factor (sl_foreign_export_nutr_comb$sector, levels = c ("Foreign catch", "Domestic catch"))
sl_foreign_export_nutr_comb$exports <- factor(sl_foreign_export_nutr_comb$exports, levels = c ("Exported","Retained",  "Foreign catch"))

sl_foreign_export_nutr_comb %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.05, .05)) +
  labs(y = "Child RNI equiv., millions", x = "") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  theme_minimal() +
  ggtitle("National allocative drivers") +
  scale_fill_brewer (palette = "Set1") +
  theme ( 
    axis.text.x = element_blank(),
    axis.text.y = element_text (size = 9),
    axis.title = element_text (size = 12),
    plot.title = element_text (size = 13),
    plot.margin=unit(c(1,1,1,1), 'mm'),
    legend.position = "none")
ggsave ("Figures/FigXD_driver_SierraLeone.svg", width = 74, height = 70, units = "mm")

# try networkd3 to make space btw the nodes
library (networkD3)

sl_trade_group_sm <- sl_foreign_export_nutr_comb %>%
  filter (!nutrient %in% c("Selenium", "Protein"))

nodes <- data.frame (name = unique (c(as.character(sl_trade_group_sm$nutrient),
                                      as.character(sl_trade_group_sm$sector),
                                      as.character(sl_trade_group_sm$exports)
)))

# links df
links <- data.frame(source = match(sl_trade_group_sm$nutrient, nodes$name) - 1,
                    target = match(sl_trade_group_sm$sector, nodes$name) - 1,
                    value = sl_trade_group_sm$rni_equivalents,
                    stringsAsFactors = FALSE)

# add second layer
links <- rbind (links, 
                data.frame(source = match(sl_trade_group_sm$sector, nodes$name) - 1,
                           target = match(sl_trade_group_sm$exports, nodes$name) - 1,
                           value = sl_trade_group_sm$rni_equivalents,
                           stringsAsFactors = FALSE)
)

links$group <- as.factor (rep(c("Calcium", "Iron", "Omega_3", "Vitamin_A", "Zinc"), nrow(links)/5))
nodes$group <- as.factor(c("Calcium", "Iron", "Omega_3", "Vitamin_A", "Zinc", rep("group", 5)))

library (scales)
show_col(brewer_pal(palette = "Set1")(5))

my_color <- 'd3.scaleOrdinal() .domain(["Calcium", "Iron", "Omega_3", "Vitamin_A", "Zinc", "group"]) .range(["#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "gray90"])'

p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                   Value = "value", NodeID = "name", 
                   colourScale=my_color, LinkGroup="group", NodeGroup="group")

p
# chile just artisanal vs. industrial----
chl_foreign_sector_nutr <- readRDS("Data/levers_RNI_pop_foreign_sector_Chile.Rds")

png("Figures/Sankey_Chl_sector.png", width = 8, height = 6, units = "in", res = 300)
chl_foreign_sector_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium"), sector != "Foreign catch") %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Chile") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")

dev.off()

# try to do 2 tier for all countries ----

plot_2tier_sankey_function <- function (country_name) {
  # indonesia is Indo, also have Peru_anchov and Peru_noanchov
  
  export_nutr <- readRDS(paste0("Data/levers_RNI_pop_export_", country_name, ".Rds"))
  foreign_sector_nutr <- readRDS(paste0("Data/levers_RNI_pop_foreign_sector_", country_name, ".Rds"))
  
  # add a column to each dataframe and rbind
  export_nutr_comb <- export_nutr %>%
    mutate (sector = "Domestic catch") %>%
    select (country, sector, exports, nutrient, rni_equivalents, perc_demand_met)
  
  foreign_export_nutr_comb <- foreign_sector_nutr %>%
    # just take foreign catch
    filter (sector == "Foreign catch") %>%
    # make exports column
    mutate (exports = NA) %>%
    group_by (country, sector, exports, nutrient) %>% 
    summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    # rbind to exports--this represents all domestic
    rbind (export_nutr_comb)
  
  # set levels
  # I'm not sure why this works, but this makes the foreign catch export flow disappear!
  foreign_export_nutr_comb$sector <- factor (foreign_export_nutr_comb$sector, levels = c ("Foreign catch", "Domestic catch"))
  foreign_export_nutr_comb$exports <- factor(foreign_export_nutr_comb$exports, levels = c ("Exported","Retained",  "Foreign catch"))
  
  foreign_export_nutr_comb %>%
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    ggplot (aes (axis1 = nutrient,
                 axis2 = sector,
                 axis3 = exports,
                 y = rni_equivalents/1000000)) +
    scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.05, .05)) +
    labs(y = "Child RNI equiv., millions", x = "") +
    geom_flow(aes(fill = nutrient)) +
    geom_stratum(aes(fill = nutrient)) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
    theme_minimal() +
    ggtitle("National allocative drivers") +
    scale_fill_brewer (palette = "Set1") +
    theme ( 
      axis.text.x = element_blank(),
      axis.text.y = element_text (size = 9),
      axis.title = element_text (size = 12),
      plot.title = element_text (size = 13),
      plot.margin=unit(c(1,1,1,1), 'mm'),
      legend.position = "none")
 
   ggsave (paste0("Figures/FigXD_driver_", country_name, "_2tier.svg"), width = 74, height = 70, units = "mm")
                                 
} 


country_names <- c("Indo", "Chile", "Peru_anchov", "Peru_noanchov", "SierraLeone")
map (country_names, plot_2tier_sankey_function)


chl_export_nutr <- readRDS("Data/levers_RNI_pop_export_Indo.Rds")
indo_foreign_sector_nutr <- readRDS("Data/levers_RNI_pop_foreign_sector_Indo.Rds")

# add a column to each dataframe and rbind
indo_export_nutr_comb <- indo_export_nutr %>%
  mutate (sector = "Domestic catch") %>%
  select (country, sector, exports, nutrient, rni_equivalents, perc_demand_met)

indo_foreign_export_nutr_comb <- indo_foreign_sector_nutr %>%
  # just take foreign catch
  filter (sector == "Foreign catch") %>%
  # make exports column
  mutate (exports = NA) %>%
  group_by (country, sector, exports, nutrient) %>% #summarize (rni_equiv = sum (rni_equivalents))
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # rbind to exports--this represents all domestic
  rbind (indo_export_nutr_comb)

# set levels
# I'm not sure why this works, but this makes the foreign catch export flow disappear!
indo_foreign_export_nutr_comb$sector <- factor (indo_foreign_export_nutr_comb$sector, levels = c ("Foreign catch", "Domestic catch"))
indo_foreign_export_nutr_comb$exports <- factor(indo_foreign_export_nutr_comb$exports, levels = c ("Exported","Retained",  "Foreign catch"))

indo_foreign_export_nutr_comb %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.05, .05)) +
  labs(y = "Child RNI equiv., millions", x = "") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  theme_minimal() +
  ggtitle("National allocative drivers") +
  scale_fill_brewer (palette = "Set1") +
  theme ( 
    axis.text.x = element_blank(),
    axis.text.y = element_text (size = 9),
    axis.title = element_text (size = 12),
    plot.title = element_text (size = 13),
    plot.margin=unit(c(1,1,1,1), 'mm'),
    legend.position = "none")
ggsave ("Figures/FigXD_driver_Indo_2tier.svg", width = 74, height = 70, units = "mm")

# Indonesia ----

# exports
indo_export_nutr <- readRDS("Data/levers_RNI_pop_export_Indo.Rds")

indo_export_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  mutate (nutrient = case_when (
    nutrient == "Omega_3" ~ "Omega 3",
    nutrient == "Vitamin_A" ~ "Vit. A",
    TRUE ~ nutrient)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.05, .05)) +
  labs(y = "Child RNI equiv., millions", x = "") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  theme_minimal() +
  ggtitle("National allocative driver: Exports") +
  scale_fill_brewer (palette = "Set1") +
  theme ( 
    axis.text.x = element_blank(),
    axis.text.y = element_text (size = 9),
    axis.title = element_text (size = 12),
    plot.title = element_text (size = 13),
    plot.margin=unit(c(1,1,1,1), 'mm'),
    legend.position = "none")

ggsave ("Figures/FigXD_driver_Indo.svg", width = 74, height = 70, units = "mm")




