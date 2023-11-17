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

png("Figures/Sankey_SL_foreign_exports.png", width = 8, height = 6, units = "in", res = 300)
sl_foreign_export_nutr_comb %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = sector,
               axis3 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "sector", "exports"), expand = c(.15, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_flow(aes(fill = nutrient)) +
  geom_stratum(aes(fill = nutrient)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text (stat = "flow", nudge_x = 0.2, aes (label = round(rni_equivalents/1000000, 1))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Sierra Leone") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

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

# Indonesia ----

# exports
indo_export_nutr <- readRDS("Data/levers_RNI_pop_export_Indo.Rds")

# plot
png("Figures/Sankey_Indo_exports.png", width = 8, height = 6, units = "in", res = 300)
indo_export_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000,
               fill = nutrient)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.02, .05)) +
  labs(y = "Child RNI equiv., millions", x = "Allocation levers") +
  geom_alluvium() +
  geom_stratum(fill = "gray70") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) +
  #geom_text(stat = "stratum", aes(label = after_stat(round(prop, 2)))) +
  scale_fill_viridis_d() +
  theme_minimal() +
  ggtitle(paste0("National allocative lever: Exports")) +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         plot.title = element_text (size = 18),
         legend.position = "none")
dev.off()

p <- indo_export_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  mutate (nutrient = case_when (
    nutrient == "Omega_3" ~ "Omega 3",
    nutrient == "Vitamin_A" ~ "Vit. A",
    TRUE ~ nutrient)) %>%
  ggplot (aes (axis1 = nutrient,
               axis2 = exports,
               y = rni_equivalents/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.05, .05)) +
  labs(y = "Child RNI equiv., millions", x = "Allocation levers") +
  geom_alluvium(aes(fill = nutrient)) +
  geom_stratum() +
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

ggsave ("Figures/FigXD_driver_Indo.svg", width = 74, height = 70, units = "mm",)




