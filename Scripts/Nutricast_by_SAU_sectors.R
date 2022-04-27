## Nutricast projections for SSF species 
# 4/7/22
# JGM

library (tidyverse)

# projected nutritrient yield under management scenarios ----
# in terms of RDAs met
ds_projected_RDAS_met <- readRDS("Data/ds_projected_RDAs_met.Rds")

#summarise by whole population
ds_pop_proj_RDAS_met <- ds_projected_RDAS_met %>%
  group_by (country, rcp, scenario, period, nutrient, species) %>%
  mutate (tot_pop = sum(population),
          tot_rda_needs = sum(rda_needs),
          tot_nutr_servings = first (nutr_servings),
          tot_rda_met = nutr_servings / tot_rda_needs) %>%
  select (country, rcp, scenario, period, species, nutrient, tot_rda_met) %>%
  distinct()


# SAU direct human consumption data with nutrients ----
# this is 2000-2015, direct human consumption only
sau_dhc_nutr <- readRDS("Data/SAU_nutr.Rds")

# for sau spp, calculate proportion by volume for each country caught in SSF
# I want three columns, country, species, ssf proportion. 
# do both ssf and industrial just in case
sau_sector_prop <- sau_dhc_nutr %>%
  group_by (country, species) %>%
  summarise (prop_ssf = sum (tonnes_tot[fishing_sector == "Artisanal"]) / sum (tonnes_tot),
             prop_ind = sum (tonnes_tot[fishing_sector == "Industrial"]) / sum (tonnes_tot))


# subset projected nutrient yield by ssf ----
proj_RDA_met_sector <- ds_pop_proj_RDAS_met %>%
  left_join (sau_sector_prop, by = c ("country", "species")) %>%
  filter (!is.na (prop_ssf)) %>%
  mutate (tot_rda_met_ssf = tot_rda_met * prop_ssf,
          tot_rda_met_ind = tot_rda_met * prop_ind)


# set levels
proj_RDA_met_sector$scenario <- factor (proj_RDA_met_sector$scenario, levels = c ("No Adaptation", "Imperfect Productivity Only", "Productivity Only", "Range Shift Only", "Imperfect Full Adaptation","Full Adaptation" ))



# plot all countries
png (filename = "Figures/SSF_nutricast_rcp60.png", width = 6.5, height = 6, units = "in", res = 360)
proj_RDA_met_sector %>%
  filter (!grepl("Imperfect", scenario), !grepl ("Range", scenario), rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (sum_rda = sum (tot_rda_met_ssf)) %>%
  ggplot (aes (y = sum_rda, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (country ~ nutrient, scales = "free") +
  theme_bw() +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9, size = 8),
         axis.text.y = element_text (size = 8),
         legend.text = element_text (size = 8),
         plot.title = element_text (size = 10),
         strip.text = element_text (size = 8)) +
  
  labs(y="Proportion RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of RDAs met by SSF under future management scenarios \n RCP 6.0")
dev.off()

proj_RDA_met_sector %>%
  filter (!grepl("Imperfect", scenario), !grepl ("Range", scenario), rcp == "RCP60") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (sum_rda = sum (tot_rda_met_ind)) %>%
  ggplot (aes (y = sum_rda, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (country ~ nutrient, scales = "free") +
  theme_bw() +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9)) +
  labs(y="Proportion RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of RDAs met by industrial fisheries under future management scenarios \n RCP 6.0")






















#########################################################################################
# sanity check, what kind of values are we working with overall
y <- ds_projected_RDAS_met %>%
  filter (scenario == "Full Adaptation", rcp == "RCP26", period == "2025-2035") %>%
  group_by (country, nutrient, species) %>%
  mutate (tot_pop = sum(population),
          tot_rda_needs = sum(rda_needs),
          tot_nutr_servings = first (nutr_servings),
          tot_rda_met = nutr_servings / tot_rda_needs) %>%
  # get rid of groups, otherwise triple counting
  select (country, rcp, scenario, period, species, nutrient, tot_rda_met) %>%
  distinct()

y %>%
  group_by (country, nutrient) %>%
  summarise (sum_rda = sum (tot_rda_met))

y_sector <- y %>%
  left_join (sector_spp, by = c ("country", "species")) %>%
  group_by (country, nutrient, spp_sector) %>%
  summarise (sum_rda = sum (tot_rda_met))

y_pop <- rdas_met_proj_sector %>%
  filter (scenario == "Full Adaptation", rcp == "RCP26", period == "2025-2035")

y_pop %>% group_by (country, nutrient, spp_sector) %>%
  summarise (sum_rda = sum (tot_rda_met)) 
# same as y_sector

rdas_met_proj_sector %>%
  filter (spp_sector == "Industrial") %>%
  filter (country == "Chile", !grepl("Imperfect", scenario)) %>%
  ggplot (aes (y = tot_rda_met, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (nutrient ~ rcp, scales = "free") +
  theme_bw() +
  labs(y="Percent of RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of population RDAs met under future management scenarios \n Species caught in SSF")

y_sector %>%
  filter (spp_sector == "Industrial", country == "Chile") %>%
  mutate (period = "2025", rcp = "RCP26", scenario = "Full Adaptation") %>%
  ggplot (aes (y = sum_rda, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (nutrient ~ rcp, scales = "free") +
  theme_bw() +
  labs(y="Percent of RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of population RDAs met under future management scenarios \n Species caught in SSF")
  

# different! why...
# sum makes more sense to me...show that because at least I know what went in it?
rdas_met_proj_sector %>%
  filter (spp_sector == "SSF", country == "Chile", !grepl("Imperfect", scenario)) %>%
  group_by (rcp, scenario, period, nutrient) %>%
  summarise (sum_rda = sum (tot_rda_met)) %>%
  ggplot (aes (y = sum_rda, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (nutrient ~ rcp, scales = "free") +
  theme_bw() +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9)) +
  labs(y="Percent of RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of Chile's population RDAs met under future management scenarios \n Species caught in SSF")

rdas_met_proj_sector %>%
  filter (spp_sector == "Industrial", country == "Chile", !grepl("Imperfect", scenario), !grepl ("Range", scenario)) %>%
  group_by (rcp, scenario, period, nutrient) %>%
  summarise (sum_rda = sum (tot_rda_met)) %>%
  ggplot (aes (y = sum_rda, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (nutrient ~ rcp, scales = "free") +
  theme_bw() +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9)) +
  labs(y="Percent of RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of Chile's population RDAs met under future management scenarios \n Species caught in Industrial fisheries")
  





############################
# this is sorting out grouping 

ssf_rdas <- rdas_met_proj_sector %>%
  filter (spp_sector == "SSF") %>%
  group_by (country, rcp, scenario, period, nutrient, species) %>%
  mutate (tot_pop = sum(population),
          tot_rda_needs = sum(rda_needs),
          tot_nutr_servings = first (nutr_servings),
          tot_rda_met = nutr_servings / tot_rda_needs) %>%
  # get rid of groups, otherwise triple counting
  select (country, rcp, scenario, period, species, nutrient, tot_rda_met) %>%
  distinct()

x <- ssf_rdas %>% filter (country == "Chile", rcp == "RCP26", period == "2025-2035", scenario == "No Adaptation")
x %>% group_by (nutrient) %>% summarise (tot_rda = sum (tot_rda_met))

indus_rdas <- rdas_met_proj_sector %>%
  filter (spp_sector == "Industrial") %>%
  group_by (country, rcp, scenario, period, nutrient, species) %>%
  mutate (tot_pop = sum(population),
          tot_rda_needs = sum(rda_needs),
          tot_nutr_servings = first (nutr_servings),
          tot_rda_met = nutr_servings / tot_rda_needs) %>%
  # get rid of groups, otherwise triple counting
  select (country, rcp, scenario, period, species, nutrient, tot_rda_met) %>%
  distinct()

ssf_rdas %>% 
  filter (country == "Chile", !grepl("Imperfect", scenario)) %>%
  ggplot (aes (y = tot_rda_met, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (nutrient ~ rcp, scales = "free") +
  theme_bw() +
  labs(y="Percent of RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of population RDAs met under future management scenarios \n Species caught in SSF")

# seems SO small. reality check...is it because we're losing so many species??
c <- ssf_rdas %>% filter (country == "Chile", rcp == "RCP26", scenario == "No Adaptation")
c %>% group_by (period, nutrient) %>% summarise (tot_rda = sum (tot_rda_met))

indus_rdas %>%
  filter (country == "Sierra Leone", !grepl("Imperfect", scenario)) %>%
  ggplot (aes (y = tot_rda_met, x = period, fill = scenario)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_grid (nutrient ~ rcp, scales = "free") +
  theme_bw() +
  labs(y="Percent of RDAs met", x="", fill = "Management\nscenario") +
  ggtitle ("Proportion of population RDAs met under future management scenarios \n Species caught in industrial fisheries")

# instead show individuals fed?

