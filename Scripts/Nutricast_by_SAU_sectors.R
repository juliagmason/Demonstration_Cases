## Nutricast projections for SSF species 
# 4/7/22
# JGM

library (tidyverse)

# projected nutritrient yield under management scenarios ----
# from Free data. Convert_catch_to_nutrients.R
ds_catch_nutr_yield_projected <- readRDS("Data/ds_catch_nutr_yield_projected.Rds")

# species by sector 
# from demonstration cases_sau_ssf_explore
# classified as ssf if >75% of catch from 2000-2015 was in artisanal. all dhc
sector_spp <- readRDS("Data/SAU_spp_by_sector_75cutoff.Rds")

nutr_yield_proj_sector <- ds_catch_nutr_yield_projected %>%
  left_join (sector_spp, by = c ("country", "species")) %>%
  filter (!is.na (spp_sector))

nutr_yield_proj_sector %>%
  group_by (country, spp_sector) %>%
  summarise (n_spp = length (unique (species)))

# do lose a lot of species; should check. 

# how much of the yield are we losing?
ds_catch_nutr_yield_projected %>%
  filter (scenario == "No Adaptation", rcp == "RCP26", year == 2050) %>%
  left_join (sector_spp, by = c ("country", "species")) %>%
  group_by (spp_sector, country) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE))
# chile a lot is mixed, that must be anchovy. NAs are maybe comparable to industrial scale in most cases. so maybe a quarter or a third, but not the vast majority. 

# projected population data ----
# 3 periods
pop_future <- readRDS("Data/country_pop_projections.Rds")

# projected nutrient yield ---
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


rdas_met_proj_sector <- ds_pop_proj_RDAS_met %>%
  left_join (sector_spp, by = c ("country", "species")) %>%
  filter (!is.na (spp_sector))

rdas_met_proj_sector %>% 
  group_by (country, spp_sector) %>%
  summarise (n_spp = length (unique (species)))

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
  filter (spp_sector == "Industrial", country == "Chile", !grepl("Imperfect", scenario)) %>%
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

