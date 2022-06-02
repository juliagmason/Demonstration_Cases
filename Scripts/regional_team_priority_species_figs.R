# Chile priority species
# 5/27/22
library (tidyverse)
library (stringr)

# nutrient data and rda data
rda_groups <- readRDS("Data/RDAs_5groups.Rds")
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu"))

# SAU data
# also just want top ssf spp for each country
sau <- read.csv ("Data/SAU_EEZ_landings.csv")

# full sau nutr data
sau_nutr <- readRDS("Data/SAU_nutr_content_sector_enduse.Rds")

# nutricast full data
ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")


# indonesia team ----
# focus on skipjack tuna
sau_indo <- sau %>%
  filter (grepl("Indo", area_name), between (year, 2000, 2015)) %>%
  mutate (country = "Indonesia") %>%
  rename (species = scientific_name)

indo_spp_nutr <-  fishnutr_mu %>%
  filter (species %in% sau_indo$species)  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup()

spp_nutr_density <- spp_nutr %>% 
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  select (species, nutrient, perc_rda) %>%
  distinct() %>%
  group_by (species) %>%
  summarise (micronutrient_density = sum (perc_rda)) %>%
  filter (species %in% sau_indo$species)

# skipjack landings
png ("Figures/Indo_skipjack_landings.png", res = 300, width = 4, height = 5, units = "in")  
sau_indo %>%
  filter (species == "Katsuwonus pelamis") %>%
  ggplot (aes (x = year, y = tonnes/1000, fill = species)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual (values = c("dodgerblue3")) +
  labs (x = "", y = "Catch, thousand tons") +
  ggtitle ("Skipjack tuna catch in Indonesia's EEZ \n Sea Around Us project data") +
  theme (
    axis.title = element_text (size = 12),
    axis.text = element_text (size = 10),
    plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# foreign vs. domestic
skipjack_country <- sau_indo %>%
  filter (species == "Katsuwonus pelamis") %>%
  mutate (fishing_country = ifelse (fishing_entity == "Indonesia", "Domestic catch", "Foreign catch"))
skipjack_country$fishing_country <- factor (skipjack_country$fishing_country, levels = c("Foreign catch", "Domestic catch"))

png ("Figures/Indo_skipjack_landings_domestic.png", res = 300, width = 5, height = 5, units = "in")  
skipjack_country %>%
  filter (species == "Katsuwonus pelamis") %>%
  ggplot (aes (x = year, y = tonnes/1000, fill = fishing_country)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual (values = c("gray70", "dodgerblue3")) +
  labs (x = "", y = "Catch, thousand tons", fill = "") +
  ggtitle ("Skipjack tuna catch in Indonesia's EEZ \n Sea Around Us project data") +
  theme (
    axis.title = element_text (size = 12),
    axis.text = element_text (size = 10),
    plot.title = element_text (size = 14)
  )
dev.off()



# skipjack as nutrients, just 2015
png ("Figures/Indo_skipjack_nutr.png", res = 300, width = 4, height = 5, units = "in")
sau_nutr %>%
  filter (country %in% c("Indonesia"), species == "Katsuwonus pelamis", year == 2015) %>%
  group_by (species, nutrient) %>%
  summarise (tot_servings = sum (nutr_servings)) %>%
  left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (needs_met = tot_servings / mean_rda) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  
  ggplot (aes (x = nutrient, y = needs_met/1000000, fill = species)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
  ggtitle ("# children's daily intake needs met, 2015") +
  ylim(c(0, 3.25)) +
  scale_fill_manual (values = c("dodgerblue3")) +
  theme (
    axis.title = element_text (size = 12),
    axis.text = element_text (size = 10),
    plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# domestic skipjack as nutrients, just 2015
png ("Figures/Indo_skipjack_nutr_domestic.png", res = 300, width = 4, height = 5, units = "in")
sau_nutr %>%
  filter (country %in% c("Indonesia"), species == "Katsuwonus pelamis", year == 2015, fishing_entity == "Indonesia") %>%
  group_by (nutrient, species) %>%
  summarise (tot_servings = sum (nutr_servings)) %>%
  left_join (filter (rda_groups, group == "Child"), by = "nutrient") %>%
  mutate (needs_met = tot_servings / mean_rda) %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  
  ggplot (aes (x = nutrient, y = needs_met/1000000, fill = species)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  ylim(c(0, 3.25)) +
  labs (x = "Nutrient", y = "Children fed, millions", fill = "Sector") +
  ggtitle ("# children's daily intake needs met, 2015\n Domestic catch") +
  scale_fill_manual (values = c("dodgerblue3")) +
  theme (
    axis.title = element_text (size = 12),
    axis.text = element_text (size = 10),
    plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

# nutricast time series
indo_kp <- ds_spp %>%
  filter (country == "Indonesia", species == "Katsuwonus pelamis", rcp == "RCP60", scenario %in% c("No Adaptation", "Full Adaptation"))

png ("Figures/Indo_skipjack_nutricast_ts.png", res = 300, width = 4, height = 5, units = "in")
indo_kp %>%
  filter (year > 2025) %>%
  ggplot (aes (x = year, y = catch_mt/1000, col = scenario)) +
  geom_line(lwd = 1.5) +
  theme_bw() +
  labs (x = "", y = "Catch, 1000 tonnes", fill = "Management\n scenario") +
  ggtitle ("Projected skipjack tuna catch\nClimate scenario: RCP 6.0") +
  theme (
    axis.title = element_text (size = 12),
    axis.text = element_text (size = 10),
    plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()

png ("Figures/Indo_skipjack_nutrient_upside.png", res = 300, width = 4, height = 5, units = "in")
proj_inds_fed_sector %>%
  filter (group == "Child", country =="Indonesia", rcp == "RCP60", scenario %in% c("No Adaptation", "Full Adaptation"), period == "2050-2060", !nutrient %in% c("Protein", "Selenium"), species =="Katsuwonus pelamis") %>%
  # dumb but group by sector and calculate difference
  mutate (tot_fed = inds_fed_industrial + inds_fed_ssf) %>%
  group_by (nutrient) %>%
  summarise (diff_fed = tot_fed[scenario == "Full Adaptation"] - tot_fed[scenario == "No Adaptation"]) %>%
  ggplot (aes (x = nutrient, y = diff_fed/1000, fill = nutrient)) +
  geom_bar (stat = "identity", position = "dodge") +
  scale_fill_manual (values = rep("dodgerblue3", 5)) +
  theme_bw() +
  labs (x = "Nutrient", y = "Additional children fed, thousands", fill = "Management\n scenario") +
  ggtitle ("Additional nutrient needs met by \nimplementing adaptive management,\nSkipjack tuna, 2050-2060, RCP 6.0") +
  theme (
    axis.title = element_text (size = 12),
    axis.text = element_text (size = 10),
    plot.title = element_text (size = 14),
    legend.position = "none"
  )
dev.off()
  

# Chile team ----
nutricast_chile <- ds_spp %>% 
  filter (country == "Chile")

nutricast_chile_baseline <- nutricast_chile %>%
  filter (year %in% c(2012:2020), catch_mt > 0, rcp == "RCP26", scenario == "No Adaptation") %>%
  group_by (species) %>%
  summarise (baseline_catch = mean (catch_mt, na.rm = TRUE))


upside_nonzero <- readRDS("Data/nutricast_catch_upside.Rds")

chl_upside <- upside_nonzero %>%
  filter (country == "Chile", period == "2050-2060", rcp == "RCP60")


chl_spp_key <- read.csv ("Data/Chile_spp_names_key.csv")
# only has fishes; colnames not cooperating
colnames (chl_spp_key)[1] <- "common_name"

# landings data from sergio, 2021
chl_landings_artesanal <- read.csv ("Data/2021_030202_desembarque_artesanal_por_region_Chile.csv", skip = 5, header = T) %>%
  select (ESPECIE, Total) %>%
  rename (common_name = ESPECIE) %>%
  left_join (chl_spp_key, by = "common_name") %>%
  as_tibble()

View (chl_landings_artesanal)

chl_landings_data_compare <- chl_landings_artesanal %>%
  filter (!grepl("TOTAL", common_name)) %>%
  arrange (desc (Total)) %>%
  mutate (species = case_when (
    species == "Merluccius gayi" ~ "Merluccius gayi gayi", 
    common_name == "Jibia O Calamar Rojo" ~ "Dosidicus gigas",
    common_name == "Erizo" ~ "Loxechinus albus",
    TRUE ~ species),
    Nutrient_data = ifelse (species %in% fishnutr_mu$species, "Yes", "No"),
          Nutricast_data = ifelse (species %in% nutricast_chile$species, "Yes", "No")) %>%
  left_join (nutricast_chile_baseline, by = "species") %>%
  left_join (chl_upside, by = "species")

write.excel (chl_landings_data_compare)

# SAU data
sau_chl <- sau %>%
  filter (area_name == "Chile (mainland)", between (year, 2000, 2015)) %>%
  mutate (country = "Chile")

# join to nutrient content
chl_spp_nutr <- fishnutr_mu %>%
  filter (species %in% sau_chl$scientific_name)  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4)) %>%
  # join to rda data
  left_join (filter(rda_groups, group == "Child"), by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup()

# First, radar plots of their desired species nutrient content----
library (ggradar)

  
  # filter nutrient content data
  nutr_radar_plot <- chl_spp_nutr %>%
    filter (species %in% c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus"), !nutrient %in% c("Protein", "Selenium")) %>%
    select (species, nutrient, perc_rda) %>%
    pivot_wider (
      names_from = nutrient,
      values_from = perc_rda) %>%
    # radar plot can't deal with NA and non-fish don't have selenium
    replace_na (list (Selenium = 0)) 
  
  # set levels so colors stay the same
  nutr_radar_plot$species <- factor (nutr_radar_plot$species, levels = c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus"))
  

  png ("Figures/Chl_radar_reg_tm_spp.png", res = 300, width = 8, height = 5, units = "in")  
  ggradar(nutr_radar_plot,
          grid.min = 0, grid.max = 100, 
          group.point.size = 2,
          group.line.width = 1,
          legend.text.size = 8,
          axis.label.size = 4,
          grid.label.size = 4,
          legend.position = "right") +
    ggtitle ("Child's daily nutrition needs from one serving") +
    theme (plot.title = element_text (size = 14))
  dev.off()
  
# compare to anchovy and jurel
  
  # filter nutrient content data
  nutr_radar_plot_anchov <- chl_spp_nutr %>%
    filter (species %in% c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"), !nutrient %in% c("Protein", "Selenium")) %>%
    select (species, nutrient, perc_rda) %>%
    pivot_wider (
      names_from = nutrient,
      values_from = perc_rda) %>%
    # radar plot can't deal with NA and non-fish don't have selenium
    replace_na (list (Selenium = 0)) 
  
# set levels so colors stay the same
  nutr_radar_plot_anchov$species <- factor (nutr_radar_plot_anchov$species, levels = c ("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"))

  
  png ("Figures/Chl_radar_reg_tm_spp_anchov_ref.png", res = 300, width = 8, height = 5, units = "in")  
  ggradar(nutr_radar_plot_anchov,
          grid.min = 0, grid.max = 100, 
          group.point.size = 2,
          group.line.width = 1,
          legend.text.size = 8,
          axis.label.size = 4,
          grid.label.size = 4,
          legend.position = "right") +
    ggtitle ("Child's daily nutrition needs from one serving") +
    theme (plot.title = element_text (size = 14))
  dev.off()
  
  # display nutr density
  chl_spp_nutr %>%
    filter (species %in% c("Merluccius gayi gayi", "Merluccius australis", "Genypterus maculatus", "Engraulis ringens", "Trachurus murphyi"), !nutrient %in% c("Protein", "Selenium")) %>%
    select (species, nutrient, perc_rda) %>%
    distinct() %>%
    group_by (species) %>%
    summarise (micronutrient_density = sum (perc_rda))
  
  
  # overall chl sau----
  # boxplot of micronutrient density of export, industrial, end use?
  # show avg nutr content of each too?
sau_chl %>%
  