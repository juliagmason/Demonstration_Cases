# Plot aggregate nutrient upsides
# 3/21/23
# JGM

library (tidyverse)

# catch upside relative, from calculate_nutritional_upsides.r
# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")

# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")

# landings data ----
# Chile country specific 
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

  

# SAU data 

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds") 


# Chile ----

nutricast_chile <- catch_upside_relative %>% filter (country == "Chile")
# lose 10 species from 2021 data...should be more
View (sort(unique (nutricast_chile$species)))
unique (chl_landings$species[which(!chl_landings$species %in% nutricast_chile$species)])

upside_ratios_chl <- chl_landings %>%
  filter (year == 2021) %>%
  group_by (species, taxa) %>%
  summarise (total_tonnes = sum (catch_mt)) %>%
  mutate (country = "Chile") %>%
  left_join(catch_upside_relative, by = c ("country", "species")) %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (country, rcp, species, taxa, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") %>%
  # get rid of non-matching species, NAs
  filter (!is.na (rcp)) %>%
  # convert to nutrients
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique")

upside_ratios_chl$upside <- factor(upside_ratios_chl$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))

# try without grouping by species--this looks weird, has bars that cross zero

upside_ratios_chl %>%
  filter (upside %in% c("mey_2050", "adapt_2050"), !nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (x = nutrient, y = children_fed/1000000)) +
  geom_col () +
  geom_hline (yintercept = 0, lty = 2) +
  facet_grid (upside ~ rcp, scales = "free_y") +
  theme_bw() +
  labs (y = "Change in # children fed, millions", x = "") +
  ggtitle ("Production upside from climate-adaptive management, Chile") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 14),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 


upside_ratios_chl %>%
  group_by (rcp, upside, nutrient) %>%
  summarise (total_fed = sum (children_fed, na.rm = TRUE)) %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          upside %in% c("mey_2050", "adapt_2050")) %>%
  ggplot (aes (x = nutrient, y = total_fed/1000000, fill = upside)) +
  geom_col (position = "dodge") +
  geom_hline (yintercept = 0, lty = 2) +
  facet_wrap ( ~ rcp) +
  theme_bw() +
  labs (y = "Change in # children fed, millions", x = "", fill = "Management \nstrategy") +
  ggtitle ("Production upside from climate-adaptive management, Chile") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 14),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 

upside_ratios_chl %>%
  group_by (rcp, upside, nutrient) %>%
  summarise (total_fed = sum (children_fed, na.rm = TRUE)) %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          upside %in% c("mey_2050", "adapt_2050")) %>%
  ggplot (aes (x = nutrient, y = total_fed/1000000, fill = rcp)) +
  geom_col (position = "dodge") +
  geom_hline (yintercept = 0, lty = 2) +
  facet_wrap ( ~ upside) +
  theme_bw() +
  labs (y = "Change in # children fed, millions", x = "") +
  ggtitle ("Production upside from climate-adaptive management, Chile") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 14),
         axis.title = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12)) 

png ("Figures/Chile_nutricast_upside_overall.png", width = 10, height = 8, units= "in", res = 300)

dev.off()

# Peru ----
nutricast_peru <- catch_upside_relative %>% filter (country == "Peru")

upside_ratios_peru <- sau_2019 %>%
  filter (country == "Peru") %>%
  left_join(sau_2019_taxa, by = "species") %>%
  group_by (country, species, taxa) %>%
  summarise (total_tonnes = sum (tonnes)) %>%
  left_join(catch_upside_relative, by = c ("country", "species")) %>%
  mutate (# multiply ratio by current landings
    across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
    #convert to upside, subtract 
    mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
    mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
    adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
    adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
  
  select (country, rcp, species, taxa, mey_2050:adapt_2100) %>%
  pivot_longer(mey_2050:adapt_2100, 
               names_to = "upside",
               values_to = "tonnes") %>%
  # get rid of non-matching species, NAs
  filter (!is.na (rcp)) %>%
  # convert to nutrients
  mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique")

upside_ratios_peru$upside <- factor(upside_ratios_peru$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))

png ("Figures/Peru_nutricast_upside_overall.png", width = 6, height = 5, units= "in", res = 300)
upside_ratios_peru %>%
  group_by (rcp, upside, nutrient) %>%
  summarise (total_fed = sum (children_fed, na.rm = TRUE)) %>%
  filter (!nutrient %in% c("Protein", "Selenium"),
          upside %in% c("mey_2050", "adapt_2050")) %>%
  ggplot (aes (x = nutrient, y = total_fed/1000000, fill = upside)) +
  geom_col (position = "dodge") +
  geom_hline (yintercept = 0, lty = 2) +
  facet_wrap ( ~ rcp) +
  theme_bw() +
  labs (y = "Change in # children fed, millions", x = "", fill = "Management \nstrategy") +
  ggtitle ("Nutrition upside from climate-adaptive management, Peru") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 11),
         axis.text.x = element_text (angle = 60, hjust = 1),
         strip.text.x =  element_text (size = 12),
         axis.title = element_text (size = 16),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 11)) 
dev.off()