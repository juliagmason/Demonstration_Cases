# Plot aggregate SAU allocative losses
# 3/22/23
# JGM

# separating from plot_allocative_losses_SAU

library (tidyverse)
library (stringr)

# function for converting catch in mt to children fed ----
source ("Scripts/Function_convert_catch_amt_children_fed.R")

sau_2019 <- readRDS ("Data/SAU_2019.Rds")


# mean of most recent 5 years *doesn't currently include indonesia* 
# just using this for peru anchovy correction
sau_2015_2019 <- readRDS("Data/SAU_2015_2019.Rds")


# fix peru 2019 anchovy issue
# messy hack, but replace Peru anchovy dhc value with 2018 value. in 2018, all artisanal was dhc and all industrial is fmfo
peru_anchov_dhc <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2018, species == "Engraulis ringens") %>%
  group_by (country, species, year) %>%
  summarise (prop_non_dhc = sum(tonnes[end_use_type == "Fishmeal and fish oil" & fishing_entity == "Peru"])/sum(tonnes[fishing_entity == "Peru"])) # 0.955

peru_anchov_total_2019 <- sau_2015_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru", year == 2019, species == "Engraulis ringens") %>%
  pull (tonnes) %>% sum()

# exports ARTIS ----
# placeholder for now; need full exports
# emailed 10 19 2022
exports <- read_csv ("Data/20221019_edf_ARTIS_snet.csv")

# additional Indo species emailed 1/28/23
exports_indo <- read_csv("Data/20230125_edf_ARTIS_indo.csv")

exports <- rbind (exports, exports_indo)

# take a five year mean of proportion exported
exports_5yr_mean <- exports %>%
  filter (between (year, 2015, 2019)) %>%
  group_by (exporter_iso3c, sciname) %>%
  summarise (mn_prop_exp = mean (exports_percent_of_prod, na.rm = TRUE)/100) %>%
  ungroup() %>%
  mutate (species = str_to_sentence(sciname),
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone"
          ), .keep = "unused") 


# plot exports and foreign fishing----

# assume foreign catch is separate from exports. that is, export proportions are for domestic catch

plot_trade_levers_aggregate <- function (country_name, Selenium = FALSE) {
 
   z <- sau_2019 %>%
    filter (country == country_name) %>%
    left_join (exports_5yr_mean, by = c("species", "country")) %>%
    replace_na (list(mn_prop_exp = 0)) %>%
    group_by (species) %>%
    summarise (foreign_catch = sum (tonnes[fishing_entity != country_name]),
               domestic_catch = sum (tonnes[fishing_entity == country_name]) * (1-mn_prop_exp),
               exported = sum (tonnes[fishing_entity == country_name]) * mn_prop_exp) %>%
    # creating many repeats, not sure why
     # warning about reframe()
    distinct () %>%
    # pivot longer
    pivot_longer (foreign_catch:exported,
                  names_to = "lever",
                  values_to = "catch_mt") %>%
    mutate (children_fed = pmap (list (species = species, amount = catch_mt, country_name = country_name), calc_children_fed_func)) %>%
    unnest (cols = c(children_fed)) %>%
    rename (mt = catch_mt)
  
  z$lever <- factor (z$lever, levels = c ("exported", "foreign_catch", "domestic_catch"))
  
  if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}
  
  w <- z %>%
    filter (!nutrient %in% omit_nutrients) %>%
    group_by (nutrient, lever) %>%
    summarise (children_fed = sum (children_fed, na.rm = TRUE)) %>%
    ggplot (aes (y = children_fed/1000000, x = reorder(nutrient, -children_fed), fill = lever)) +
    geom_col () +
    theme_bw()+
    scale_fill_grey(start = 0.8, end = 0.2) +
    labs (y = "Child RNIs met, millions", x = "", fill = "Policy lever") +
    ggtitle (paste0("Allocative losses, trade/foreign catch, ", country_name)) 
  
}


png ("Figures/Peru_trade_levers_aggregate.png", width = 5, height = 4, unit = "in", res = 300)

plot_trade_levers_aggregate("Peru") +    
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = c(0.8, 0.8))

dev.off()


png ("Figures/Indo_trade_levers_aggregate.png", width = 5, height = 4, unit = "in", res = 300)

 plot_trade_levers_aggregate("Indonesia") +    
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = "none")
dev.off()

png ("Figures/SL_trade_levers_aggregate.png", width = 5, height = 4, unit = "in", res = 300)

plot_trade_levers_aggregate("Sierra Leone") +    
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = c(0.8, 0.8))

dev.off()


png ("Figures/Chl_trade_levers_aggregate.png", width = 5, height = 4, unit = "in", res = 300)

plot_trade_levers_aggregate("Chile")  +   
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = c(0.8, 0.8))
dev.off()


# Plot end use types ----
# NOTE: 2019 SAU reconstruction no longer has discards, for any year. Discards were present in my previous download.

plot_end_use_levers_aggregate <- function (country_name, Selenium = FALSE) {
c <- sau_2019 %>%
  filter (country == country_name) %>%
  left_join (sau_2019_taxa, by = "species") %>%
  # mutate(case_when (is.nan(end_use_type) ~ "Discards",
  #                   TRUE ~ end_use_type)) %>%
  # 
  group_by (species, taxa, end_use_type) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  
  # mutate hack, fix peru anchovy. multiply total domestic anchov production * proportion non dhc from 2018
  mutate (catch_mt = case_when (
    country_name == "Peru" & species == "Engraulis ringens" & end_use_type == "Fishmeal and fish oil" ~ peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,
    country_name == "Peru" & species == "Engraulis ringens" & end_use_type == "Direct human consumption" ~ (1 - peru_anchov_dhc$prop_non_dhc) * peru_anchov_total_2019,
    TRUE ~ catch_mt),
    children_fed = pmap (list (species = species, amount = catch_mt, country_name = country_name), calc_children_fed_func)
    ) %>%
  unnest (cols = c(children_fed)) %>%
  rename (mt = catch_mt)

c$end_use_type <- factor (c$end_use_type, levels = c ("Fishmeal and fish oil", "Other", "Direct human consumption"))

if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}

c %>%
  filter (!nutrient %in% omit_nutrients, !is.na (end_use_type)) %>%
  group_by (nutrient, end_use_type) %>%
  summarise (children_fed = sum (children_fed, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(nutrient, -children_fed), y = children_fed/1000000, fill = end_use_type)) +
  geom_col () +
  theme_bw()+
  scale_fill_grey(start = 0.8, end = 0.2) +
  labs (y = "Child RNIs met, millions", x = "", fill = "Policy lever") +
  ggtitle (paste0("Allocative losses, end uses, ", country_name)) 

}

#, Selenium = TRUE

png ("Figures/Peru_end_use_levers_aggregate_Selenium.png", width = 5, height = 4, unit = "in", res = 300)

plot_end_use_levers_aggregate("Peru", Selenium = TRUE) +    
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = c(0.7, 0.8))
dev.off()


png ("Figures/Indo_end_use_levers_aggregate_Selenium.png", width = 5, height = 4, unit = "in", res = 300)

plot_end_use_levers_aggregate("Indonesia", Selenium = TRUE) +    
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = "none")
dev.off()


png ("Figures/SL_end_use_levers_aggregate_Selenium.png", width = 5, height = 4, unit = "in", res = 300)

plot_end_use_levers_aggregate("Sierra Leone", Selenium = TRUE) +   
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = "none")
dev.off()


png ("Figures/Chl_end_use_levers_aggregate_Selenium.png", width = 5, height = 4, unit = "in", res = 300)

plot_end_use_levers_aggregate("Chile", Selenium = TRUE) +    
  theme ( 
  axis.text.y = element_text (size = 11),
  axis.text.x = element_text (size = 11),
  axis.title = element_text (size = 16),
  legend.text = element_text (size = 11),
  legend.title = element_text (size = 13),
  plot.title = element_text (size = 18),
  legend.position = c(0.7, 0.7))
dev.off()

# Chile use ratios to connect to landings----
# take 5 yr mean of sau values
# I'm sure there's a way to do this all at once but it's hurting my brain

chl_sau_end_use_ratios <- 
  sau_2015_2019 %>%
  filter (country == "Chile") %>%
  group_by (fishing_sector, species, year) %>%
  summarise (prop_dhc = sum(tonnes[end_use_type == "Direct human consumption"])/sum (tonnes),
                        prop_fmfo = sum(tonnes[end_use_type == "Fishmeal and fish oil"])/sum (tonnes),
                        prop_other = sum(tonnes[end_use_type == "Other"])/sum (tonnes)
             ) %>%
  ungroup() %>%
  group_by (fishing_sector, species) %>%
  summarise (across(prop_dhc:prop_other, mean)) %>%
  rename (sector = fishing_sector)

chl_sau_foreign_ratios <- 
  sau_2015_2019 %>%
  filter (country == "Chile") %>%
  group_by (fishing_sector, species, year) %>%
  summarise (prop_foreign = sum(tonnes[fishing_entity != "Chile"])/sum(tonnes)) %>%
  ungroup() %>%
  group_by (fishing_sector, species) %>%
  summarise (prop_foreign = mean (prop_foreign)) %>%
  rename (sector = fishing_sector)

chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

x <- chl_landings %>%
  filter (year == 2021) %>%
  left_join (chl_sau_end_use_ratios, by = c ("species", "sector")) %>%
  pivot_longer (prop_dhc:prop_other,
                names_prefix = "prop_",
                names_to = "end_use_type",
                values_to = "prop") %>%
  mutate (catch_end_use = catch_mt * prop) 

x_nutr <- x %>%
  mutate (children_fed = pmap (list (species = species, amount = catch_end_use, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(children_fed)) %>%
  rename (mt = catch_end_use)

x_nutr$end_use_type <- factor (x_nutr$end_use_type, levels = c ("fmfo", "other", "dhc"))

x_nutr %>%
  group_by (nutrient, end_use_type) %>%
  summarise (children_fed = sum (children_fed, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(nutrient, -children_fed), y = children_fed/1000000, fill = end_use_type)) +
  geom_col () +
  theme_bw()+
  scale_fill_grey(start = 0.8, end = 0.2, labels = c ("Fishmeal and fish oil", "Other", "Direct human consumption")) +
  labs (y = "Child RNIs met, millions", x = "", fill = "Policy lever") +
  ggtitle ("Allocative losses, end uses, Chile") 

png ("Figures/Chl_end_use_levers_aggregate_officiallandings.png", width = 5, height = 4, unit = "in", res = 300)

x_nutr %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  group_by (nutrient, end_use_type) %>%
  summarise (children_fed = sum (children_fed, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(nutrient, -children_fed), y = children_fed/1000000, fill = end_use_type)) +
  geom_col () +
  theme_bw()+
  scale_fill_grey(start = 0.8, end = 0.2, labels = c ("Fishmeal and fish oil", "Other", "Direct human consumption")) +
  labs (y = "Child RNIs met, millions", x = "", fill = "Policy lever") +
  ggtitle ("Allocative losses, end uses, Chile")  +    
  theme ( 
    axis.text.y = element_text (size = 11),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 13),
    plot.title = element_text (size = 18),
    legend.position = c(0.7, 0.8))
dev.off()

# chl foreign and exports

z <- chl_landings %>%
  filter (year == 2021) %>%
  left_join (chl_sau_foreign_ratios, by = c ("species", "sector")) %>%
  mutate (country = "Chile",
          foreign_catch = catch_mt * prop_foreign,
          domestic = catch_mt * (1-prop_foreign)) %>%
  left_join (exports_5yr_mean, by = c("species", "country")) %>%
  replace_na (list(mn_prop_exp = 0)) %>%
  mutate (exported = domestic * mn_prop_exp,
          domestic_catch = domestic - exported) %>%
  select (species, sector, foreign_catch, domestic_catch, exported) %>%
  # pivot longer
  pivot_longer (foreign_catch:exported,
                names_to = "lever",
                values_to = "catch_mt") %>%
  mutate (children_fed = pmap (list (species = species, amount = catch_mt, country_name = "Chile"), calc_children_fed_func)) %>%
  unnest (cols = c(children_fed)) %>%
  rename (mt = catch_mt)

z$lever <- factor (z$lever, levels = c ("exported", "foreign_catch", "domestic_catch"))


png ("Figures/Chl_trade_levers_aggregate_officiallandings.png", width = 5, height = 4, unit = "in", res = 300)
z %>%  
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  group_by (nutrient, lever) %>%
  summarise (children_fed = sum (children_fed, na.rm = TRUE)) %>%
  ggplot (aes (x = reorder(nutrient, -children_fed), y = children_fed/1000000, fill = lever)) +
  geom_col () +
  theme_bw()+
  scale_fill_grey(start = 0.8, end = 0.2, labels = c ("Exported", "Foreign catch", "DHC")) +
  labs (y = "Child RNIs met, millions", x = "", fill = "Policy lever") +
  ggtitle ("Allocative losses, trade/foreign catch, Chile") +
  theme ( 
    axis.text.y = element_text (size = 11),
    axis.text.x = element_text (size = 11),
    axis.title = element_text (size = 16),
    legend.text = element_text (size = 11),
    legend.title = element_text (size = 13),
    plot.title = element_text (size = 18),
    legend.position = c(0.7, 0.8))
dev.off()
