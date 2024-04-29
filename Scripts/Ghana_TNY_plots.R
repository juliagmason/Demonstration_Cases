# Ghana TNY plots #
# 4/29/24

# for EDF workshop june 2024
library (tidyverse)

# downloaded 4/29/2024
sau_ghana <- read.csv ("Data/SAU EEZ ghana.csv") %>%
  filter (year >= 2000) %>%
  rename (species = scientific_name, country = area_name) %>%
    # lump foreign and domestic 
  mutate(
    fleet = ifelse (fishing_entity == country, "Domestic catch", "Foreign catch")
  ) %>%
  
  group_by (country, species, year, fishing_sector, fleet, commercial_group, end_use_type) %>%
  summarise (tonnes = sum(tonnes))


sau_ghana %>%
  # reduce # groups
  mutate (commercial_group = case_when(
    commercial_group %in% c ("Cod-likes", "Flatfishes", "Scorpionfishes") ~ "Other fishes & inverts",
    TRUE ~ commercial_group)
  ) %>%
  group_by(year, commercial_group) %>%
  summarise (tonnes = sum (tonnes)) %>%
  
  ggplot (aes (x = year, y = tonnes/1000000)) +
  geom_area(aes (fill = commercial_group), position = "stack") +
  labs (y ="Catch, million tonnes", x = "", fill = "Commercial group")+
  theme_bw() +
  theme (axis.text.y = element_text (size = 8),
         axis.text.x = element_text (size = 7),
         axis.title = element_text (size = 9),
         plot.title = element_text (size = 12),
         # legend.text = element_text (size = 11),
         # legend.title = element_text (size = 12),
         # legend.key.size = unit (3.5, "mm"),
         # legend.margin=margin(1,1,1,2),
         # legend.box.margin=margin(-10,-10,-10,-10),
         #legend.position = "none",
         plot.margin=unit(c(1,1,1,1), 'mm')) +
  
  
  # qualitative color scale for species, Dark1
  scale_fill_brewer(palette = "Dark2") +
  ggtitle ("Catch in Ghana's EEZ, Sea Around Us")
ggsave ("Figures/Ghana_SAU_catch.png", width = 74, height = 60, units = "mm")

## total nutrient yield
# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")

ghana_tny <- sau_ghana %>%
  filter (year == 2019) %>%
group_by (species, commercial_group) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  mutate (rni_equivalents = pmap (list (species = species, amount = catch_mt, country_name = "Ghana"), calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique")

# standardize color scale, dark1
landings$commercial_group <- factor (landings$commercial_group, levels = c("Anchovies", "Crustaceans","Herring-likes", "Molluscs", "Other fishes & inverts", "Perch-likes","Sharks & rays", "Tuna & billfishes") )


ghana_tny %>%
  
  filter (!nutrient %in% c("Protein")) %>%
  group_by (nutrient, commercial_group) %>%
  summarize (rni_equivalents = sum (rni_equivalents, na.rm = TRUE)) %>%
  # just have alphabetical so nutrients are always in the same order
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = commercial_group)) +
  geom_col() +
  theme_bw() +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Comm. group") +
  scale_fill_brewer(palette = "Dark2") +
  guides (fill = "none") +
  # abbreviate nutrient names
  scale_x_discrete (labels = c ("Calcium", "Iron", "Omg. 3", "Selenium", "Vit. A", "Zinc" )) +
  ggtitle ("Total nutrient yield") +
  labs (y = "Child RNI equiv., millions") +
  theme (axis.text.y = element_text (size = 8),
         axis.text.x = element_text (size = 7),
         axis.title = element_text (size = 9),
         plot.title = element_text (size = 12),
         plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/Ghana_TNY_2019.png", width = 74, height = 60, units = "mm")


# nutrient dodged bar----
# bring in compiled instead; compile_species_nutrition_data.R
compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")


# RNI data 
# from RNI_explore; WHO
rni_child <- readRDS("Data/RNI_child.Rds")


# join nutrients and rni
spp_nutr <-  compiled_nutr %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rni = amount/RNI * 100,
          # optional--cap at 100?
          #perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          # shorten nutrient names so they fit on the x axis
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup()


# function to plot 
plot_colorful_spp_nutr_dodge_bar <- function (species_names, Selenium = FALSE) {
  
  #species_names is a vector of scientific names
  
  if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}
  
  spp_nutr %>%
    filter (!nutrient %in% omit_nutrients, 
            species %in% species_names) %>%
    group_by (species) %>%
    # order by overall nutrient density, from Maire et al. 2021
    mutate (micronutrient_density = sum (perc_rni),
            # shorten spp names
            # https://stackoverflow.com/questions/8299978/splitting-a-string-on-the-first-space
            spp_short = ifelse (
              grepl(" ", species),
              paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
              species),
            #cutoff at 100
            perc_rni = ifelse (perc_rni >= 100, 100, perc_rni)
    ) %>%
    ungroup() %>%
    
    ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
    geom_col (position = "dodge") +
    theme_bw() +
    labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") +
    #ylim (c(0,100)) +
    ggtitle ("Nutrient content of selected species") +
    theme ( 
      axis.text.y = element_text (size = 8),
      axis.text.x = element_text (size = 7),
      axis.title = element_text (size = 9),
      plot.title = element_text (size = 12),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 8),
      legend.title = element_text (size = 9))
}

ghana_top_spp <- sau_ghana %>%
  filter (year > 2015) %>%
  group_by (species) %>%
  summarize (tot_mt = sum (tonnes)) %>%
  slice_max(tot_mt, n= 6)

plot_colorful_spp_nutr_dodge_bar (ghana_top_spp$species, Selenium = FALSE) +
  theme (legend.key.size = unit (3.5, "mm"))
ggsave ("Figures/Ghana_nutr_content.png", width = 100, height = 60, units = "mm")


# foreign sankey ----
library (ggalluvial)
# country nutrient demand data 
# from calculate_population_percent_nutrition...R
wpp_country_aggregate <- readRDS("Data/annual_nutr_demand_rni_by_country.Rds")

# function for converting catch in mt to tonnes of nutrient, for pop needs
# from calculate_population_percent_nutrition.R
convert_catch_to_nutr_tons <- function (species_name, catch_mt, country_name) {
  
  if (species_name %in% compiled_nutr$species) {
    nutr_content <- compiled_nutr %>% filter (species == species_name)
  } else  {
    nutr_content <- fish_taxamatch_nutr %>% filter (species == species_name, country == country_name)
  }
  
  catch_nutrients <- nutr_content %>%
    mutate (
      p_edible = case_when (
        taxa == "Finfish" ~ 0.87,
        taxa == "Crustacean" ~ 0.36,
        taxa == "Mollusc" ~ 0.17,
        # GENuS/nutricast is 0.21 for cephalopods. Using 0.67, Bianchi et al. 2022 value for D. gigas; only cephalopod in our priority species. They also have a blanket 0.7 value for cephalopods.  
        taxa == "Cephalopod" & species == "Dosidicus gigas" ~ 0.67,
        taxa == "Cephalopod" & species != "Dosidicus gigas" ~ 0.21,
        taxa == "Other" ~ 1,
        taxa == "Algae" ~ 1),
      # have to put back into units
      scalar = case_when (
        nutrient %in% c("Protein", "Omega_3") ~ 1,
        nutrient %in% c("Calcium", "Zinc", "Iron") ~ 1/1000,
        nutrient %in% c("Vitamin_A", "Selenium") ~ 1/1e6
      ),
      # input (catch_mt) is in metric tons. amount is in units / 100g. so divide by 100 to account for serving size, and multiply by scalar to cancel out g
      nutr_tonnes = catch_mt * p_edible * amount * scalar / 100 ) %>%
    select (nutrient, nutr_tonnes)
  
  return (catch_nutrients)
}

foreign_sector_vol <- sau_ghana %>%
  filter (year == 2019) %>%
  # make sector category with foreign, artisanal, industrial 
  mutate (sector = ifelse (fleet == "Foreign catch", "Foreign catch", fishing_sector),
          # group subsistence into artisanal
          sector = ifelse (sector =="Subsistence", "Artisanal", sector)) %>%
  ungroup() %>%
  select (country, species, sector, tonnes)

# calculate RNI equivalents and percent population needs met
foreign_sector_nutr <- foreign_sector_vol %>%
  # attempting to do both rni equiv and nutr_tonnes in one 
  # use sierra leone spp
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Sierra Leone"), calc_children_fed_func),
         nutr_tonnes = pmap (list (species_name = species, catch_mt = tonnes, country_name = "Sierra Leone"), convert_catch_to_nutr_tons)) %>%
  unnest (cols = c(rni_equivalents, nutr_tonnes),  names_repair = "check_unique", names_sep = ".") %>%
  # this makes weird column names because I didn't think about running the functions together. duplicates the nutrient column
  select (-c(nutr_tonnes.nutrient, tonnes)) %>%
  # remove text before "." in column names
  rename_with (~gsub(".*\\.", "", .x)) %>%
  # group by nutrient, this makes it cleaner
  group_by (country, sector, nutrient) %>%
  summarise (across( where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  # join to population demand
  left_join (filter (wpp_country_aggregate, Time == 2019), by = c("country", "nutrient")) %>%
  mutate (perc_demand_met = nutr_tonnes / tot_nutr_annual_demand * 100) %>%
  select (-c(Time, nutr_tonnes, tot_pop, tot_nutr_annual_demand))


  #set levels
sector_levels = c("Foreign catch", "Artisanal", "Industrial")
foreign_sector_nutr$sector <- factor (foreign_sector_nutr$sector, levels = sector_levels)
  
foreign_sector_nutr  %>% 
    filter (!nutrient %in% c("Protein", "Selenium")) %>%
    mutate (nutrient = case_when (
      nutrient == "Omega_3" ~ "Omega 3",
      nutrient == "Vitamin_A" ~ "Vit. A",
      TRUE ~ nutrient)) %>%
    ggplot (aes (axis1 = nutrient,
                 axis2 = sector,
                 y = rni_equivalents/1000000,
                 fill = nutrient)) +
    scale_x_discrete (limits = c ("nutrient", "sector"), expand = c(.05, .05)) +
    labs(y = "Child RNI equiv., millions", x = "") +
    geom_flow(aes(fill = nutrient)) +
    geom_stratum(aes(fill = nutrient)) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
    theme_minimal() +
    #ggtitle("National allocative driver") +
    #scale_fill_brewer (palette = "Set1") +
    #scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A","#FFFF33", "#984EA3", "#FF7F00")) +
    theme ( 
      axis.text.x = element_blank(),
      axis.text.y = element_text (size = 9),
      axis.title = element_text (size = 10),
      plot.title = element_text (size = 12),
      plot.margin=unit(c(1,1,1,1), 'mm'),
      legend.position = "none")
  
  ggsave ("Figures/Ghana_foreign_sector_driver.png", width = 90, height = 70, units = "mm")
