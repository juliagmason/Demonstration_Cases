## SAU nutrients by end use

# 4/23/22
# JGM

# where I left this: 
# ther eare 2 species not in sau_nutr: Anchoa nasus and Hippoglossina macrops. Make a mini join for those two and then tack on to sau_nutr. 
# then make dodged bar graph with the end use types by country


# compare nutrition density and contribution of direct human consumption vs. fmfo
library (tidyverse)


# SAU data, use all end use types
sau <- read.csv ("Data/SAU_EEZ_landings.csv")

# truncate years and landings
sau_sm <- sau %>%
  filter (between (year, 2000, 2015), 
          tonnes > 0) %>%
  mutate (country = 
            case_when (grepl ("Indo", area_name) ~ "Indonesia",
                       grepl ("Chile", area_name) ~ "Chile", 
                       TRUE ~ area_name)
  ) %>%
  select (country, data_layer, uncertainty_score, year, scientific_name, commercial_group, functional_group, fishing_entity, fishing_sector, tonnes, landed_value, end_use_type) %>%
  # consolidate indonesia eez
  group_by (country, data_layer, year, scientific_name, fishing_entity, fishing_sector, end_use_type) %>%
  mutate (tonnes_tot = sum (tonnes),
          landed_value_tot = sum(landed_value),
          uncertainty_score = weighted.mean(uncertainty_score, w = tonnes, na.rm = TRUE), 
          .keep = "unused") %>%
  distinct() %>%
  rename (species = scientific_name) %>% 
  ungroup()

# nutrient data ----
fishnutr <- read_csv("Data/Species_Nutrient_Predictions.csv")

fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 

# genus data for nonfish [eventually could use AFCD] ----
# cephalopods only in functional group, but crustaceans, mollusks are commercial groups
spp_key <- read.csv(file.path ("Data/Gaines_species_nutrient_content_key.csv"), as.is=T) %>% 
  # just do by major group, not species
  select (major_group, genus_food_name, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg) %>%
  # recode major_Group_ name from nutricast code
  mutate (
    major_group=recode(genus_food_name,
                       "Cephalopods"="Cephalopods",
                       "Crustaceans"="Crustaceans",
                       "Demersal Fish"="Finfish",
                       "Marine Fish; Other"="Finfish",
                       "Molluscs; Other"="Molluscs",
                       "Pelagic Fish"="Finfish")
    
  ) %>%
  distinct()


# rda data ----
rda_groups <- readRDS("Data/RDAs_5groups.Rds")

# population data----
pop_2020 <- read_csv("Data/WPP2019_PopulationByAgeSex_Medium.csv") %>%
  filter (Location %in% c("Sierra Leone", "Indonesia", "Peru", "Chile", "Malawi"), Time ==2020) %>%
  mutate (group = 
            case_when (
              AgeGrp %in% c("0-4", "5-9") ~ "Child",
              !AgeGrp %in% c("0-4", "5-9") ~ "Adult")
  ) %>%
  group_by (Location) %>%
  summarize (Pop_Males =  1000 * sum (PopMale[group == "Adult"]),
             Pop_Females = 1000 * sum(PopFemale[group == "Adult"]),
             Pop_Child = 1000 * sum(PopTotal[group == "Child"])) %>%
  # also have total population that would eat fish--sum of males, females, and children maybe do this later?
  #Pop_Total = sum (Pop_Males, Pop_Females, Pop_Child)) %>%
  pivot_longer (cols = starts_with("Pop"),
                names_prefix = "Pop_", 
                names_to = "group", 
                values_to = "population") %>%
  rename (country = Location)


pop_rda_needs <- pop_2020 %>%
  left_join (rda_groups, by = "group") %>%
  mutate (rda_needs = mean_rda * population) %>%
  group_by (country, nutrient) %>%
  summarise (
    tot_rda_needs = sum (rda_needs, na.rm = TRUE))



#Function to calculate mt of nutrient from mt of edible meat ----
# units: mg, ug=mcg 
# meat_mt <- 29.88111; nutr_dens <- 35.5; nutr_dens_units <- "mg"
# from nutrient_endowment --> shiny --> v3 --> :Page3_Fig2c_reforms_prop_demand, line 84

# adding--divide by 100 in here. I think Chris did that in his head but making me doubt everything
calc_nutr_supply_mt <- function(meat_mt, nutr_dens, nutr_dens_units){
  
  # Convert meat to grams
  # "Mg" is metric tons
  meat_g <- measurements::conv_unit(meat_mt, "Mg", "g")
  
  # Calculate amount of nutrient in density units. divide by 100 because density units are per 100g
  nutrient_q <- meat_g *  nutr_dens / 100
  
  # Calculate amount of nutrient in metric tons
  nutrient_mt <- measurements::conv_unit(nutrient_q, nutr_dens_units, "Mg")
  
  # Return
  return(nutrient_mt)
  
}







###########################################
# join species and nutrient data

sau_sm_test <- sample_n(sau_sm, 1000)

# convert catch to nutrients ----
# baseline SAU by sector and end use 
sau_enduse_nutr_contribution  <- sau_sm %>%
  # make genus_food_name column to join
  mutate (genus_food_name = 
            case_when (functional_group == "Cephalopods" ~ "Cephalopods",
                       commercial_group == "Crustaceans" ~ "Crustaceans",
                       commercial_group == "Molluscs" ~ "Molluscs; Other",
                       grepl ("pelagic", functional_group) ~ "Pelagic Fish",
                       # plural demersals to avoid "other demersal invertebrates
                       grepl ("demersals", functional_group) ~ "Demersal Fish")
          # still a lot of random fish to figure out
  ) %>%
  left_join (spp_key, by = "genus_food_name") %>%
  left_join (fishnutr_mu, by = "species") %>%
  mutate (
    # select appropriate source for finfish vs. other. 
    Selenium =  Selenium_mu, 
    # dumb hack to keep major_group for later bc discarding unused
    Zinc = ifelse (!is.na (Zinc_mu), Zinc_mu,
                   zinc_mg),
    
    Protein = ifelse (!is.na (Protein_mu), Protein_mu,
                      protein_g),
    #### NEED to figure out omega 3 situation #####
    Omega_3 = ifelse (!is.na (Omega_3_mu),
                      Omega_3_mu, 
                      polyunsaturated_fatty_acids_g),
    Calcium = ifelse (!is.na (Calcium_mu),
                      Calcium_mu,
                      calcium_mg),
    Iron = ifelse (!is.na (Iron_mu),
                   Iron_mu,
                   iron_mg),
    Vitamin_A = ifelse (!is.na (Vitamin_A_mu),
                        Vitamin_A_mu,
                        vitamin_a_mcg_rae),
    .keep = "unused"
  ) %>%
  pivot_longer (Selenium:Vitamin_A,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate(pedible=recode(major_group, 
                        "Finfish"= 0.87, 
                        "Crustaceans"=0.36, 
                        "Molluscs"=0.17, 
                        "Cephalopods"=0.21), 
         
         # units
         dens_units = 
           case_when (
             nutrient %in% c("Protein", "Omega_3") ~ "g",
             nutrient %in% c("Vitamin_A", "Selenium") ~ "ug",
             TRUE ~ "mg"
           ),
         # edible meat in mt
         meat_mt = tonnes_tot * pedible,
         
         
         #available nutrient in mt
         #nutr_mt = mapply(calc_nutr_supply_mt, meat_mt, amount, dens_units)
         nutr_mt = pmap (list (meat_mt = meat_mt, nutr_dens = amount, nutr_dens_units = dens_units), calc_nutr_supply_mt),
         
         # edible meat in 100g servings/day
         # meat in metric tons/yr *1000 kg/ton * 1000g/kg * 1 serving /100 g * 1 yr/365 days
         meat_servings = meat_mt * 1000 * 1000 / 100 / 365,
         
         # nutrient content in servings/day
         nutr_servings = meat_servings * amount) %>% 
  unnest(cols = c(nutr_mt))

saveRDS (sau_enduse_nutr_contribution, file = "Data/SAU_nutr_content_sector_enduse.Rds")

sau_nutr <- readRDS("Data/SAU_nutr.Rds")

# population level rdas met ----
rdas_pop_level_sau_enduse <- sau_enduse_nutr_contribution %>%
  group_by (country, data_layer, fishing_sector, end_use_type, species, nutrient) %>%
  mutate (mean_tonnes = mean (tonnes_tot, na.rm = TRUE),
          mean_servings = mean (nutr_servings, na.rm = TRUE)) %>%
  select (country, species, major_group, data_layer, fishing_sector, end_use_type, nutrient, mean_servings) %>%
  distinct() %>%
  left_join (pop_rda_needs, by = c("country", "nutrient")) %>%
  mutate (#yield_per_cap = nutr_servings / population,
    
    # proportion of population rda met for each group
    rda_met = mean_servings/tot_rda_needs)

saveRDS (rdas_pop_level_sau_enduse, file = "Data/SAU_popRDAs_sector_enduse.Rds")

# individuals fed
inds_fed_sau_enduse <- sau_enduse_nutr_contribution %>%
  group_by (country, data_layer, fishing_sector, end_use_type, species, nutrient) %>%
  mutate (mean_tonnes = mean (tonnes_tot, na.rm = TRUE),
          mean_servings = mean (nutr_servings, na.rm = TRUE)) %>%
  select (country, species, major_group, data_layer, fishing_sector, end_use_type, nutrient, mean_servings) %>%
  distinct() %>%
  left_join (rda_groups, by = "nutrient") %>%
  mutate (inds_fed = mean_servings / mean_rda)

saveRDS(inds_fed_sau_enduse, file = "Data/SAU_inds_fed_sector_enduse.Rds")
  
#######################################################################################



sau_spp_nutr_alluse <- sau_sm %>%
  select (species, functional_group, commercial_group) %>%
  distinct() %>%
  # make genus_food_name column to join
  mutate (genus_food_name = 
            case_when (functional_group == "Cephalopods" ~ "Cephalopods",
                       commercial_group == "Crustaceans" ~ "Crustaceans",
                       commercial_group == "Molluscs" ~ "Molluscs; Other",
                       grepl ("pelagic", functional_group) ~ "Pelagic Fish",
                       # plural demersals to avoid "other demersal invertebrates
                       grepl ("demersals", functional_group) ~ "Demersal Fish")
          # still a lot of random fish to figure out
  ) %>%
  left_join (spp_key, by = "genus_food_name") %>%
  left_join (fishnutr_mu, by = "species") %>%
  mutate (
    # select appropriate source for finfish vs. other. 
    Selenium =  Selenium_mu, 
    # dumb hack to keep major_group for later bc discarding unused
    Zinc = ifelse (!is.na (Zinc_mu), Zinc_mu,
                   zinc_mg),
    
    Protein = ifelse (!is.na (Protein_mu), Protein_mu,
                      protein_g),
    #### NEED to figure out omega 3 situation #####
    Omega_3 = ifelse (!is.na (Omega_3_mu),
                      Omega_3_mu, 
                      polyunsaturated_fatty_acids_g),
    Calcium = ifelse (!is.na (Calcium_mu),
                      Calcium_mu,
                      calcium_mg),
    Iron = ifelse (!is.na (Iron_mu),
                   Iron_mu,
                   iron_mg),
    Vitamin_A = ifelse (!is.na (Vitamin_A_mu),
                        Vitamin_A_mu,
                        vitamin_a_mcg_rae),
    .keep = "unused"
  ) %>%
  pivot_longer (Selenium:Vitamin_A,
                names_to = "nutrient",
                values_to = "amount") %>%
  # join to rda data
  left_join (rda_groups, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rda = amount/mean_rda * 100,
          perc_rda = ifelse (perc_rda > 100, 100, perc_rda)) %>%
  ungroup() %>%
  # remove groups here to join back to catch data?
  select (-c("commercial_group", "functional_group"))


#####################################################
# compare nutrient concentration of end use species ----

spp_micronutr_density <- sau_spp_nutr_alluse %>%
  filter (group == "Child", ! nutrient %in% c("Protein", "Selenium")) %>%
  distinct() %>%
  group_by (species) %>% 
  summarise (micronutrient_density = sum (perc_rda, na.rm = TRUE))

end_use_spp %>%
  left_join (spp_micronutr_density, by = "species") %>%
  ggplot (aes (y = micronutrient_density, x = spp_enduse)) +
  geom_boxplot () +
  facet_wrap (~country) + 
  theme_bw() +
  labs (y = "Micronutrient density", x = "") +
  ggtitle ("Micronutrient density of the species by end use \n DHC = direct human consumption; FMFO = fish meal and fish oil.\n 75% cutoff by weight for end use classification; Vit A, Calcium, Omega 3, Iron, Zinc")


#####################################################



sau_sm_nutr <- sau_spp_nutr_alluse %>%
  select (species, major_group, nutrient, amount, unit) %>% 
  distinct()

sau_enduse_nutr_contribution <- sau_sm %>%
left_join (sau_sm_nutr, by = "species") %>%
  mutate(pedible=recode(major_group, 
                        "Finfish"= 0.87, 
                        "Crustaceans"=0.36, 
                        "Molluscs"=0.17, 
                        "Cephalopods"=0.21), 
         
         # units
         dens_units = 
           case_when (
             nutrient %in% c("Protein", "Omega_3") ~ "g",
             nutrient %in% c("Vitamin_A", "Selenium") ~ "ug",
             TRUE ~ "mg"
           ),
         # edible meat in mt
         meat_mt = tonnes_tot * pedible,
         
         
         #available nutrient in mt
         #nutr_mt = mapply(calc_nutr_supply_mt, meat_mt, amount, dens_units)
         nutr_mt = pmap (list (meat_mt = meat_mt, nutr_dens = amount, nutr_dens_units = dens_units), calc_nutr_supply_mt),
         
         # edible meat in 100g servings/day
         # meat in metric tons/yr *1000 kg/ton * 1000g/kg * 1 serving /100 g * 1 yr/365 days
         meat_servings = meat_mt * 1000 * 1000 / 100 / 365,
         
         # nutrient content in servings/day
         nutr_servings = meat_servings * amount) %>% 
  unnest(cols = c(nutr_mt))
  
  

rdas_pop_level_sau_enduse <- sau_enduse_nutr_contribution %>%
  group_by (country, fishing_sector, end_use_type, species, nutrient) %>%
  mutate (mean_tonnes = mean (tonnes_tot, na.rm = TRUE),
          mean_servings = mean (nutr_servings, na.rm = TRUE)) %>%
  select (country, species, major_group, fishing_sector, end_use_type, nutrient, mean_servings) %>%
  distinct() %>%
  left_join (pop_rda_needs, by = c("country", "nutrient")) %>%
  mutate (#yield_per_cap = nutr_servings / population,
    
    # proportion of population rda met for each group
    rda_met = mean_servings/tot_rda_needs)


# Chile and peru nutrient content by end use type
rdas_pop_level_sau_enduse %>%
  filter (country %in% c("Chile", "Peru"), end_use_type != "Other") %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = end_use_type)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free") +
  labs (y = "Proportion of population RDAs met", fill = "End use") +
  ggtitle ("Proportion of population RDAs met by current catch\nSAU data, average catch 2000-2015") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9))


# chile and per nutrient content by end use type and sector
rdas_pop_level_sau_enduse %>%
  filter (country %in% c("Chile", "Peru"), fishing_sector %in% c("Industrial", "Artisanal"), end_use_type != "Other") %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = end_use_type)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_grid (country~ fishing_sector, scales = "free") +
  labs (y = "Proportion of population RDAs met", fill = "End use") +
  ggtitle ("Proportion of population RDAs met by current catch\nSAU data, average catch 2000-2015") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9))

# all four countries by end use type and sector
png (filename = "Figures/SAU_DHC_RDAs_met_enduse_sector.png", width = 6.5, height = 8, units = "in", res = 360)
rdas_pop_level_sau_enduse %>%
  filter (fishing_sector %in% c("Industrial", "Artisanal"), end_use_type != "Other") %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = end_use_type)) +
  geom_bar (stat = "identity", position = "dodge") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_grid (country~ fishing_sector, scales = "free") +
  labs (y = "Proportion of population RDAs met", fill = "End use") +
  ggtitle ("Proportion of population RDAs met by current catch\nSAU data, average catch 2000-2015") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9, size = 10),
         axis.text.y = element_text (size = 8),
         legend.text = element_text (size = 8),
         plot.title = element_text (size = 10),
         strip.text = element_text (size = 8))
dev.off()

# all four countries by end use type
png (filename = "Figures/SAU_DHC_RDAs_met_enduse.png", width = 6.5, height = 4, units = "in", res = 360)
rdas_pop_level_sau_enduse %>% 
  ggplot (aes (y = rda_met, x = nutrient, fill = end_use_type)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free_y") +
  labs (y = "Proportion of population RDAs met", fill = "End use") +
  ggtitle ("Proportion of population RDAs met by current catch\nSAU data, average catch 2000-2015") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9, size = 8),
         axis.text.y = element_text (size = 8),
         legend.text = element_text (size = 8),
         plot.title = element_text (size = 10),
         strip.text = element_text (size = 8))
dev.off()

rdas_pop_level_sau_enduse %>% 
  filter (fishing_sector != "Recreational", end_use_type == "Direct human consumption") %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = fishing_sector)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free") +
  labs (y = "Proportion of population RDAs met", fill = "Sector") +
  ggtitle ("Proportion of population RDAs met by current catch\nSAU data, average catch 2000-2015") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9))


# all four countries SSF by end use type

rdas_pop_level_sau_enduse %>% 
  filter (fishing_sector == "Artisanal") %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = end_use_type)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free") +
  labs (y = "Proportion of population RDAs met", fill = "Sector") +
  ggtitle ("Proportion of population RDAs met by current catch\nSAU data, average catch 2000-2015") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9))

# all four countries industrial vs artisanal, DHC only
png (filename = "Figures/SAU_DHC_RDAs_met_sector.png", width = 6.5, height = 4, units = "in", res = 360)
rdas_pop_level_sau_enduse %>% 
  filter (end_use_type == "Direct human consumption", fishing_sector %in% c("Industrial", "Artisanal")) %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = fishing_sector)) +
  geom_bar (stat = "identity", position = "dodge") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free_y") +
  labs (y = "Proportion of population RDAs met", fill = "Sector") +
  ggtitle ("Proportion of population RDAs met by current catch\nSAU data, average catch 2000-2015 \nCatch for direct human consumption only") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9, size = 8),
         axis.text.y = element_text (size = 8),
         legend.text = element_text (size = 8),
         plot.title = element_text (size = 10),
         strip.text = element_text (size = 8))

dev.off()

# this looks different from new figure in 4WSFC, e.g. peru industrial selenium. don't think this version properly excludes foreign catch but that's not the full story
