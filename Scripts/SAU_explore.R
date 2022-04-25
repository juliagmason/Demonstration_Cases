# SAU explore
# 3/31/22
# JGM

library (tidyverse)

sau <- read.csv ("Data/SAU_EEZ_landings.csv")

# have to consolidate indo eezs, restrict to "Direct human consumption", truncate year, remove discards
unique (sau$end_use_type)
unique (sau$area_name)
unique (sau$catch_type)
unique (sau$data_layer) # domestic, inferred foreign catch, tuna RFMO catch

sau_artisanal <- sau_ds %>%
  filter (fishing_sector== "Artisanal")
unique (sau_artisanal$fishing_entity)

# peru has extremely high anchoveta catch 2016-2018, order of magnitude higher
# no one has fishmeal/oil in 2016-2018. cut to 2015. 

sau_ds <- sau_artisanal %>%
  filter (between (year, 2000, 2015), 
          tonnes > 0,
          catch_type == "Landings",
          end_use_type == "Direct human consumption") %>%
  mutate (country = 
            case_when (grepl ("Indo", area_name) ~ "Indonesia",
                       grepl ("Chile", area_name) ~ "Chile", 
          TRUE ~ area_name)
  ) %>%
  select (country, data_layer, uncertainty_score, year, scientific_name, commercial_group, functional_group, fishing_entity, fishing_sector, tonnes, landed_value) %>%
  # consolidate indonesia eez
  group_by (country, data_layer, year, scientific_name, fishing_entity, fishing_sector) %>%
  mutate (tonnes_tot = sum (tonnes),
             landed_value_tot = sum(landed_value),
             uncertainty_score = weighted.mean(uncertainty_score, w = tonnes, na.rm = TRUE), 
          .keep = "unused") %>%
  distinct()

saveRDS (sau_ds, file = "Data/SAU_countries_dirhumcons_2000_2015.Rds")



sau_ds_tmp %>%
  filter (data_layer == "Assigned Tuna RFMO catch") %>%
  ggplot (aes (x = year, y = tonnes)) +
    geom_line() +
  facet_wrap (~country, scales = "free")

sau_ds_tmp %>%
  filter (country == "Peru") %>%
  ggplot (aes (x = year, y = tonnes)) +
  geom_line() +
  facet_wrap (~data_layer, scales = "free")
  

sau_ds %>%
  ggplot (aes (x = year, y = tonnes_tot, fill = fishing_sector)) +
  geom_bar (stat = "identity") +
facet_wrap (~country, scales = "free")


# is this reasonable with nutricast
ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")

ds_spp %>%
  filter (year < 2020, rcp == "RCP26", scenario == "No Adaptation") %>%
  ggplot (aes (x = year, y = catch_mt)) +
  geom_bar (stat = "identity") +
  facet_wrap (~country, scales= "free")

ds_yr_comparison <- ds_spp %>%
  filter (year < 2020, rcp == "RCP26", scenario == "No Adaptation") %>%
  group_by (country, year) %>%
  summarise (tot_cat_nutr = sum (catch_mt, na.rm = TRUE))


sau_ds %>%
  
  ggplot () +
  geom_bar (aes (x = year, y = tonnes_tot, fill = fishing_sector), stat = "identity") +
  geom_point (data = ds_yr_comparison, aes (x = year, y = tot_cat)) +
  facet_wrap (~country, scales = "free") +
  theme_bw()

# kind of

## join to nutrition data ----
# can i use summarized data or go back to original csv?
spp_nutr <- readRDS("Data/ds_spp_nutr_content_FishNutrientsGENuS_RDA_groups.Rds") %>%
  select (-c(rcp, scenario, country, catch_mt)) %>%
  distinct()

nutrspp <- unique(sau_ds$scientific_name[which (!sau_ds$scientific_name %in% spp_nutr$species)])

fishnutrspp <- unique(sau_ds$scientific_name[which (!sau_ds$scientific_name %in% fishnutr$species)])

nutr[which(!nutr %in% fishnutr)]

# prob better orig cv
fishnutr <- read_csv("Data/Species_Nutrient_Predictions.csv")

fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 

# genus data for nonfish [eventually could use AFCD]
# cephalopods only in functional group, but crustaceans, mollusks are commercial groups
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T) %>% 
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


# rda data
rda_groups <- readRDS("Data/RDAs_5groups.Rds")


# join 
sau_spp_nutr <- sau_ds %>%
  ungroup() %>%
  select (scientific_name, functional_group, commercial_group) %>%
  distinct() %>%
  rename (species = scientific_name) %>%
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
    

saveRDS(sau_spp_nutr, file = "Data/SAU_spp_nutrient_data.Rds")

# convert catch to nutrients ----

# Function to calculate mt of nutrient from mt of edible meat ----
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


sau_ds_nutr  <- sau_ds %>%
  filter (scientific_name %in% c("Anchoa nasus", "Hipploglossina macrops"))
  rename (species = scientific_name) %>%
  # # make major_food_name column to join
  # mutate (major_group = 
  #           case_when (functional_group == "Cephalopods" ~ "Cephalopods",
  #                      commercial_group == "Crustaceans" ~ "Crustaceans",
  #                      commercial_group == "Molluscs" ~ "Molluscs",
  #                      commercial_group %in% c("Sharks & rays","Scorpionfishes","Perch-likes", "Flatfishes","Herring-likes", "Tuna & billfishes","Anchovies", "Cod-likes") ~ "Finfish",
  #                      commercial_group == "Other fishes & inverts" & !functional_group %in% c("Jellyfish", "Cephalopods", "Other demersal invertebrates") ~ "Finfish"
  #           )
  # ) %>%
  left_join (sau_spp_nutr, by = "species") %>%
  # calculate edible meat.
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

# quick fix here; had .x and .y functional groups
sau_ds_nutr2 <- sau_ds_nutr %>%
  select (-c ("functional_group.y", "commercial_group.y")) %>%
  rename (functional_group = functional_group.x, 
          commercial_group = commercial_group.x)

saveRDS(sau_ds_nutr2, file = "Data/SAU_nutr.Rds")


### plot distribution of micronutrient density by sector
spp_micronutr_density <- spp_nutr %>%
  filter (group == "Child", !nutrient == "Protein") %>%
  distinct() %>%
  group_by (species) %>% 
  summarise (micronutrient_density = sum (perc_rda, na.rm = TRUE))

sau_ds %>%
  rename (species = scientific_name) %>%
  left_join (spp_micronutr_density, by = "species") %>%
  ggplot () +
  geom_density (aes (x = micronutrient_density, col = fishing_sector), alpha = 0.5) +
  facet_wrap (~country) +
  theme_bw()


sau_ds_nutr %>%
  filter(group == "Child", year == 2015) %>%

  
# plot RDAs met by sector ----
  
  # population data ----

# downloaded from https://population.un.org/wpp/Download/Standard/CSV/ on 3/17/2022
# Population by 5-year age groups.
# PopMale: Male population in the age group (thousands)
# PopFemale: Female population in the age group (thousands)
# PopTotal: Total population in the age group (thousands)

pop <- read_csv("Data/WPP2019_PopulationByAgeSex_Medium.csv")

# separate into broad adult male/female and child categories to  match RDAs
pop_current <- pop %>%
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



# just want population level rda for now 
pop_rda_needs <- pop_current %>%
  left_join (rda_groups, by = "group") %>%
  mutate (rda_needs = mean_rda * population) %>%
  group_by (country, nutrient) %>%
  summarise (
    tot_rda_needs = sum (rda_needs, na.rm = TRUE))


rdas_pop_level_sau <- sau_ds_nutr %>%
  ungroup() %>%
  filter (year == 2012, !is.na (nutr_servings)) %>%
  select (country, species, major_group, fishing_sector, nutrient, nutr_servings) %>%
  distinct() %>%
 left_join (pop_rda_needs, by = c("country", "nutrient")) %>%
  mutate (#yield_per_cap = nutr_servings / population,
          
          # proportion of population rda met for each group
          rda_met = nutr_servings/tot_rda_needs)
 

rdas_pop_level_sau %>%
  ggplot (aes (y = rda_met, x = nutrient, fill = fishing_sector)) +
  geom_bar (stat = "identity") +
  facet_wrap (~country, nrow = 4, scales = "free") +
  theme_bw() +
  labs (x = "", fill = "", y = "") +
  facet_wrap (~country, scales = "free") +
  labs (y = "Proportion of population RDAs met") +
  ggtitle ("Proportion of population RDAs met by current catch, SAU 2012 data") +
  theme (axis.text.x = element_text (angle = 60, hjust = 0.9))

# what is going on with peru 
sau_ds %>%
  filter (country == "Peru") %>%
  mutate (anchov = ifelse (scientific_name == "Engraulis ringens", "Anchovy", "Other")) %>%
  ggplot (aes (x = year, y = tonnes_tot, fill = anchov)) +
  geom_bar (stat = "identity") +
  facet_wrap (~fishing_sector, scales = "free")

sau %>%
  filter (area_name == "Peru", year > 2000, catch_type == "Landings", fishing_sector == "Industrial") %>% 
  ggplot () +
  geom_bar (aes (x = year, y = tonnes, fill = end_use_type), stat = "identity")


# compare industrial vs. artisanal fishing vuln, micro density, climate vuln ----

# can prob get clim vuln from SAU, steal from maire 
maire_vuln <- read_csv ("../NutrientGlobalFisheries_Maire/data/species_data.csv") %>%
  select (species, Fishing_V, Climate_V.index)

spp_profiles_vuln <- spp_micronutr_density %>%
left_join (maire_vuln, by = "species") %>%
  rename (fishing_vuln = Fishing_V, climate_vuln = Climate_V.index)

library (RColorBrewer)
ggplot (spp_profiles_vuln) +
  geom_point (aes (x = fishing_vuln, y = micronutrient_density, col = climate_vuln)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlGn")

sau_ds %>%
  filter (year == 2012, country == "Peru") %>%
  rename (species = scientific_name) %>%
  left_join (spp_profiles_vuln, by = "species") %>%
  ggplot() +
  geom_point (aes (x = fishing_vuln, y = micronutrient_density, col = climate_vuln, size = tonnes_tot)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlGn") +
  facet_wrap (~fishing_sector)
  
sau_ds %>%
  filter (year == 2012) %>%
  rename (species = scientific_name) %>%
  left_join (spp_profiles_vuln, by = "species") %>%
  ggplot() +
  geom_point (aes (x = log(tonnes_tot), y = micronutrient_density, col = fishing_sector)) +
  theme_bw() +
  facet_wrap (~country)
  