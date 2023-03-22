###Check SAU/Free close species

# 4/22/22
# JGM

# updating 3/21/23 since we decided on aggregate data. 
#alter nutricast to match landings data where possible.
# if landings has just genus and nutricast has several species, take average for nutricast
# if landings has genus and several species and nutricast has just species, combine
# dealing with genus first, then family, e.g. bivalvia, chondricthyes
# using fishbase for families: https://fishbase.se/Nomenclature/FamilySearchList.php?
# but only has finfish, no sharks

# the problem is, nutricast data is fully species specific, whereas SAU is more likely to have genus/family level, especially for indonesia. 
# so if we change nutricast to higher-level genus or family, we lose the nutrient data anyway. 
# could change SAU data based on ratios in nutricast baseline
# Do I first match nut


# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# nutricast
# already filtered for catch_mt > 0
# catch upside relative, from calculate_nutritional_upsides.r
# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds") 


# Peru, SAU ----
# no merluccius at all in nutricast. 
# biggest missing species identified to species level are Argopecten purpuratus, Cynoscion analyis, Doryteuthis gah, Merluccius gayi peruanus

# families
#ignore chondrychthes, just 0.4 tonnes
sau_peru_catch <- sau_2019 %>%
  filter (country == "Peru") %>%
  group_by (species) %>%
  summarise (tot_catch = sum (tonnes, na.rm = TRUE))

write.excel(sau_peru_catch)

# not sure what to do about sau Seriola
sau_peru_match <- sau_2019 %>%
  filter (country == "Peru") %>%
  mutate (species = 
            case_when (
              species == "Coryphaena" ~ "Coryphaena hippurus",
              species == "Istiophorus" ~ "Istiophorus platypterus",
            species == "Paralabrax" ~ "Paralabrax humeralis",
              TRUE ~ species
              
            ))

sau_peru_match %>%
  group_by (species) %>%
  # can just take the sum because I'm combining species?
  summarise (tot_catch = sum (tonnes, na.rm = TRUE)) %>%
  write.excel()
  

nutricast_peru_match <- catch_upside_relative %>%
  filter (country == "Peru") %>%
  # manually fix species, change nutricast if in same genus
  mutate (species = 
            case_when (
              species == "Aulacomya ater" ~ "Aulacomya atra",
              species %in% c("Caranx melampygus", "Caranx sexfasciatus") ~ "Caranx",
              species %in% c("Elagatis bipinnulata","Naucrates ductor","Pseudocaranx dentex","Scomberoides tol") ~ "Carangidae",
              species == "Farfantepenaeus brevirostris" ~ "Farfantepenaeus californiensis",
              grepl ("Genypterus", species) ~ "Genypterus",
              grepl ("Holothuria", species) ~ "Holothuriidae",
              species == "Penaeus monodon" ~ "Penaeidae",
              species == "Rhinobatos planiceps" ~ "Pseudobatos planiceps",
              species == "Sarda orientalis" ~ "Sarda chiliensis",
              species %in% c("Acanthocybium solandri", "Gasterochisma melampus") ~ "Scombridae"
              TRUE ~ species
              
              
              
              
            )) %>%
  group_by (rcp, species, country) %>%
  # take mean of grouped species
  summarise_at (vars(-group_cols()),mean, na.rm = TRUE)

write.excel (sort(unique(nutricast_peru_match$species)))

# Indonesia, SAU ----
indo_sau <-  sau_2019 %>%
  filter (country == "Indonesia") %>%
  group_by (species) %>%
  summarise (tot_catch = sum (tonnes, na.rm = TRUE)) %>%
  write.excel()

nutricast_indo_match <- catch_upside_relative %>%
  filter (country == "Indonesia") %>%
  pull (species) %>%
  unique () %>%
  write.excel()
  # manually fix species, change nutricast if in same genus
  mutate (species = 
            case_when (
              species == "Aulacomya ater" ~ "Aulacomya atra",
              species %in% c("Caranx melampygus", "Caranx sexfasciatus") ~ "Caranx",
              species == "Farfantepenaeus brevirostris" ~ "Farfantepenaeus californiensis",
              grepl ("Genypterus", species) ~ "Genypterus",
              grepl ("Holothuria", species) ~ "Holothuriidae",
              species == "Penaeus monodon" ~ "Penaeidae",
              species == "Sarda orientalis" ~ "Sarda chiliensis",
              TRUE ~ species




#For Chile: Brama australis--> Brama brama? Atlantic pomfret. different resilience and temperature...Just the one other fish in teh genus

#Merluccius gayi---M. australis?
  
#For Peru: 
# Sarda chiliensis do have nutrient info. In fishcast. So is M. gayi
# have Sarda orientalis, and sarda (atlantic)

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")


merluc <- ds_spp %>%
  filter (grepl ("Merluccius", species), country %in% c ("Chile", "Peru"), catch_mt > 0)


merluc %>%
  filter (country == "Chile") %>%
  ggplot() +
  geom_line (aes (x = year, y = catch_mt, col = species)) +
  facet_grid(scenario ~ rcp) +
  theme_bw()

merluc %>%
  filter (country == "Peru") %>%
  ggplot() +
  geom_line (aes (x = year, y = catch_mt, col = species)) +
  facet_grid(scenario ~ rcp) +
  theme_bw()
# no catch for peru

sarda <- ds_spp %>%
  filter (grepl ("Sarda", species))

sarda %>%
  #filter (country == "Chile") %>%
  filter (species == "Sarda orientalis", country %in% c ("Chile", "Peru")) %>%
  ggplot() +
  geom_line (aes (x = year, y = catch_mt, col = country)) +
  facet_grid(scenario ~ rcp) +
  theme_bw()

# no catch of sarda sarda in chile obv. Sarda doesn't show in nutricast so I don't know what to compare it to?