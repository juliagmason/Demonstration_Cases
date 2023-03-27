###Check SAU/Free close species

# 4/22/22
# JGM

library (tidyverse)
# https://rstats-tips.net/2020/07/31/get-rid-of-info-of-dplyr-when-grouping-summarise-regrouping-output-by-species-override-with-groups-argument/
options(dplyr.summarise.inform = FALSE)


library (stringr)
#remotes::install_github("ropensci/rfishbase")
library (rfishbase)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# updating 3/21/23 since we decided on aggregate data. 
#alter nutricast to match landings data where possible.
# if landings has just genus and nutricast has several species, take average for nutricast
# if landings has genus and several species and nutricast has just species, combine
# dealing with genus first, then family, e.g. bivalvia, chondricthyes
# using fishbase for families: https://fishbase.se/Nomenclature/FamilySearchList.php?
# but only has finfish, no sharks

# the problem is, nutricast data is fully species specific, whereas SAU is more likely to have genus/family level, especially for indonesia. 
# so if we change nutricast to higher-level genus or family, we lose the nutrient data anyway. 
# could change SAU data based on ratios in nutricast baseline?

# SAU and fishnutr all matched now. so now should definitely match nutricast to SAU, and then mutiply children fed. 



# nutricast
# already filtered for catch_mt > 0
# catch upside relative, from calculate_nutritional_upsides.r
# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")
# 503 species, not all are in fishbase. get 481

# match to family, get family info from fb?
# species table just has family code. match from "families" table
famcodes_fb <- fb_tbl("families") %>%
  select ("FamCode", "Family")

famcodes_slb <- fb_tbl("families", "sealifebase") %>%
  select ("FamCode", "Family") 
# these have overlap with fb


nutricast_fams_fb <- fb_tbl ("species") %>%
  mutate(species = paste(Genus, Species)) %>%
  filter (species %in% catch_upside_relative$species) %>%
  left_join (famcodes_fb, by = "FamCode") %>%
  select (species, Genus, Family)

nutricast_fams_slb <- fb_tbl ("species", "sealifebase") %>%
  mutate(species = paste(Genus, Species)) %>%
  filter (species %in% catch_upside_relative$species) %>%
  left_join (famcodes_slb, by = "FamCode") %>%
  select (species, Genus, Family)

nutricast_fams <- rbind (nutricast_fams_fb, nutricast_fams_slb)

catch_upside_relative_fam <- catch_upside_relative %>%
  left_join (nutricast_fams, by = "species") %>%
  # grab Genus even if not in fishbase
  mutate (Genus = 
            case_when (is.na (Genus) ~ word(species, 1),
                       TRUE ~ Genus)
  )

# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
# just grab species names
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")
sau_2019_country_spp <- readRDS("Data/SAU_2019.Rds") %>%
  ungroup() %>%
  select (country, species) %>%
  distinct()

chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
chl_landings_spp <- chl_landings %>%
  filter (year == 2021) %>%
  mutate (country = "Chile") %>%
  select (country, species) %>%
  distinct()

sau_peru <- sau_2019_country_spp %>% filter (country == "Peru")

peru_nutricast_missing <- nutricast_peru %>% 
  filter (!species %in% sau_peru$species)

peru_sau_missing <- sau_peru %>%
  filter (!species %in% nutricast_peru $species)



match_nutricast_taxa <- function (species_name, country_name) {
  # send SAU species name into nutricast df
  
  catch_upside_country <- catch_upside_relative_fam %>%
    filter (country == country_name) 
  
  # if identified to species level, clip to Genus
  if (grepl(" ", species_name)) {species_name = word(species_name, 1)}
  
  if (grepl ("ae", str_sub(species_name, -2, -1))) {
    match <- filter (catch_upside_country, Family == species_name)
    
    match_mean <- match %>%
      group_by (rcp, Family) %>%
      # take mean of all the ratio columns
      summarise (across(bau_ratio_midcentury:adapt_ratio_endcentury, mean, na.rm = TRUE)) %>%
      #rename (species = Family) %>%
      # remove to avoid name duplication
      select (-Family)
     
  
  } else {
    match <- filter (catch_upside_country, Genus == species_name)
    
    match_mean <- match %>%
      group_by (rcp, Genus) %>%
      # take mean of all the ratio columns
      summarise (across(bau_ratio_midcentury:adapt_ratio_endcentury, mean, na.rm = TRUE)) %>%
      #rename (species = Genus)
      select (-Genus)
    
  }

  
  return (tibble(match_mean))
  
}

match_nutricast_taxa("Caranx", "Peru")

peru_match_taxa_nutricast <- peru_sau_missing %>%
  mutate (nutricast = pmap(list(species_name = species, country = "Peru"), match_nutricast_taxa)
  ) %>% 
  unnest (cols = c(nutricast), names_repair = "check_unique")

length (unique (peru_match_taxa_nutricast$species)) # 17 of 65

# can I combine missing and non-missing
mega_peru <- sau_peru %>%
  left_join (catch_upside_relative, by = c("country", "species")) %>%
  mutate (nutricast = case_when (
    is.na (rcp) ~ pmap(list(species_name = species, country = "Peru"), match_nutricast_taxa), 
    TRUE ~ "NA"
  ))
# no...

# what is a smooth way of doing this all at once? making the missing data with 
check_missing_nutricast_spp <- function (country_name) {
  
  if (country_name == "Chile") {
    
    landed_spp <- chl_landings_spp 
  } else {
    
    landed_spp <- sau_2019_country_spp %>% filter (country == country_name)
  }
  
  
  nutricast_country <- catch_upside_relative %>% filter (country == country_name)
  
  landed_missing <- landed_spp %>%
    filter (!species %in% nutricast_country$species) %>%
    distinct()
  
  return (tibble(landed_missing))
  
}
  
i <- check_missing_nutricast_spp("Indonesia")
c <- check_missing_nutricast_spp("Chile")



nutricast_missing_spp <- map_dfr(as.list (c("Chile", "Peru", "Indonesia", "Sierra Leone")), check_missing_nutricast_spp)

nutricast_repair <- nutricast_missing_spp %>%
 mutate (nutricast = pmap(list(species_name = species, country_name = country), match_nutricast_taxa)
  ) %>% 
    unnest (cols = c(nutricast), names_repair = "check_unique")

saveRDS(nutricast_repair, file = "Data/catch_upside_relative_repair_missing.Rds")


#######################################################################################

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