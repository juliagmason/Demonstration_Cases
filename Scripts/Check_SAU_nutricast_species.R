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
# write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
#   write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
# }

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

# 6/2/23 updating with SierraLeone IHH data
sl_landings_ihh <- readRDS("Data/SLE_landings_IHH.Rds")
sl_ihh_spp <- sl_landings_ihh %>%
  mutate (country == "Sierra Leone") %>%
  select (country, species) %>%
  distinct()


# Function to take landed species name and match to nutricast spp by genus or  ----

match_nutricast_taxa <- function (species_name, country_name) {
  # send SAU or lande species name into nutricast df
  
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

# Function to grab missing species that don't match nutricast ----
check_missing_nutricast_spp <- function (country_name) {
  
  if (country_name == "Chile") {
    
    landed_spp <- chl_landings_spp 
  } else if (country_name == "Sierra Leone") {
    landed_spp <- sl_ihh_spp
  } else {
    
    landed_spp <- sau_2019_country_spp %>% filter (country == country_name)
  }
  
  
  nutricast_country <- catch_upside_relative %>% filter (country == country_name)
  
  landed_missing <- landed_spp %>%
    filter (!species %in% nutricast_country$species) %>%
    distinct()
  
  return (tibble(landed_missing))
  
}
  
# i <- check_missing_nutricast_spp("Indonesia")
# c <- check_missing_nutricast_spp("Chile")
# 


nutricast_missing_spp <- map_dfr(as.list (c("Chile", "Peru", "Indonesia", "Sierra Leone")), check_missing_nutricast_spp)

nutricast_repair <- nutricast_missing_spp %>%
 mutate (nutricast = pmap(list(species_name = species, country_name = country), match_nutricast_taxa)
  ) %>% 
    unnest (cols = c(nutricast), names_repair = "check_unique")

# update with ihh
sl_ihh_missing_spp <- check_missing_nutricast_spp("Sierra Leone")
sl_ihh_repair <- sl_ihh_missing_spp %>%
  mutate (nutricast = pmap(list(species_name = species, country_name = country), match_nutricast_taxa)
  ) %>% 
  unnest (cols = c(nutricast), names_repair = "check_unique")

nutricast_repair <- readRDS("Data/catch_upside_relative_repair_missing.Rds")

nutricast_repair_ihh <- rbind (nutricast_repair, sl_ihh_repair)

# remove duplicates
nutricast_repair_ihh <- nutricast_repair_ihh [!duplicated(nutricast_repair_ihh ), ]

saveRDS(nutricast_repair, file = "Data/catch_upside_relative_repair_missing.Rds")

# also do this with annual ts ----
nutricast_annual <- readRDS ("Data/nutricast_upside_relative_annual_ratio.Rds")


nutricast_annual_fam <- nutricast_annual %>%
  left_join (nutricast_fams, by = "species") %>%
  # grab Genus even if not in fishbase
  mutate (Genus = 
            case_when (is.na (Genus) ~ word(species, 1),
                       TRUE ~ Genus)
  )

# for this one we only need to mutate one column, catch_ratio
match_nutricast_taxa_annual <- function (species_name, country_name) {
  # send SAU species name into nutricast df
  
  catch_upside_country <- nutricast_annual_fam %>%
    filter (country == country_name) 
  
  # if identified to species level, clip to Genus
  if (grepl(" ", species_name)) {species_name = word(species_name, 1)}
  
  # if identified at family level, choose family
  if (grepl ("ae", str_sub(species_name, -2, -1))) {
    match <- filter (catch_upside_country, Family == species_name)
    
    match_mean <- match %>%
      group_by (rcp, scenario, year, Family) %>%
      # take mean of all the ratio columns
      summarise (catch_ratio = mean (catch_ratio, na.rm = TRUE)) %>%
      #rename (species = Family) %>%
      # remove to avoid name duplication
      select (-Family)
    
    
  } else {
    match <- filter (catch_upside_country, Genus == species_name)
    
    match_mean <- match %>%
      group_by (rcp, scenario, year, Genus) %>%
      # take mean of all the ratio columns
      summarise (catch_ratio = mean (catch_ratio, na.rm = TRUE)) %>%
      #rename (species = Genus)
      select (-Genus)
    
  }
  
  
  return (tibble(match_mean))
  
}

nutricast_repair_annual <- nutricast_missing_spp %>%
  mutate (nutricast = pmap(list(species_name = species, country_name = country), match_nutricast_taxa_annual)
  ) %>% 
  unnest (cols = c(nutricast), names_repair = "check_unique")

saveRDS(nutricast_repair_annual, file = "Data/nutricast_upside_relative_annual_repair_missing.Rds")

# update with ihh
sl_ihh_missing_spp <- check_missing_nutricast_spp("Sierra Leone")
sl_ihh_repair_annual <- sl_ihh_missing_spp %>%
  mutate (nutricast = pmap(list(species_name = species, country_name = country), match_nutricast_taxa_annual)
  ) %>% 
  unnest (cols = c(nutricast), names_repair = "check_unique")

nutricast_repair_annual <- readRDS("Data/nutricast_upside_relative_annual_repair_missing.Rds")

nutricast_repair_annual_ihh <- rbind (nutricast_repair_annual, sl_ihh_repair_annual)

# remove duplicates
nutricast_repair_annual_ihh <- nutricast_repair_annual_ihh [!duplicated(nutricast_repair_annual_ihh), ]

saveRDS(nutricast_repair_annual, file = "Data/nutricast_upside_relative_annual_repair_missing.Rds")

#######################################################################################

