# Clean and compile Sernapesca chile landings data
# 9 26 22
# JGM

# species names:
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/http://www.sernapesca.cl/sites/default/files/2021_0104_nomina_de_especies.pdf


# 2017 and earlier: http://www.sernapesca.cl/informes/estadisticas
# 2018-2021: http://www.sernapesca.cl/informacion-utilidad/anuarios-estadisticos-de-pesca-y-acuicultura

# the files all have different naming conventions
# AND SPECIES NAMES ARE DIFFERENT, in all caps for some years...

# https://stackoverflow.com/questions/27198984/regex-match-if-strings-starts-with-a-letter-or-number-and-then


library (tidyverse)
library (readxl)

chl_spp_names <- read.csv("Data/Chile_spp_names_key.csv") %>% 
  mutate (ESPECIE = tolower (ESPECIE))

# functions 

# cleaning commas out
# http://earlh.com/blog/2009/06/29/cleaning-data-in-r-csv-files/
clean_commas <- function(ttt){
  as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
}

chl_files <- list.files ("Data/Chile_sernapesca/") %>% as.list()

clean_chl_data <- function (filename) {
  
  # if file starts with 2, year will be the first 4 characters. if file starts with d, year will be the last four characters before the period
  # for the second one, I could split at the period, and then take last 4 characters?
  if (grepl ("^d", filename)) {
    file_split = strsplit (filename, "\\.")[[1]][1] 
    year = str_sub (file_split, -4)
    } else {year = substr (filename, 1, 4)}
  
  sector <- ifelse (grepl ("arte", filename), "Artisanal", "Industrial")
  
  # skip is different too
  skip_lines <- ifelse (year == 2021, 5, 4)
  
  dat <- read_excel (paste0("Data/Chile_sernapesca/", filename), skip = skip_lines, col_names = TRUE) %>%
    select (ESPECIE, Total) %>%
    mutate (catch_mt = clean_commas (Total),
            ESPECIE = tolower (ESPECIE),
            year = year,
            sector = sector) %>% 
    filter (!is.na (ESPECIE)) %>%
    # get rid of rows with totals
    filter (!grepl("total", ESPECIE))
    
  
}

t <- map_dfr (chl_files, clean_chl_data)

# got all of the 2021 species. still 53 that don't match, some due to typos/small differences. 

chl_landings  <- t %>%
  left_join (chl_spp_names, by = "ESPECIE") %>%
  select (species, catch_mt, year, sector, taxa) %>%
  filter (!is.na (species)) %>%
  # fix some species names
  mutate (species = 
            case_when (species == "Kiphosus sandwicensis" ~ "Kyphosus sandwicensis",
                       species == "Mujil cephalus" ~ "Mugil cephalus", 
                       species == "Scomberesox saurus scombroides" ~ "Scomberesox saurus",
                       
                       # THIS ISN'T WORKING...but only 6 tonnes....
                       species == "Kiphosus sandwicensis" ~ "Kyphosus sandwicensis",
                       # trim white spaces
                       TRUE ~ trimws(species))) %>%
  rename (chl_taxa = taxa)

saveRDS (chl_landings, file = "Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# add SAU commercial group
chl_landings <- readRDS("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

#https://www.rdocumentation.org/packages/seaaroundus/versions/1.2.0
devtools::install_github("ropensci/seaaroundus")

# noooo not available
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

# SAU comm groups search
# https://www.seaaroundus.org/data/#/topic/biodiversity

# ones I classified without SAU:
#Gaterochisma melampus -- is a scombrid, therefore perch-like https://www.fishbase.se/summary/100
# Kiphosus--other spelling kyphosus. perch like. fix above...
# Sicyases sanguineus is a goby, perch-like
# Congiopodus peruvianus is scorpionfish? not sure, keep with other https://www.fishbase.se/summary/8398

# Genypterus is rightly in other fishes (cusk eel), as is Odonthestes regia, silverside, Cheilopogon rapanouiensis flying fish

#can't find Navodon paschalis


chl_spp <- chl_landings %>%
  select (species, chl_taxa) %>%
  distinct() %>%
  left_join (sau_2019_taxa, by = "species") %>%
  # fix missing
  mutate (
    commercial_group = case_when (
      chl_taxa == "Algae" ~ "Algae",
      chl_taxa == "Crustacean" ~ "Crustaceans",
      chl_taxa ==  "Mollusc" ~ "Mollusc", 
      chl_taxa == "Cephalopod" ~ "Other fishes & inverts",
      species %in% c("Sphyraena spp", "Callorhinchus callorhynchus") ~ "Sharks & rays",
      # change Kiphosus to Kyphosus here if can successfully change above
      species %in% c("Anisotremus scapularis", "Caranx georgianus", "Eleginops maclovinus", "Epigonus crassicaudus", "Nemadactylus gayi", "Polyprion oxygeneios", "Sciaena deliciosa", "Micropogonias manni", "Menticirrhus ophicephalus", "Gaterochisma melampus", "Kiphosus sandwicensis", "Sicyases sanguineus") ~ "Perch-likes",
      species %in% c("Cheilopogon rapanouiensis, Genypterus chilensis", "Gymnothorax porphyreus", "Scomberesox saurus", "Ophichthus remiger", "Eptatretus polytrema") ~ "Other fishes & inverts",
      species %in% c("Hippoglossina macrops", "Paralichthys spp") ~ "Flatfishes",
      species %in% c("Oncorhynchus tshawytscha") ~ "Salmon, smelts, etc",
      species %in% c("Sebastes capensis", "Helicolenus lengerichi") ~ "Scorpionfishes",
      species %in% c("Tetrapturus audax", "Tetrapturus spp") ~ "Tuna & billfishes",
      #species %in% c("Argobuccinum spp.", "Aulacomya ater") ~ "Molluscs",
      is.na (commercial_group) & chl_taxa == "Finfish" ~ "Other fishes & inverts",
      TRUE ~ commercial_group)
    )

chl_spp %>% filter (is.na(commercial_group)) %>% arrange (species) %>% View()


# join to landings data
chl_landings <- chl_landings %>%
  #remove repeated chl_taxa column
  select (-chl_taxa) %>%
  left_join (chl_spp, by = "species")

saveRDS (chl_landings, file = "Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
