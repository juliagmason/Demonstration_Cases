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

chl_spp <- read.csv("Data/Chile_spp_names_key.csv") %>% 
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
  left_join (chl_spp, by = "ESPECIE") %>%
  select (species, catch_mt, year, sector, taxa) %>%
  filter (!is.na (species)) %>%
  # fix some species names
  mutate (species = 
            case_when (species == "Mujil cephalus" ~ "Mugil cephalus", 
                       # trim white spaces
                       TRUE ~ trimws(species)))

saveRDS (chl_landings, file = "Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# add SAU commercial group
chl_landings <- readRDS("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

#https://www.rdocumentation.org/packages/seaaroundus/versions/1.2.0
devtools::install_github("ropensci/seaaroundus")

# noooo not available
sau_2019_taxa <- readRDS("Data/SAU_2019_taxa.Rds")

chl_spp <- chl_landings %>%
  select (species, taxa) %>%
  distinct() %>%
  rename (chl_taxa = taxa) %>%
  left_join (sau_2019_taxa, by = "species") %>%
  # fix missing
  mutate (
    commercial_group = case_when (
      chl_taxa == "Algae" ~ "Algae",
      chl_taxa == "Crustacean" ~ "Crustaceans",
      chl_taxa ==  "Mollusc" ~ "Mollusc", 
      chl_taxa == "Cephalopod" ~ "Other fishes & inverts",
      species %in% c("Sphyraena spp", "Callorhinchus callorhynchus") ~ "Sharks & rays",
      species %in% c("Anisotremus scapularis") ~ "Perch-likes",
      # haven't gone through to correct this yet
      is.na (commercial_group) & chl_taxa == "Finfish" ~ "Other fishes & inverts",
      TRUE ~ commercial_group)
    )

chl_spp %>% filter (is.na(commercial_group)) %>% View()

View (chl_spp)

# join to landings data
chl_landings <- chl_landings %>%
  #remove repeated chl_taxa column
  select (-taxa) %>%
  left_join (chl_spp, by = "species")

saveRDS (chl_landings, file = "Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
