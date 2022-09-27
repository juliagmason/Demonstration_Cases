# Clean and compile Sernapesca chile landings data
# 9 26 22
# JGM

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
    filter (!is.na (ESPECIE))
    
  
}

t <- map_dfr (chl_files, clean_chl_data)

chl_landings  <- t %>%
  left_join (chl_spp, by = "ESPECIE") %>%
  select (species, catch_mt, year, sector) %>%
  filter (!is.na (species))

saveRDS (chl_landings, file = "Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
