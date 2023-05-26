# Clean Malawi landings
# 5/26/23
# JGM

# Abby emailed 5/26/23

library (tidyverse)

mal_names <- read.csv ("Data/Malawi_names.csv") %>%
  rename (comm_name = Common.name,
          species = Scientific.name) %>%
  select (-Notes)

mal_ind <- read.csv("Data/Malawi_industrial.csv", header = T) %>%
  select (-Total) %>%
  # pivot_longer
  pivot_longer (-Year, 
                names_to = "comm_name",
                values_to = "tonnes") %>%
  left_join (mal_names, by = "comm_name") %>%
  mutate (sector = "Industrial")

mal_ssf <- read.csv ("Data/Malawi_SSF.csv", skip = 2, header= T) %>%
  select (-c(TOTAL, X))

# cut 2nd table
mal_ssf <- mal_ssf[1:12,] %>%
  pivot_longer (-Year, 
                names_to = "comm_name",
                values_to = "tonnes") %>%
  mutate (tonnes = trimws(tonnes),
          tonnes = as.double(str_replace(tonnes, ",", ""))) %>%
  left_join (mal_names, by = "comm_name") %>%
  mutate (sector ="Artisanal")


mal_landings <- rbind (mal_ind, mal_ssf)

saveRDS(mal_landings, file= "Data/Malawi_landings_cleaned.Rds")
