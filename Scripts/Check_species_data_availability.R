# Check data availability for regional priority species
# 1/31/23 from previous regional_team_priority_species_figs.R code


library (tidyverse)


## Check data availability

# requested species
priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv") %>%
  # just change S. japonicus peruanus to S. japonicus; no nutrient or SAU or nutricast data
  mutate (species = case_when (species == "Scomber japonicus peruanus" ~ "Scomber japonicus",
                               TRUE ~ species)
  )



# nutrition ----
fishnutr_mu %>%
  right_join (priority_spp, by = "species")  %>%
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  filter (is.na(amount)) %>% 
  select (country, comm_name, species) %>% 
  distinct()


# related sp?
fishnutr_mu %>% filter (grepl("Stolephorus", species))

# sau ----
sau_avail <- sau_2019 %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  
  group_by (country, species, year) %>%
  summarise (sum_tonnes = sum (tonnes, na.rm = TRUE)) %>%
  group_by (country, species) %>%
  summarise (mean_tonnes = mean (sum_tonnes, na.rm = TRUE))

sau_avail %>% filter (country == "Mexico")

sau_avail %>% filter (country == "Indonesia")
# climate projections--which ones DONT have data
cc <- ds_spp %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (is.na (catch_mt)) %>%
  select (country, species) %>% 
  distinct() %>% 
  arrange (country)

cc %>% filter (country == "Indonesia")

ds_spp %>% filter (country == "Mexico", species == "Epinephelus morio") %>% View()