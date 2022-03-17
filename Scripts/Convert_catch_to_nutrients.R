## Translate Indonesia aquaculture production to nutrient content
# 20211907
# updated 20220317

library (tidyverse)
library (measurements) # for converting units

# catch data/projections ----
# Species specific projection data from Chris Free 2020 ----

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")

# nutrient content for each species ----
# this has country and catch data for reference; take this out to join to catch
ds_spp_nutr_content_distinct <- readRDS ("Data/ds_spp_nutr_content_FishNutrientsGENuS_RDA_groups.Rds") %>%
  select (-c(rcp, scenario, country, catch_mt)) %>%
  distinct()

# maybe want just amount, not more info?
ds_spp_nutr_amount <- ds_spp_nutr_content_distinct %>%
  select (species, major_group, nutrient, amount) %>%
  distinct() %>%
  # specify units in terms that conv_unit can use
  mutate (dens_units = 
            case_when (
              nutrient %in% c("Protein", "Omega_3") ~ "g",
              nutrient %in% c("Vitamin_A", "Selenium") ~ "ug",
              TRUE ~ "mg"
            ))


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

# need to vectorize?
# https://stackoverflow.com/questions/44730774/how-to-use-custom-functions-in-mutate-dplyr
calc_nutr_supply_mt_V <- Vectorize (calc_nutr_supply_mt)
# scary, takes forever though

ds_spp_sm <- sample_n(ds_spp, 100)

ds_catch_nutr_content <- ds_spp_sm %>% 
  as_tibble() %>%
  filter (year > 2025, catch_mt > 0) %>%
  select (rcp, scenario, country, species, year, catch_mt) %>%
  # add nutrient data
  left_join (ds_spp_nutr_amount, by = "species") %>%
  # calculate edible meat.
  mutate(pedible=recode(major_group, 
                        "Finfish"= 0.87, 
                        "Crustaceans"=0.36, 
                        "Molluscs"=0.17, 
                        "Cephalopods"=0.21), 
         # edible meat in mt
         meat_mt = catch_mt * pedible,
         
         #available nutrient in mt
         #nutr_mt = mapply(calc_nutr_supply_mt, meat_mt, amount, dens_units)
         nutr_mt = pmap (list (meat_mt = meat_mt, nutr_dens = amount, nutr_dens_units = dens_units), calc_nutr_supply_mt)
  )  %>% unnest(cols = c(nutr_mt))


# doesn't make sense to do by year bc nutr_demand is in 5 year blocks. do three periods, 2020-2030, 2050-2060, 2090-2100
nutr_demand_periods <- nutr_demand %>%
  mutate (period = case_when (
    year %in% c(2020:2030) ~ "2020-2030",
    year %in% c(2050:2060) ~ "2050-2060",
    year %in% c(2090:2100) ~ "2090-2100"
  )) %>%
  filter (!is.na(period)) %>%
  group_by (country, nutrient, period) %>%
  summarise (mean_supply_req = mean (supply_req_mt_yr_50perc, na.rm = TRUE))

  





































#################################################
# Load and clean aquaculture data ----
# read in indonesia aquaculture production data
indo_aq <- read.csv ("Data/Indo_aq.csv", 
                     # skip tells R to skip the first row, which doesn't contain information
                     skip = 1,
                     # header tells R that there's a row with the names of the columns
                     header = T)

# get rid of the first row that just says 2017
indo_aq <- indo_aq[-1, ]

# rename the first column since we lost the "province" column name
colnames (indo_aq)[1] <- "Province"


# take a look at the data. This tells us that something weird happened and Snapper is being interpreted as a character vector
str(indo_aq)
indo_aq$Snapper <- as.integer (indo_aq$Snapper)


# Now we want to put the data into "long" format, where instead of one column for each species, we have a column called "species" and a column called "amount." This will make it easier to tack on nutrition data. We'll use the pivot_longer() command, which is super useful but I have to look up how to do it each time! https://tidyr.tidyverse.org/reference/pivot_longer.html and also probably exercises in the r4ds book
indo_aq_long <- indo_aq %>%
  pivot_longer (-Province, # telling R that we want to keep the "Province" column
                names_to = "Species", 
                values_to = "Catch_tons")

head (indo_aq_long)
# should have 3 columns, Province (chr), Species (chr), and Amount (int)

# Finally, we need to fix the wonky species names so they match the nutrition data names. There are fancier ways of doing this, and it might be easier to have changed the column names in the original excel! This is more labor intensive but more flexible if you eventually find species names for snapper and grouper etc.
indo_aq_long <- indo_aq_long %>%
  mutate (Species = 
    case_when (Species == "Gourame..Osphronemus.goramy." ~ "Osphronemus goramy",
               Species == "Patin..Pangasius." ~ "Pangasius pangasius",
               Species == "Cat.fish..Clarias.batrachus." ~ "Clarias batrachus",
               Species == "Nile.Tilapia..Oreochromis.niloticus." ~ "Oreochromis niloticus",
               Species == "Common.carp..Cyprinus.carpio." ~ "Cyprinus carpio",
               TRUE ~ Species) # this tells R to leave everything else as is
  )
    # I'm not sure about pangasius, there are 2 species in the nutrient database. Double check to see if one is more commonly cultured than the other! And you can convert the other species into latin names if you find them by adding additional clauses in the case_when command.


# Load nutrient data ----
# Got this from https://fishbase.ca/Nutrients/NutrientSearch.php?&search_crit=geography&filter1=country&loc_list=360:Indonesia&nut=1&showAll=yes#!, selected all and clicked "Download"
indo_nutr <- read_csv ("Data/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_INDONESIA.csv")

# Join catch and nutrient data----
# We're going to use a join() command, which is extremely useful and should have exercises in the r4ds book
# We need to match the titles of the columns with the species names, which is the column that links the two datasets together
indo_nutr <- indo_nutr %>%
  rename (Species = `Scientific Name`)

indo_catch_nutr <- indo_aq_long %>%
  # using left_join, which preserves all the information from indo_aq_long and joins the relevant information (just our 5 species so far). It will fill in NAs for Snapper, Shrimp etc. that don't match the nutrient data
  left_join (indo_nutr, by = "Species") %>%
  # use mutate to multiply the amount of catch by the nutrient content. catch is in tons, and nutrients are per 100 g. We want to convert everything to mg to match the nutrient requirement data. 
  mutate (Calcium = Catch_tons * `Calcium (mg/100g)` * 10000,
          Iron = Catch_tons * `Iron (mg/100g)` * 10000,
          `Vitamin A` = Catch_tons * `Vitamin A (µg/100g)` * 10000,
          Protein = `Protein (g/100g)`) %>% # continue this with the rest of the nutrient conversions...
  select (Province, Species, Catch_tons, Calcium, Iron, `Vitamin A`, Protein) %>% # We don't need all of the columns. Just select the new nutrient columns you've made
  
  # Now we're going to convert to long format again, so we have one column with all the nutrients
  pivot_longer (-c(Province, Species, Catch_tons),
                names_to = "nutrient",
                values_to = "mg")

indo_catch_nutr %>%
  ggplot () +
  geom_bar (aes (x = Species, y = mg, fill = nutrient), stat = "identity", position = "dodge")


# Join nutrient intake requirement data ----
# This is EAR (Estimated Average Requirement) data I downloaded from nutricast, with the daily recommended intake in mg/day. It's broken down by age and sex demographics, so if we can find those data for each province that would be ideal! Instead we'll just look at the average for adult men and women.
ear <- readRDS ("Data/dietary_reference_intake_data.Rds")

# take average for adult men and women
ear_avg <- ear %>%
  filter (stage == "None") %>%
  group_by (nutrient) %>%
  summarize (mn_value = mean(value, na.rm = TRUE))

indo_catch_nutr <- indo_catch_nutr %>%
  left_join (ear_avg, by = "nutrient") %>%
  mutate (inds_per_yr = (mg / mn_value / 365))


# Just look at Lampung province
indo_catch_nutr %>%
  filter (Province == "LAMPUNG") %>%
  ggplot () +
  geom_bar (aes (x = Species, y = inds_per_year, fill = nutrient), stat = "identity", position = "dodge")
