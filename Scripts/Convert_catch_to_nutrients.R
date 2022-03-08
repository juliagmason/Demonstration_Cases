## Translate Indonesia aquaculture production to nutrient content
# 20211907

library (tidyverse)

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
