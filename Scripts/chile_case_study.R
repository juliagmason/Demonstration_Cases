## Translate Indonesia aquaculture production to nutrient content
# 20211907

library (tidyverse)

# Load and clean aquaculture data ----
# read in indonesia aquaculture production data

chile_data <- read.csv ("Data/chile_total.csv", 
                     # skip tells R to skip the first row, which doesn't contain information
                     skip = 4,
                     # header tells R that there's a row with the names of the columns
                     header = T)
head(chile_data)




# take a look at the data. This tells us that something weird happened and Snapper is being interpreted as a character vector
str(chile_data)
#deleting last two columns
chile_data[ , c('X.1', 'X.2')] <- list(NULL)

# Now we want to put the data into "long" format, where instead of one column for each species, we have a column called "species" and a column called "amount." This will make it easier to tack on nutrition data. We'll use the pivot_longer() command, which is super useful but I have to look up how to do it each time! https://tidyr.tidyverse.org/reference/pivot_longer.html and also probably exercises in the r4ds book
chile_data_long <- chile_data %>%
  pivot_longer (-Species, # telling R that we want to keep the "Species" column
                names_to = "Province", 
                values_to = "Catch_tons")

head (chile_data_long)
# should have 3 columns, Province (chr), Species (chr), and Amount (int)

# Finally, we need to fix the wonky species names so they match the nutrition data names. There are fancier ways of doing this, and it might be easier to have changed the column names in the original excel! This is more labor intensive but more flexible if you eventually find species names for snapper and grouper etc.
chile_data_long <- chile_data_long %>%
  mutate (Species = 
            case_when (Species == "Scomberesox saurus scombroides" ~ "Scomberesox saurus",
                       TRUE ~ Species) # this tells R to leave everything else as is
  )
# I'm not sure about pangasius, there are 2 species in the nutrient database. Double check to see if one is more commonly cultured than the other! And you can convert the other species into latin names if you find them by adding additional clauses in the case_when command.


# Load nutrient data ----
# Got this from https://fishbase.ca/Nutrients/NutrientSearch.php?&search_crit=geography&filter1=country&loc_list=360:Indonesia&nut=1&showAll=yes#!, selected all and clicked "Download"
chile_nutr <- read_csv ("Data/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_CHILE.csv")

head(chile_nutr)
# Join catch and nutrient data----
# We're going to use a join() command, which is extremely useful and should have exercises in the r4ds book
# We need to match the titles of the columns with the species names, which is the column that links the two datasets together
chile_nutr <- chile_nutr %>%
  rename (Species = `Scientific Name`)


chile_catch_nutr <- chile_data_long %>%
  # using left_join, which preserves all the information from indo_aq_long and joins the relevant information (just our 5 species so far). It will fill in NAs for Snapper, Shrimp etc. that don't match the nutrient data
  left_join (chile_nutr, by = "Species") %>%
  # use mutate to multiply the amount of catch by the nutrient content. catch is in tons, and nutrients are per 100 g. We want to convert everything to mg to match the nutrient requirement data. 
  mutate (Calcium = Catch_tons * `Calcium (mg/100g)` * 10000,
          Iron = Catch_tons * `Iron (mg/100g)` * 10000,
          `Vitamin A` = Catch_tons * `Vitamin A (μg/100g)` * 10000,
          Zinc = Catch_tons * `Zinc (mg/100g)` * 10000) %>% # continue this with the rest of the nutrient conversions...
  select (Province, Species, Catch_tons, Calcium, Iron, `Vitamin A`, Zinc) %>% # We don't need all of the columns. Just select the new nutrient columns you've made
  
  # Now we're going to convert to long format again, so we have one column with all the nutrients
  pivot_longer (-c(Province, Species, Catch_tons),
                names_to = "nutrient",
                values_to = "mg")

head(chile_catch_nutr)
# Join nutrient intake requirement data ----
# This is EAR (Estimated Average Requirement) data I downloaded from nutricast, with the daily recommended intake in mg/day. It's broken down by age and sex demographics, so if we can find those data for each province that would be ideal! Instead we'll just look at the average for adult men and women.
ear <- readRDS ("Data/dietary_reference_intake_data.Rds")
head(ear)
# take average for adult men and women
ear_avg <- ear %>%
  filter (stage == "None") %>%
  group_by (nutrient) %>%
  summarize (mn_value = mean(value, na.rm = TRUE))

chile_catch_nutr <- chile_catch_nutr %>%
  left_join (ear_avg, by = "nutrient") %>%
  mutate (inds_per_yr = (mg / mn_value / 365))


# Just look at Lampung province
vit_A_data_lampung<-indo_catch_nutr %>%
  filter (Province == "LAMPUNG")

show(vit_A_data_lampung)


indo_catch_nutr %>%
  mutate(Percent_pop = case_when(
    Province=="LAMPUNG"~ inds_per_yr/8458000 * 100,
    Province=="ACEH"~ inds_per_yr/5372000 * 100,
    Province=="SUMATERA UTARA"~ inds_per_yr/1456000 * 100,
    Province=="SUMATERA BARAT"~ 5534000,
    Province=="RIAU"~ inds_per_yr/6835000 * 100,
    Province=="JAMBI"~ inds_per_yr/3566000 * 100,
    Province=="SUMATERA SELATAN"~ inds_per_yr/8497000 * 100,
    Province=="BENGKULU"~ inds_per_yr/1972000 * 100,
    Province=="KEP. BANGKA BELITUNG"~ inds_per_yr/1517590 * 100,
    Province=="KEP. RIAU"~ inds_per_yr/2242000 * 100,
    Province=="DKI JAKARTA"~ inds_per_yr/1056000 * 100,
    Province=="JAWA BARAT"~ inds_per_yr/4994000 * 100,
    Province=="JAWA TENGAH"~ inds_per_yr/3455000 * 100,
    Province=="DI YOGYAKARTA"~ inds_per_yr/3689000 * 100,
    Province=="JAWA TIMUR"~ inds_per_yr/3974000 * 100,
    Province=="BANTEN"~ inds_per_yr/1316000 * 100,
    Province=="BALI"~ inds_per_yr/4362000 * 100,
    Province=="NUSA TENGGARA BARAT"~ inds_per_yr/5126000 * 100,
    Province=="KALIMANTAN BARAT"~ inds_per_yr/5069000 * 100,
    Province=="KALIMANTAN TENGAH"~ inds_per_yr/2650000 * 100,
    Province=="KALIMANTAN SELATAN"~ inds_per_yr/4304000 * 100,
    Province=="KALIMANTAN TIMUR"~ inds_per_yr/3721000 * 100,
    Province=="KALIMANTAN UTARA"~ inds_per_yr/701784 * 100,
    Province=="SULAWESI UTARA"~ inds_per_yr/2507000 * 100,
    Province=="SULAWESI TENGAH"~ inds_per_yr/3042000 * 100,
    Province=="SULAWESI SELATAN"~ inds_per_yr/8851000 * 100,
    Province=="SULAWESI TENGGARA"~ inds_per_yr/3042000 * 100,
    Province=="GORONTALO"~ inds_per_yr/1171681 * 100,
    Province=="SULAWESI BARAT"~ inds_per_yr/1536000 * 100,
    Province=="MALUKU"~ inds_per_yr/1848923 * 100,
    Province=="MALUKU UTARA"~ inds_per_yr/1278764 * 100,
    Province=="PAPUA BARAT"~ inds_per_yr/1134068 * 100,
    Province=="PAPUA"~ inds_per_yr/3379000 * 100,
    Province=="INDONESIA"~ inds_per_yr/270600000 * 100,
  ))






indo_catch_nutr %>%
  filter (Province == "LAMPUNG")

