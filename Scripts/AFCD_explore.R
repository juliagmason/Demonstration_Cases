## explore AFCD data
# 12 13 2021
# JGM


# 1.	You have problems accessing the data
# 2.	You have problems understanding the data
# 3.	You have problems or greater than necessary challenges using the data
# 4.	The data looked super f***ed up

# cleaned version from CF
# https://github.com/cfree14/AFCD
#library (devtools)
#devtools::install_github("cfree14/AFCD", force=T)
library(AFCD)
library (tidyverse)

# cleaned version from zach, 5/31/22
#https://github.com/Aquatic-Food-Composition-Database/AFCD
devtools::install_github("Aquatic-Food-Composition-Database/AFCD", force=T)
library(AFCD)


# regional team priority spp--check inverts 7/27/22
afcd %>% filter (sciname == "Dosidicus gigas") %>% View()

"Megapitaria squalida" %in% afcd$sciname #no
"Chione californiensis" %in% afcd$sciname #no

"Scylla serrata" %in% afcd$sciname

# Indo team requested Scylla serrata, Kepiting bakau 


# picking somewhat randomly. note that there's a separate dha + epa for omegas, and different vitamin As
scylla_nutr <- afcd_sci %>% 
  filter (sciname == "Scylla serrata",
          nutrient_code_fao %in% c(
            "CA", "ZN", "FE", "SE", "Protein", "FAPU", "VITA")) %>%
            group_by(nutrient_code_fao) %>%
            summarise (amount = mean (value, na.rm = TRUE))

# Kelso requests ----

# blue swimming crab??

"Portunus pelagicus" %in% afcd_sci$sciname
afcd_sci %>% filter (sciname == "Portunus pelagicus") %>% View()
# KK request:  Kappaphycus alvarezii (commercially known as cottonii) and Eucheuma denticulatum (commercially known as spinosum).

"Kappaphycus alvarezii" %in% afcd_sci$sciname
"Eucheuma denticulatum" %in% afcd_sci$sciname

afcd_sci %>% filter (sciname == "Eucheuma denticulatum") %>% pull (country) %>% unique()
afcd_sci %>% filter (sciname == "Eucheuma denticulatum", country == "Kenya") %>% View() # protein, fat, ash
afcd_sci %>% filter (sciname == "Eucheuma denticulatum", country == "Unknown") %>% View() # different amino acids
afcd_sci %>% filter (sciname == "Eucheuma denticulatum", country == "United States") %>% View()
  pull(nutrient) %>% unique()

afcd_sci %>% filter (sciname == "Kappaphycus alvarezii") %>% pull (country) %>% unique()
afcd_sci %>% filter (sciname == "Kappaphycus alvarezii", country != "Unknown") %>% View()


colnames (afcd1)

# available "studies"
sort (unique (afcd1$country))

# what do some of these entail
# what does just Ufish look like--this is just shellfish and inverts
Ufish_afcd <- afcd1 %>%
  filter (country %in% c("FAO INFOODS Ufish"))
sort (unique(Ufish_afcd$sciname)) # 66 spp


afcd1 %>% filter (country %in% "FAO Biodiv 3") %>% select (sciname) %>% distinct() %>% pull (sciname) # 436 spp, looks like all fish


afcd1 %>% filter (country %in% "Not provided in unformatted AFCD") %>% View() # a ton of stuff, 6500 entries, all different nutrients
select (sciname) %>% distinct() %>% pull (sciname)

# do they have "Loxechinus albus", chilean sea urchin
"Loxechinus albus" %in% afcd1$sciname
afcd1 %>% filter (sciname == "Loxechinus albus") %>% View()
# yes, but fatty acid and protein only


# do they have the species from the country-level data

ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")
# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 

# probably will just use productivity only
# productivity difference from calculate_nutritional_upsides
ds_prod_diff <- readRDS("Data/ds_prod_diff.Rds")




# just look at sierra leone for prep meeting
sl_spp <- ds_prod_diff %>%
  filter (country == "Sierra Leone", rcp == "RCP60", year > 2025) %>%
  group_by (species) %>%
  summarise (cumulative_diff = sum(diff, na.rm = TRUE)) # 143 spp

length (which (sl_spp$species %in% afcd1$sciname)) # 91

# what are the top species for sierra leone now?
sl_top_catch <- ds_spp %>%
  filter (country == "Sierra Leone", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

sl_top5 <- afcd1 %>%
  filter (sciname %in% sl_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "FAO INFOODS West Africa", "FAO Biodiv 3", "Liberia", "Côte d’Ivoire", "Ghana", "Senegal", "Mauritania", "Nigeria" )) 

table (sl_top5$sciname)

# relevant "countries:" "FAO INFOODS Ufish"                "FAO INFOODS West Africa" "FAO Biodiv 3"  
# neighboring countries: "Côte d’Ivoire" "Ghana", "Senegal" "Mauritania" ? Nigeria?

# 779 spp for each country, so some must have NAs

chl_spp <- ds_spp %>%
  filter (country == "Chile", rcp == "RCP85", scenario == "Full Adaptation") %>%
  group_by (species) %>%
  summarise (tot_cat = sum (catch_mt)) %>%
  filter (tot_cat > 0) # 133 spp

indo_spp <- ds_spp %>%
  filter (country == "Indonesia", rcp == "RCP85", scenario == "Full Adaptation") %>%
  group_by (species) %>%
  summarise (tot_cat = sum (catch_mt)) %>%
  filter (tot_cat > 0) # 227spp

head (chl_spp$species)
length (which (!chl_spp$species %in% afcd1$sciname)) # 39
chl_spp$species[which (!chl_spp$species %in% afcd1$sciname)]

chl_afdc <- afcd1 %>%
  filter (country == "Chile")
chl_spp$species[which (!chl_spp$species %in% chl_afdc$sciname)]

length (which (!indo_spp$species %in% afcd1$sciname)) # 61
indo_spp$species[which (!indo_spp$species %in% afcd1$sciname)]


# filter relevant nutrients for top 5 species for each country ----
# omega 3, vitamin A, calcium, zinc, selenium, iron
# tons of fatty acids! Omega 3 is DHA, EPA, and alpha linolenic. DHA is 22:6(n-3). EPA is 20:5(n-3). ALA is 18:3, and (n−3).

afcd_top_nutr <- afcd1 %>%
  filter (nutrient %in% c ("Calcium", "Iron, total", "Selenium", 'Zinc', 
                           "Vitamin A; method of determination unknown",
                           "Vitamin A; sum of carotenoids",
                           "Vitamin A; sum of retinol/carotenoids", 
                           "Fatty acid 18:3",
                          "Fatty acid 18:3, cis n3",
                           "Fatty acid 20:5",
                           "Fatty acid 20:5, cis n3",
                           "Fatty acid 20:5, n3",
                           "Fatty acid 22:6",
                           "Fatty acid 22:6, cis n3",
                           "Fatty acid 22:6, n3")
          ) %>%
  mutate (nutrient = case_when (
    nutrient %in% c("Vitamin A; method of determination unknown",
                    "Vitamin A; sum of carotenoids",
                    "Vitamin A; sum of retinol/carotenoids") ~ "Vitamin A",
    nutrient %in% c("Fatty acid 18:3",
                    "Fatty acid 18:3, cis n3",
                    "Fatty acid 20:5",
                    "Fatty acid 20:5, cis n3",
                    "Fatty acid 20:5, n3",
                    "Fatty acid 22:6",
                    "Fatty acid 22:6, cis n3",
                    "Fatty acid 22:6, n3") ~ "Omega 3",
    nutrient == "Iron, total" ~ "Iron",
    TRUE ~ as.character(nutrient)
  ))

# 

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

chl_10 <- ds_spp %>%
  filter (country == "Peru", rcp == "RCP85", scenario == "No Adaptation", year == 2012) %>%
  slice_max(catch_mt, n = 10)
  
chl_top_nutr <-  afcd_top_nutr %>%
  filter (sciname %in% chl_10$species) %>%
  arrange (sciname)

write.excel(chl_top_nutr[1:50,])
write.excel(chl_top_nutr[51:101,])
write.excel(chl_top_nutr[102:200,])
write.excel(chl_top_nutr[201:281,])
write.excel(chl_top_nutr[301:373,])

# which nutrients are missing for each species?
missing <- chl_top_nutr %>%
  select (sciname, nutrient) %>%
  distinct() %>% 
  mutate (present = 1) %>%
  pivot_wider (names_from = nutrient, values_from = present) %>% 
  replace(is.na(.), 0) 

write.excel(missing)

# what would be the best way to do this? if there are multiple entries for a species, would want to get one from the same country or continent, and then look for preparations?

# start with top 10 catch spp for chile
chl_10 <- ds_spp %>%
  filter (country == "Chile", rcp == "RCP85", scenario == "No Adaptation", year == 2012) %>%
  slice_max(catch_mt, n = 10)

chl_10_nutr <- afcd1 %>%
  filter (sciname %in% chl_10$species, # 2021 entries
          country %in% c ("Chile", "FAO Latin Foods", "Peru", "Argentina", "FAO Biodiv 3", "Not provided in unformatted AFCD")) %>%
  arrange (sciname)# 203 entries, but 8 species, lose "Pseudocyttus maculatus"  and "Macruronus novaezelandiae" ) 

View (chl_10_nutr)
#note: d. gigas only toxins; e. ringens only fats; G. blacodes crazy amount; t. albacares only smoked

          
afcd1 %>%
  filter (sciname== "Pseudocyttus maculatus") # only one entry from new zealand, mercury
bg <- afcd1 %>% filter (sciname == "Macruronus novaezelandiae") # 237 obs
sort(unique (bg$country)) # aus, japan, nz
sort (unique (bg$nutrient))
unique (bg$food_part)
unique (bg$food_prep)


# move forward with 8 species
chl_10_nutr %>%
  select (sciname, food_prep, nutrient) %>%
  pivot_wider (names_from = food_prep, 
               values_from = nutrient) 

# dumb, t. albacares only smoked? no common food prep
# just proceed with means for now
chl_8_nutr_mn <- chl_10_nutr %>%
  group_by (sciname, nutrient) %>%
  summarise (mn_nutr = mean(value, na.rm = TRUE))# doesn't factor in pedible. could do an ifelse with pedible and then genus group values using gaines spp key from nutricast

# do a bigger cut
chl_mns <- afcd1 %>%
  filter (country %in% c ("Chile", "FAO Latin Foods", "Peru", "Argentina", "FAO Biodiv 3",  "FAO INFOODS Ufish",  "Not provided in unformatted AFCD")) %>%
  # do nutrients with the same unit, chose mg for now
  filter (nutrient_units == "mg") %>%
  group_by (sciname, nutrient) %>%
  summarise (mn_nutr = mean(value, na.rm = TRUE),
             mn_pedible = mean(edible_prop, na.rm = TRUE))

# filter a few nutrients of interest
# have to retain units. choose a few that have the same?
chl_mns_select <- chl_mns %>%
  filter (nutrient %in% c ( "Calcium","Iron, total","Zinc")) %>%
  rename (species = sciname, Metric = nutrient, Amount_mg = mn_nutr) # 466 species

# okay. not all have prop-edible. so would need to join to gaines key to get prop edible from genus and replace if NA. 
# need to get nutrient key from nutrient_endowment
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T) 
# 778 spp

spp_key_genus <- spp_key %>% select (species, genus_food_name)

length (which (unique(chl_afcd_nutr$species) %in% spp_key_genus$species)) # 123 species

# make a skinny version of ds_spp to multiply by catch?
ds_catch_chl <- ds_spp %>%
  filter (country == "Chile", year > 2030) %>%
  select (country, rcp, scenario, species, year, catch_mt) # have to keep country to join pop growth

length (which (unique(chl_afcd_nutr$species) %in% unique(ds_catch_chl$species))) # 125 species
length (which ( unique(ds_catch_chl$species) %in% spp_key_genus$species))#770

# of the 466 species in chl_mns_select, still missing genus or prop edible for 296. but when we just look at the species we have catch data for, only two are missing both: liza aurata (finfish) and theragra chalcogramma (finfish)



# pop growth
# Read projected human population growth
pop_growth <- readRDS(file.path( "../nutrient_endowment/data/population_growth/processed/WB_UN_1960_2100_human_population_by_country.Rds"))



# big join 

chl_afcd_nutr <- 
  chl_mns_select %>%
  left_join (spp_key_genus, by = "species") %>%
  # just retain species we have catch data for
  inner_join (ds_catch_chl, by = "species") %>%

 
  # from nutricast_Explore line 75, code from nutrient_endowment
  #filter(!is.na(genus_food_name)) %>% 
  mutate(major_group=recode(genus_food_name,
                            "Cephalopods"="Cephalopods",
                            "Crustaceans"="Crustaceans",
                            "Demersal Fish"="Finfish",
                            "Marine Fish; Other"="Finfish",
                            "Molluscs; Other"="Molluscs",
                            "Pelagic Fish"="Finfish"),
         genus_pedible=recode(major_group, 
                        "Finfish"=0.87, 
                        "Crustaceans"=0.36, 
                        "Molluscs"=0.17, 
                        "Cephalopods"=0.21), 
         meat_mt = case_when (
           !is.na (mn_pedible) ~ catch_mt * mn_pedible,
           is.na (mn_pedible) & !is.na (genus_pedible) ~ catch_mt * genus_pedible,
           is.na (mn_pedible) & is.na (genus_pedible) ~ catch_mt * 0.87)
           #is.na (mn_pedible) & is.na (genus_pedible) ~ catch_mt * mean (c(0.87, 0.36, 0.17, 0.21))
         ) %>%
  # Step 4. Add human population size
  left_join(pop_growth %>% select(country, year, pop_size_50perc), by=c("country", "year")) %>% 
  rename(human_pop_size=pop_size_50perc) %>% 
  # Step 5. Calculate daily per capita meat availability
  mutate(meat_g_dcap= (meat_mt*1000*1000) / 365 / human_pop_size,
         # changing code because this is in long format. make Amount column?
         nutr_mg_dcap = meat_g_dcap * Amount_mg / 100 # assuming values are for 100g serving
         ) %>% 
  # Step 7. Final cleanup
  # Eliminate regions without population growth data
  # Format RCP scenarios
  filter(!is.na(human_pop_size), catch_mt > 0)

# do these values make sense?? check with nutricast_explore
# Metric        mn_mg
# <chr>         <dbl>
#   1 Calcium     0.897  
# 2 Iron, total 0.0141 
# 3 Zinc        0.00958

# ca: 0.54, iron 0.01, zinc 0.006 for nutricast original


# do same thing as with nutricast
# join back to profits and catch? still have half long, half wide situation. do with rbind??
ds_chl <- ds_spp %>% 
  filter (country == "Chile") %>%
  select (rcp, scenario, species, year, catch_mt, profits_usd, bbmsy, ffmsy) %>%
  pivot_longer (catch_mt:ffmsy,
                names_to = "Metric",
                values_to = "Amount")

chl_afcd_long <- chl_afcd_nutr %>%
  rename (Amount = nutr_mg_dcap) %>%
  select (rcp, scenario, species, year, Metric, Amount)

chl_afcd_allmetrics <- rbind (ds_chl, chl_afcd_long)

# look at just no adaptation
chl_afcd1_BAU <- chl_afcd_allmetrics %>%
  filter (year > 2025, scenario == "No Adaptation", !Metric %in% c("bbmsy", "ffmsy")) %>%
  group_by (rcp, species, Metric) %>%
  summarise_if (is.numeric, sum, na.rm = TRUE) %>% 
  select (-year) %>%
  ungroup() %>% 
  group_by(rcp, Metric) %>%
  slice (which.max (Amount)) %>%
  pivot_wider (!Amount, names_from = Metric, 
               values_from = species) 

# adapt benefit
minus <- function(x,y){x-y}

chl_afcd1_adapt <-  chl_afcd_allmetrics %>%
  filter (year > 2025, scenario %in% c("No Adaptation", "Full Adaptation"), !Metric %in% c("bbmsy", "ffmsy")) %>%
  group_by (rcp, species, scenario, Metric) %>%
  summarise_if (is.numeric, sum, na.rm = TRUE) %>% 
  select (-year) %>%
  ungroup() %>% 
  pivot_wider (names_from = scenario, 
               names_sep = ".", 
               values_from = Amount) %>%
  mutate (adapt_diff = `Full Adaptation` - `No Adaptation`, 
          .keep = "unused") %>%
  group_by (rcp, Metric) %>%
  slice (which.max (adapt_diff)) %>%
  pivot_wider (!adapt_diff, names_from = Metric, values_from = species)

write.excel (chl_afcd1_adapt)
# look at most nutritious species? just look at calcium?
# note--this doesn't account for proportion edible

# takes forever to filter for species in ds_chl. make a vector of species for which there's catch in Chile
chl_catch_spp <- ds_spp %>% 
  filter (country == "Chile") %>% 
  group_by (species) %>% 
  summarise (sum_catch = sum(catch_mt)) %>%
  filter (sum_catch > 0) # 133 spp

# What are the chilean species that have the highest calcium content?
chl_top_ca <- chl_mns_select %>%
  filter (Metric == "Calcium",
          species %in% chl_catch_spp$species) %>%  # only makes sense if we have catch data
  ungroup () %>%
  slice_max (Amount_mg, n = 10) %>%
  left_join (spp_key_genus, by = "species") %>%
  mutate(major_group=recode(genus_food_name,
                            "Cephalopods"="Cephalopods",
                            "Crustaceans"="Crustaceans",
                            "Demersal Fish"="Finfish",
                            "Marine Fish; Other"="Finfish",
                            "Molluscs; Other"="Molluscs",
                            "Pelagic Fish"="Finfish"),
         genus_pedible=recode(major_group, 
                              "Finfish"=0.87, 
                              "Crustaceans"=0.36, 
                              "Molluscs"=0.17, 
                              "Cephalopods"=0.21)
  ) 

# how are these species doing under no adaptation?
ds_spp %>%
  filter (country == "Chile", species %in% chl_top_ca$species, scenario == "No Adaptation") %>%
  ggplot () +
  geom_line (aes (x = year, y = bbmsy, col = species)) +
  facet_wrap(~rcp) +
  geom_hline (yintercept = 1, lty = 2)

# or just look at bbmsy in 2012?
ds_spp %>%
  filter (country == "Chile", species %in% chl_top_ca$species, scenario == "No Adaptation", year == 2012, rcp == "RCP26") %>%
  select (species, bbmsy, ffmsy)

# make some kind of plot having to do with 2012 that shows status and catch proportion
ds_chl_totcat_2012 <- ds_spp %>%
  filter (country == "Chile", scenario == "No Adaptation", rcp == "RCP26", year == 2012) %>%
  summarise (tot_cat = sum(catch_mt)) %>%
  pull (tot_cat)

ds_spp %>%
  filter (country == "Chile", species %in% chl_top_ca$species, scenario == "No Adaptation", year == 2012, rcp == "RCP26") %>%
  left_join (chl_top_ca, by = "species") %>%
  mutate (catch_prop = catch_mt/ds_chl_totcat_2012,
          ca_level = ifelse (
            !is.na(mn_pedible), Amount_mg * mn_pedible, Amount_mg* genus_pedible)) %>% 

  ggplot (aes (x = bbmsy, y = ffmsy, label = species, col = ca_level)) +
  geom_point (aes(size = catch_prop), alpha = 0.5) +
  geom_text (size = 5, vjust = 1) +
  geom_hline (yintercept = 1, lty = 2) +
  geom_vline (xintercept = 1, lty = 2) +
  theme_bw()


##### initial explore
?afcd1

head (unique (afcd1$sciname))


"Merluccius gayi" %in% afcd1$sciname # YES!!

"Engraulis ringens" %in% afcd1$sciname # YES

#WRI sophie wood
"Merluccius capensis" %in% afcd1$sciname #no
"Merluccius paradoxus" %in% afcd1$sciname # no
"Trachurus trachurus" %in% afcd1$sciname # yes
"Trachurus capensis" %in% afcd1$sciname # no

anchov <- afcd1 %>%
  filter (grepl("Engraulis", sciname))
unique (anchov$sciname)# still some errors

anchov %>%
  filter (sciname == "Engraulis ringens") %>%  View()
filter (nutrient %in% c("Zinc", "Folate, total", "Total fatty acids, polyunsaturated")) %>% View()
group_by (country, food_prep, nutrient) %>%
  summarise (nutr_mn = mean (value, na.rm = TRUE)) %>% View()
pivot_wider (names_from = nutrient,
             values_from = nutr_mn)

anchov %>%
  filter (nutrient %in% c("Zinc", "Folate, total", "Total fatty acids, polyunsaturated")) %>%
  group_by (food_prep, nutrient) %>%
  summarise (mean_val = mean (value * edible_prop, na.rm = TRUE)) %>%
  ggplot (aes (x = food_prep, y = mean_val, fill = nutrient)) +
  geom_bar(stat = "identity")


m_gayi <- afcd1 %>%
  filter (sciname == "Merluccius gayi")

m_gayi_gayi <- afcd1 %>%
  filter (sciname == "Merluccius gayi gayi") # actually more, all chile, but all fatty acids. but here food_part is unique and food_prep is the same. 

# https://github.com/zachkoehn/aquatic_foods_nutrient_database
# column descriptions: https://docs.google.com/spreadsheets/d/1edCEIJftNJQc3TG5z4nDaho6_T638BlS/edit#gid=1373938481
# download: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KI0NYM 

afcd <- read_csv ("Data/20210914_AFCD.csv")


m_gayi <- afcd %>%
  filter (taxa_name == "merluccius gayi")
# data from fao latin and mauritania. mauritania has more minerals [toxics? mercury, cadmium, arsenic, lead. not necessarily, this category also has calcium, zinc. any element]. food prep would make most sense to categorize
m_gayi %>%
  group_by (food_prep, nutrient) %>%
  summarise (mn_nutr = mean (value, na.rm = TRUE)) %>%
  arrange (desc (mn_nutr))

m_gayi[,10:20]
View (m_gayi) # 7 records, 6 seem identical, no nutrition info?

m_gayi_num <- m_gayi %>%
  select (where(is.numeric)) %>% # grab only numeric columns
  sums_gayi <-  apply (m_gayi_num, 2, sum, na.rm = TRUE)
length (which (sums_gayi > 0))
m_gayi[, which (sums_gayi > 0)]

anchov <- afcd %>%
  filter (genus == "engraulis") %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x) 

unique (anchov$taxa_name) # engraulis ringeRs???
eng_ring <- afcd %>%
  filter (taxa_name == "engraulis ringers") %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x) %>% View()

# Delve into AFCD data ----
sort (unique (afcd1$country))


# just look at top catch spp for each country

# sierra leone
sl_catch <- ds_spp %>%
  filter (country == "Sierra Leone", rcp == "RCP60", year == 2012, scenario == "No Adaptation", catch_mt > 0)
sl_top_catch <- ds_spp %>%
  filter (country == "Sierra Leone", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

sl_top5_afcd <- afcd1 %>%
  filter (sciname %in% sl_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "FAO INFOODS West Africa", "FAO Biodiv 3", "Liberia", "Côte d’Ivoire", "Ghana", "Senegal", "Mauritania", "Nigeria" )) 



# chile 
chl_top_catch <- ds_spp %>%
  filter (country == "Chile", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

chl_top5_afcd <- afcd1 %>%
  filter (sciname %in% chl_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "Chile", "FAO Latin Foods", "Peru", "Argentina", "FAO Biodiv 3", "Not provided in unformatted AFCD")) 

# Peru
peru_top_catch <- ds_spp %>%
  filter (country == "Peru", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

# can probably use the same sources?
peru_top5_afcd <- afcd1 %>%
  filter (sciname %in% peru_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "Chile", "FAO Latin Foods", "Peru", "Argentina", "FAO Biodiv 3", "Not provided in unformatted AFCD")) 

# indo
indo_top_catch <- ds_spp %>%
  filter (country == "Indonesia", rcp == "RCP60", year == 2012, scenario == "No Adaptation") %>%
  slice_max (catch_mt, n = 5)

indo_top5_afcd <- afcd1 %>%
  filter (sciname %in% indo_top_catch$species,
          country %in% c ("FAO INFOODS Ufish", "FAO Biodiv 3", "Not provided in unformatted AFCD", "Indonesia", "Pacific Region", "Malaysia"))


