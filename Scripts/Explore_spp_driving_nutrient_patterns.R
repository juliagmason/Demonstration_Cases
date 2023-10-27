# explore which species are driving nutrient patterns
# 10/26/23
# JGM

library (tidyverse)
library (treemap)
# indonesia has unusually high yield of vitamin A. I want to be able to look at the species contributing to catch, and color them by vitamin A intensity?

# compiled in compile_species_nutrition_data.R
#this has Fishnutr, AFCD, and D. gigas
compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")

compiled_nutr %>%
  ggplot (aes (fill = taxa)) +
  geom_histogram(aes(amount)) +
  facet_wrap(~nutrient, scales = "free")

# matched fishnutr data for missing species ----
# from Match_SAU_FishNutrients_taxa.R
# depends on country. Finfish only; potentially could do with nonfish, but might have them all through AFCD?
fish_taxamatch_nutr <- readRDS("Data/Matched_finfish_nutr.Rds") 

sau_2019 <- readRDS("Data/SAU_2019.Rds")


indo_landings <- sau_2019 %>% filter (country == "Indonesia") %>%
  group_by (species) %>% 
  summarise (tonnes = sum (tonnes))

indo_landings_nutr <- indo_landings %>% left_join (compiled_nutr, by = "species")

# grab species that didn't match and try again with missing species df. have to add country column
indo_missing <- indo_landings_nutr %>%
  filter (is.na (nutrient)) %>%
  select (species, tonnes) %>%
  mutate (country = "Indonesia") %>%
  left_join (fish_taxamatch_nutr, by = c ("species", "country")) %>%
  # reorder columns to match indo_landings_nutr
  select (species, tonnes, nutrient, amount, taxa)

indo_landings_nutr <- rbind (indo_landings_nutr, indo_missing) 

# try to visualize with treemap?
library (treemapify)


indo_landings_nutr %>%
  filter (nutrient == "Vitamin_A") %>%
ggplot (aes(area = tonnes, fill = amount, label = species, subgroup = taxa)) +
  geom_treemap() +
  geom_treemap_text()

# it's really Lutjanus as a category
# look back at data I drew from

fishnutr_indo <- read.csv("Data/FishNutrients_country/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_INDONESIA.csv")
fishnutr_indo %>% filter (Family == "Lutjanidae") %>% View()


indo_landings_nutr %>%
  filter (nutrient == "Calcium") %>%
  ggplot (aes(area = tonnes, fill = amount, label = species)) +
  geom_treemap() +
  geom_treemap_text()

# https://r-graph-gallery.com/236-custom-your-treemap.html
library (treemap)

#mapping=c(min(indo_vitA$amount), mean(range(indo_vitA$amount)), max(indo_vitA$amount))

treemap (filter (indo_landings_nutr, nutrient == "Vitamin_A"), index = c("taxa", "species"),
         vSize="tonnes", type="value",
         vColor = "amount",
         #palette="RdYlBu",
         #mapping=c(min(indo_vitA$amount, na.rm = TRUE), mean(range(indo_vitA$amount, na.rm = TRUE)), max(indo_vitA$amount, na.rm = TRUE)),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Indonesia catch, Vitamin A"
)


# try with exports?? ----
# Sierra Leone IHH data
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

# exports ARTIS ----
# updated with 8/30 data using harmonized SAU names. This has 2011-2019.
# just use 2019?
# not taking five year mean, this is just 2019 data. so just need to fix countries and species names
# export percent production is in percentage points, convert
exports <- read_csv ("Data/20231016_edf_ARTIS_snet.csv") %>%
  mutate (species = str_to_sentence(sciname),
          prop_exp = exports_percent_of_prod/100,
          # just doing country names by hand...
          country = case_when (
            exporter_iso3c == "CHL" ~ "Chile",
            exporter_iso3c == "IDN" ~ "Indonesia",
            exporter_iso3c == "PER" ~ "Peru", 
            exporter_iso3c == "MEX" ~ "Mexico",
            exporter_iso3c == "SLE" ~ "Sierra Leone",
            exporter_iso3c == "MWI" ~ "Malawi"), 
          .keep = "unused") %>%
  # # remove aquaculture and inland
  filter (method == "capture",
          habitat == case_when(country == "Malawi" ~ "inland",
                               TRUE ~ "marine")) %>%
  filter (year == 2019)


sl_export <- sl_landings %>%
  filter (year == 2017, !is.na(catch_mt)) %>%
  # aggregate small and large scale
  group_by (country, species) %>%
  summarise (catch_mt = sum(catch_mt, na.rm = TRUE)) %>%
  # join to ARTIS data
  left_join(exports, by = c("species", "country")) %>%
  #for species missing data, assume zero exports
  replace_na (list(prop_exp = 0)) %>%
  mutate (Exported = catch_mt * prop_exp,
          Retained = catch_mt * (1-prop_exp)) %>%
  # cut unnecessary columns
  select (country, species, Exported, Retained) %>%
  # pivot longer
  pivot_longer (Exported:Retained, 
                names_to = "exports",
                values_to = "catch_mt") 

# join to nutrients
sl_export_nutr <-  sl_export %>% left_join (compiled_nutr, by = "species")

# grab species that didn't match and try again with missing species df. have to add country column
sl_missing <- sl_export_nutr %>%
  filter (is.na (nutrient)) %>%
  select (country, species, exports, catch_mt) %>%
  left_join (fish_taxamatch_nutr, by = c ("species", "country")) %>%
  # reorder columns to match indo_landings_nutr
  select (country, species, exports, catch_mt, nutrient, amount, taxa)

sl_export_nutr <- rbind (sl_export_nutr, sl_missing) 


treemap (filter(sl_export_nutr, nutrient == "Vitamin_A"), index = c("exports", "species"),
         vSize="catch_mt", type="value",
         vColor = "amount",
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Vitamin A, Sierra Leone exports"
)

treemap (filter(sl_export_nutr, nutrient == "Calcium"), index = c("exports", "species"),
         vSize="catch_mt", type="value",
         vColor = "amount",
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Sierra Leone exports, Calcium"
)

treemap (filter(sl_export_nutr, nutrient == "Calcium"), index = c("exports", "species"),
         vSize="catch_mt", type="value",
         vColor = "amount",
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Sierra Leone exports, Calcium"
)

treemap (filter(sl_export_nutr, nutrient == "Selenium"), index = c("exports", "species"),
         vSize="catch_mt", type="value",
         vColor = "amount",
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Sierra Leone exports, Selenium"
)


# sl compare large and small ----
sl_ssf <- sl_landings %>%
  filter (year == 2017, !is.na(catch_mt)) %>%
  select (-taxa) %>%
  # join to nutrients
  left_join (compiled_nutr, by = "species") %>%
  select (country, sector, species, catch_mt, nutrient, amount, taxa)

# grab species that didn't match and try again with missing species df. have to add country column
sl_missing_ssf <- sl_ssf %>%
  filter (is.na (nutrient)) %>%
  select (country, sector, species,catch_mt) %>%
  left_join (fish_taxamatch_nutr, by = c ("species", "country")) %>%
  # reorder columns to match indo_landings_nutr
  select (country, sector, species, catch_mt, nutrient, amount, taxa)

sl_ssf_nutr <- rbind (sl_ssf, sl_missing_ssf) 


treemap (filter(sl_ssf_nutr, nutrient == "Vitamin_A"), index = c("sector", "species"),
         vSize="catch_mt", type="value",
         vColor = "amount",
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Sierra Leone sectors, Vitamin A"
)
