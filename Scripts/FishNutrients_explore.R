# FishNutrients explore
# 3/2/21
# JGM

library (tidyverse)


# can I get this from fishbase or do i need to relearn python
library (rfishbase)

fb_tbl("species") %>% 
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name =="Engraulis ringens")

#fb_tables()
  # there is a specific nutrients table, but not attached to species??

nutr_tbl <- fb_tbl("nutrients")  # this has a column for nutrient but not for species
spp_ref_tbl <- fb_tbl("nutrientssummary") # this has a ScientificName column. maybe NutrientsCode links them?

length (unique(nutr_tbl$NutrientsCode)) # 986
length (unique (spp_ref_tbl$NutrientsCode)) # 995

brama_test <- nutr_tbl %>% filter (NutrientsCode == 1) # only has protein. Fishbase entry has all nutrients. does not match fishbase entry. but Nutreitn RefNo does match

# doesn't seem ready: https://github.com/ropensci/rfishbase/issues/230
  
  