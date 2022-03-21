# Plot most nutritious species
# 3/11/22

library (tidyverse)
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)
library (ggradar) # for spider plots
library (ggrepel) # for scatter plots

# compiled species-level nutrient data in Scripts --> Species_level_nutrient_content
#use the one with RDAs calculated
ds_spp_nutr_content <- readRDS("Data/ds_spp_nutr_content_FishNutrientsGENuS_RDA_groups.Rds")

#FishNutrients data for finfish and GENuS data for nonfish. units are the database units, Calcium is mg. Iron is mg Selenium ug, Zinc mg, Vit A ug, Omega 3 g, Protein g


# What are the top five species that provide each nutrient? ----

# careful, get a lot of ties for nonfish. high iron and zinc levels
# 
# ds_spp_nutr_content %>%
#   filter (!is.na (amount)) %>%
#   group_by (country, nutrient) %>%
#   slice_max (amount, n = 10) %>%
#   ungroup() %>%
#   arrange (country, nutrient, desc(amount)) %>%
#   left_join (catch_prop, by = c("country", "species")) %>% View()


# spider plots for top catch for each country ----
# code following nutrient_endowment --> shiny --> v3 --> Page3_Fig2a_fish_radar.R

# Changing as of 3/18/22. Instead of relative nutrient yields compared to other species the country catches, y axis is the percent of RDA for children that one 100g serving of that species would provide. 

plot_spp_nutr_radar <- function (country_name, n_spp) {
  
  # grab desired # of species
  top_catch <- ds_spp_nutr_content %>% 
    filter (country == country_name) %>%
    select (species, catch_mt) %>%
    distinct() %>%
    slice_max (catch_mt, n = n_spp)
  
  # filter nutrient content data
  nutr_radar_plot <- ds_spp_nutr_content %>%
    # from nutricast, express in terms of proportion of maximum. so first get proportion of maximum from within country catch, and then filter the top species
    filter (country == country_name, 
            group == "Child",
            species %in% top_catch$species) %>%
    select (species, nutrient, perc_rda) %>%
    pivot_wider (
      names_from = nutrient,
      values_from = perc_rda) %>%
    # radar plot can't deal with NA and non-fish don't have selenium
    replace_na (list (Selenium = 0)) 
  
  # calculate maximum value for plot specification; looks like will be calcium
  #max_value = max(nutr_radar_plot$Calcium)
  
  
  ggradar(nutr_radar_plot,
          grid.min = 0, grid.max = 100, 
          group.point.size = 2,
          group.line.width = 1,
          legend.text.size = 8,
          axis.label.size = 4,
          grid.label.size = 4,
          legend.position = "right") +
    ggtitle (country_name) +
    theme (plot.title = element_text (size = 14))

  }

#plot_spp_nutr_radar(country_name = "Sierra Leone", n_spp = 10)



### plot nutrient density vs. amount of catch
# function to make smaller spp name
# i know there's a cleaner way to do this....smaller name for labels
# https://stackoverflow.com/questions/38665221/get-all-the-string-elements-after-first-space
truncate_name <- function (name){
  genus <- substr (name, 1,1)
  #spp <- strsplit(name, " ",)[[1]][2]
  spp <- sub("^\\S+\\s+", '', name)
  
  name_sm <- paste0 (genus, ". ", spp)
  return (name_sm)
}

plot_spp_nutr_v_catch <- function (vuln_group, anchovy = TRUE) {
  # vuln_group could be e.g. "Child", "Lactating", "Pregnant
  #micronutrient density as sum of percent RDA for children from Maire et al. 2021
  
  country_spp_dens <- ds_spp_nutr_content %>%
    filter (group == vuln_group, !is.na (perc_rda)) %>%
    group_by (country, species) %>%
    mutate (micronutrient_density = sum(perc_rda)) %>%
    select (country, species, catch_mt, major_group, micronutrient_density, label) %>%
    distinct() %>%
    ungroup()
  
  label_spp <- country_spp_dens %>%
    group_by (country) %>%
    slice_max (amount, prop = 0.05) %>%
    mutate (name_sm = truncate_name(species))

  
  ggplot (country_spp, aes (x = log(tot_cat), y = amount)) +
  #geom_point (aes(col = major_group)) +
    geom_point() +
    facet_wrap (~nutrient, scales = "free") +
    theme_bw() +
  geom_text_repel (data = label_spp, aes(x = log(tot_cat), y = amount, label = name_sm),
         size = 2.5) +
    ggtitle (country_name) +
    labs (x = "Log(catch), mt", col = "", y = "Nutrient density, units vary")

}

#plot_spp_nutr_v_catch ("Peru")
# plot overall micronutrient density vs. catch ----  
catch_micronutr_density <- ds_spp_nutr_content %>%
  filter (group == "Child", !is.na (perc_rda)) %>%
  group_by (country, species) %>%
  mutate (micronutrient_density = sum(perc_rda),
          label = ifelse (micronutrient_density > 450 | catch_mt > 500000, 1, 0)) %>%
  select (country, species, catch_mt, major_group, micronutrient_density, label) %>%
  distinct() %>%
  ungroup()

ggplot (catch_micronutr_density, aes (x = log(catch_mt), y = micronutrient_density)) +
  geom_point (aes(col = major_group)) +
  geom_text (data = filter (catch_micronutr_density, label == 1), aes (label = species)) +
  theme_bw() +
  facet_wrap (~country, scales = "free") +
  labs (y = "Micronutrient density", x = "log (Catch, mt)", col = "") +
  ggtitle ("Micronutrient density vs current catch volume")