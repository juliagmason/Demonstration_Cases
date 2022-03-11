# Plot most nutritious species
# 3/11/22

library (tidyverse)
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)
library (ggradar) # for spider plots
library (ggrepel) # for scatter plots

# compiled species-level nutrient data in Scripts --> Species_level_nutrient_content
#ds_spp_nutr_content <- readRDS("Data/ds_spp_nutr_content_FishNutrientsGENuS.Rds")

#FishNutrients data for finfish and GENuS data for nonfish. units are the database units, Calcium is mg. Iron is mg Selenium ug, Zinc mg, Vit A ug, Omega 3 g, Protein g

# catch proportions, from Scripts --> Species_catch_proportions
#catch_prop <- readRDS("Data/ds_spp_catch_proportions_2012.Rds")



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

# This will be a spider/radar plot for the desired number of species representing the most catch by volume. The y axis represents the relative nutrient content compared to the species with the highest density of that nutrient. So 100% zinc means that species provides the most zinc of any species in that country. 50% means that species has half as much zinc as the highest zinc providing species.
plot_spp_nutr_radar <- function (country_name, n_spp) {
  
  # grab desired # of species
  top_catch <- catch_prop %>% 
    filter (country == country_name) %>%
    slice_max (prop_catch, n = n_spp)
  
  # filter nutrient content data
  nutr_radar_plot <- ds_spp_nutr_content %>%
    # from nutricast, express in terms of proportion of maximum. so first get proportion of maximum from within country catch, and then filter the top species
    filter (country == country_name) %>%
    group_by (nutrient) %>%
    mutate(amount_prop=amount/max(amount, na.rm = TRUE)) %>% 
    ungroup() %>%
    filter (species %in% top_catch$species) %>%
    select (-c(country, genus_food_name, catch_mt, amount)) %>%
    pivot_wider (
      names_from = nutrient,
      values_from = amount_prop) %>%
    # radar plot can't deal with NA and non-fish don't have selenium
    replace_na (list (Selenium = 0)) 
  
  # calculate maximum value for plot specification; looks like will be calcium
  #max_value = max(nutr_radar_plot$Calcium)
  
  
  ggradar(nutr_radar_plot,
          grid.min = 0, grid.max = 1, 
          group.point.size = 1.5,
          group.line.width = 1,
          legend.text.size = 8,
          legend.position = "right") +
    ggtitle (country_name)
}

#plot_spp_nutr_radar(country_name = "Sierra Leone", n_spp = 5)



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

plot_spp_nutr_v_catch <- function (country_name) {
  
  country_spp <- ds_spp_nutr_content %>%
    filter (country == country_name) %>%
    left_join (catch_prop, by = c("country", "species"))
  
  label_spp <- country_spp %>%
    ungroup() %>%
    filter (country == country_name) %>%
    group_by (nutrient) %>%
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
