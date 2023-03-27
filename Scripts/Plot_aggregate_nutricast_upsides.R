# Plot aggregate nutrient upsides
# 3/21/23
# JGM

library (tidyverse)

# major update 3/27/23 is that I'm going to calculate children fed from current landings, and then multiply ratios for nutrient yield. doing this to preserve matched species. but maybe doesn't matter?

# catch upside relative, from calculate_nutritional_upsides.r
# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")

# averaged data for missing spp
catch_upside_relative_missing <- readRDS("Data/catch_upside_relative_repair_missing.Rds")

catch_upside_relative_repaired <- 
  rbind (catch_upside_relative, catch_upside_relative_missing)

# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")

# landings data ----
# Chile country specific 
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")


# SAU data 
# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds") 


# Function to convert landings to ratio to children fed, by country ----
# may need to specify division
plot_nutr_upside_ratios <- function (country_name, Selenium = FALSE) {
 
  if (country_name == "Chile") {
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      group_by (species, taxa) %>%
      summarise (total_tonnes = sum (catch_mt)) %>%
      mutate (country = "Chile")
  } else {
    
    landings <- sau_2019 %>%
      filter (country == country_name) %>%
      left_join(sau_2019_taxa, by = "species") %>%
      group_by (country, species, taxa) %>%
      summarise (total_tonnes = sum (tonnes))
  }
  
  upside_ratios <- landings %>% left_join(catch_upside_relative_repaired, by = c ("country", "species")) %>%
    mutate (# multiply ratio by current landings
      across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * total_tonnes),
      #convert to upside, subtract 
      mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
      mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
      adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
      adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
    
    select (country, rcp, species, taxa, mey_2050:adapt_2100) %>%
    pivot_longer(mey_2050:adapt_2100, 
                 names_to = "upside",
                 values_to = "tonnes") %>%
    # get rid of non-matching species, NAs
    filter (!is.na (rcp)) %>%
    # convert to nutrients
    mutate (children_fed = pmap (list (species = species, taxa = taxa, amount = tonnes, country = "Peru"), calc_children_fed_func)) %>%
    unnest(cols = c(children_fed),  names_repair = "check_unique")
  
  # fix ratios
  upside_ratios$upside <- factor(upside_ratios$upside, levels = c ("mey_2050", "mey_2100", "adapt_2050", "adapt_2100"))
  
  if (Selenium == TRUE) {
    upside_summary <- upside_ratios %>%
      group_by (rcp, upside, nutrient) %>%
      summarise (total_fed = sum (children_fed, na.rm = TRUE)) %>%
      filter (!nutrient %in% c("Protein"),
              upside %in% c("mey_2050", "adapt_2050")) 
  } else {
    
    upside_summary <- upside_ratios %>%
      group_by (rcp, upside, nutrient) %>%
      summarise (total_fed = sum (children_fed, na.rm = TRUE)) %>%
      filter (!nutrient %in% c("Protein", "Selenium"),
              upside %in% c("mey_2050", "adapt_2050")) 
  }
  
    upside_summary %>%
    # preliminary plot
    ggplot (aes (x = reorder(nutrient, -total_fed), y = total_fed/1000000, fill = upside)) +
    geom_col (position = "dodge") +
    geom_hline (yintercept = 0, lty = 2) +
    facet_wrap ( ~ rcp) +
    theme_bw() +
    # roughly match colors from gaines et al
    scale_fill_manual (values = c ("mediumseagreen", "dodgerblue4")) +
    labs (y = "Change in # children fed, millions", x = "", fill = "Management \nstrategy") +
    ggtitle ("Nutrition upside from climate-adaptive management")
}


p <- plot_nutr_upside_ratios("Peru")

png ("Figures/Peru_nutricast_upside_overall_repaired.png", width = 6, height = 5, units= "in", res = 300)
p + theme (plot.title = element_text (size = 17),
       axis.text = element_text (size = 11),
       axis.text.x = element_text (angle = 60, hjust = 1),
       strip.text.x =  element_text (size = 12),
       axis.title = element_text (size = 16),
       legend.title = element_text (size = 14),
       legend.text = element_text (size = 11)) 
dev.off()

i <- plot_nutr_upside_ratios("Indonesia")

png ("Figures/Indo_nutricast_upside_overall_repaired.png", width = 6, height = 5, units= "in", res = 300)
i + theme (plot.title = element_text (size = 17),
           axis.text = element_text (size = 11),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 12),
           axis.title = element_text (size = 16),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 11)) 
dev.off()

s <- plot_nutr_upside_ratios("Sierra Leone")
png ("Figures/SL_nutricast_upside_overall_repaired.png", width = 6, height = 5, units= "in", res = 300)
s + theme (plot.title = element_text (size = 17),
           axis.text = element_text (size = 11),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 12),
           axis.title = element_text (size = 16),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 11)) 
dev.off()

c <- plot_nutr_upside_ratios("Chile")
png ("Figures/Chile_nutricast_upside_overall_repaired.png", width = 6, height = 5, units= "in", res = 300)
c + theme (plot.title = element_text (size = 17),
           axis.text = element_text (size = 11),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 12),
           axis.title = element_text (size = 16),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 11)) 
dev.off()
