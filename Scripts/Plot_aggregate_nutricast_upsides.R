# Plot aggregate nutrient upsides
# 3/21/23
# JGM

library (tidyverse)

# major update 3/27/23 is that I'm going to calculate children fed from current landings, and then multiply ratios for nutrient yield. doing this to preserve matched species. but maybe doesn't matter?

# catch upside relative, from calculate_nutritional_upsides.r
# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative.Rds")

# averaged data for missing spp, scripts/check_SAU_nutricast_species
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

# request to plot current provisions 


# Function to convert landings to ratio to children fed, by country ----
# may need to specify division
calc_nutr_upside_tonnes <- function (country_name) {
 
  if (country_name == "Chile") {
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      group_by (species) %>%
      summarise (bau_current = sum (catch_mt)) %>%
      mutate (country = "Chile")
  } else {
    
    landings <- sau_2019 %>%
      filter (country == country_name) %>%
      group_by (country, species) %>%
      summarise (bau_current = sum (tonnes))
  }
  
 nutr_upside <- catch_upside_relative_repaired %>%
    #okay. if I'm just trying to get the projected tons, makes sense to pivot longer, then multiply by current landings all in one go. but since I want to have the baseline value repeated across each scenario, going to mutate across, then pivot longer
    inner_join (landings, by = c ("country", "species")) %>%
   mutate (# multiply ratio by current landings
     across(bau_ratio_midcentury:adapt_ratio_endcentury, ~.x * bau_current),
     # make fake current columns
     mey_current = bau_current,
     adapt_current = bau_current) %>%
     # remove "ratio" from column names, https://stackoverflow.com/questions/63459369/remove-prefix-letter-from-column-variables
    rename_all (~stringr::str_replace(., "ratio_", "")) %>%
    
    pivot_longer (-c(country, rcp, species),
                  names_to = c("scenario", "period"),
                  names_sep = "_",
                  values_to = "tonnes"
                  ) %>%
   # get rid of non-matching species, NAs
   filter (!is.na (rcp)) %>%
   # convert to nutrients
   mutate (children_fed = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func)) %>%
   unnest(cols = c(children_fed),  names_repair = "check_unique")
  
  
      
      # #convert to upside, subtract 
      # mey_2050 = mey_ratio_midcentury - bau_ratio_midcentury,
      # mey_2100 = mey_ratio_endcentury - bau_ratio_endcentury,
      # adapt_2050 = adapt_ratio_midcentury - bau_ratio_midcentury,
      # adapt_2100 = adapt_ratio_endcentury - bau_ratio_endcentury) %>%
    
  # fix levels
 nutr_upside$scenario <- factor(nutr_upside$scenario, levels = c ("bau", "mey", "adapt"))
 nutr_upside$period <- factor(nutr_upside$period, levels = c("current", "midcentury", "endcentury"))
 
 return (nutr_upside)
  
}


# plot as 3 point time series, line graph, scenario as color ----
plot_nutr_absolutes_tonnes <- function (country_name) {
  
  upsides <- calc_nutr_upside_tonnes(country_name)
  
  q <- upsides %>%
    group_by (country, rcp, scenario, period, nutrient) %>%
    summarise (tot_fed = sum (children_fed, na.rm = TRUE)) %>%
    filter (rcp == "RCP60", !nutrient == "Protein") %>%
    filter (!nutrient == "Protein") %>%
    ggplot (aes (x = factor(period), y = tot_fed/1000000, col = scenario, group = scenario)) +
    geom_point() +
    geom_line() +
    facet_wrap (~nutrient) +
    theme_bw() +
    labs (y = "Child RNI equivalents, millions", x = "", col = "Mgmt\nscenario") +
    ggtitle (paste0 ("Projected nutrient yield for ", country_name, ", RCP 6.0"))
  
}
  

a <- plot_nutr_absolutes_tonnes("Peru")
a + 
  theme (axis.text = element_text (size = 12),
         axis.title = element_text (size = 14)) 

c <- plot_nutr_absolutes_tonnes("Chile")
  
# plot as bar, just mey and adapt difference ----
# have to retool and take subtractions
plot_bar_nutr_upside_ratios <- function (country_name, Selenium = FALSE) {
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

## plot BAU and amount, not difference ----

plot_nutr_upside_absolute <- function (country_name, Selenium = FALSE) {
  
  if (country_name == "Chile") {
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      group_by (species) %>%
      summarise (total_tonnes = sum (catch_mt)) %>%
      mutate (country = "Chile")
  } else {
    
    landings <- sau_2019 %>%
      filter (country == country_name) %>%
      group_by (country, species) %>%
      summarise (total_tonnes = sum (tonnes))
  }
  
  # fix colnames so can pivot_longer and break into period and scenario
  better_scenario_colnames <- gsub("ratio_", "", colnames(catch_upside_relative_repaired)[4:9])
  old_colnames <- colnames(catch_upside_relative_repaired)[4:9]
  
  upside_ratios_absolute <- landings %>% 
    left_join(catch_upside_relative_repaired, by = c ("country", "species")) %>%
    rename_with (~ better_scenario_colnames, all_of(old_colnames)) %>%
    mutate (# multiply ratio by current landings
      across(bau_midcentury:adapt_endcentury, ~.x * total_tonnes)) %>%
    
    select (country, rcp, species, bau_midcentury:adapt_endcentury) %>%
    pivot_longer(bau_midcentury:adapt_endcentury, 
                 names_to = c("scenario", "period"),
                 names_sep = "_",
                 values_to = "tonnes") %>%
    # get rid of non-matching species, NAs
    filter (!is.na (rcp)) %>%
    # convert to nutrients
    mutate (children_fed = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func)) %>%
    unnest(cols = c(children_fed),  names_repair = "check_unique") 
  
  # fix levels
  upside_ratios_absolute$scenario <- factor(upside_ratios_absolute$scenario, levels = c ("bau", "mey", "adapt"))
  upside_ratios_absolute$period <- factor(upside_ratios_absolute$period, levels = c ("midcentury", "endcentury"))
  
  if (Selenium == TRUE) {
    upside_summary <- upside_ratios_absolute %>%
      group_by (rcp, scenario, period, nutrient) %>%
      summarise (total_fed = sum (children_fed, na.rm = TRUE)) %>%
      filter (!nutrient %in% c("Protein")) 
  } else {
    
    upside_summary <- upside_ratios_absolute %>%
      group_by (rcp, scenario, period, nutrient) %>%
      summarise (total_fed = sum (children_fed, na.rm = TRUE)) %>%
      filter (!nutrient %in% c("Protein", "Selenium")) 
  }
  
  upside_summary %>%
    # preliminary plot
    ggplot (aes (x = reorder(nutrient, -total_fed), y = total_fed/1000000, fill = scenario)) +
    geom_col (position = "dodge") +
    geom_hline (yintercept = 0, lty = 2) +
    facet_wrap (period ~ rcp, ncol =4) +
    theme_bw() +
    # roughly match colors from gaines et al
    scale_fill_manual (values = c ("firebrick", "mediumseagreen", "dodgerblue4")) +
    labs (y = "# Child RNIs met, millions", x = "", fill = "Management \nstrategy") +
    ggtitle ("Nutrition provisioning from climate-adaptive management")
}

png ("Figures/Peru_nutricast_upside_overall_repaired_3scen.png", width = 6, height = 5, units= "in", res = 300)
plot_nutr_upside_absolute("Peru") + theme (plot.title = element_text (size = 17),
           axis.text = element_text (size = 11),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 12),
           axis.title = element_text (size = 16),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 11)) 
dev.off()



png ("Figures/Indo_nutricast_upside_overall_repaired_3scen.png", width = 6, height = 5, units= "in", res = 300)
plot_nutr_upside_absolute("Indonesia") + theme (plot.title = element_text (size = 17),
           axis.text = element_text (size = 11),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 12),
           axis.title = element_text (size = 16),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 11)) 
dev.off()


png ("Figures/SL_nutricast_upside_overall_repaired_3scen.png", width = 6, height = 5, units= "in", res = 300)
plot_nutr_upside_absolute("Sierra Leone") + theme (plot.title = element_text (size = 17),
           axis.text = element_text (size = 11),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 12),
           axis.title = element_text (size = 16),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 11)) 
dev.off()


png ("Figures/Chile_nutricast_upside_overall_repaired_3scen.png", width = 6, height = 5, units= "in", res = 300)
plot_nutr_upside_absolute("Chile") + theme (plot.title = element_text (size = 17),
           axis.text = element_text (size = 11),
           axis.text.x = element_text (angle = 60, hjust = 1),
           strip.text.x =  element_text (size = 12),
           axis.title = element_text (size = 16),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 11)) 
dev.off()

