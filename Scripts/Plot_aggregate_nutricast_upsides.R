# Calculate aggregate nutrient upsides
# 3/21/23
# JGM

# Calculate the ratio of projected catch to a baseline period for each year, under each scenario. translate that catch into nutrient yield

library (tidyverse)
library (beepr) # calc_nutr_upside_tonnes_annual takes many minutes!

# major update 3/27/23 is that I'm going to calculate children fed from current landings, and then multiply ratios for nutrient yield. doing this to preserve matched species. but maybe doesn't matter?
# 11/21/23 overhauling code--this is calculating, not plotting. bring in calculation of catch upsides from calculate_nutritional_upsides.R, which was misnamed, only calculated catch upsides there


# function for converting catch in mt to children fed ----
# this will also bring in fishnutr data and RNI data
source ("Scripts/Function_convert_catch_amt_children_fed.R")

# landings data ----
# Chile country specific 
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# sierra leone country specific
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")


# SAU data 
# as of 10/25/22 just 2019 data, suggested by Deng Palomares. Clipped in SAU_explore.R
sau_2019 <- readRDS("Data/SAU_2019.Rds")

# request to plot current provisioning




# plot as full time series ----
# these are identical for the nutrients....
# annual nutricast time series, with repaired spp (calculate_projected_nutritional_upside.R)
catch_upside_annual <- readRDS ("Data/nutricast_upside_relative_annual_ratio.Rds")


# baseline catch from compiled data; from plot_contextual_landings.forecasts.R
full_baseline <- readRDS("Data/baseline_catch_sau_chl_ihh.Rds")


# multiply ratio by baseline
catch_upside_ts <- catch_upside_annual %>%
  # join to baseline
  inner_join(full_baseline, by = c ("country", "species")) %>%
  mutate (tonnes = catch_ratio * bl_tonnes)


# function to convert to rni equiv
calc_nutr_upside_tonnes_annual <- function (country_name) {
  
  nutr_upside_annual <- catch_upside_ts %>%
    filter (country == country_name) %>%
    filter (!is.na (rcp)) %>%
    # convert to nutrients
    mutate (rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = country), calc_children_fed_func)) %>%
    unnest(cols = c(rni_equivalents),  names_repair = "check_unique")
 
  # fix levels
  nutr_upside_annual$scenario <- factor(nutr_upside_annual$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  return (nutr_upside_annual)
  
}

# this takes several minutes
indo <-   calc_nutr_upside_tonnes_annual ("Indonesia"); beep()
saveRDS(indo, file = "Data/annual_nutr_upside_childRNI_Indonesia.Rds")

peru <-   calc_nutr_upside_tonnes_annual ("Peru"); beep()
saveRDS(peru, file = "Data/annual_nutr_upside_childRNI_Peru.Rds")

sl <-   calc_nutr_upside_tonnes_annual ("Sierra Leone"); beep()
saveRDS(sl, file = "Data/annual_nutr_upside_childRNI_Sierra Leone.Rds")

chl <-   calc_nutr_upside_tonnes_annual ("Chile"); beep()
saveRDS(chl, file = "Data/annual_nutr_upside_childRNI_Chile.Rds")


################################################
# plot annual ts-----

#Plot for each country and RCP
plot_child_RNI_proj <- function (country_name, RCP) {

  #projected nutrient yield in rni_equivalents for each country
  nutr_ts <- readRDS(paste0("Data/annual_nutr_upside_childRNI_", country_name, ".Rds"))
  
  # aggregate by rcp, scenario, year, nutrient
  nutr_agg_ts <- nutr_ts %>%
    group_by (rcp, scenario, year, nutrient) %>%
    summarise (tot_fed = sum (rni_equivalents, na.rm = TRUE)) %>%
    filter (rcp == RCP, !nutrient %in% c("Protein"))
  
  plot_ts <- nutr_agg_ts %>% 
    ggplot (aes (x = year, y = tot_fed/1000000, col = scenario, group = scenario)) +
    geom_line() +
    facet_wrap (~nutrient, scales = "free_y") +
    theme_bw() +
    labs (y = "Child RNI equivalents, millions", x = "", col = "Mgmt\nscenario") +
    ggtitle (paste0("Projected nutrient yield for ", country_name, "; ", RCP)) +
    theme (axis.text.y = element_text (size = 14),
           axis.text.x = element_text (size = 13),
           axis.title = element_text (size = 16),
           strip.text = element_text (size = 15),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18))
  
  # save png
  png (paste0("Figures/annual_nutr_ts_childRNI_", country_name, "_", RCP, ".png"), width = 9, height = 6, units = "in", res = 300)
  print (plot_ts + theme(legend.position="bottom"))
  dev.off()
  
}

plot_child_RNI_proj("Chile", RCP = "RCP26")

# Apply to all countries, all RCPs
pmap (expand_grid(country_name = c ("Chile", "Peru", "Sierra Leone", "Indonesia"), RCP = c("RCP26", "RCP45", "RCP60", "RCP85")), plot_child_RNI_proj)                   
                     

# Peru anchovy ----

peru_nutr_ts <- readRDS("Data/annual_nutr_upside_childRNI_Peru.Rds")

for (RCP in c("RCP26", "RCP45", "RCP60", "RCP85")) {
  
  plot_noanchov <- peru_nutr_ts %>%
    filter (species != "Engraulis ringens") %>%
    group_by (rcp, scenario, year, nutrient) %>%
    summarise (tot_fed = sum (rni_equivalents, na.rm = TRUE)) %>%
    filter (rcp == RCP, !nutrient %in% c("Protein")) %>%
    ggplot (aes (x = year, y = tot_fed/1000000, col = scenario, group = scenario)) +
    geom_line() +
    facet_wrap (~nutrient, scales = "free_y") +
    theme_bw() +
    labs (y = "Child RNI equivalents, millions", x = "", col = "Mgmt\nscenario") +
    ggtitle (paste0("Projected nutrient yield for Peru, ", RCP, "; Anchoveta removed")) +
    theme (axis.text.y = element_text (size = 14),
           axis.text.x = element_text (size = 13),
           axis.title = element_text (size = 16),
           strip.text = element_text (size = 15),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18))

  png (paste0("Figures/annual_nutr_ts_childRNI_Peru_NOanchov_", RCP, ".png"), width = 9, height = 6, units = "in", res = 300)
  print(plot_noanchov + theme(legend.position="bottom"))
  dev.off()
  
  # JUST anchovy
  plot_anchov <- peru_nutr_ts %>%
    filter (species == "Engraulis ringens") %>%
    group_by (rcp, scenario, year, nutrient) %>%
    summarise (tot_fed = sum (rni_equivalents, na.rm = TRUE)) %>%
    filter (rcp == RCP, !nutrient %in% c("Protein")) %>%
    ggplot (aes (x = year, y = tot_fed/1000000, col = scenario, group = scenario)) +
    geom_line() +
    facet_wrap (~nutrient, scales = "free_y") +
    theme_bw() +
    labs (y = "Child RNI equivalents, millions", x = "", col = "Mgmt\nscenario") +
    ggtitle (paste0("Projected nutrient yield for Peru, ", RCP, "; Anchoveta only")) +
    theme (axis.text.y = element_text (size = 14),
           axis.text.x = element_text (size = 13),
           axis.title = element_text (size = 16),
           strip.text = element_text (size = 15),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18))
  
  png (paste0("Figures/annual_nutr_ts_childRNI_Peru_anchov_", RCP, ".png"), width = 9, height = 6, units = "in", res = 300)
  print(plot_anchov + theme(legend.position="bottom"))
  dev.off()
  
}
    
    
################################################################################################################
# Calculate values to report in results ----
# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# report
report_RNI_met_values <- function (country_name, anchovy = TRUE) {
  
  rni_nutr_ts <- readRDS(paste0("Data/annual_nutr_upside_childRNI_", country_name, ".Rds"))
  
  # aggregate by nutrient, rcp, scenario
  # for peru, default without anchovy
  if (country_name == "Peru") {
    
    if (anchovy == TRUE) {
      rni_nutr_agg_ts <-  rni_nutr_ts %>%
        filter (species == "Engraulis ringens") %>%
        group_by (country, year, nutrient, rcp, scenario) %>%
        summarise (tot_rni = sum (rni_equivalents, na.rm = TRUE))
    } else if (anchovy == FALSE) {
      rni_nutr_agg_ts <-  tonnes_nutr_ts %>%
        filter (species != "Engraulis ringens") %>%
        group_by (country, year, nutrient, rcp, scenario) %>%
        summarise (tot_rni = sum (rni_equivalents, na.rm = TRUE))
    }  # end country ifelse
  } else {  
    # if not peru, don't worry about anchovy
    rni_nutr_agg_ts <-  rni_nutr_ts %>%
      group_by (country, year, nutrient, rcp, scenario) %>%
      summarise (tot_rni = sum (rni_equivalents, na.rm = TRUE))
  }
  
  # mid and end century averages
  rni_period <- rni_nutr_agg_ts %>%
    mutate (
      period = case_when (year %in% 2051:2060 ~ "midcentury",
                        year %in% 2091:2100 ~ "endcentury")
    ) %>%
    # cut to periods of interest
  filter (!is.na (period), rcp == "RCP60") %>%
    # take mean per period
    group_by (country, rcp, nutrient, scenario, period) %>%
    summarise (mean_rni_equiv = mean (tot_rni, na.rm = TRUE))

}  

report_RNI_met_values("Indonesia") %>% write.excel()
  
################################################################################
# Reject figs ----


# plot as 3 point time series, line graph, scenario as color ----
# catch upside relative by period, with missing species, from calculate_projected_nutritional_upsides.r
# expressed as catch ratios relative to base year for midcentury and end century, can multiply by landings
catch_upside_relative <- readRDS("Data/nutricast_upside_relative_repaired.Rds")


# Function to convert landings to ratio to children fed, by country ----
# may need to specify division
calc_nutr_upside_tonnes <- function (country_name) {
  
  if (country_name == "Chile") {
    landings <- chl_landings %>%
      filter (year == 2021) %>%
      group_by (species) %>%
      summarise (bau_current = sum (catch_mt)) %>%
      mutate (country = "Chile")
    
  } else if (country_name == "Sierra Leone") {
    landings <- sl_landings %>%
      filter (year == 2017) %>%
      group_by (species) %>%
      summarise (bau_current = sum (catch_mt)) %>%
      mutate (country = "Sierra Leone") 
  } else {
    
    landings <- sau_2019 %>%
      filter (country == country_name) %>%
      group_by (country, species) %>%
      summarise (bau_current = sum (tonnes))
  }
  
  nutr_upside <- catch_upside_relative %>%
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

  
  # fix levels
  nutr_upside$scenario <- factor(nutr_upside$scenario, levels = c ("bau", "mey", "adapt"))
  nutr_upside$period <- factor(nutr_upside$period, levels = c("current", "midcentury", "endcentury"))
  
  return (nutr_upside)
  
}

# calculate percentages
# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

baseline_nutr <- full_baseline %>%
  mutate (children_fed = pmap (list (species = species, amount = bl_tonnes, country_name = "Indonesia"), calc_children_fed_func)) %>%
  unnest(cols = c(children_fed),  names_repair = "check_unique") %>%
  group_by (country, nutrient) %>%
  summarise (baseline_rni = sum (rni_equivalents, na.rm = TRUE))

up <- nutr_upside %>%
  filter (rcp == "RCP60", species == "Engraulis ringens") %>%
  group_by (country, scenario, period, nutrient) %>%
  summarise (future_rni = sum(rni_equivalents, na.rm = TRUE)) %>%
  left_join (baseline_nutr, by = c("nutrient", "country")) %>%
  mutate (perc_change = (future_rni - baseline_rni)/baseline_rni)

up %>% filter (!period == "current") %>% write.excel()


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

for (country in c("Chile", "Indonesia", "Peru", "Sierra Leone")) {
  plot <- plot_nutr_absolutes_tonnes(country)
  
  png (paste0("Figures/nutricast_3pt_ts_", country, "_free.png"), width = 6.5, height = 4, units = "in", res = 300)
  print (
    plot +  
      facet_wrap (~ nutrient, scales = "free_y") +
      scale_x_discrete (labels = c ("current", "2050s", "2090s")) +
      theme (axis.text = element_text (size = 10),
             axis.title = element_text (size = 14)) 
  )
  
  dev.off()
}


############################################################################
# WHY is 3 point showing different dynamics for different nutrients?!?!?
# indonesia is an example, vitamin A very different than the others
# looking at one species, bau is same but adapt/mey is different. HUGE ratios for non-bau scenarios. 

#NOW it works

indo6 <- catch_upside_relative %>%
  filter (country == "Indonesia", rcp == "RCP60")

#is this consistent if I take averages of annual?
indo6_annual_recreate <- catch_upside_annual %>%
  filter (country == "Indonesia", rcp == "RCP60") %>%
  mutate (
    # baseline and mid century and end century
    period = case_when (
      year %in% c(2012:2021) ~ "2012-2021",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100")) %>%

  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, species) %>%
  summarise (mean_ratio = mean (catch_ratio, na.rm = TRUE))

indo6 %>% filter (species == "Lutjanus")
indo6_annual_recreate %>% filter (species == "Lutjanus")



############################################################################
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

for (country in c("Chile", "Indonesia", "Peru", "Sierra Leone")) {
  plot <- plot_bar_nutr_upside_ratios(country)
  
  png (paste0("Figures/nutricast_upside_overall_repaired_", country, ".png"), wwidth = 6, height = 5, units = "in", res = 300)
  print (
    plot +  
      theme (plot.title = element_text (size = 17),
             axis.text = element_text (size = 11),
             axis.text.x = element_text (angle = 60, hjust = 1),
             strip.text.x =  element_text (size = 12),
             axis.title = element_text (size = 16),
             legend.title = element_text (size = 14),
             legend.text = element_text (size = 11)) 
  )
  
  dev.off()
}


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
  better_scenario_colnames <- gsub("ratio_", "", colnames(catch_upside_relative)[4:9])
  old_colnames <- colnames(catch_upside_relative_repaired)[4:9]
  
  upside_ratios_absolute <- landings %>% 
    left_join(catch_upside_relative, by = c ("country", "species")) %>%
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

for (country in c("Chile", "Indonesia", "Peru", "Sierra Leone")) {
  plot <- plot_nutr_upside_absolute(country)
  
  png (paste0("Figures/nutricast_upside_overall_repaired_3scen_", country, ".png"), wwidth = 6, height = 5, units = "in", res = 300)
  print (
    plot +  
      theme (plot.title = element_text (size = 17),
             axis.text = element_text (size = 11),
             axis.text.x = element_text (angle = 60, hjust = 1),
             strip.text.x =  element_text (size = 12),
             axis.title = element_text (size = 16),
             legend.title = element_text (size = 14),
             legend.text = element_text (size = 11)) 
  )
  
  dev.off()
}