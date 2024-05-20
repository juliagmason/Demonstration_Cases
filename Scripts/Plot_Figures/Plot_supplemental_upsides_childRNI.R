# Plot supplemental figures--upsides child RNI all nutrients all rcps
# 5/13/24

# taking code from Plot_Fig_XE_nutrient_projections. but want one faceted figure for all RCPs and nutrients

library (tidyverse)

#Plot for each country 
plot_child_RNI_proj_facet <- function (country_name) {
  
  #projected nutrient yield in rni_equivalents for each country
  nutr_ts <- readRDS(paste0("Data/annual_nutr_upside_childRNI_", country_name, ".Rds"))
  
  # aggregate by rcp, scenario, year, nutrient
  nutr_agg_ts <- nutr_ts %>%
    group_by (rcp, scenario, year, nutrient) %>%
    summarise (tot_fed = sum (rni_equivalents, na.rm = TRUE)) %>%
    filter (!nutrient %in% c("Protein"))
  
  # fix levels
  nutr_agg_ts$scenario  <- factor(nutr_agg_ts$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  

  # specify anchoveta in title
  # https://stackoverflow.com/questions/40113963/how-to-extract-everything-until-first-occurrence-of-pattern
  if (country_name %in% c("Peru_anchoveta", "Chile_anchoveta")) {
    country_title <-  gsub("(.+?)(\\_.*)", "\\1", country_name)
    plot_title <- paste0("Projected nutrient yield for ", country_title, "; Anchoveta only")
  } else if (country_name %in% c("Peru", "Chile")) {
    plot_title <- paste0("Projected nutrient yield for ", country_name, "; Anchoveta removed")
    } else {plot_title <- paste0("Projected nutrient yield for ", country_name)}

  plot_ts <- nutr_agg_ts %>% 
    # beautify strip totles 
    mutate (nutrient = gsub ("_", " ", nutrient),
            # https://stackoverflow.com/questions/13863599/insert-a-character-at-a-specific-location-in-a-string
            # clumsy but here we go
            # insert decimal between the digits
            rcp = gsub('^(.{4})(.*)$', '\\1.\\2', rcp),
            # insert space after the letters
            rcp = gsub('^(.{3})(.*)$', '\\1 \\2', rcp)
            ) %>%
    ggplot (aes (x = year, y = tot_fed/1000000, col = scenario, group = scenario)) +
    geom_line() +
    facet_wrap (nutrient~rcp, scales = "free_y", ncol = 4) +
    theme_bw() +
    labs (y = "Child RNI equivalents, millions", x = "", col = "Mgmt. scenario") +
    ggtitle (plot_title) +
    theme (axis.text.y = element_text (size = 12),
           axis.text.x = element_text (size = 10),
           axis.title = element_text (size = 14),
           strip.text = element_text (size = 10),
           legend.text = element_text (size = 12),
           legend.title = element_text (size = 14),
           plot.title = element_text (size = 18),
           # bring legend closer to plot
           # legend.margin=margin(1,1,1,1),
           # legend.box.margin=margin(-10,-10,-10,0),
           legend.box.spacing = margin(0.2),
           legend.position = "bottom",
           plot.margin=unit(c(1,1,1,1), 'mm'))
  
 

  # save png - use this for supplemental figs only
  png (paste0("Figures/annual_nutr_ts_childRNI_facet_", country_name, ".png"), width = 7.5, height = 9.5, units = "in", res = 300)
  print (plot_ts)
  dev.off()
  
}

countries <- c("Peru", "Peru_anchoveta", "Chile", "Chile_anchoveta", "Indonesia", "Sierra Leone")

map (countries, plot_child_RNI_proj_facet)
