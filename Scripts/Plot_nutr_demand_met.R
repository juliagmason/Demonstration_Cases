# Plot nutrient demand met
# 3/11/22

library (tidyverse)

##### Do this by all management scenarios ----

# from calculate_nutrition_demand_met.R
#demand_met_allscen <- readRDS("Data/nutr_demand_met_allscen.Rds")



# plot total nutrient demand met by scenario ----

plot_demand_met_scen <- function (country_name, imperfect = TRUE) {
  
  # Do we want the imperfect scenarios shown?
  if (imperfect == TRUE) {
    country_demand_met <- demand_met_allscen %>%
      filter (country == country_name, !is.na(demand_prop))
  } else {
    country_demand_met <- demand_met_allscen %>%
      filter (country == country_name, !is.na(demand_prop),
              !grepl("Imperfect", scenario))
  }
  
  
  # set levels
  country_demand_met$scenario <- factor (country_demand_met$scenario, levels = c("No Adaptation", "Productivity Only", "Range Shift Only", "Imperfect Productivity Only", "Imperfect Full Adaptation", "Full Adaptation"))
  
  country_demand_met$period <- factor(country_demand_met$period, levels = c("2090-2100", "2050-2060", "2020-2030"))
  
  # plot
  ggplot(country_demand_met, aes(x=demand_prop * 100, y=period, fill = scenario)) +
    facet_grid(nutrient~ rcp) +
    geom_bar(stat="identity", position = position_dodge2(reverse = TRUE)) +
    # Labels
    labs(x="Percent of nutrient demand\nmet from fisheries reforms", y="", fill = "Management\nscenario") +
    # Theme
    theme_bw() +
    ggtitle (country_name)
  
  
}

#plot_demand_met_scen("Peru")
