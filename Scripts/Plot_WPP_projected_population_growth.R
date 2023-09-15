# Plot projected population growth
# 9/15/23
# JGM

# moving from calculate_population_percent_nutrition.R

library (tidyverse)

# plot projected pop growth ----
wpp_pop <- read_csv("Data/WPP2022_Population1JanuaryByAge5GroupSex_Medium.csv")

# This is updated WPP population growth data
# https://population.un.org/wpp/Download/Standard/CSV/
# chose option population -1 January by 5 yr age groups, medium variant
# numbers are in 1000s

plot_proj_pop_growth <- function (country_name) {
  
  pop <- wpp_pop %>%
    select (Location, ISO3_code, Time, AgeGrp, PopMale, PopFemale) %>%
    filter (Location == country_name, Time > 2010) %>%
    rename (country = Location, age_range_wpp = AgeGrp, year= Time) %>%
    pivot_longer (PopMale:PopFemale,
                  names_prefix = "Pop",
                  names_to = "Sex",
                  values_to = "Pop") %>%
    group_by (year) %>%
    summarise (population = sum (Pop))
  
  plot <-  pop %>%
    ggplot (aes (x = year, y = population/1000)) +
    geom_line() +
    theme_bw() +
    labs (x = "", y = "Projected population, millions") + 
    theme (axis.text = element_text (size = 14),
           axis.title = element_text (size = 16))
  
  return (plot)
  
  
}



png ("Figures/Peru_population_proj.png", width = 6, height = 6, units = "in", res = 300)
print (plot_proj_pop_growth("Peru"))
dev.off()


png ("Figures/SL_population_proj.png", width = 6, height = 6, units = "in", res = 300)
print (plot_proj_pop_growth("Sierra Leone"))
dev.off()

png ("Figures/Indo_population_proj.png", width = 6, height = 6, units = "in", res = 300)
print (plot_proj_pop_growth("Indonesia"))
dev.off()

png ("Figures/Chile_population_proj.png", width = 6, height = 6, units = "in", res = 300)
print (plot_proj_pop_growth("Chile"))
dev.off()