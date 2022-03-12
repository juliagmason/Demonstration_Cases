# Plot number individuals nutrition needs met
# 3 11 22
# JGM

library (tidyverse)

# intake requirement data

# convert to # individuals nutrition needs met
ear <- readRDS ("Data/dietary_reference_intake_data.Rds") # this is intake per day

# take average for adult men and women
ear_avg <- ear %>%
  filter (stage == "None") %>%
  group_by (nutrient) %>%
  summarize (mn_value = mean(value, na.rm = TRUE)) %>%
  # match nutrient names
  mutate (nutrient = ifelse (grepl("Linolenic", nutrient), "Omega_3", nutrient),
          #manually make a dumb units column?
          units = case_when (
            nutrient %in% c("Calcium", "Zinc", "Iron") ~ "mg",
            nutrient %in% c("Protein", "Omega_3") ~ "g",
            nutrient %in% c("Vitamin A", "Selenium") ~ "ug"
          ))

# try calculating difference from demand_met_ds
demand_met_allscen <- readRDS("Data/nutr_demand_met_allscen.Rds")

demand_met_diff <- demand_met_allscen %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation")) %>%
  group_by (rcp, country, period, nutrient) %>%
  summarize (diff_prod = mean_avail_mt[scenario == "Productivity Only"] - mean_avail_mt[scenario == "No Adaptation"],
             diff_adapt = mean_avail_mt[scenario == "Full Adaptation"] - mean_avail_mt[scenario == "No Adaptation"]) 


# number adults whose nutrition requirements could be met
inds_met_upside <- demand_met_diff %>%
  left_join (ear_avg, by = "nutrient") %>%
  mutate (
    # scalar from nutrient_endowment calc_nutr_deficiencies_step4_calcu_nutrient_demand
    scalar=recode(units, 
                  "g"=1,
                  "mg"=1e3,
                  "ug"=1e6),
    inds_prod = diff_prod * scalar * 1000 * 1000 / 365 / mn_value,
    inds_adapt = diff_adapt * scalar * 1000 * 1000 / 365/ mn_value
  )

# plot
plot_n_inds_met_prod <- function (country_name) {
  
  country_upside <- inds_met_upside %>%
    filter (country == country_name)
  
  # set levels
  country_upside$period <- factor(country_upside$period, levels = c("2090-2100", "2050-2060", "2020-2030"))
  
  ggplot (country_upside, aes (x = inds_prod, y = period, fill = nutrient)) +
    geom_bar (stat = "identity", position = position_dodge2(reverse = TRUE)) +
    facet_wrap (~rcp, scales = "free") +
    theme_bw() +
    ggtitle (paste0(country_name, ", Upside with Productivity Only reforms (MEY)")) +
    labs (x = "Number of additional adults' nutrition requirements met", y = "", fill = "Nutrient")
  
  
}

plot_n_inds_met_adapt <- function (country_name) {
  
  country_upside <- inds_met_upside %>%
    filter (country == country_name)
  
  # set levels
  country_upside$period <- factor(country_upside$period, levels = c("2090-2100", "2050-2060", "2020-2030"))
  
  ggplot (country_upside, aes (x = inds_adapt, y = period, fill = nutrient)) +
    geom_bar (stat = "identity", position = position_dodge2(reverse = TRUE)) +
    facet_wrap (~rcp, scales = "free") +
    theme_bw() +
    ggtitle (paste0(country_name, ", Upside with Fully Adaptive reforms")) +
    labs (x = "Number of additional adults' nutrition requirements met", y = "", fill = "Nutrient")
  
}



# plot_n_inds_met_prod (country_name = "Chile")
# plot_n_inds_met_adapt (country_name = "Sierra Leone")