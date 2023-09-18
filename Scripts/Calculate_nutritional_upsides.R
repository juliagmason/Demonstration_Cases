## Calculate nutritional upside
# 2 14 22
# JGM

library (tidyverse)

# smaller, just 3 management scenarios. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

# 2/1/23 moving code from regional team priority spp
# 9/16/23 update: changing baseline year to 2017-2021. Weird dynamics in the first years of the model are creating strange results

# upside in terms of relation to baseline catch----
# to be able to use with our updated landings data, instead I want to calculate what the catch in 2050 is relative to baseline under the different scenarios. 

catch_upside_relative <-  ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0) %>%
  mutate (
    # baseline and mid century and end century
    period = case_when (
      year %in% c(2017:2021) ~ "2017-2021",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100")) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, species) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, rcp, species) %>%
  summarize (bau_ratio_midcentury = catch_mt[scenario == "No Adaptation" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
             bau_ratio_endcentury = catch_mt[scenario == "No Adaptation" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
             mey_ratio_midcentury = catch_mt[scenario == "Productivity Only" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
             mey_ratio_endcentury = catch_mt[scenario == "Productivity Only" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
             adapt_ratio_midcentury = catch_mt[scenario == "Full Adaptation" & period == "2051-2060"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"],
             adapt_ratio_endcentury = catch_mt[scenario == "Full Adaptation" & period == "2091-2100"]/ catch_mt[scenario == "No Adaptation" & period == "2012-2021"]
  )

saveRDS (catch_upside_relative, file = "Data/nutricast_upside_relative.Rds")

catch_upside_relative %>%
  filter (rcp == "RCP60") %>%
  right_join (priority_spp, by = c("country", "species")) %>%
  filter (!country == "Indonesia") %>%
  arrange (country, rank) %>%
  write.excel()

# also do this for each year so I can plot a full time series----

# probably can do this with brackets but just make a baseline value separately
ds_spp_baseline <- ds_spp %>%
  filter (scenario %in% "No Adaptation", catch_mt > 0, between (year, 2017, 2021)) %>%
  group_by (country, rcp, scenario, species) %>%
  summarise (baseline_catch = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup()

# then need to make 2 more fake ones with the other scenarios??
ds_spp_baseline_mey <- ds_spp_baseline %>%
  mutate (scenario = "Productivity Only")

ds_spp_baseline_adapt <- ds_spp_baseline %>%
  mutate (scenario = "Full Adaptation")

ds_spp_baseline <- rbind (ds_spp_baseline, ds_spp_baseline_mey, ds_spp_baseline_adapt)
  
catch_upside_relative_annual <-  ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0, year > 2021) %>%
  left_join (ds_spp_baseline, by = c("country", "rcp", "scenario", "species")) %>%
  mutate (catch_ratio = catch_mt / baseline_catch) %>%
  select (country, rcp, scenario, year, species, baseline_catch, catch_mt, catch_ratio)
  

saveRDS (catch_upside_relative_annual, file = "Data/nutricast_upside_relative_annual_ratio.Rds")
# need to fix SAU missing species, check_SAU_nutricast_Species
# did this 5/22/23 in check_sau_nutricast_species, but only saved catch_ratio, i think this is all that I need
# "Data/nutricast_upside_relative_annual_repair_missing.Rds"

# show full time series to account for weird behavior early in projection
catch_upside_relative_annual_full_ts <-  ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0) %>%
  left_join (ds_spp_baseline, by = c("country", "rcp", "scenario", "species")) %>%
  mutate (catch_ratio = catch_mt / baseline_catch) %>%
  select (country, rcp, scenario, year, species, baseline_catch, catch_mt, catch_ratio)

saveRDS (catch_upside_relative_annual_full_ts, file = "Data/nutricast_upside_relative_annual_ratio_full_ts.Rds")

######################################

# copying code here on 2/1 from regional team priority species but not checking
# upside BAU to MEY ----

# nutricast upside ---- [move to calculate_nutritional_upsides script??]

# for spreadsheet just calculate the tons
pri_spp_catch_upside <- ds_spp %>% 
  right_join (priority_spp, by = c ("country",  "species")) %>%
  mutate (
    period = case_when (
      year %in% c(2026:2035) ~ "2026-2035",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  filter (!is.na (catch_mt), !is.na (period), scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation")) %>%
  #take mean projected catch for the decade period
  group_by (country, rcp, period, species, scenario) %>%
  summarise (catch_mt = mean (catch_mt)) %>%
  ungroup() %>%
  # find difference among scenarios--absolute and percent diff
  group_by (country, rcp, period, species) %>%
  summarize (mey_diff_mt = catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"],
             mey_diff_percent = (catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100,
             adapt_diff_mt = catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"],
             adapt_diff_percent = (catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100) %>%
  ungroup()

# rejoin to rank
pri_spp_catch_upside %>%
  filter (period == "2051-2060", rcp == "RCP60") %>%
  left_join (priority_spp, by = c("country", "species")) %>%
  arrange (country, rank) %>%
  write.excel()


## nutritional upside inds fed

nutr_upside_excel <- nutr_upside %>%
  right_join (priority_spp, by = c ("country",  "species")) %>%
  filter (rcp == "RCP60", period == "2051-2060", !nutrient == "Protein")

# prop is proportion, so multiply by 100 for percent

nutr_upside_excel %>%
  ungroup() %>%
  select (country, species, nutrient, mey_diff_child_rda, mey_diff_rdas_prop, rank) %>%
  arrange (country, rank) %>%
  write.excel()


nutr_upside_excel %>%
  ungroup() %>%
  select (country, species, nutrient, adapt_diff_child_rda, rank) %>%
  arrange (country, rank) %>%
  write.excel()

# upside full adapt to no adapt percent changes---
catch_upside <- ds_spp %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), catch_mt > 0) %>%
  mutate (
    period = case_when (
      year %in% c(2026:2035) ~ "2026-2035",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, species) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, rcp, period, species) %>%
  summarize (mey_diff_mt = catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"],
             mey_diff_percent = (catch_mt[scenario == "Productivity Only"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100,
             adapt_diff_mt = catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"],
             adapt_diff_percent = (catch_mt[scenario == "Full Adaptation"] - catch_mt[scenario == "No Adaptation"])/catch_mt[scenario == "No Adaptation"] * 100)

catch_upside %>%
  filter (period == "2051-2060", rcp == "RCP60") %>%
  right_join (priority_spp, by = c ("species", "country")) %>% View()



  


# updated 3/25/22 to use rda data
# projected nutrient yield from FishNutrients for fish, GENuS for nonfish. in mt (per year) and servings (native units per day)
# made this in Convert_catch_to_nutrients.R
ds_catch_nutr_yield_projected <- readRDS ("Data/ds_catch_nutr_yield_projected.Rds")

# RDAs data ----
# made in Species_level_nutrient_content
rda_groups <- readRDS("Data/RDAs_5groups.Rds")

# population data----
# made in calculate_RDAs_met
pop_future <- readRDS("Data/country_pop_projections.Rds")

# calculate population level rda needs
rda_needs_pop <- rda_groups %>%
  left_join (pop_future, by = "group") %>%
  filter (!is.na (population)) %>%
  mutate (rda_needs_group = mean_rda * population) %>%
  group_by (country, period, nutrient) %>%
  summarize (rda_needs_pop = sum(rda_needs_group)) 

# calculate rda needs just for children
rda_needs_child <- rda_groups %>%
  left_join (pop_future, by = "group") %>%
  filter (!is.na (population), group == "Child") %>%
  mutate (rda_needs_child = mean_rda * population) %>%
  select (country, period, nutrient, mean_rda, rda_needs_child)

#dumb
rda_needs <- rda_needs_pop %>%
  left_join (rda_needs_child, by = c ("country", "period", "nutrient"))


# calculate difference in mt and servings between MEY/adaptive management and BAU for periods of interest. translate into projected RDAs met
# small test dataset
ds_sm <- ds_catch_nutr_yield_projected %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"), year %in% c(2026:2035, 2051:2060, 2091:2100)) %>%
            sample_n(10000)

nutr_upside <- ds_catch_nutr_yield_projected %>% #ds_catch_nutr_yield_projected %>%
  filter (scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation")) %>%
  mutate (
    period = case_when (
      year %in% c(2026:2035) ~ "2026-2035",
      year %in% c(2051:2060) ~ "2051-2060",
      year %in% c(2091:2100) ~ "2091-2100"
    )) %>%
  filter (!is.na (period)) %>%
  group_by (country, rcp, scenario, period, nutrient, species) %>%
  summarise (nutr_mt = mean (nutr_mt, na.rm = TRUE),
             nutr_servings = mean (nutr_servings, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, rcp, period, nutrient, species) %>%
  summarize (mey_diff_mt = nutr_mt[scenario == "Productivity Only"] - nutr_mt[scenario == "No Adaptation"],
             adapt_diff_mt = nutr_mt[scenario == "Full Adaptation"] - nutr_mt[scenario == "No Adaptation"],
             mey_diff_servings = nutr_servings[scenario == "Productivity Only"] - nutr_servings[scenario == "No Adaptation"],
             adapt_diff_servings = nutr_servings[scenario == "Full Adaptation"] - nutr_servings[scenario == "No Adaptation"]) %>%
  # also show rdas met for children, population
  left_join (rda_needs, by = c("country", "period", "nutrient")) %>%
  mutate (mey_diff_rdas_prop = mey_diff_servings / rda_needs_pop, # this is the proportion of population's needs met
          mey_diff_child_rda = mey_diff_servings / mean_rda, # this is how many child rdas met
          mey_diff_child_prop = mey_diff_servings / rda_needs_child, # this is proportion of child rdas met
          adapt_diff_rdas_prop = adapt_diff_servings / rda_needs_pop,
          adapt_diff_child_rda = adapt_diff_servings / mean_rda,
          adapt_diff_child_prop = adapt_diff_servings / rda_needs_child)

saveRDS(nutr_upside, file = "Data/ds_nutr_upside.Rds")


#############################################################
#nutricast method demand required
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








# has 6 management scenarios, 4 rcps. 
# Management scenarios: Full Adaptation, Imperfect Full Adaptation, Imperfect Productivity Only--don't know what this means. 5,10,20 year intervals? but what interval?, No Adaptation (BAU, current mortality maintained and gradually shifts to open access for transboundary stocks), Productivity Only (economically optimal fishing morality for static stocks; gradual shift to open access for transboundary), Range Shift Only. 

# imperfect scenarios have big fluctuations. stick to the 4 main ones, and skip burn-in
# I think what we want is productivity only, compare to BAU

# filtering out zero catch, hopefully helps make the dataset more manageable
ds_prod_diff <- ds_spp %>%
  filter (year > 2025, catch_mt > 0, scenario %in% c("No Adaptation", "Productivity Only")) %>%
  select (rcp, scenario, country, species, year, msy_mt, biomass_mt, catch_mt, profits_usd, bbmsy, ffmsy) %>%
  pivot_longer(c(msy_mt:ffmsy),
               names_to = "metric",
               values_to = "value") %>%
  group_by (rcp, country, species, year, metric) %>%
  summarize (diff = value[scenario == "Productivity Only"] - value[scenario == "No Adaptation"]) %>%
  filter (!is.na(diff))

saveRDS(ds_prod_diff, file = "Data/ds_prod_diff.Rds")


# differences in other metrics--catch, profits ----

png ("Figures/Diff_catch_MEY_byspecies.png", width = 14, height = 6, units = "in", res = 300)
ds_prod_diff %>%
  filter (metric == "catch_mt") %>%
  ggplot (aes (x = year, y = diff)) +
  geom_area (aes (fill = species)) +
  facet_grid(country ~ rcp, scales = "free") +
  guides (fill = "none") + 
  theme_bw() +
  ggtitle ("Difference in catch (MT) by achieving MEY")
dev.off()

png ("Figures/Diff_profits_MEY_byspecies.png", width = 14, height = 6, units = "in", res = 300)
ds_prod_diff %>%
  filter (metric == "profits_usd") %>%
  ggplot (aes (x = year, y = diff)) +
  geom_area (aes (fill = species)) +
  facet_grid(country ~ rcp, scales = "free") +
  guides (fill = "none") + 
  theme_bw() +
  ggtitle ("Difference in profits (USD) by achieving MEY")
dev.off()

# Convert difference to nutrients using FishNutrients data ----
# tidied productivity only vs BAU
ds_prod_diff <- readRDS("Data/ds_prod_diff.Rds")

# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv")
#"a flat file with summary statistics for each nutrient for each species, including the scientific name of the species (species), the FishBase species code (spec_code), a highest posterior predictive density value (X_mu), a lower 95% highest posterior predictive density interval (X_l95), a lower 50% highest posterior predictive density interval (X_l50), an upper 50% highest posterior predictive density interval (X_h95), and an upper 95% highest posterior predictive density interval (X_h95)."

# from fishbase: Calcium is mg/100g. Iron is mg/100g, Selenium ug/100g, Zinc mg, Vit A ug, Omega 3 g, Protein g

# summarise fishnutr, just estimate and an average?? for most nutritious?? robinson et al. 2022 sums, but that can't be right for different units? check their code

# do fishnutr
fishnutr_mu <- fishnutr %>%
  select (species, ends_with ("_mu")) 



# use genus data for non-fish
spp_key <- read.csv(file.path ("../nutrient_endowment/output/Gaines_species_nutrient_content_key.csv"), as.is=T)

#truncate to the nutrients we're using for fishnutrients. have to figure out how to get omegas? use PUFAs for now
spp_key_sm <- spp_key %>% 
  select (species, major_group, genus_food_name, calcium_mg, iron_mg, polyunsaturated_fatty_acids_g, protein_g, vitamin_a_mcg_rae, zinc_mg)


# total nutrient availability ----
ds_prod_diff_nutr_totavail <- ds_prod_diff %>%
  filter (metric == "catch_mt") %>%
  left_join (spp_key_sm, by = "species") %>%
  # Step 3. Convert catch to edible meat
  filter(!is.na(genus_food_name)) %>% 
  mutate(major_group=recode(genus_food_name,
                            "Cephalopods"="Cephalopods",
                            "Crustaceans"="Crustaceans",
                            "Demersal Fish"="Finfish",
                            "Marine Fish; Other"="Finfish",
                            "Molluscs; Other"="Molluscs",
                            "Pelagic Fish"="Finfish"),
         pedible=recode(major_group, 
                        "Finfish"=0.87, 
                        "Crustaceans"=0.36, 
                        "Molluscs"=0.17, 
                        "Cephalopods"=0.21), 
         # multiply by diff by proportion edible, convert to grams. d
         meat_g= diff * pedible * 1000*1000/100) %>% 
  left_join (fishnutr_mu, by = "species") %>%
# convert to amount available. have to convert mt to grams, and divide by 100 grams
  mutate (
    # no selenium in genus
    Selenium = meat_g * Selenium_mu, 
    Zinc = ifelse (major_group == "Finfish", 
                   meat_g * Zinc_mu,
                   meat_g * zinc_mg),
    Protein = ifelse (major_group == "Finfish",
                      meat_g * Protein_mu,
                      meat_g * protein_g),
    #### NEED to figure out omega 3 situation #####
    Omega_3 = ifelse (major_group == "Finfish",
                         meat_g * Omega_3_mu, 
                         meat_g * polyunsaturated_fatty_acids_g),
    Calcium = ifelse (major_group == "Finfish",
                      meat_g * Calcium_mu,
                      meat_g * calcium_mg),
    Iron = ifelse (major_group == "Finfish",
                   meat_g * Iron_mu,
                   meat_g * iron_mg),
    Vitamin_A = ifelse (major_group == "Finfish",
                        meat_g * Vitamin_A_mu,
                        meat_g * vitamin_a_mcg_rae),
    .keep = "unused"
  ) %>%
  pivot_longer (Selenium:Vitamin_A,
                names_to = "nutrient",
                values_to = "amount") 
# amount is in original units, e.g. ug Selenium, mg Zinc

# plot available nutrients by country ----

# code vaguely following nutrient_endownment --> shiny --> edible_meat_bau_reforms.R
plot_nutr_avail_upside <- function (country_name) {
  
  nutr_avail_plot <- ds_prod_diff_nutr_totavail %>%
    filter (country == country_name,
            year %in% c(2040:2060)) %>%
    group_by (rcp, nutrient) %>%
    summarise (mean_avail = mean (amount, na.rm = TRUE)) %>%
    ungroup()
  # amount is in original units, e.g. ug Selenium, mg Zinc
  
    ggplot (nutr_avail_plot, aes (y = nutrient, x = mean_avail)) +
    geom_bar (stat = "identity") +
    facet_wrap (~rcp, ncol = 4, scales = "free_x") +
      geom_vline (xintercept = 0, lty = 2) +
    theme_bw() +
      labs(x = "Mean availability of nutrient, average of 2040-2060, units vary", y = "") +
    ggtitle (paste0("Additional available nutrients from fisheries reforms at mid-century (2040-2060), ", country_name))
  
}
}

plot_nutr_avail_upside ("Chile")





# plot nutrient needs met ----

#### *** also check out ouptut --> nutr_deficiencies_by_cntry_Sex_age

# looking at nutrient_endowment --> shiny -->v3 --> Page3_Figc_reforms_prop_demand.R, code 


# nutrition demand by country, from nutricast code --> calc_nutr_deficiencies/Step4_calculate_nutrient_demand_hist_proj.R
# supply required is in metric tons for the whole year
nutr_demand <- readRDS(file.path ("../nutrient_endowment/output/1960_2100_nutrient_demand_by_country.Rds"))
# CF used _50perc for calculations
# this is summed for the whole country, with projected population growth (50% estimate; this is where the 50% comes from)

# take mid century average
nutr_demand_midcentury <- nutr_demand %>%
  filter (year %in% c(2040:2060)) %>%
  group_by (country, nutrient) %>%
  mutate (across (starts_with ("supply"), ~mean (.x))) %>%
  # this is doing what I want, but retaining the years; they all have the same value
  select (-year) %>%
  distinct()


# so i have to convert the totavail nutrients back into mt. right now they're in their native units. 
# can do overall by nutrient, and also by species?

# mot all of the nutrients are available. calcium, iron, selenium, vitamin a, zinc
library (measurements)

# can't use conv_unit inside mutate. make a new function? from nutricast code
# Function to calculate mt of nutrient from mt of edible meat
# units: mg, ug=mcg 
# meat_mt <- 29.88111; nutr_dens <- 35.5; nutr_dens_units <- "mg"
convert_supply_mt <- function(nutrient_input, units_input){
  unit <- ifelse (units_input == "µg", "ug", units_input)
  avail_mt <- conv_unit (nutrient_input, from = unit, to = "metric_ton")
  return (avail_mt)
}
  
  
# first group by nutrient
demand_met_by_nutr <- ds_prod_diff_nutr_totavail %>%
  filter (year %in% c(2040:2060)) %>%
  group_by (country, rcp, nutrient) %>%
  summarise (mean_avail = mean (amount, na.rm = TRUE)) %>%
  # rename to match nutr_demand 
  mutate (nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vitamin A", 
                       TRUE ~ nutrient)) %>%
  left_join (nutr_demand_midcentury, by = c("country", "nutrient")) %>%
  # get rid of omega 3 and protein, don't have that info
  filter (!is.na (units))  %>%
  # convert to units
  mutate (
    #avail_mt = convert_supply_mt (mean_avail, units_input = units_short)
    avail_mt = ifelse (
      units_short == "µg",
      conv_unit (mean_avail, from = "ug", to = "metric_ton"),
      conv_unit (mean_avail, from = "mg", to = "metric_ton")
    ),
    demand_prop = avail_mt/supply_req_mt_yr_50perc
    )



demand_met_by_nutr %>%
    filter (country == "Peru") %>%
ggplot(aes(x=demand_prop, y=nutrient)) +
    facet_wrap(~ rcp, ncol = 4, scales = "free_x") +
    geom_bar(stat="identity") +
    # Labels
    labs(x="Percent of nutrient demand\nmet from capture fisheries reforms", y="") +
    # Theme
    theme_bw()


# just show percent -- this seems extremely low?
demand_met_by_nutr %>%
  filter (country == "Sierra Leone") %>%
  select (rcp, nutrient, demand_prop) %>%
  pivot_wider (names_from = nutrient,
               values_from = demand_prop)

# sanity check. peru supply req is 29036 MT CA, 300 MT iron, 20.2 MT vitamin A, 1.58 ug selenium. per year. 288 MT zinc. for avg 2040-2060
nutr_avail_allscen %>%
  filter (country == "Peru", year == 2050, rcp == "RCP26", scenario == "No Adaptation")
# mora moro, about 1 mt catch in 2050. that's 3642 mg zinc. There are 0.421 mg zinc/100g in fishnutrients
#1 mt * 1000kg/ton * 1000g/kg * 0.421 mg/100g = 4210 mg
1 * 1000 * 1000 * 0.421 / 100 

# from nutrient endowment code --> Step4_calculate_nutrient_demand_hist, DRIs intake for adult man is ~ 10mg/day for zinc. intake requirement maybe to overcome deficiency is more like 15-16. 
# peru population 2020 50% for 55 yo men is 757902. supply req is 12099939.812
12099939.812/757902 # 15.9, intake required. per day
# this is then summed across the whole population. peru required zinc, mg/day is 670619039 for 2020
# supply_req_mt_yr_50perc is the supply required * units / 1000 / 1000 * 365
# 670619039 mg/day * 1g/1000mg * 1kg/1000g * 1mt/1000 kg * 365 day/yr
670619039 /1000 /1000/1000 * 365 # 245, same as CF's calculation. so this is what the whole population needs in a year, in METRIC TONS

# in rcp26, mean avail zinc for peru is 13017943 mg
13017943 /1000/1000/1000 # 0.013 metric tons
# but nutricast has  significant zinc needs met, more like 5%. is there just a really big difference between MEY only and fully adaptive? he is only showing the reforms scenario. so showing total with reforms, not the difference. 

conv_unit (1, "Mg", "g")
conv_unit (1, "metric_ton", "g")

  

