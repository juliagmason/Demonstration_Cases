# Can I calculate absolute nutrition gap?
# 9/16/22

# Theoretically the pdeficient golden values are derived from Genus nutrient supply and calculated population needs. Can I make this into just a nutrition gap?

# digging into nutrient_endowment

# population data by country, 2011
pop_2011 <- readRDS("../nutrient_endowment/data/population_growth/processed/WB_2011_population_size_by_country_agesex.Rds") %>%
  filter (country_use %in% c("Chile", "Peru", "Mexico", "Indonesia", "Malawi", "Sierra Leone", "Guinea"))

# rdas
# go back to original bc need more fine scale rda data
# format code from nutrient_endowment C:/Users/jmason/nutrient_endowment/code/calc_nutr_deficiencies/Step4_calculate_nutrient_demand_hist_proj.R

# chris has an age/sex key
# Read age/sex key 
agesex_key <- readxl::read_excel("../nutrient_endowment/data/genus_ear_wb_age_sex_key.xlsx", sheet=2)

dris <- readRDS("Data/dietary_reference_intake_data.Rds") %>%
  # Format age range
  mutate(age_range=gsub(" yr", "", age_range))%>%
  # Format sex
filter(sex!="Both") %>% 
  mutate(sex=recode(sex, 
                    "Males"="male", 
                    "Females"="female"))


# nutrient supply, GENuS 2011. I *think* this is country level supply available to each age and sex group. daily
# daily per capita supply
# in nutrient_endowment, seems like they're using as a proxy for consumption. that also seems to make sense based on GENuS paper which I don't really understand--energy adjusted calories etc. 
# see data --> genus --> step1_format_genus_data.R
# no data for sierra leone
genus_supply <- readRDS("../nutrient_endowment/data/genus/processed/genus_nutrient_supplies_by_age_sex_2011.Rds") %>%
  filter (country_use %in% c("Chile", "Peru", "Mexico", "Indonesia", "Malawi", "Sierra Leone", "Guinea"),
          nutrient %in% c("Vitamin A", "Calcium", "Iron", "Zinc", "Polyunsaturated fatty acids")) %>%
  select (country_use, age_range, sex, nutrient, units_short, value_med)

# or just total nutrient supply by country?
genus_total_supply <- readRDS("../nutrient_endowment/data/genus/processed/genus_nutrient_supplies_by_cntry_year.Rds") %>%
  filter (country_use %in% c("Chile", "Peru", "Mexico", "Indonesia", "Malawi", "Sierra Leone", "Guinea"),
          year == 2011,
          nutrient %in% c("Vitamin A", "Calcium", "Iron", "Zinc", "Polyunsaturated fatty acids")) %>%
  select (country_use, nutrient, units_short, value_med)

# quick check, does this add up sum no definitely not, but like mean. so this is per capita. 
genus_supply %>%
  group_by (country_use, nutrient) %>%
  summarise (tot_supply = sum (value_med))

genus_supply %>%
  group_by (country_use, nutrient) %>%
  summarise (meantot_supply = mean (value_med),
             med_supply = median(value_med))

genus_total_supply %>% arrange (country_use, nutrient)
