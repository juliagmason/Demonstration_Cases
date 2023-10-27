# Explore species driving climate projection patterns
# 10/27/23
# JGM

library (tidyverse)
library (treemap)
library (RColorBrewer)

# smaller, just rcp 60 and 85. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")
ds_spp$scenario <- factor (ds_spp$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))


# make a treemap where volume of square is amount of baseline catch, and color is the DIFFERENCE in adapt vs BAU?
# maybe first treemap where color is % change in catch BAU

# change in catch under BAU ----
perc_change_BAU <- ds_spp %>% 
  mutate (
    period = case_when (
      year %in% c(2017:2021) ~ "2017-2021",
      year %in% c(2051:2060) ~ "2051-2060")) %>%
  filter (!is.na (catch_mt), !is.na (period), scenario == "No Adaptation", rcp == "RCP60", ) %>%
  #take mean projected catch for the decade period
  group_by (country, rcp, period, species, scenario) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, species) %>%
  # find difference among scenarios--absolute and percent diff
  reframe (
    initial = sum (catch_mt[period == "2017-2021"]),
    initial_log = log (initial + 1),
    perc_diff = (sum (catch_mt[period == "2051-2060"]) - sum (catch_mt[period == "2017-2021"]))/sum (catch_mt[period == "2017-2021"]) * 100)


treemap (filter (perc_change_BAU, country == "Chile"), index = "species",
         vSize="initial_log", type="value",
         vColor = "perc_diff",
         palette = "RdYlBu",
         mapping = c(-100, 0,  400),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Chile projections % change under BAU"
)

treemap (filter (perc_change_BAU, country == "Peru"), index = "species",
         vSize="initial_log", type="value",
         vColor = "perc_diff",
         palette = "RdYlBu",
         mapping = c(-100, 0,  400),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Peru projections % change under BAU"
)

treemap (filter (perc_change_BAU, country == "Indonesia"), index = "species",
         vSize="initial_log", type="value",
         vColor = "perc_diff",
         palette = "RdYlBu",
         mapping = c(-100, 0,  400),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Indonesia projections % change under BAU"
)
set.seed (5)

treemap (filter (perc_change_BAU, country == "Sierra Leone"), index = "species",
         vSize="initial_log", type="value",
         vColor = "perc_diff",
         palette = "RdYlBu",
         mapping = c(-100, 0,  400),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Sierra Leone projections % change under BAU"
)

# DIFFERENce in adapt catch vs bau catch mid century ----
scenario_diff <- ds_spp %>% 
  mutate (
    period = case_when (
      # only care about midcentury?
      year %in% c(2017:2021) ~ "2017-2021",
      year %in% c(2051:2060) ~ "2051-2060")) %>%
  filter (!is.na (catch_mt), !is.na (period), rcp == "RCP60", ) %>%
  #take mean projected catch for the decade period
  group_by (country, rcp, period, species, scenario) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by (country, species) %>%
  # find difference among scenarios--absolute and percent diff
  reframe (
    initial = sum (catch_mt[period == "2017-2021"]),
    initial_log = log (initial + 1),
    mey_diff_mt = catch_mt[scenario == "Productivity Only" & period == "2051-2060"] - catch_mt[scenario == "No Adaptation"& period == "2051-2060"],
            mey_diff_percent = (catch_mt[scenario == "Productivity Only"& period == "2051-2060"] - catch_mt[scenario == "No Adaptation"& period == "2051-2060"])/catch_mt[scenario == "No Adaptation"& period == "2051-2060"] * 100,
            adapt_diff_mt = catch_mt[scenario == "Full Adaptation"& period == "2051-2060"] - catch_mt[scenario == "No Adaptation"& period == "2051-2060"],
            adapt_diff_percent = (catch_mt[scenario == "Full Adaptation"& period == "2051-2060"] - catch_mt[scenario == "No Adaptation"& period == "2051-2060"])/catch_mt[scenario == "No Adaptation"& period == "2051-2060"] * 100)


treemap (chl_scenario_diff, index = "species",
         vSize="initial_log", type="value",
         vColor = "mey_diff_percent",
         palette = "RdYlBu",
         mapping = c(-100, 0,  400),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Chile projections, improvement under MEY vs BAU"
)  

treemap (filter (scenario_diff, country == "Peru"), index = "species",
         vSize="initial_log", type="value",
         vColor = "mey_diff_percent",
         palette = "RdYlBu",
         mapping = c(-100, 0,  2000),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Peru projections, improvement under MEY vs BAU"
)  
set.seed(5)
treemap (filter (scenario_diff, country == "Indonesia"), index = "species",
         vSize="initial_log", type="value",
         vColor = "mey_diff_percent",
         palette = "RdYlBu",
         mapping = c(-100, 0,  400),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Indonesia projections, improvement under MEY vs BAU"
) 

treemap (filter (scenario_diff, country == "Sierra Leone"), index = "species",
         vSize="initial_log", type="value",
         vColor = "mey_diff_percent",
         palette = "RdYlBu",
         mapping = c(-50, 0,  200),
         fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
         fontcolor.labels=c("black","orange"),    # Color of labels
         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
         #border.lwds=c(7,2),
         bg.labels=c("transparent"),              # Background color of labels
         align.labels=list(
           c("center", "center"), 
           c("right", "bottom")
         ),                                   # Where to place labels in the rectangle?
         overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
         inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
         title = "Sierra Leone projections, improvement under MEY vs BAU"
) 
  
# just species projection ts for ones with big differences

chl_large_improvement_spp <- chl_scenario_diff %>% filter (mey_diff_percent > 250 & initial > 100)

ds_chl %>%
  filter (species %in% chl_large_improvement_spp$species, year > 2017, rcp == "RCP60") %>%
  ggplot (aes (x = year, y = catch_mt/1000000, col = scenario), lwd = 1.5) +
  geom_line() +
  theme_bw() +
  facet_wrap ( ~ species, scales = "free_y") +
  labs (x = "", y = "Catch, million metric tons", col = "Mgmt. scenario") +
  ggtitle (paste0 ("Chile top species improvements")) +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         strip.text = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12),
         axis.title = element_text (size = 14))

# how much do these contribute to current catch?
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")
chl_spp_catch <- chl_landings %>%
  filter (year == 2021) %>%
  group_by (species) %>%
  summarise (catch_mt = sum (catch_mt)) %>%
  ungroup() %>%
  mutate (perc_catch = catch_mt/sum(catch_mt)) %>%
  arrange (desc(perc_catch))

chl_spp_catch %>% filter (species %in% chl_large_improvement_spp$species)

chl_projection_explore <- chl_spp_catch %>%
  left_join (chl_scenario_diff, by = "species") %>%
  left_join (chl_perc_change_BAU, by = "species") %>%
  select (species, catch_mt, perc_catch, perc_diff, mey_diff_percent, adapt_diff_percent)

ds_chl %>%
  filter (species == "Trachurus murphyi", rcp == "RCP60") %>%
  ggplot (aes (x = year, y = catch_mt/1000, col = scenario)) +
  geom_line () +
  theme_bw()

ds_chl %>%
  filter (species == "Trachurus murphyi", rcp == "RCP60") %>%
  pivot_longer(range_km2:ffmsy, 
               names_to = "variable",
               values_to = "value") %>%
  ggplot (aes (x = year, y = value, col = scenario)) +
  geom_line () +
  facet_wrap (~variable, scales = "free") +
  theme_bw() +
  ggtitle ("Chile, T. murphyi, RCP 6.0")
  

ds_spp %>%
  filter (country == "Peru", species == "Trachurus murphyi", rcp == "RCP60") %>%
  pivot_longer(range_km2:ffmsy, 
               names_to = "variable",
               values_to = "value") %>%
  ggplot (aes (x = year, y = value, col = scenario)) +
  geom_line () +
  facet_wrap (~variable, scales = "free") +
  theme_bw() +
  ggtitle ("Peru, T. murphyi, RCP 6.0")

# for sierra leone, s. aurita vs. s. maderensis
ds_spp %>%
  filter (country == "Sierra Leone", species == "Sardinella maderensis", rcp == "RCP60") %>%
  pivot_longer(range_km2:ffmsy, 
               names_to = "variable",
               values_to = "value") %>%
  ggplot (aes (x = year, y = value, col = scenario)) +
  geom_line () +
  facet_wrap (~variable, scales = "free") +
  theme_bw() +
  ggtitle ("Sierra Leone, S. maderensis, RCP 6.0")

ds_spp %>%
  filter (country == "Sierra Leone", species == "Sardinella aurita", rcp == "RCP60") %>%
  pivot_longer(range_km2:ffmsy, 
               names_to = "variable",
               values_to = "value") %>%
  ggplot (aes (x = year, y = value, col = scenario)) +
  geom_line () +
  facet_wrap (~variable, scales = "free") +
  theme_bw() +
  ggtitle ("Sierra Leone, S. aurita, RCP 6.0")

# quick nutrient check
# compiled in compile_species_nutrition_data.R
#this has Fishnutr, AFCD, and D. gigas
compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")
# use WHO RNI
rni_child <- readRDS("Data/RNI_child.Rds") 


compiled_nutr %>%
  filter (species %in% c ("Sardinella maderensis", "Sardinella aurita"), !nutrient %in% c("Protein", "Selenium")) %>%
  left_join (rni_child, by = "nutrient") %>%
  mutate (perc_rni = amount / RNI * 100) %>%
  ggplot (aes (x = species, y = perc_rni, fill = nutrient)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  ggtitle ("Nutrient content per 100g serving") +
  labs (x = "", y = "% Child RNI met")
