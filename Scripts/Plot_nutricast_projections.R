# Plot nutricast projections
library (tidyverse)
# 1/31/23 from regional_team_priority_species_figs older code

# smaller, just rcp 60 and 85. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

# calculate % loss by 2051-2060 relative to 2017-2021 baseline to report in summary table ----
t <- ds_spp %>% 
  mutate (
    period = case_when (
      year %in% c(2017:2021) ~ "2017-2021",
      year %in% c(2051:2060) ~ "2051-2060")) %>%
  filter (!is.na (catch_mt), !is.na (period), scenario == "No Adaptation", rcp == "RCP60", ) %>%
  #take mean projected catch for the decade period
  group_by (country, rcp, period, species, scenario) %>%
  summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  # find difference among scenarios--absolute and percent diff
  group_by (country) %>%
  reframe (perc_diff = (sum (catch_mt[period == "2051-2060"]) - sum (catch_mt[period == "2017-2021"]))/sum (catch_mt[period == "2017-2021"]) * 100)
  

  
  # peru anchovy vs. no anchovy
  ds_spp %>% 
    filter (country == "Peru", species == "Engraulis ringens") %>%
    mutate (
      period = case_when (
        year %in% c(2017:2021) ~ "2017-2021",
        year %in% c(2051:2060) ~ "2051-2060")) %>%
    filter (!is.na (catch_mt), !is.na (period), scenario == "No Adaptation", rcp == "RCP60", ) %>%
    #take mean projected catch for the decade period
    group_by (country, rcp, period, species, scenario) %>%
    summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
    ungroup() %>%
    # find difference among scenarios--absolute and percent diff
    group_by (country) %>%
    reframe (perc_diff = (sum (catch_mt[period == "2051-2060"]) - sum (catch_mt[period == "2017-2021"]))/sum (catch_mt[period == "2017-2021"]) * 100)
  
  ds_spp %>% 
    filter (country == "Peru", species != "Engraulis ringens") %>%
    mutate (
      period = case_when (
        year %in% c(2017:2021) ~ "2017-2021",
        year %in% c(2051:2060) ~ "2051-2060")) %>%
    filter (!is.na (catch_mt), !is.na (period), scenario == "No Adaptation", rcp == "RCP60", ) %>%
    #take mean projected catch for the decade period
    group_by (country, rcp, period, species, scenario) %>%
    summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
    ungroup() %>%
    # find difference among scenarios--absolute and percent diff
    group_by (country) %>%
    reframe (perc_diff = (sum (catch_mt[period == "2051-2060"]) - sum (catch_mt[period == "2017-2021"]))/sum (catch_mt[period == "2017-2021"]) * 100)
  
  ds_spp %>% 
    filter (country == "Peru", species == "Engraulis ringens") %>%
    mutate (
      period = case_when (
        year %in% c(2017:2021) ~ "2017-2021",
        year %in% c(2091:2100) ~ "2051-2060")) %>%
    filter (!is.na (catch_mt), !is.na (period), scenario == "No Adaptation", rcp == "RCP60", ) %>%
    #take mean projected catch for the decade period
    group_by (country, rcp, period, species, scenario) %>%
    summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
    ungroup() %>%
    # find difference among scenarios--absolute and percent diff
    group_by (country) %>%
    reframe (perc_diff = (sum (catch_mt[period == "2051-2060"]) - sum (catch_mt[period == "2017-2021"]))/sum (catch_mt[period == "2017-2021"]) * 100)
  
  ds_spp %>% 
    filter (country == "Peru", species != "Engraulis ringens") %>%
    mutate (
      period = case_when (
        year %in% c(2017:2021) ~ "2017-2021",
        year %in% c(2091:2100) ~ "2051-2060")) %>%
    filter (!is.na (catch_mt), !is.na (period), scenario == "No Adaptation", rcp == "RCP60", ) %>%
    #take mean projected catch for the decade period
    group_by (country, rcp, period, species, scenario) %>%
    summarise (catch_mt = mean (catch_mt, na.rm = TRUE)) %>%
    ungroup() %>%
    # find difference among scenarios--absolute and percent diff
    group_by (country) %>%
    reframe (perc_diff = (sum (catch_mt[period == "2051-2060"]) - sum (catch_mt[period == "2017-2021"]))/sum (catch_mt[period == "2017-2021"]) * 100)
  
# Plot overall yield time series ----
  
ds_spp$scenario <- factor (ds_spp$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))

plot_catch_proj <- function (country_name)  {
  plot <- ds_spp %>%
    filter (country == country_name, scenario %in% c ("No Adaptation", "Productivity Only", "Full Adaptation"), year > 2017) %>%
    group_by (year, rcp, scenario) %>%
    summarise (tot_catch = sum (catch_mt, na.rm = TRUE)) %>%
    ggplot (aes (x = year, y = tot_catch/1000000, col = scenario), lwd = 1.5) +
    geom_line() +
    theme_bw() +
    facet_wrap ( ~ rcp, scales = "free_y") +
    labs (x = "", y = "Catch, million metric tons", col = "Mgmt. scenario") +
    ggtitle (paste0 ("Total catch projections for ", country_name, "\nFree et al. (2020) ")) +
    theme (plot.title = element_text (size = 18),
           axis.text = element_text (size = 12),
           strip.text = element_text (size = 14),
           legend.title = element_text (size = 14),
           legend.text = element_text (size = 12),
           axis.title = element_text (size = 14))
  
  png (paste0("Figures/Total_catch_projections_", country_name, ".png"), width = 8, height = 6, units = "in", res = 300)
  print(plot)
  dev.off()
  
}

sapply (c("Chile", "Sierra Leone", "Indonesia"), plot_catch_proj)

# peru no anchov vs anchov
png ("Figures/Total_catch_projections_Peru_noanchov.png", width = 8, height = 6, units = "in", res = 300)
ds_spp %>%
  filter (country == "Peru", species != "Engraulis ringens", scenario %in% c ("No Adaptation", "Productivity Only", "Full Adaptation"), year > 2017) %>%
  group_by (year, rcp, scenario) %>%
  summarise (tot_catch = sum (catch_mt, na.rm = TRUE)) %>%
  ggplot (aes (x = year, y = tot_catch/1000000, col = scenario), lwd = 1.5) +
  geom_line() +
  theme_bw() +
  facet_wrap ( ~ rcp, scales = "free_y") +
  labs (x = "", y = "Catch, million metric tons", col = "Mgmt. scenario") +
  ggtitle ("Total catch projections for Peru, anchoveta removed\nFree et al. (2020)") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         strip.text = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12),
         axis.title = element_text (size = 14))

dev.off()   

png ("Figures/Total_catch_projections_Peru_anchov.png", width = 8, height = 6, units = "in", res = 300)
ds_spp %>%
  filter (country == "Peru", species == "Engraulis ringens", scenario %in% c ("No Adaptation", "Productivity Only", "Full Adaptation"), year > 2017) %>%
  group_by (year, rcp, scenario) %>%
  summarise (tot_catch = sum (catch_mt, na.rm = TRUE)) %>%
  ggplot (aes (x = year, y = tot_catch/1000000, col = scenario), lwd = 1.5) +
  geom_line() +
  theme_bw() +
  facet_wrap ( ~ rcp, scales = "free_y") +
  labs (x = "", y = "Catch, million metric tons", col = "Mgmt. scenario") +
  ggtitle ("Total catch projections for Peru, anchoveta only\nFree et al. (2020)") +
  theme (plot.title = element_text (size = 18),
         axis.text = element_text (size = 12),
         strip.text = element_text (size = 14),
         legend.title = element_text (size = 14),
         legend.text = element_text (size = 12),
         axis.title = element_text (size = 14))

dev.off()  
  

priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv") %>%
  # just change S. japonicus peruanus to S. japonicus; no nutrient or SAU or nutricast data
  mutate (species = case_when (species == "Scomber japonicus peruanus" ~ "Scomber japonicus",
                               TRUE ~ species)
  ) # 


dat <- ds_spp%>% 
  filter (country == "Indonesia", species == "Lutjanus gibbus", rcp %in% c("RCP26", "RCP60"))

dat$scenario <- factor (dat$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))

p <- dat %>% ggplot (aes (x = year, y = catch_mt, col = scenario)) +
  geom_line() +
  theme_bw() +
  facet_wrap ( ~ rcp, scales = "free_y", ncol = 2) +
  labs (x = "", y = "Catch, metric tons", col = "Mgmt scenario") +
  #ggtitle (paste0 ("Free et al. (2020) projections for ", spp_name, ", ", country_name)) +
  theme (plot.title = element_text (size = 14),
         axis.text = element_text (size = 12),
         legend.position = "none")

print (p)

# plot nutricast projections for all priority spp ----

plot_nutricast_proj<- function (country_name, spp_name) {
  
  # dat <- ds_spp%>% 
  #   filter (country == country_name, species == spp_name, year  >2030, rcp %in% c("RCP26", "RCP60"))
  # 
  dat <- ds_spp%>% 
    filter (country == country_name, species == spp_name, rcp %in% c("RCP26", "RCP60"))
  
  dat$scenario <- factor (dat$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  p <- dat %>% ggplot (aes (x = year, y = catch_mt, col = scenario)) +
    geom_line() +
    theme_bw() +
    facet_grid (species ~ rcp, scales = "free_y") +
    labs (x = "", y = "Catch, metric tons", col = "Mgmt scenario") +
    ggtitle (paste0 ("Free et al. (2020) projections for ", spp_name, ", ", country_name)) +
    theme (plot.title = element_text (size = 14),
           axis.text = element_text (size = 12))
  
  print (p)
  
}

plot_nutricast_proj(country_name = "Peru",spp_name = "Engraulis ringens")

# need to take out missing country/spp combos? must be a faster way
ds_pri_spp_input <- ds_pri_spp %>% filter (!is.na (catch_mt), catch_mt > 0) %>% select (country, species, rank) %>% distinct %>%
  arrange (country, rank)

pri_input_ls <- list (country_name = ds_pri_spp_input$country, spp_name = ds_pri_spp_input$species)

pdf (file = "Figures/Priority_spp_Nutricast_timeseries_26_60.pdf", width = 12, height = 8)
print(
  mapply (plot_nutricast_proj, ds_pri_spp_input$country, ds_pri_spp_input$species)
)
dev.off()

# Indonesia
indo_spp <- c(pri_spp_indo$species, "Epinephelus tauvina", "Engraulis japonicus", "Encrasicholina punctifer")
# clip to species in ds
indo_spp <- indo_spp[which (indo_spp %in% ds_spp$species)]


indo_input_ls <- list (country_name = "Indonesia", spp_name = indo_spp)

pdf (file = "Figures/Indo_Pri_spp_Nutricast_timeseries_26_60.pdf", width = 12, height = 8)

  lapply (indo_spp, plot_nutricast_proj, country_name = "Indonesia") #indo_input_ls$country_name, indo_input_ls$spp_name)

dev.off()
# print doesn't work with lapply (or really mapply) and it does that weird doubling thing...


# John also requested biomass, profits

# try to pivot_long so can show on the same graph?
ds_pri_long <- ds_pri_spp %>%
  pivot_longer(range_km2:ffmsy, 
               names_to = "variable",
               values_to = "value")

ds_pri_long %>%
  filter (country == "Peru", species == "Mugil cephalus", 
          !variable %in% c ("catch_mt", "biomass_mt", "profits_usd")) %>%
  ggplot (aes (x = year, y = value, col = scenario)) +
  geom_line () +
  facet_grid (variable ~ rcp, scales = "free_y") +
  theme_bw() +
  labs (x = "", y = "") +
  ggtitle (paste0 ("Free et al. (2020) projections for ", spp_name, ", ", country_name)) +
  theme (plot.title = element_text (size = 14),
         axis.text = element_text (size = 12))


ds_pri_long %>%
  filter (country == "Peru", species == "Mugil cephalus", 
          variable %in% c ("catch_mt", "msy_mt")) %>%
  ggplot (aes (x = year, y = value, col = variable)) +
  geom_line () +
  facet_grid (scenario ~ rcp, scales = "free_y") +
  theme_bw() +
  labs (x = "", y = "") +
  ggtitle (paste0 ("Free et al. (2020) projections for ", spp_name, ", ", country_name)) +
  theme (plot.title = element_text (size = 14),
         axis.text = element_text (size = 12))


# Plot biomass, catch, profits 
plot_nutricast_proj<- function (country_name, spp_name) {
  
  dat <- ds_pri_long %>% 
    filter (country == country_name, species == spp_name, year  >2030)
  
  dat$scenario <- factor (dat$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  dat$variable <- factor (dat$variable, levels = c ("biomass_mt", "catch_mt", "profits_usd", "range_km2", "msy_mt", "bbmsy", "ffmsy"))
  
  p <- dat %>% 
    filter (variable %in% c ("catch_mt", "biomass_mt", "profits_usd")) %>%
    ggplot (aes (x = year, y = value, col = scenario)) +
    geom_line () +
    facet_grid (variable ~ rcp, scales = "free_y") +
    theme_bw() +
    labs (x = "", y = "") +
    ggtitle (paste0 ("Free et al. (2020) projections for ", spp_name, ", ", country_name)) +
    theme (plot.title = element_text (size = 14),
           axis.text = element_text (size = 12))
  
  
  print (p)
  
}

plot_nutricast_proj ("Sierra Leone", "Sardinella maderensis")

ds_pri_spp_input <- ds_pri_spp %>% filter (!is.na (catch_mt), catch_mt > 0) %>% select (country, species, rank) %>% distinct %>%
  arrange (country, rank)

pri_input_ls <- list (country_name = ds_pri_spp_input$country, spp_name = ds_pri_spp_input$species)

pdf (file = "Figures/Nutricast_timeseries_Biomass_Catch_Profits.pdf", width = 12, height = 8)
print(
  mapply (plot_nutricast_proj, ds_pri_spp_input$country, ds_pri_spp_input$species)
)
dev.off()

# Plot other variables
plot_nutricast_proj<- function (country_name, spp_name) {
  
  dat <- ds_pri_long %>% 
    filter (country == country_name, species == spp_name, year  >2030)
  
  dat$scenario <- factor (dat$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  dat$variable <- factor (dat$variable, levels = c ("biomass_mt", "catch_mt", "profits_usd", "range_km2", "msy_mt", "bbmsy", "ffmsy"))
  
  p <- dat %>% 
    filter (!variable %in% c ("catch_mt", "biomass_mt", "profits_usd")) %>%
    ggplot (aes (x = year, y = value, col = scenario)) +
    geom_line () +
    facet_grid (variable ~ rcp, scales = "free_y") +
    theme_bw() +
    labs (x = "", y = "") +
    ggtitle (paste0 ("Free et al. (2020) projections for ", spp_name, ", ", country_name)) +
    theme (plot.title = element_text (size = 14),
           axis.text = element_text (size = 12))
  
  
  print (p)
  
}


pdf (file = "Figures/Nutricast_timeseries_MSY_BBMSY_FFMSY.pdf", width = 12, height = 8)
print(
  mapply (plot_nutricast_proj, ds_pri_spp_input$country, ds_pri_spp_input$species)
)
dev.off()

# plot catch and msy together
plot_nutricast_proj<- function (country_name, spp_name) {
  
  dat <- ds_pri_long %>% 
    filter (country == country_name, species == spp_name, year  >2030)
  
  dat$scenario <- factor (dat$scenario, levels = c ("No Adaptation", "Productivity Only", "Full Adaptation"))
  
  dat$variable <- factor (dat$variable, levels = c ("biomass_mt", "catch_mt", "profits_usd", "range_km2", "msy_mt", "bbmsy", "ffmsy"))
  
  p <- dat %>% 
    filter (variable %in% c ("catch_mt", "msy_mt")) %>%
    ggplot (aes (x = year, y = value, col = variable)) +
    geom_line () +
    facet_grid (scenario ~ rcp, scales = "free_y") +
    theme_bw() +
    labs (x = "", y = "") +
    ggtitle (paste0 ("Free et al. (2020) projections for ", spp_name, ", ", country_name)) +
    theme (plot.title = element_text (size = 14),
           axis.text = element_text (size = 12))
  
  
  print (p)
  
}


pdf (file = "Figures/Nutricast_timeseries_catch_vs_MSY.pdf", width = 12, height = 8)
print(
  mapply (plot_nutricast_proj, ds_pri_spp_input$country, ds_pri_spp_input$species)
)
dev.off()

# plot FULL time series, aggregated catch 
png ("Figures/Nutricast_catch_full_ts_all_scen.png", width = 12, height = 8, units = "in", res = 300)
ds_spp %>%
  filter (!country %in% c("Ghana", "Mexico")) %>%
  group_by (country, rcp, scenario, year) %>%
  summarise (tot = sum (catch_mt, na.rm = TRUE)) %>%
  ggplot (aes (x = year, y = tot/1000000, col = scenario)) +
  geom_line() +
  facet_grid (country ~ rcp, scales = "free") +
  labs (x= "", y = "Catch, million metric tonnes")+
  theme_bw() +
  ggtitle ("Nutricast full projection ts") +
  theme (axis.text.y = element_text (size = 14),
         axis.text.x = element_text (size = 13),
         axis.title = element_text (size = 16),
         strip.text = element_text (size = 15),
         legend.text = element_text (size = 12),
         legend.title = element_text (size = 14),
         plot.title = element_text (size = 18))
dev.off()

# zoom in on early part of ts
ds_spp %>%
  filter (!country %in% c("Ghana", "Mexico"),
          between (year, 2017, 2025)) %>%
  group_by (country, rcp, scenario, year) %>%
  summarise (tot = sum (catch_mt, na.rm = TRUE)) %>%
  ggplot (aes (x = year, y = tot/1000000, col = scenario)) +
  geom_line() +
  facet_grid (country ~ rcp, scales = "free") +
  labs (x= "", y = "Catch, million metric tonnes")+
  theme_bw() +
  ggtitle ("Nutricast full projection ts") +
  theme (axis.text.y = element_text (size = 14),
         axis.text.x = element_text (size = 13),
         axis.title = element_text (size = 16),
         strip.text = element_text (size = 15),
         legend.text = element_text (size = 12),
         legend.title = element_text (size = 14),
         plot.title = element_text (size = 18))
