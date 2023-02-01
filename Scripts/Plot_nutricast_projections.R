# Plot nutricast projections

# 1/31/23 from regional_team_priority_species_figs older code

# smaller, just rcp 60 and 85. now has mexico
ds_spp <- readRDS("Data/Free_etal_proj_smaller.Rds")

priority_spp <- read_csv ("Data/regional_teams_priority_spp.csv") %>%
  # just change S. japonicus peruanus to S. japonicus; no nutrient or SAU or nutricast data
  mutate (species = case_when (species == "Scomber japonicus peruanus" ~ "Scomber japonicus",
                               TRUE ~ species)
  ) # 
# Add E. 

# plot nutricast projections for all priority spp ----

plot_nutricast_proj<- function (country_name, spp_name) {
  
  dat <- ds_spp%>% 
    filter (country == country_name, species == spp_name, year  >2030, rcp %in% c("RCP26", "RCP60"))
  
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