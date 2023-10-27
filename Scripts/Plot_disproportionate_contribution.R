# data vis options for disproportionate contribution 
# 10/26/23
# JGM

library (tidyverse)

# compiled levers in Calculate_national_allocative_levers.R

sl_foreign <- readRDS("Data/levers_RNI_pop_foreign_sector_SierraLeone.Rds")

# make a dummy column??
# just nabbing values from google sheet, https://docs.google.com/spreadsheets/d/1-GRwNuAAnM_OzxaCzDJW4xqm0vIPpbE9z3Mqm4mPvxs/edit#gid=0
sl_foreign_dummy <- tibble (
  country = "Sierra Leone",
  sector = c ("Foreign catch", "Domestic catch"),
  nutrient = "Volume",
  rni_equivalents = c (24.77, (100-24.77)),
  perc_demand_met = 0
)

#combine ssf/lsf into domestic, and rbind with dummy df
sl_ds <- sl_foreign %>%
  filter (!nutrient == "Protein") %>%
  mutate (sector = ifelse (sector == "Foreign catch", "Foreign catch", "Domestic catch")) %>%
  rbind (sl_foreign_dummy)

sl_ds$nutrient <- factor (sl_ds$nutrient, levels = c ("Volume", "Calcium", "Iron", "Omega_3", "Selenium", "Vitamin_A", "Zinc"))

sl_ds %>%
  
  ggplot (aes(x = nutrient, y = rni_equivalents, fill = sector)) +
  geom_bar (position = "fill", stat = "identity")+
  geom_hline (yintercept = 24.77/100, lty = 2) +
  labs (x = "", y = "% yield")+
  theme(legend.position ="bottom")

# SSF/lsf
sl_ssf_dummy <- tibble (
  country = "Sierra Leone",
  sector = c ("Large-scale", "Small-scale"),
  nutrient = "Volume",
  rni_equivalents = c (24.61, (100-24.61)),
  perc_demand_met = 0
)

sl_ssf <- sl_foreign %>%
  filter (!nutrient == "Protein", !sector == "Foreign catch") %>%
  rbind (sl_ssf_dummy)

sl_ssf$nutrient <- factor (sl_ssf$nutrient, levels = c ("Volume", "Calcium", "Iron", "Omega_3", "Selenium", "Vitamin_A", "Zinc"))
sl_ssf %>%
  
  ggplot (aes(x = nutrient, y = rni_equivalents, fill = sector)) +
  geom_bar (position = "fill", stat = "identity")+
  geom_hline (yintercept = (100-24.61)/100, lty = 2)

# exports
sl_exports <- readRDS("Data/levers_RNI_pop_export_SierraLeone.Rds")

sl_exports_dummy <- tibble (
    country = "Sierra Leone",
    sector = c ("Exported", "Retained"),
    nutrient = "Volume",
    rni_equivalents = c (13.91, (100-13.91)),
    perc_demand_met = 0
)

# rbind with dummy df, rename so can eventually combine?
sl_ds_exp <- sl_exports %>%
  rename (sector = exports) %>%
  filter (!nutrient == "Protein") %>%
  rbind (sl_exports_dummy)

sl_ds_exp$nutrient <- factor (sl_dr_exp$nutrient, levels = c ("Volume", "Calcium", "Iron", "Omega_3", "Selenium", "Vitamin_A", "Zinc"))
sl_ds_exp$sector <- factor (sl_dr_exp$sector, levels = c ("Retained", "Exported"))

sl_ds_exp %>%
  
  ggplot (aes(x = nutrient, y = rni_equivalents, fill = sector)) +
  geom_bar (position = "fill", stat = "identity")+
  geom_hline (yintercept = 13.91/100, lty = 2) +
  labs (x = "", y = "% yield") +
  theme(legend.position ="bottom")

# facet
sl_ds_exp$lever = "Exports"
sl_ds$lever = "Foreign fishing"
sl_ssf$lever = "Artisanal"

sl_combined <- rbind (sl_dr_exp, sl_dr, sl_ssf)

sl_combined %>%
  ggplot (aes(x = nutrient, y = rni_equivalents, fill = sector)) +
  geom_bar (position = "fill", stat = "identity")+
  geom_hline (yintercept = 13.91/100, lty = 2) +
  facet_wrap (~lever)

# no....


## circles.....
# https://r-graph-gallery.com/305-basic-circle-packing-with-one-level.html
# https://r-graph-gallery.com/306-custom-circle-packing-with-one-level.html
library (packcircles)

# i think data need to be standardized already
sl_Exp_percents <- sl_ds_exp %>%
  group_by (nutrient) %>%
  mutate (tot_rni = sum (rni_equivalents),
          perc_rni = rni_equivalents / tot_rni * 100)


packing <- circleProgressiveLayout(sl_Exp_percents$perc_rni, sizetype='area')
data <- cbind(sl_Exp_percents, packing)
plot(data$radius, data$perc_rni)
dat.gg <- circleLayoutVertices(packing, npoints=50)
dat.gg$nutrient <- c(rep (data$nutrient, each = 50), rep (NA, 14))

ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=nutrient), colour = "black", alpha = 0.6) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=perc_rni, label = nutrient)) +
  scale_size_continuous(range = c(1,4)) +
  facet_wrap (~sector) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()


# fake network diagram?!