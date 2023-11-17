# Plot Fig XC, nutrient content of top species

# 3/2/23, moving from regional_team_priority_spp_figs

library (tidyverse)
library (stringr)


# nutrient data ----

# bring in compiled instead; compile_species_nutrition_data.R
compiled_nutr <- readRDS("Data/species_nutrients_compiled.Rds")


# RNI data ----
# from RNI_explore; WHO
rni_child <- readRDS("Data/RNI_child.Rds")


# join nutrients and rni ----
spp_nutr <-  compiled_nutr %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rni = amount/RNI * 100,
          # optional--cap at 100?
          #perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          # shorten nutrient names so they fit on the x axis
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup()


# function to plot ----
plot_colorful_spp_nutr_dodge_bar <- function (species_names, Selenium = FALSE) {
  
  #species_names is a vector of scientific names
  
  if (Selenium == TRUE) {omit_nutrients <- "Protein"} else {omit_nutrients <- c("Protein", "Selenium")}
  
  spp_nutr %>%
    filter (!nutrient %in% omit_nutrients, 
            species %in% species_names) %>%
    group_by (species) %>%
    # order by overall nutrient density, from Maire et al. 2021
    mutate (micronutrient_density = sum (perc_rni),
            # shorten spp names
            # https://stackoverflow.com/questions/8299978/splitting-a-string-on-the-first-space
            spp_short = ifelse (
              grepl(" ", species),
              paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
              species)
    ) %>%
    ungroup() %>%
    
    ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
    geom_col (position = "dodge") +
    theme_bw() +
    labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") +
    #ylim (c(0,100)) +
    ggtitle ("Nutrient content of selected species") +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 16),
      strip.text = element_text(size = 16),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 14),
      plot.title = element_text (size = 18))
}


# Malawi top spp ----

# cleaned names from clean_malawi_landings
mal_top_names <- mal_names %>% filter (comm_name %in% c("Chambo", "Usipa", "Utaka", "Ndunduma"))

png ("Figures/Malawi_top_spp_nutr_dodge_bar.png", width = 5, height = 4, units = "in", res = 300)
print(
plot_colorful_spp_nutr_dodge_bar(mal_top_names$species, Selenium = FALSE) +
      ggtitle ("Nutrient content of top species by volume") +
     scale_x_discrete(labels = c ("Usipa", "Utaka", "Ndunduma", "Chambo")) +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 13),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 13),
      plot.title = element_text (size = 16))
)
dev.off()

# sierra leone ----

sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

# john wanted top industrial vs. artisanal
sl_top_sector <- sl_landings %>%
  filter (year == 2017) %>%
  #summarise (tonnes = sum(catch_mt, na.rm = TRUE)) %>%
  #ungroup() %>%
  group_by (sector) %>%
  slice_max (catch_mt, n=4)

# ss is E. fim, Sard, P. elongatus.. Large is sard, c. rhoncus, G. decadactylus for 2017. same for overall years. 

# plot with facet
sl_facet <- spp_nutr %>%
  filter (!nutrient %in% c("Selenium", "Protein"), 
          species %in% sl_top_sector$species) %>%
  group_by (species) %>%
  mutate (micronutrient_density = sum (perc_rni),
          spp_short = ifelse (
            grepl(" ", species),
            paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
            species)
  ) %>% 
  ungroup() %>%
  mutate (sector = case_when (
    species %in% c("Sardinella", "Ethmalosa fimbriata", "Pseudotolithus elongatus") ~ "Small-scale",
    TRUE ~ "Large-scale"
  ))

 # make fake additional sardinella
ls_sard <- sl_facet %>% filter (species == "Sardinella") %>% mutate (sector = "Large-scale")

sl_facet <- rbind (sl_facet, ls_sard)
sl_facet$sector <- factor (sl_facet$sector, levels = c ("Small-scale", "Large-scale"))


png ("Figures/SL_spp_nutr_dodge_bar_sector.png", width = 6, height = 4, units = "in", res = 300)
 sl_facet %>% 
  ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
  geom_col (position = "dodge") +
  theme_bw() +
  labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") +
  facet_wrap (~sector, scales = "free_x") +
  #ylim (c(0,100)) +
  ggtitle ("Nutrient content of top species by volume") +
  theme ( 
    axis.text.y = element_text (size = 13),
    axis.text.x = element_text (size = 11, angle = 45, vjust =1, hjust = 1),
    axis.title = element_text (size = 13),
    strip.text = element_text(size = 13),
    legend.text = element_text (size = 12),
    legend.title = element_text (size = 13),
    plot.title = element_text (size = 16))
dev.off()


# indonesia top spp ----
indo_top <- sau_2019 %>%
  filter (country == "Indonesia") %>%
  group_by (species) %>%
  summarise (tot = sum (tonnes)) %>%
  slice_max (tot, n = 5)

png ("Figures/Indo_top_spp_nutr_dodge_bar.png", width = 5, height = 4, units = "in", res = 300)
print(
  plot_colorful_spp_nutr_dodge_bar(indo_top$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top species by volume") +
    # scale_x_discrete(labels = c ())
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 13),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 13),
      plot.title = element_text (size = 16))
)
dev.off()

# try to find high selenium indo??
indo_spp <- sau_2019 %>%
  filter (country == "Indonesia") %>%
  group_by (species) %>%
  summarise (tot_catch = sum (tonnes)) %>%
  top_n(20, tot_catch)

plot_colorful_spp_nutr_dodge_bar(indo_spp$species, Selenium = TRUE)
# Sardinella species, and scomberomorous


# peru spp ----
peru_top <- sau_2019 %>%
  filter (country == "Peru") %>%
  group_by (species) %>%
  summarise (tot = sum (tonnes)) %>%
  slice_max (tot, n = 5)

png ("Figures/Peru_top_spp_nutr_dodge_bar.png", width = 5, height = 4, units = "in", res = 300)
print(
  plot_colorful_spp_nutr_dodge_bar(peru_top$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top species by volume") +
    # scale_x_discrete(labels = c ())
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 13),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 13),
      plot.title = element_text (size = 16))
)
dev.off()

# chile ----
chl_top <- chl_landings %>%
  group_by (species) %>%
  summarise (tot = sum (catch_mt)) %>%
  slice_max (tot, n = 3)

# algae is crazy
chl_top_alg <- chl_landings %>%
  filter (commercial_group == "Algae") %>%
  group_by (species) %>%
  summarise (tot = sum (catch_mt)) %>%
  slice_max (tot, n = 3)

png ("Figures/Chl_top_spp_nutr_dodge_bar_alg.png", width = 5, height = 4, units = "in", res = 300)
print(
  plot_colorful_spp_nutr_dodge_bar(chl_top_alg$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top algal species by volume") +
    # scale_x_discrete(labels = c ())
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 13),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 13),
      plot.title = element_text (size = 16))
)
dev.off()


png ("Figures/Chl_top_spp_nutr_dodge_bar_non-alg.png", width = 5, height = 4, units = "in", res = 300)
print(
  plot_colorful_spp_nutr_dodge_bar(chl_top$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top species by volume") +
    # scale_x_discrete(labels = c ())
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 13),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 13),
      plot.title = element_text (size = 16))
)
dev.off()
