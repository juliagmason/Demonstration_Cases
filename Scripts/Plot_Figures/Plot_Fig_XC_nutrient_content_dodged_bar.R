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

# landings data to find top spp----
#  SAU data
sau_2019 <- readRDS ("Data/SAU_2019.Rds")

#Clean_Chile_Sernapesca_landings.R
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# Sierra Leone IHH data
sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

# Malawi FAO data 
mal_landings <- readRDS("Data/Malawi_landings_cleaned.Rds")


# Malawi top spp ----

# cleaned names from clean_malawi_landings
mal_top <- mal_landings %>% 
  filter (Year == 2017, comm_name %in% c("Chambo", "Usipa", "Utaka", "Ndunduma")) %>%
  group_by (species, comm_name) %>%
  summarise (tonnes = sum (tonnes)) %>%
  ungroup() %>%
  slice_max (tonnes, n=4)

plot_colorful_spp_nutr_dodge_bar(mal_top$species, Selenium = FALSE) +
  ggtitle ("Nutrient content of top species") +
  #scale_x_discrete(labels = c ("Usipa", "Utaka", "Ndunduma", "Chambo")) +
  labs (y = "% Child RNI met")+
  scale_fill_brewer(palette = "Set1") +
  theme ( 
    axis.text = element_text (size = 11),
    axis.title = element_text (size = 12),
    legend.text = element_text (size = 10),
    legend.title = element_text (size = 12),
    legend.key.size = unit (3.5, "mm"),
    plot.title = element_text (size = 13),
    plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/FigXC_nutr_Malawi.eps", width = 100, height = 60, units = "mm")

# sierra leone ----

sl_landings <- readRDS("Data/SLE_landings_IHH.Rds")

sl_top <- sl_landings %>%
  filter (year == 2017, !species %in% c("ND", "Mixed Species", "Marine species NEI")) %>%
  group_by (species) %>%
  summarise (catch_mt = sum(catch_mt, na.rm = TRUE)) %>%
  slice_max (catch_mt, n=4)

plot_colorful_spp_nutr_dodge_bar(sl_top$species, Selenium = FALSE) +
  ggtitle ("Nutrient content of top species") +
  # scale_x_discrete(labels = c ()) +
  labs (y = "% Child RNI met")+
  scale_fill_brewer(palette = "Set1") +
  theme ( 
    axis.text = element_text (size = 11),
    axis.title = element_text (size = 12),
    legend.text = element_text (size = 10),
    legend.title = element_text (size = 12),
    legend.key.size = unit (3.5, "mm"),
    plot.title = element_text (size = 13),
    plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/FigXC_nutr_SierraLeone.eps", width = 100, height = 60, units = "mm")

# indonesia top spp ----
indo_top <- sau_2019 %>%
  filter (country == "Indonesia") %>%
  group_by (species) %>%
  summarise (tot = sum (tonnes)) %>%
  slice_max (tot, n = 7)

  plot_colorful_spp_nutr_dodge_bar(indo_top$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top species") +
    # scale_x_discrete(labels = c ()) +
    labs (y = "% Child RNI met")+
    scale_fill_brewer(palette = "Set1") +
    theme ( 
      axis.text = element_text (size = 11),
      axis.title = element_text (size = 12),
      legend.text = element_text (size = 10),
      legend.title = element_text (size = 12),
      legend.key.size = unit (3.5, "mm"),
      plot.title = element_text (size = 13),
      plot.margin=unit(c(1,1,1,1), 'mm'))
# )
# dev.off()

ggsave ("Figures/FigXC_nutr_Indo.eps", width = 100, height = 60, units = "mm")

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

# png ("Figures/Peru_top_spp_nutr_dodge_bar.png", width = 5, height = 4, units = "in", res = 300)
# print(
# )
# dev.off()

  plot_colorful_spp_nutr_dodge_bar(peru_top$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top species") +
    # scale_x_discrete(labels = c ()) +
    labs (y = "% Child RNI met")+
    scale_fill_brewer(palette = "Set1") +
    theme ( 
      axis.text = element_text (size = 11),
      axis.title = element_text (size = 12),
      legend.text = element_text (size = 10),
      legend.title = element_text (size = 12),
      legend.key.size = unit (3.5, "mm"),
      plot.title = element_text (size = 13),
      plot.margin=unit(c(1,1,1,1), 'mm'))

  
  ggsave ("Figures/FigXC_nutr_Peru.eps", width = 100, height = 60, units = "mm")


# chile ----
chl_top <- chl_landings %>%
  group_by (species) %>%
  summarise (tot = sum (catch_mt)) %>%
  slice_max (tot, n = 4)
  
  
  plot_colorful_spp_nutr_dodge_bar(chl_top$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top species") +
    # scale_x_discrete(labels = c ()) +
    labs (y = "% Child RNI met")+
    scale_fill_brewer(palette = "Set1") +
    theme ( 
      axis.text = element_text (size = 11),
      axis.title = element_text (size = 12),
      legend.text = element_text (size = 10),
      legend.title = element_text (size = 12),
      legend.key.size = unit (3.5, "mm"),
      plot.title = element_text (size = 13),
      plot.margin=unit(c(1,1,1,1), 'mm'))
  
  
  ggsave ("Figures/FigXC_nutr_Chile.eps", width = 100, height = 60, units = "mm")

# algae is crazy
chl_top_alg <- chl_landings %>%
  filter (commercial_group == "Algae") %>%
  group_by (species) %>%
  summarise (tot = sum (catch_mt)) %>%
  slice_max (tot, n = 5) %>%
  # no nutrient data for M. pyriphera
  filter (species != "Macrocystus pyriphera")

# supplemental figure with algae
png ("Figures/FigSX_Chl_nutr_alg.png", width = 7, height = 4, units = "in", res = 300)
print(
  plot_colorful_spp_nutr_dodge_bar(chl_top_alg$species, Selenium = FALSE) +
    ggtitle ("Nutrient content of top algal species by volume") +
    scale_fill_brewer(palette = "Set1") +
    theme ( 
      axis.text.y = element_text (size = 13),
      axis.text.x = element_text (size = 11),
      axis.title = element_text (size = 13),
      legend.text = element_text (size = 12),
      legend.title = element_text (size = 13),
      plot.title = element_text (size = 16))
)
dev.off()

