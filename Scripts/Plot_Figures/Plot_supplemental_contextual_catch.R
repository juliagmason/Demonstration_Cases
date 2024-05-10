# Plot supplemental catch/landings full data 
# 5/10/24
# JGM

# moving from Plot_contextual_landings_forecasts.R
# Plot longer time series and full landings or catch data that isn't clipped to nutricast species


# full sau, at least ten years ----
download_2019_full <- read.csv("Data/SAU EEZ 2019.csv")%>% # this doesn't have indonesia
  mutate (country = case_when (grepl ("Mex", area_name) ~ "Mexico",
                               grepl ("Chile", area_name) ~ "Chile",
                               TRUE ~ area_name))


indo_2019_download_full <- read.csv("Data/SAU EEZ indonesia.csv") %>%
  mutate (country = "Indonesia") 

sau_full <- rbind (download_2019_full, indo_2019_download_full)

sau_20yr <- sau_full %>%
  filter (between (year, 2000, 2019)) %>%
  rename (species = scientific_name)


# Supplemental info: full landings trend and compare data sources ----
# plot landings trend for contextual figure ----

# Full SAU data for each case
country_names <- c("Peru", "Chile", "Sierra Leone", "Indonesia")

for (country_name in country_names) {
  png (paste0("Figures/FigSX_", country_name, "_landings_trend_SAU.png"), width = 7, height = 4, units = "in", res = 300)
  
  sau_20yr %>%
    filter (country == country_name, year > 2000) %>%
    # cut to 8 comm groups
    mutate (commercial_group = case_when (
      commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
      TRUE ~ commercial_group
    )) %>%
    group_by(year, commercial_group) %>%
    summarise (tonnes = sum (tonnes)) %>%
    
    ggplot (aes (x = year, y = tonnes/1000000)) +
    geom_area(aes (fill = commercial_group), position = "stack") +
    
    # qualitative color scale for species, 
    scale_fill_brewer(palette = "Dark2") +
    labs (y ="Catch, million tonnes", x = "", fill = "Commercial group")+
    ggtitle (paste0("Landings trends, ", country_name," SAU")) +
    theme_bw() +
    theme (axis.text = element_text (size = 11),
           axis.title = element_text (size = 12),
           legend.text = element_text (size = 11),
           legend.title = element_text (size = 12),
           legend.key.size = unit (3.5, "mm"),
           legend.margin=margin(1,1,1,2),
           legend.box.margin=margin(-10,-10,-10,-10),
           plot.title = element_text(size = 13),
           plot.margin=unit(c(1,1,1,1), 'mm')) 

  
  dev.off()
}


# chile sernapesca compare ----
# note: as of 6/6/23, have added commercial_group column but very preliminary, just lumped all the fish that weren't in SAU into "other"
chl_landings <- readRDS ("Data/Chl_sernapesca_landings_compiled_2012_2021.Rds")

# trick color scale....
chl_landings$commercial_group = factor(chl_landings$commercial_group, levels = c("Anchovies","Cod-likes","Crustaceans","Flatfishes","Herring-likes","Molluscs","Other fishes & inverts",
                                                                                 "Perch-likes","Salmon, smelts, etc","Scorpionfishes","Sharks & rays","Tuna & billfishes", "Algae"))

png ("Figures/FigSX_Chile_landings_trend_Sernapesca.png", width = 7, height = 4, units = "in", res = 300)
chl_landings %>%
  # cut to 8 comm groups
  mutate (commercial_group = case_when (
    commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes", "Salmon, smelts, etc") ~ "Other fishes & inverts",
    TRUE ~ commercial_group
  )) %>%
  group_by (year, commercial_group) %>%
  summarise (catch_mt = sum(catch_mt, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot (aes (x = year, y = catch_mt/1000000, fill = commercial_group, group = commercial_group)) +
  # have to add a color for algae
  scale_fill_manual(values = c("#4EB3D3", "#1B9E77", "#D95F02", "#7570b3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  geom_area()+
  theme_bw() +
  labs (x = "", y = "Catch, million tonnes", fill = "Commercial group") +
  ggtitle ("Landings trends, Chile official statistics") +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 11),
         legend.title = element_text (size = 12),
         legend.key.size = unit (3.5, "mm"),
         legend.margin=margin(1,1,1,2),
         legend.box.margin=margin(-10,-10,-10,-10),
         plot.title = element_text(size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm')) 

dev.off()


# Sierra Leone  IHH compare ----
# year with data for both artisanal and industrial is 2017
sl_ihh_landings <- readRDS("Data/SLE_landings_IHH.Rds")

png ("Figures/FigSX_SL_landings_trend_IHH.png", width = 7, height = 4, units = "in", res = 300)
sl_ihh_landings %>%
  filter (!year == 2018) %>% # all NA
  mutate (commercial_group = case_when (
    commercial_group %in% c("Flatfishes", "Scorpionfishes", "Cod-likes") ~ "Other fishes & inverts",
    is.na (commercial_group) ~ "Other fishes & inverts",
    TRUE ~ commercial_group
  )) %>%
  group_by(year, commercial_group) %>%
  summarise (tonnes = sum (catch_mt, na.rm = TRUE)) %>%
  
  ggplot (aes (x = year, y = tonnes/1000000)) +
  geom_area(aes (fill = commercial_group), position = "stack") +
  
  # qualitative color scale for species, 
  scale_fill_manual(values = c("#1B9E77","#D95F02", "#7570b3", "#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  labs (y ="Catch, million tonnes", x = "", fill = "Commercial group")+
  ggtitle ("Landings trends, Sierra Leone IHH") +
  theme_bw() +
  theme (axis.text = element_text (size = 11),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 11),
         legend.title = element_text (size = 12),
         legend.key.size = unit (3.5, "mm"),
         legend.margin=margin(1,1,1,2),
         legend.box.margin=margin(-10,-10,-10,-10),
         plot.title = element_text(size = 13),
         plot.margin=unit(c(1,1,1,1), 'mm')) 

dev.off()
