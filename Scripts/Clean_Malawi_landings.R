# Clean Malawi landings
# 5/26/23
# JGM

# Abby emailed 5/26/23
# Abby: I suggested that we just use the species in non-grayed rows on the scientific names tab; the others I have fairly confidently determined are aquarium species, or there are no nutrient estimates on fish base; for the latter categories, they are harvested in such low volumes anyway.
#I still need to ask a couple of questions about Kambuzi, but I’m pretty sure we will leave it out.. 
#I suggest averaging lidole and karongae (chambo)


# For utaka, M. inornata in fishnutr but not C. inornata
# Call chambo O. lidole combined; this is what I called it in fishnutr

# greyed out: kambuzi, chisawasawa

# missing: Mlamba"        "Kampango"      "Others"        "Sanjika"       "Ntchila"       "Mpasa"    "Other.Tilapia"

library (tidyverse)

# common name/scientific name key
mal_names <- read.csv ("Data/Malawi_names.csv") %>%
  rename (comm_name = Common.name,
          species = Scientific.name) %>%
  select (-Notes) %>%
  # replace species names to match fishnutr
  mutate (species = case_when (
    comm_name == "Chambo" ~ "Oreochromis lidole combined",
    comm_name == "Utaka" ~ "Mchenga inornata",
    TRUE ~ species
  ))


# for landings tabs, remove "Total" column and pivot long
mal_ind <- read.csv("Data/Malawi_industrial.csv", header = T) %>%
  select (-Total) %>%
  # pivot_longer
  pivot_longer (-Year, 
                names_to = "comm_name",
                values_to = "tonnes") %>%
  left_join (mal_names, by = "comm_name") %>%
  mutate (sector = "Industrial")

# missing sci names for Mlamba and Kampango

mal_ssf <- read.csv ("Data/Malawi_SSF.csv", skip = 2, header= T) %>%
  select (-c(TOTAL, X))

# cut 2nd table
mal_ssf <- mal_ssf[1:12,] %>%
  pivot_longer (-Year, 
                names_to = "comm_name",
                values_to = "tonnes") %>%
  mutate (tonnes = trimws(tonnes),
          tonnes = as.double(str_replace(tonnes, ",", "")),
          Year = as.integer(Year)) %>%
  replace_na (list (tonnes = 0)) %>%
  left_join (mal_names, by = "comm_name") %>%
  mutate (sector ="Artisanal")


mal_landings <- rbind (mal_ind, mal_ssf)

saveRDS(mal_landings, file= "Data/Malawi_landings_cleaned.Rds")

mal_landings <- readRDS("Data/Malawi_landings_cleaned.Rds")

# Plot time series:
png ("Figures/Malawi_landings_all_spp.png", width = 10, height = 8, res = 300, units = "in")
mal_landings %>%
  filter (!comm_name %in% c("Chisawasawa", "Kambuzi")) %>%
  ggplot (aes (x= Year, y = tonnes/1000,fill = comm_name)) +
  geom_area(position = "stack") +
  facet_wrap (~sector, scales = "free_y", nrow = 2) +
  theme_bw() +
  labs (y = "Catch, 1000 tonnes", fill = "Species", x = "") +
  ggtitle("Malawi fish production by sector") +
  theme (axis.text = element_text (size = 12),
         axis.title = element_text (size = 16),
         plot.title = element_text(size = 20),
         strip.text = element_text (size =16),
         legend.text = element_text (size = 12),
         legend.title = element_text (size = 16)) 
dev.off()

# plot just the main species
# Usipa, Utaka, Chambo. Then Ndunduma (no nutrition data), Mlamba (no scientific name). Then Mbaba (once n = 5)
top_spp <- mal_landings %>% 
  filter (!comm_name %in% c("Chisawasawa", "Kambuzi")) %>%
  group_by (sector,comm_name) %>%
  summarise (tonnes = sum (tonnes, na.rm = TRUE)) %>%
  slice_max (tonnes, n= 3) 

mal_top <- mal_landings %>%
  filter (!comm_name %in% c("Chisawasawa", "Kambuzi")) %>%
  mutate (comm_name = ifelse (comm_name %in% top_spp$comm_name, comm_name, "Others"))

mal_top$comm_name = factor(mal_top$comm_name, levels = c("Chambo", "Ndunduma", "Usipa", "Utaka", "Others"))

# save this as R object
saveRDS(mal_top, file = "Data/malawi_landings_top.Rds")

png ("Figures/Malawi_landings_top_spp.png", width = 10, height = 8, res = 300, units = "in")

mal_top %>%
  ggplot (aes (x= Year, y = tonnes/1000,fill = comm_name)) +
  geom_area(position = "stack") +
  facet_wrap (~sector, scales = "free_y", nrow = 2) +
  theme_bw() +
  labs (y = "Catch, 1000 tonnes", fill = "Species", x = "") +
  ggtitle("Malawi fish production by sector") +
  theme (axis.text = element_text (size = 12),
         axis.title = element_text (size = 16),
         plot.title = element_text(size = 20),
         strip.text = element_text (size =16),
         legend.text = element_text (size = 12),
         legend.title = element_text (size = 16)) 
dev.off()