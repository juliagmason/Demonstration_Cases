# Plot sankeys allocative levers
#5 24 23
# JGM


#https://corybrunson.github.io/ggalluvial/

library (tidyverse)
library (ggalluvial)

# going back to just exports for all countries ----
plot_exports_sankey <- function (country_name) {
  # country names are Chile *this has anchov excluded, Chle_anchov, Peru, Peru_anchov, Peru_noanchov, SierraLeone, Indo 
 
  export_nutr <- readRDS(paste0("Data/levers_RNI_pop_export_", country_name, ".Rds"))
  # these were calculated in calculate_national_allocative_levers.R
  
  export_nutr  %>% 
    filter (!nutrient %in% c("Protein")) %>%
    mutate (nutrient = case_when (
      nutrient == "Omega_3" ~ "Omega 3",
      nutrient == "Vitamin_A" ~ "Vit. A",
      TRUE ~ nutrient)) %>%
    ggplot (aes (axis1 = nutrient,
                 axis2 = exports,
                 y = rni_equivalents/1000000,
                 fill = nutrient)) +
    scale_x_discrete (limits = c ("nutrient", "exports"), expand = c(.05, .05)) +
    labs(y = "Child RNI equiv., millions", x = "") +
    geom_flow(aes(fill = nutrient)) +
    geom_stratum(aes(fill = nutrient)) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
    theme_minimal() +
    ggtitle("National allocative drivers") +
    #scale_fill_brewer (palette = "Set1") +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A","#FFFF33", "#984EA3", "#FF7F00"))
    theme ( 
      axis.text.x = element_blank(),
      axis.text.y = element_text (size = 9),
      axis.title = element_text (size = 12),
      plot.title = element_text (size = 13),
      plot.margin=unit(c(1,1,1,1), 'mm'),
      legend.position = "none")
  
  ggsave (paste0("Figures/FigXD_exports_driver_", country_name, ".svg"), width = 74, height = 70, units = "mm")
}

map (c("Chile", "Chile_anchov", "Peru_anchov", "Peru_noanchov", "SierraLeone", "Indo"), plot_exports_sankey)
