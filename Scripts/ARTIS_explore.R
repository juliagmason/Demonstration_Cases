# ARTIS explore
# 8/15/22

# https://github.com/Seafood-Globalization-Lab/exploreARTIS

library (sf)
#devtools::install_github("Seafood-Globalization-Lab/exploreARTIS", dependencies = TRUE)
library(exploreARTIS)
library (tidyverse)

plot_bar(artis, bar_group = "exporter_iso3c")

colnames (artis)
