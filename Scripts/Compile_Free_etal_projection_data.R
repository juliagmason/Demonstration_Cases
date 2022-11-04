# Collate nutricast species data
# 9/15/22
# JGM

library (tidyverse)

# got mexico and ghana data from chris, put it all together

#previous pull
ds_spp <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia.Rds")

mex_26 <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia_take2_rcp26.Rds")
mex_45 <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia_take2_rcp45.Rds")
mex_60 <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia_take2_rcp60.Rds")
mex_85 <- readRDS("Data/Free_etal_2020_country_level_outcomes_time_series_for_julia_take2_rcp85.Rds")

ds_spp_full <- rbind (ds_spp, mex_26, mex_45, mex_60, mex_85)
saveRDS (ds_spp_full, file = "Data/Free_etal_proj_full_Chl_Per_Ind_SL_Mex_Gha.Rds")

# smaller file to make life easier
ds_spp_sm <- ds_spp_full %>%
  filter (catch_mt > 0, scenario %in% c("No Adaptation", "Productivity Only", "Full Adaptation"))

saveRDS(ds_spp_sm, file = "Data/Free_etal_proj_smaller.Rds")
