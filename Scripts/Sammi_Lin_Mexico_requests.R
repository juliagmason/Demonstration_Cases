# 8/10/22 mexico request from sammi lin


sammi_spp <- c("Thunnus thynnus", "Diplodus vulgaris", "Galeorhinus galeus", "Argyrosomus regius", "Dicentrarchus labrax", "Sphoeroides annulatus", "Mugil cephalus", "Scomberomorus sierra", "Sardina pilchardus")


sau_mex <- read.csv("Data/SAU_mexico_landings.csv")

sau_mex %>% filter (scientific_name == "Megapitaria squalida") %>% View()
