# Plot sankeys allocative levers
#5 24 23
# JGM

# sankey diagrams ----
# https://r-charts.com/flow/sankey-diagram-ggplot2/

# start with end use, that one is most straightforward I think
c <- sau_2019 %>%
  filter (country == country_name) %>% 
  group_by (species, end_use_type) %>%
  summarise (catch_mt = sum (tonnes, na.rm = TRUE)) %>%
  
  # mutate hack, fix peru anchovy. multiply total domestic anchov production * proportion non dhc from 2018
  mutate (catch_mt = case_when (
    country_name == "Peru" & species == "Engraulis ringens" & end_use_type == "Fishmeal and fish oil" ~ peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,
    country_name == "Peru" & species == "Engraulis ringens" & end_use_type == "Direct human consumption" ~ (1 - peru_anchov_dhc$prop_non_dhc) * peru_anchov_total_2019,
    TRUE ~ catch_mt),
    children_fed = pmap (list (species = species, amount = catch_mt, country_name = country_name), calc_children_fed_func)
  ) %>%
  unnest (cols = c(children_fed)) %>%
  rename (mt = catch_mt)

c$end_use_type <- factor (c$end_use_type, levels = c ("Fishmeal and fish oil", "Other", "Direct human consumption"))

# maybe need to make REALLY long? repeat the nutrient name 30873 times??

# seems easier to do this with network D3
#https://stackoverflow.com/questions/69780665/ggplot-sankey-diagram-of-income-to-expenses-ggsankey
library (networkD3)
library (plotly)

# https://www.marsja.se/create-a-sankey-plot-in-r-ggplot2-plotly/
# think this would be cleaner if we grouped by species
c_group <- c %>%
  filter (!nutrient == "Protein") %>%
  group_by (nutrient, end_use_type) %>%
  summarise (children_fed = sum (children_fed, na.rm = TRUE))

nodes <- data.frame (name = unique (c(as.character(c_group$nutrient),
                                      as.character(c_group$end_use_type)
)))

# links df
links <- data.frame(source = match(c_group$nutrient, nodes$name) - 1,
                    target = match(c_group$end_use_type, nodes$name) - 1,
                    value = c_group$children_fed,
                    stringsAsFactors = FALSE)

# https://community.plotly.com/t/changing-trace-colors-in-sankey/10691
# https://plotly.com/python/reference/sankey/

plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(pad = 15,
              thickness = 20,
              line = list(color = "black", width = 0.5),
              label = nodes$name,
              color = c("blue", "red", "purple", "orange", "brown", "green", "yellow", "gray", "black")),
  link = list(source = links$source,
              target = links$target,
              value = links$value,
              color = c(rep("blue", 3), rep("red", 3), rep("purple", 3), rep("orange", 3), rep("brown",3), rep("green",3), rep("yellow", 3), rep("gray", 3), rep("black", 3))),
  textfont = list(size = 10),
  width = 720,
  height = 480) %>%
  layout(title = "Sankey Diagram: End use type, Peru",
         font = list(size = 14),
         margin = list(t = 40, l = 10, r = 10, b = 10))

# https://stackoverflow.com/questions/68449073/sankey-networkd3-set-link-colours-across-entire-flow
# https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
links$group <- as.factor (c())
links$links.col <- as.factor(gsub(" ", "_", links$links.col))
sankeyNetwork(Links = links, Nodes = nodes, Source = 'IDsource',
              Target = 'IDtarget', Value = 'value', NodeID = 'name', 
              LinkGroup="links.col", NodeGroup="node.col")

# try sierra leone foreign fishing and exports for dicastery----
# assume exports are from domestic
# exports doesn't fully capture

# i'm trying to combine fishing entity and exports in a weird way... maybe try making separate databases and left joining
sl_exports <- sau_2019 %>%
  filter (country == "Sierra Leone", fishing_entity == "Sierra Leone") %>%
  left_join (exports_5yr_mean, by = c("species", "country")) %>%
  replace_na (list(mn_prop_exp = 0)) %>%
  mutate (Exported = tonnes * mn_prop_exp,
          Kept = tonnes * (1-mn_prop_exp)) %>%
  ungroup() %>%
  select (species, Exported, Kept) %>%
  pivot_longer (-species,
                names_to = "trade", 
                values_to = "tonnes") %>%
  mutate (fleet = "Domestic")

sl_trade <- sau_2019 %>%
  filter (country == "Sierra Leone") %>%
  # need different column for foreign/domestic and exported
  mutate (fleet = case_when (fishing_entity == "Sierra Leone" ~ "Domestic",
                             fishing_entity != "Sierra Leone" ~ "Foreign fishing")
  ) %>%
  group_by (country, species, fleet) %>%
  summarise (tonnes2 = sum (tonnes)) %>%
  left_join (sl_exports, by = c ("species", "fleet")) %>%
  mutate (tonnes = ifelse (is.na (tonnes), tonnes2, tonnes),
          children_fed = pmap (list (species = species, amount = tonnes, country_name = "Sierra Leone"), calc_children_fed_func)) %>%
  unnest (cols = c(children_fed))  %>%
  group_by (fleet, trade, nutrient) %>%
  summarise (tot_fed= sum (children_fed, na.rm = TRUE))



sl_trade_group_sm <- sl_trade %>%
  filter (nutrient %in% c("Protein", "Omega_3", "Zinc", "Calcium"))

nodes <- data.frame (name = unique (c(as.character(sl_trade_group_sm$nutrient),
                                      as.character(sl_trade_group_sm$fleet),
                                      as.character(sl_trade_group_sm$trade)
)))

# links df
links <- data.frame(source = match(sl_trade_group_sm$nutrient, nodes$name) - 1,
                    target = match(sl_trade_group_sm$fleet, nodes$name) - 1,
                    value = sl_trade_group_sm$tot_fed,
                    stringsAsFactors = FALSE)

# add second layer
links <- rbind (links, 
                data.frame(source = match(sl_trade_group_sm$fleet, nodes$name) - 1,
                           target = match(sl_trade_group_sm$trade, nodes$name) - 1,
                           value = sl_trade_group_sm$tot_fed,
                           stringsAsFactors = FALSE)
)

# https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html

# repeat each nutrient 3 times, then the 4 nutrients in a row 3 times
links$group <- as.factor (c(rep(c("Protein", "Omega_3", "Calcium", "Zinc"), each = 3), rep(c("Protein", "Omega_3", "Calcium", "Zinc"), 3)))
nodes$group <- as.factor(c("group"))

my_color <- 'd3.scaleOrdinal() .domain(["Protein", "Omega_3", "Calcium", "Zinc" "group"]) .range(["green", "orange", "steelblue", "red", "grey"])'

p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                   Value = "value", NodeID = "name", 
                   colourScale=my_color, LinkGroup="group", NodeGroup="group")

p


# https://stackoverflow.com/questions/46616321/modify-networkd3-sankey-plot-with-user-defined-colors

plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(pad = 15,
              thickness = 20,
              line = list(color = "black", width = 0.5),
              label = nodes$name),
  link = list(source = links$source,
              target = links$target,
              value = links$value),
  textfont = list(size = 10),
  width = 720,
  height = 480
) %>%
  layout(title = "Sierra Leone Nutrient Flows",
         font = list(size = 14),
         margin = list(t = 40, l = 10, r = 10, b = 10))


## ggalluvial??
#https://corybrunson.github.io/ggalluvial/

library (ggalluvial)

# following sl methods from above to join

indo_sau_exports <- sau_2019 %>%
  filter (country == "Indonesia", fishing_entity == "Indonesia") %>%
  left_join (exports_5yr_mean, by = c("species", "country")) %>%
  replace_na (list(mn_prop_exp = 0)) %>%
  mutate (Exported = tonnes * mn_prop_exp,
          Kept = tonnes * (1-mn_prop_exp)) %>%
  ungroup() %>%
  select (species, Exported, Kept) %>%
  pivot_longer (-species,
                names_to = "trade", 
                values_to = "tonnes") %>%
  mutate (fleet = "Domestic")

indo_trade <- sau_2019 %>%
  filter (country == "Indonesia") %>%
  # need different column for foreign/domestic and exported
  mutate (fleet = case_when (fishing_entity == "Indonesia" ~ "Domestic",
                             fishing_entity != "Indonesia" ~ "Foreign fishing")
  ) %>%
  group_by (country, species, fleet) %>%
  summarise (tonnes2 = sum (tonnes)) %>%
  left_join (indo_sau_exports, by = c ("species", "fleet")) %>%
  mutate (tonnes = ifelse (is.na (tonnes), tonnes2, tonnes),
          rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Indonesia"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents))  %>%
  group_by (fleet, trade, nutrient) %>%
  summarise (tot_fed= sum (rni_equivalents, na.rm = TRUE)) 


# this one keeps NAs, so maybe just say foreign fishing is exported?
indo_trade <- indo_trade %>%
  mutate (trade = case_when (fleet == "Foreign fishing" ~ "Exported", TRUE ~ trade))

indo_trade$trade <- factor (indo_trade$trade, levels = c("Kept", "Exported"))

indo_trade %>%
  filter (!nutrient %in% c("Protein", "Selenium")) %>%
  ggplot (aes (axis1 = nutrient, 
               axis2 = fleet,
               axis3 = trade,
               y = tot_fed/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "fleet", "trade"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_alluvium(aes(fill = nutrient)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("title",
          "subtitle")



# try peru
# instead have middle axis be domestic, exported, foreign. assume all exports are fishmeal
peru_exports <- sau_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru") %>%
  left_join (exports_5yr_mean, by = c("species", "country")) %>%
  replace_na (list(mn_prop_exp = 0)) %>%
  mutate (Exports = tonnes * mn_prop_exp,
          Domestic_catch = tonnes * (1-mn_prop_exp)) %>%
  ungroup() %>%
  select (species, Exports, Domestic_catch) %>%
  pivot_longer (-species,
                names_to = "Allocation", 
                values_to = "tonnes") 

peru_allocation <- sau_2019 %>%
  # first take foreign fishing
  filter (country == "Peru", fishing_entity != "Peru") %>%
  mutate (Allocation = "Foreign_catch") %>%
  group_by (species, Allocation) %>%
  summarise (tonnes = sum (tonnes)) %>%
  # just rbind to domestic/exported
  rbind (peru_exports)

# new df for end use
# assume exports are in same proportion of end_use_type, even though we don't know
# assume all exports are from domestic
# make separate foreign and domestic

# have to hack

# mutate hack, fix peru anchovy. multiply total domestic anchov production * proportion non dhc from 2018


peru_end_use_dom <- sau_2019 %>%
  filter (country == "Peru", fishing_entity == "Peru") %>%
  group_by (country, species, end_use_type) %>%
  summarise (tonnes = sum (tonnes)) %>%
  mutate (tonnes = case_when (
    species == "Engraulis ringens" & end_use_type == "Fishmeal and fish oil" ~ peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019,
    species == "Engraulis ringens" & end_use_type == "Direct human consumption" ~ (1 - peru_anchov_dhc$prop_non_dhc) * peru_anchov_total_2019,
    TRUE ~ tonnes)) %>%
  left_join (exports_5yr_mean, by = c("species", "country")) %>%
  replace_na (list(mn_prop_exp = 0)) %>%
  mutate (Domestic_catch = tonnes * (1-mn_prop_exp),
          Exports = tonnes * mn_prop_exp, .keep = "unused") %>%
  pivot_longer (cols = c("Domestic_catch", "Exports"),
                names_to = "Allocation",
                values_to = "tonnes")

peru_end_use_foreign <- sau_2019 %>%
  filter (country == "Peru", fishing_entity != "Peru") %>%
  mutate (Allocation = "Foreign_catch") %>%
  group_by (country, species, end_use_type, Allocation) %>%
  summarise (tonnes = sum (tonnes))%>%
  mutate (tonnes = case_when (
    species == "Engraulis ringens" & end_use_type == "Fishmeal and fish oil" ~ peru_anchov_dhc$prop_non_dhc * peru_anchov_total_2019_foreign,
    species == "Engraulis ringens" & end_use_type == "Direct human consumption" ~ (1 - peru_anchov_dhc$prop_non_dhc) * peru_anchov_total_2019_foreign,
    TRUE ~ tonnes)) 

peru_end_use <- rbind (peru_end_use_dom, peru_end_use_foreign) %>%
  mutate(rni_equivalents = pmap (list (species = species, amount = tonnes, country_name = "Peru"), calc_children_fed_func)) %>%
  unnest (cols = c(rni_equivalents))  %>%
  group_by (end_use_type, Allocation, nutrient) %>%
  summarise (tot_fed= sum (rni_equivalents, na.rm = TRUE))

indo_trade$trade <- factor (indo_trade$trade, levels = c("Kept", "Exported"))

png ("Figures/Sankey_peru.png", width = 10, height = 6, units = "in", res = 300)
peru_end_use %>%
  filter (!nutrient %in% c("Protein", "Selenium"), !end_use_type == "Other") %>%
  ggplot (aes (axis1 = nutrient, 
               axis2 = Allocation,
               axis3 = end_use_type,
               y = tot_fed/1000000)) +
  scale_x_discrete (limits = c ("nutrient", "Allocation", "end_use_type"), expand = c(.2, .05)) +
  labs(y = "RNI equivalents, millions", x = "Allocation levers") +
  geom_alluvium(aes(fill = nutrient)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Nutrient flows, Peru") +
  theme (axis.text = element_text (size = 14),
         axis.title = element_text (size = 16),
         legend.position = "none")

dev.off()

table (peru_end_use$end_use_type, peru_end_use$tot_fed)