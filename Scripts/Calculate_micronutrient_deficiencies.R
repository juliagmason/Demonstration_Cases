# Micronutrient deficiency data
# 3/17/2022
# JGM

# start with nutricast data, but should use Beal eventually

# Golden uses nutrient supply, population size, and EARs to calculate the proportion of the population who are deficient. 
# this tells us what proportion of the population is deficient, but not HOW deficient they are
# no data for sierra leone, but does have Guinea
# can do calcium, vitamin A, iron, zinc, same as Hicks
golden_mic_def <- readRDS("../nutrient_endowment/output/nutr_deficiencies_by_cntry_sex_age_2011.Rds") 


mic_pdef_golden  <-  golden_mic_def %>%
  # no data for sierra leone. use guinea
  filter (country %in% c("Sierra Leone", "Indonesia", "Peru", "Chile", "Malawi", "Guinea"),
          # no children in this dataset. 5-9 cutpoint too similar to our 4-8, just ignore?
          age != "5-9",
          nutrient %in% c("Calcium", "Iron", "Zinc", "Vitamin A")) %>% 
  group_by (country, sex, nutrient) %>%
  summarise (ndef = sum(ndeficient),
             nhealthy = sum(nhealthy),
             pdef = ndef/(ndef + nhealthy)
             ) %>%
  arrange (country, sex, desc(pdef))
  

# look at beal data, from 2017 PlosONE SI, in crfs maps project. use data from 2011
# not separated into age groups, this has been included? should I use fortification? maybe?
# can do calcium, zinc, iron, vitamin A, same as Hicks
beal_mic_def <- read_csv ("Data/Beal_2017_SI_inadequate_micronutrients.csv")

mic_def_rda_beal <- beal_mic_def %>%
  filter (Year == 2011, Country %in% c("Sierra Leone", "Indonesia", "Peru", "Chile", "Malawi"), 
          Micronutrient %in% c("Calcium", "Iron", "Zinc", "Vitamin A")) %>%
  select (-c(Zone, ISO3, Year, PCDEA, MDI, Tagname)) %>%
  arrange (Country, desc(`Prevalence of Inadequate Intake`))

# compare? ----

mic_pdef_golden_by_sex <- mic_pdef_golden %>%
  mutate(pdef = round (pdef * 100, 2)) %>%
  pivot_wider (-c(ndef, nhealthy),
               names_from = sex,
               values_from = pdef, 
               names_glue = "Golden_{.value}_{sex}") %>%
  mutate (country = ifelse (country == "Guinea", "Sierra Leone", country))
  
mic_def_compare_beal_golden <- mic_def_rda_beal %>%
  rename (pdef = `Prevalence of Inadequate Intake`) %>%
  select (Country, Fortification, Micronutrient, pdef) %>%
  pivot_wider (names_from = Fortification, 
               values_from = pdef) %>%
  rename (country = Country,
          nutrient = Micronutrient,
          Beal_pdef_fortification = `1`,
          Beal_pdef_nofortification = `0`) %>%
  left_join (mic_pdef_golden_by_sex, by = c("country", "nutrient")) %>%
  arrange (country, desc (Beal_pdef_fortification))
  

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


write.excel (mic_def_compare_beal_golden)

write.csv (mic_def_compare_beal_golden, file = "Data/mic_def_compare_beal_golden.csv", row.names = FALSE)

# Chile: Calcium and vitamin A. 
# Indo: calcium and vitamin A, maybe zinc with golden
# Malawi: calcium, maybe vitamin A
# Peru: calcium, zinc
#Sierra leone: calcium, viatmin A, iron, zinc