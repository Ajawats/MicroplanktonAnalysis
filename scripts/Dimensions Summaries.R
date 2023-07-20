### 6/22/22
### Dimensions Summaries for each organism
### See file, 100x_Corrected_06_17.Rmd for the source file, 
####  volbio_all_no0

library(tidyverse)
library(openxlsx)
getwd()

load("data/volbio_all_no0")
names(volbio_all_no0)
unique(volbio_all_no0$Group)


### Note that in this code, the ^ symbol means "everything that starts with" 
## the characters that follow it. Also, Luka recommended using the filter statement
## instead of manually indexing. He said: t’s more about writing in the same 
## style, and since you were using dplyr functions for the rest of the 
##  pipeline, it made sense to use the filter statement there too. 
##  Since the dplyr functions are nice and self explanatory too, it makes it 
##  easier when you go back later and quickly see that you filtered the 
##  data right there instead of having to work through the indexing logic. 
##  Another reason is that if you have to change the name of the dataframe, 
##  with filter you only need to change it in one spot, but with indexing 
##  you’ll have to change it in two places.

## Chlorophytes

chlor_dim_table <- filter(volbio_all_no0, grepl("^chloro", Group)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(tot_biomass_pgC)) %>% 
  ungroup()

write_xlsx( chlor_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Chlor_Dim.xlsx")

## Ciliate
cil_dim_table <- filter(volbio_all_no0, grepl("^Ciliate", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
#write_xlsx(cil_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Cil_Dim.xlsx")

## Cyanobacteria
cyano_dim_table <- filter(volbio_all_no0, grepl("^Cyano", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
write_xlsx(cyano_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Cyano_Dim.xlsx")

##Diatoms
dia_dim_table <- filter(volbio_all_no0, grepl("^Diatom", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
write_xlsx(dia_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Dia_Dim.xlsx")

### Need to separate all the types: centric, chain, egg-shaped, 
## other, pennate

diacen_dim_table <- filter(dia_dim_table, grepl("centric", type)) %>% 
  write_xlsx("/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Diacen_Dim.xlsx")  
diachain_dim_table <- filter(dia_dim_table, grepl("chain", type))%>% 
  write_xlsx("/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Diachain_Dim.xlsx")
diaegg_dim_table <- filter(dia_dim_table, grepl("egg shaped", type))%>% 
  write_xlsx("/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Diaegg_Dim.xlsx")
diaother_dim_table <- filter(dia_dim_table, grepl("other", type))%>% 
  write_xlsx("/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Diaother_Dim.xlsx")
diapen_dim_table <- filter(dia_dim_table, grepl("pennate", type))%>% 
  write_xlsx("/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Diapen_Dim.xlsx")

## Flagellates
flag_dim_table <- filter(volbio_all_no0, grepl("^Flagellate", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
write_xlsx(flag_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Flag_Dim.xlsx")

## Dinoflagellates
dino_dim_table <- filter(volbio_all_no0, grepl("^Dinoflagellate", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
write_xlsx(dino_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Dino_Dim.xlsx")

## Ochrophyte
ochro_dim_table <- filter(volbio_all_no0, grepl("^Ochrophyte", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
write_xlsx(ochro_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Ochro_Dim.xlsx")

## Tintinnids
tin_dim_table <- filter(volbio_all_no0, grepl("^Tintinnid", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
write_xlsx(tin_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Tin_Dim.xlsx")

## Unidentified
unid_dim_table <- filter(volbio_all_no0, grepl("^Unidentified", Organism)) %>% 
  group_by(Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) %>% 
  ungroup()
write_xlsx(unid_dim_table, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/Size Classes/Unid_Dim.xlsx")
