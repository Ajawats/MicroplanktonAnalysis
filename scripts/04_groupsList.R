############################################################################
################################ GROUPS LISTS ##############################
############################################################################
### 1/26/23

### This script is for getting a list of each Group and its distinct 
##  sizes

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")

pennate <- volbio_all %>% 
  filter(grepl('pennate', type)) %>% 
  select(Group, type, shp, sa, la, wi)
duplicated(pennate)
pennate <- pennate[!duplicated(pennate), ]
write_xlsx(pennate, "data/Main Groups/pennate.xlsx")

centric_diatom <- volbio_all %>% 
  filter(grepl('centric', type)) %>% #filter(Group == "diatom", type == "centric")%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(centric_diatom)
centric_diatom <- centric_diatom[!duplicated(centric_diatom), ]
write_xlsx(centric_diatom, "data/Main Groups/centric_diatom.xlsx")

ciliate <- volbio_all %>% 
  filter(Group == "ciliate")%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(ciliate)
ciliate <- ciliate[!duplicated(ciliate), ] 
write_xlsx(ciliate, "data/Main Groups/ciliate.xlsx")

cyano <- volbio_all %>% 
  filter(Group == "cyanobacteria")%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(cyano)
cyano <- cyano[!duplicated(cyano), ] 
write_xlsx(cyano, "data/Main Groups/cyano.xlsx")

chloro <- volbio_all %>% 
  filter(Group == "chlorophyte")%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(chloro)
chloro <- chloro[!duplicated(chloro), ]
write_xlsx(chloro, "data/Main Groups/chloro.xlsx")

flag <- volbio_all %>% 
  filter(Group %in% c("flagellate", "dinoflagellate"))%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(flag)
flag <- flag[!duplicated(flag), ]
write_xlsx(flag, "data/Main Groups/flag.xlsx")


ochro <- volbio_all %>% 
  filter(Group == "ochrophyte")%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(ochro)
ochro <- ochro[!duplicated(ochro), ]
write_xlsx(ochro, "data/Main Groups/ochro.xlsx")

tintin <- volbio_all %>% 
  filter(Group == "tintinnid")%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(tintin)
tintin <- tintin[!duplicated(tintin), ]
write_xlsx(tintin, "data/Main Groups/tintin.xlsx")

unid <- volbio_all %>% 
  filter(Group == "unidentified")%>% 
  select(Group, type, shp, sa, la, wi)
duplicated(unid)
unid <- unid[!duplicated(unid), ]
write_xlsx(unid, "data/Main Groups/unid.xlsx")





