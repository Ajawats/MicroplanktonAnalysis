##"Biomass_Selections  6/20/2022"


### This file is for finding out ways to select the data by "type" so I can look at the sizes and choose what size classes to create to group the organsim types by.

###  Chlorophytes
# Note that in this code, the ^ symbol means "everything that starts with" 
#    the characters that follow it.
# Note: %>% distinct(sa, la, .keep_all = TRUE), 
#   this line keeps the first row where those dimensions occur and eliminates all the others. So I can see all the distinct dimensions, but not how many of them there are, or the total biomass for all those rows.

names(vol_single_no0)
unique(vol_single_no0$Group)

bio_chlor <- vol_single_no0[grepl("^Chloro", volbio_all_no0$Organism), ] 
bio_chlor_dim <- vol_single_no0[grepl("^Chloro", volbio_all_no0$Organism), ]  %>% distinct(sa, la, wi, .keep_all = TRUE) 


# Tried to figure out a way for R to add the biomass of the organisms 
#  of the same dimension, and add a column saying how many entries there were for those dimensions, but haven't figured it out.

#biomass_dim <- function()
#
write_xlsx( bio_chlor, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_Chlor.xlsx")
str(bio_chlor)
write_xlsx( bio_chlor_dim, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_Chlor_Dim.xlsx")

##  Cilates

bio_cil <- vol_single_no0[grepl("^Ciliate", volbio_all_no0$Organism), ]

bio_cil_dim <- vol_single_no0[grepl("^Ciliate", volbio_all_no0$Organism), ] %>% 
  distinct(sa, la, .keep_all = TRUE)

write_xlsx(bio_cil,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_Cil.xlsx")
write_xlsx(bio_cil_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_Cil_Dim.xlsx")

##  Cyanobacteria

bio_cyano <- vol_single_no0[grepl("^Cyanobacteria", volbio_all_no0$Organism), ]

bio_cyano_dim <- vol_single_no0[grepl("^Cyanobacteria", volbio_all_no0$Organism), ] %>%
  distinct(sa, la, .keep_all = TRUE)

write_xlsx(bio_cil,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_Cyano.xlsx")
write_xlsx(bio_cil_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_Cyano_Dim.xlsx")

### Diatom

bio_diatom <- vol_single_no0[grepl("^Diatom", volbio_all_no0$Organism), ]

bio_diatom_dim <- vol_single_no0[grepl("^Diatom", volbio_all_no0$Organism), ] %>% 
  distinct(sa, la, wi, .keep_all = TRUE)

write_xlsx(bio_diatom,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_diatom.xlsx")
write_xlsx(bio_diatom_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_diatom_dim.xlsx")

### Flagellate

bio_flag <- vol_single_no0[grepl("^Flagellate", volbio_all_no0$Organism), ]

bio_flag_dim <- vol_single_no0[grepl("^Flagellate", volbio_all_no0$Organism), ] %>% 
  distinct(sa, la, .keep_all = TRUE)

write_xlsx(bio_flag,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_flag.xlsx")
write_xlsx(bio_flag_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_flag_dim.xlsx")

### Dinoflagellate

bio_dino <- vol_single_no0[grepl("^Dinoflagellate", volbio_all_no0$Organism), ]

bio_dino_dim <- vol_single_no0[grepl("^Dinoflagellate", volbio_all_no0$Organism), ] %>% 
  distinct(sa, la, .keep_all = TRUE)

write_xlsx(bio_flag,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_dino.xlsx")
write_xlsx(bio_flag_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_dino_dim.xlsx")


### Tintinnid

bio_tin <- vol_single_no0[grepl("^Tintinnid", volbio_all_no0$Organism), ]

bio_tin_dim <- vol_single_no0[grepl("^Tintinnid", volbio_all_no0$Organism), ] %>% 
  distinct(sa, la, .keep_all = TRUE)

write_xlsx(bio_tin,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_tin.xlsx")
write_xlsx(bio_tin_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_tin_dim.xlsx")

### Ochrophyte

bio_ochro <- vol_single_no0[grepl("^Ochrophyte", volbio_all_no0$Organism), ]

bio_ochro_dim <- vol_single_no0[grepl("^Ochrophyte", volbio_all_no0$Organism), ] %>%
  distinct(sa, la, .keep_all = TRUE)

write_xlsx(bio_ochro,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_ochro.xlsx")
write_xlsx(bio_ochro_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_ochro_dim.xlsx")

### Unidentified

bio_unid <- vol_single_no0[grepl("^Unidentified", volbio_all_no0$Organism), ]

bio_unid_dim <- vol_single_no0[grepl("^Unidentified", volbio_all_no0$Organism), ] %>% 
  distinct(sa, la, .keep_all = TRUE)

write_xlsx(bio_unid,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_unid.xlsx")
write_xlsx(bio_unid_dim,"/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/Bio_unid_dim.xlsx")