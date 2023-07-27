########################################################
############## FLAGELLATE DATA FOR MICHELLE ##############
########################################################


load("data/Clearance Rates/volbio_all_cr.Rdata")
load("data/volbio_all.Rdata")

### All the flagellates
flagellates <- volbio_all_cr %>% 
  filter(Group == "flagellate")
flagellates_raw <- select(flagellates, samp_date, samp_ev, exp, rep, 
                      Group, type, shp, sa, la, wi, bio_pgC_ml, 
                      bio_ugC_l, cpm, esd, group_size)
save(flagellates_raw, file = "data/flagellates_raw.Rdata")
write_xlsx(flagellates_raw, "data/flagellates_raw.xlsx")

### Abundance by group_size
load("data/Abundance/abundance.Rdata")

load
