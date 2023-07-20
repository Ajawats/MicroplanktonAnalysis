####################################################################
########## LOOK FOR DIATOM VOLUME LARGER THAN 3000 UM^3 ############
####################################################################

### Experiment with just 100x

load("data/volbio100_no0.Rdata")

### Add the extra columns and calculations that volbio_all has

volbio_100_no0_2 <- rename(volbio100_no0, tot_vol_um3 = volume, 
                           tot_biomass_pgC = biomass) %>% 
  mutate(bio_per_vol = tot_biomass_pgC/(pres_fact*vol_set_ml)) %>% 
  rename(bio_per_vol_pgc_ml = bio_per_vol) %>% 
  mutate(bio_per_vol_ugl = bio_per_vol_pgc_ml/1000) %>% 
  mutate(counts_per_ml = counts/(pres_fact*vol_set_ml))

### Extract only the diatoms
diatoms <- volbio_100_no0_2[str_detect(volbio_100_no0_2$Group, "diatom"),] #%>% 
#[str_detect(volbio_all$type, "centric"),] 
### Extract only the centric diatoms
diatoms_cen <- diatoms[str_detect(diatoms$type, "centric"),]

#diatom_size <- diatoms [str_detect(diatoms$type, "centric"),] 
#select()
names(diatoms)
diatoms_cen_size <- diatoms_cen[c(5, 6, 15)]

#diatoms_big <- subset(diatoms_size, "vol_per_org_um3">3000)

### Experiment with 400x

load("data/volbio400_no0.Rdata")

### Add the extra columns and calculations that volbio_all has

volbio_400_no0_2 <- rename(volbio400_no0, tot_vol_um3 = volume, 
                           tot_biomass_pgC = biomass) %>% 
  mutate(bio_per_vol = tot_biomass_pgC/(pres_fact*vol_set_ml)) %>% 
  rename(bio_per_vol_pgc_ml = bio_per_vol) %>% 
  mutate(bio_per_vol_ugl = bio_per_vol_pgc_ml/1000) %>% 
  mutate(counts_per_ml = counts/(pres_fact*vol_set_ml))

### Extract only the diatoms
diatoms4 <- volbio_400_no0_2[str_detect(volbio_400_no0_2$Group, "diatom"),] #%>% 
#[str_detect(volbio_all$type, "centric"),] 
### Extract only the centric diatoms
diatoms_cen4 <- diatoms4[str_detect(diatoms$type, "centric"),]

#diatom_size <- diatoms [str_detect(diatoms$type, "centric"),] 
#select()
names(diatoms)
diatoms_cen_size <- diatoms_cen[c(5, 6, 15)]

