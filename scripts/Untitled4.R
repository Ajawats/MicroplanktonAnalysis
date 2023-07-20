library(tidyverse)

load("data/volbio_all_no0.Rdata")
load("data/ct_bmn_fin.Rdata")
# ct_bmn_fin does not have the replicates
volbio_all_nosite = volbio_all_no0 %>% 
  subset(exp!= "site")
volbio_all_nosite$grp_typ <- (paste(volbio_all_nosite$Group, volbio_all_nosite$type))

## Remove the columns I don't want and reorder the ones I keep
##  And then arrange the data frame alphabetically by grp_typ

names(volbio_all_nosite)
volbio_all_nosite <- select(volbio_all_nosite, samp_ev, exp, rep, grp_typ, 
                            a, la, wi, tot_ct=counts) %>% 
  arrange(grp_typ)

### Get rid of the IC entries
volbio_all_nosite = volbio_all_nosite %>%
  subset(exp!="IC")

### Get rid of all 




