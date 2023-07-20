####################################################################
################## TEN MOST COMMON IN IC ###########################
####################################################################

### 11/17/22
### Make a stacked bar chart of the 10 most common taxa in the IC samples
##  per sampling event of the means of the taxa to compare the same major taxa

load("data/volbio_all.Rdata")
load("data/count_sum_rep_fin_grpesd.Rdata")
# above file created in 03_Biomass...2022_11_09

ic_taxa <- filter(count_sum_rep_fin_grpesd, exp=="IC")
ic_taxa_mn <- ic_taxa %>%
  group_by(samp_ev, exp, grp_esd, )%>%
  summarise_at(vars(tot_ct_ml), list(mn_ct_ml = mean))%>%
  subset(mn_ct_ml!=0) %>%
  arrange(desc(mn_ct_ml))
ic_taxa_mn_10 <- head(ic_taxa_mn, 10)

flagic <- ic_taxa_mn %>% 
  filter(grp_esd == "flagellate cryptomonas 4.20")


### Total of all taxa in all samp_ev
ic_taxa_mn_all_ampev <- ic_taxa %>%
  group_by(grp_esd, samp_ev)%>%
  summarise_at(vars(tot_ct_ml), list(mn_ct_ml = mean))%>%
  subset(mn_ct_ml!=0) %>%
  arrange(desc(mn_ct_ml))
ic_taxa_mn_10 <- head(ic_taxa_mn, 10)

### Total of all taxa mn counts
ic_taxa_mn_all <- ic_taxa %>%
  group_by(grp_esd)%>%
  summarise_at(vars(tot_ct_ml), list(mn_ct_ml = mean))%>%
  subset(mn_ct_ml!=0) %>%
  arrange(desc(mn_ct_ml))
ic_taxa_mn_10 <- head(ic_taxa_mn_all, 10)



  #theme(axis.text.x = element_text(), 
        #axis.text.y = element_text(size=10),
        #title = element_text(size=10))
 

