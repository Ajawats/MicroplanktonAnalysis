### 5/25/23


#################################################
#### TEST TO SEE WHERE NUMBERS IN BASEESD GOT OFF
#################################################

### 5/26/23, Result:
##  Two ciliate oblong entries were identical in esd, but had different
##  dimensions. Since esd is calculated with the volume per cell, which is based on
##  cell shape (in this case, both prolate spherioids) and dimensions, the two
##  dimensions of oblong cilates, 12x23 and 8x72, produced the same esd.

baseEsdFlag_CE <- baseEsdFlag %>%
  filter( str_detect(exp, "C|E"))

###### Create another df from the above, with only the control samples
baseEsdFlag_C <- baseEsdFlag_CE%>% 
  filter(exp == "C") %>% 
  rename(cpmC = cpm)

### Create a df with only experimental samples
baseEsdFlag_E <- baseEsdFlag_CE%>% 
  filter(exp == "E")%>% 
  rename(cpmE = cpm) 

### Create a df with only initial samples (for the ingestion rates)
baseEsdFlag_I <- baseEsdFlag%>% 
  filter(exp == "I")%>% 
  rename(cpmI = cpm) 

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
baseEsdFlag_Cmn <- baseEsdFlag_C %>% 
  group_by(samp_ev, Group, type, esd, exp) %>% 
  summarise(Cmn=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()

####################################
#### Centric Diatoms ####
####################################

baseEsdCendia_CE <- baseEsdCendia %>%
  filter( str_detect(exp, "C|E"))

### Create another df from the above, with only the control samples
baseEsdCendia_C <- baseEsdCendia_CE%>% 
  filter(exp == "C") %>% 
  rename(cpmC = cpm)

### Create a df with only experimental samples
baseEsdCendia_E <- baseEsdCendia_CE%>% 
  filter(exp == "E")%>% 
  rename(cpmE = cpm) 

### Create a df with only initial samples (for the ingestion rates)
baseEsdCendia_I <- baseEsdCendia%>% 
  filter(exp == "I")%>% 
  rename(cpmI = cpm) 

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
baseEsdCendia_Cmn <- baseEsdCendia_C %>% 
  group_by(samp_ev, Group, type, esd, exp) %>% 
  summarise(Cmn=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()


####################################
#### Ciliates ####
####################################

baseEsdCil_CE <- baseEsdCil %>%
  filter( str_detect(exp, "C|E"))

### Create another df from the above, with only the control samples
baseEsdCil_C <- baseEsdCil_CE%>% 
  filter(exp == "C") %>% 
  rename(cpmC = cpm)

### Create a df with only experimental samples
baseEsdCil_E <- baseEsdCil_CE%>% 
  filter(exp == "E")%>% 
  rename(cpmE = cpm) 

### Create a df with only initial samples (for the ingestion rates)
baseEsdCil_I <- baseEsdCil%>% 
  filter(exp == "I")%>% 
  rename(cpmI = cpm) 

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
baseEsdCil_Cmn <- baseEsdCil_C %>% 
  group_by(samp_ev, Group, type, esd) %>% 
  summarise(Cmn=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()

### Result: 
