############################################################################
########### CLEARANCE RATE SINGLE SAMP_EV TEST WITH SIZE GROUPS ############
############################################################################
### 1/30/23

### This script is for calcuating clearance rates of one sampling event 
##  using the new size category column of small or large; then later I
## will add biomass calculations to it, and then calculate the feeding rate

### Use crAllCE.Rdata since it already has all the data ready and I just
##  need to take out one sampling event

sjr1_crTest <- crAll1CR %>% 
  filter(grepl('SJR1', event)) %>% 
  select(event, group, cpm, Cmn, size)

sjr1Cr <- rowwise(sjr1_crTest) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
sjr1Cr_na <- sjr1Cr %>% drop_na(CR)
