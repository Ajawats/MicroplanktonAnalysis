############################################################################
#### ORGANISMS GROUPED BY SIZE, SAMP_EV AND EXP WITH MEAN COUNTS  ##########
############################################################################
### Created on 11/9/22

library(tidyverse)

### Use volbio_all_no0.Rdata from ) 03_calcs_volbio_100400.R script
load("data/volbio_all.Rdata")

### Remove site and IC from exp column
sizes = subset(volbio_all, exp!="site") %>% 
                 subset(exp!= "IC")
### Remove the columns I don't need
names(sizes)
sizes = select(sizes, samp_ev, exp, rep, Group, type, sa, la, wi, counts)

### See what the minimum and maximum measurements are in sa and la
max(sizes$la)
min(sizes$la)
max(sizes$sa)
min(sizes$sa)
### This shows which row number had the minimum
which.min(sizes$sa)
### Below, I was just curious which organism was really only 1um wide. It was in LSZ2 FC2,
##  400x, a pennate diatom
# which.min(volbio_all_no0$sa)

### Create the grp_typ categories like before so I can graph the organism
## groups by size, since it's not useful to compare organisms by just one
## of the dimensions because some are round and some are cylindrical.

sizes$grp_typ <- (paste(sizes$Group, sizes$type))
sizes$size <- (paste(sizes$sa, sizes$la))
sizes$grp_sz <-(paste(sizes$grp_sz, sizes$size))
### Remove the columns Group and type since I just combined them
sizes = select(sizes, samp_ev, exp, rep, grp_sz, counts)
 unique(sizes$grp_typ)
 
### Calculate the mean counts per exp, adding up all the reps
 