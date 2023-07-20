############################################################################
################################ CR PLOTS ##################################
############################################################################
### 1/24/23
### Updated 1/25/23

library(tidyverse)
library(writexl)
load("data/Clearance Rates/crAllCR.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

### Plot which taxa have CR > 0

###_________ Prepare the data for preliminary look__________________________
## Per notes from discussion with Wim 1/20/23:

## Take median CR of the 3 E reps
## Have one panel per sampling event
## Get rid of CR near 0, or NA, but keep the negative values

### Use the split apply combine method as for the control means

### Use crAllCR.Rdata as the base, as it already has only the E samples
## and the CR

### Apply the median function to the CR across the three replicates
crMedE1 <- crAllCR %>% 
  group_by(event, sample, group) %>%
  summarize(CRmed = median(CR)) 

### Remove all taxa with NA in the CRmed
crMedE_noNA <- drop_na(crMedE1)
write_xlsx(crMedE_noNA, "data/Clearance Rates/crMedE_noNA.xlsx")
### Get it ready to plot

### Try this code to make abbreviations in the x axis labels, from the file
##  03_calcs_Taxa_Grp_Exploration.R
x <-crMedE_noNA
head(x) # shows first six rows
x$seq <- factor(1:nrow(x), ordered=T, labels=abbreviate(x$group, minlength = 12)) #makes a
# column with the group names abbreviated so they fit better on the plot, I chose minlenght =12
## so that enough info would be in the abbreviations sos that I would know what they were.

### Plot it with the plot code (with variations) from the R data file above
p <- ggplot(x, aes(x=seq, CRmed)) +
  geom_point(size=2, color="coral") +
  scale_x_discrete ("") +
  scale_y_continuous(breaks = seq(-35, 70, by = 10))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, 
                                   vjust = 0.8, size = 6),
        strip.text.x = element_text(size = 10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Median Clearance Rate, Experimental Samples") +
  geom_hline(yintercept = 0, color = "blue")+
  geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 6)
  
p

###_____________ Below this point is code from coding experiments

### Split up the facet-wrapped plots into six separate plots
p.list = lapply(sort(unique(x$event)), function(i) {
  ggplot(x, aes(x=seq, CRmed)) +
    geom_point(size=2, color="coral") +
    scale_x_discrete ("") +
    scale_y_continuous(breaks = seq(-35, 70, by = 10))+
    theme(axis.text.x = element_text(angle = 60, hjust = 0.8, 
                                     vjust = 0.8, size = 6),
          strip.text.x = element_text(size = 10))+
    facet_wrap(~ event, ncol= 2, scales="free") +
    xlab("Taxa Groups with Sizes") +
    ylab("Median Clearance Rate, Experimental Samples") +
    geom_hline(yintercept = 0, color = "blue")+
    geom_hline(yintercept = 5, color = "cyan")+
    wimGraph(textSize = 6)
})
 p.list
 
### Try face_wrap_paginate:
install.packages("ggforce")
library(ggforce)

p <- ggplot(x, aes(x=seq, CRmed)) +
  geom_point(size=2, color="coral") +
  scale_x_discrete ("") +
  scale_y_continuous(breaks = seq(-35, 70, by = 10))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, 
                                   vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 10))+
  facet_wrap_paginate(~ event, 
                      nrow = 1,
                      ncol= 1, 
                      scales="free",
                      page = 6) +
  xlab("Taxa Groups with Sizes") +
  ylab("Median Clearance Rate, Experimental Samples") +
  geom_hline(yintercept = 0, color = "blue")+
  geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 8)

p
required_n_pages <- n_pages(p)


### Make a df like crAllCR with the df that includes the group, type, sa, la, wi instead of grp_sz
##  so that I can make an excel sheet that lists the taxa by name and size
## Take median CR of the 3 E reps

### Use the split apply combine method as for the control means
load("data/Clearance Rates/crAll1CR.Rdata")
### Apply the median function to the CR across the three replicates
crAll1CR_test <- crAll1CR %>% 
  group_by(event, sample, group, type, sa, la, wi) %>%
  summarize(CRmed = median(CR)) 

### Remove all taxa with NA in the CRmed
crAll1CR_testE_noNA <- crAll1CR_test %>% 
  drop_na(CRmed)

#write_xlsx(crMedE_noNA, "data/Clearance Rates/crMedE_noNA.xlsx")
#

###_________________________Other experimental code_______________
### Remove the exp colu

### Make a new df with just the unique group names
crMedE_noNA_grp <- data.frame(crMedE_noNA$group)
### Remove the duplicates
duplicated(crMedE_noNA$group)
crMedE_noNA_noDup <- crMedE_noNA[!duplicated(crMedE_noNA_grp), ]
### Make a data frame with just the list of the unique groups
crMedE_noNA_noDup_grp <- data.frame(crMedE_noNA_noDup$group)
write_xlsx(crMedE_noNA_noDup_grp, "data/Clearance Rates/crMedE_noNA_noDup_grp.xlsx")

### Plot it without the abbreviations, and with flipped x and y axes
p <- ggplot(crMedE_noNA, aes(group, CRmed)) +
  geom_point(size=2, color="blue") +
  scale_x_discrete ("") +
  scale_y_continuous(breaks = seq(-35, 70, by = 10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Median Clearance Rate, Experimental Samples") +
  theme(axis.text.x = element_text(angle=45, hjust = 0.5, 
                                   vjust = 0.2, size = 6))+
  wimGraph(textSize = 10)+
  coord_flip()
p


## Plot each sampling event separately and don't make the abbreviations so
##  the taxa can be seen better

crMedELSZ2 <- crMedE_noNA %>%
  subset(event == "LSZ2")

p <- ggplot(crMedELSZ2, aes(group, CRmed)) +
  geom_point(size=4, color="blue") +
  scale_x_discrete ("") +
  scale_y_continuous(breaks = seq(-35, 70, by = 10))+
  xlab("Taxa Groups with Sizes") +
  ylab("Median Clearance Rate, Experimental Samples") +
  theme(axis.text.x = element_text(angle=90, hjust = 0.8, 
                                   vjust = 0.8, size = 10))+
  wimGraph(textSize = 10)
p

### From meeting with Wim 11/22/22, graph taxa_pic_2 this way (this worked
##  really well):
#x <- taxa_pic_2
#head(x) # shows first six rows
#x <- select(x, group=grp_typ, totalCPM = total_cts_per_ml) #change the names
#x$seq <- factor(1:nrow(x), ordered=T, labels=abbreviate(x$group)) #makes a
# column with the grp_typ names abbreviated so they fit better on the plot
#write_xlsx(x, "data/taxaPic2abbrev.xlsx") 

#p <- ggplot(x, aes(x=seq, totalCPM)) +
#  geom_point(size=1, color="blue") +
#  scale_x_discrete ("") +
#  scale_y_log10("Total Counts per Ml, all samples") +
#  wimGraph()+
#  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
#                                   vjust = 0.2, size = 8))

ggplot(data = crMedE1, )


