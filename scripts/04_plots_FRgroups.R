############################################################################
################# GRAPH FR, GROUPS, SMALL AND LARGE  ######################
############################################################################

### 2/28/23
### Updated 4/12/23, 3/27/23

### Uses an updated, recalculated, streamlined df, from 03_calcs_CR_FR.Rdata


library(tidyverse)
library(writexl)
library(ggforce)
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")
load("data/Clearance Rates/FRcpm_Rep_Mn.Rdata") # biomass, cells, reps and means
load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata") #biomass FR
### with pgC mL^-1 and µgC L^-1, reps and means


###__________________________ PLOT ____________________________________________
##  Best plots are here at the top, other trial plots/code are at the bottom

###_______ PLOTS BY TAXA GROUPS ACROSS ALL SAMPLING EVENTS _________________###

### Plot the overall FRmnCpm and FRmnBpm
load("data/Clearance Rates/Feeding Rates/FrAllOverall.Rdata")
load("data/Clearance Rates/Feeding Rates/FrBpmOverall.Rdata")
# No need to use the Bpm df because we want ug per L, below
load("data/Clearance Rates/Feeding Rates/FrCpmOverall.Rdata")
load("data/Clearance Rates/Feeding Rates/FrAllOverall_ug.Rdata")

### Dot plot Cells
p <- ggplot(FRcpm_Rep_Mn, aes(x=group_size, y=FRmnCpm))+
  geom_point(stat = "identity",  color = "blue4", size = 3)+
  scale_x_discrete ("Taxa Groups with Sizes") +
  #scale_y_continuous("Biomass, \U03BCgC "~copepod^-1~d^-1) +
  #scale_y_continuous("Cells"~d^-1) +
  scale_y_continuous(limits = c(-575, 15000), 
                     breaks = c(-562, 0, 14312)) +
  #ylab("µg C"~copepod^-1~d^-1)+
  ylab("Cells"~d^-1)+
  wimGraph()+
  ggtitle("Ingestion Rates, Cells, 
          All Sampling Events")+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))
  #xlab("Taxa Groups with Sizes") +
  #ylab("Biomass, \U03BCgC "~copepod^-1~d^-1)
  #scale_y_continuous ("Flow, "~m^3~d^-1)
p 

### Dot plot Biomass
p <- ggplot(FRBio_Rep_Mn, aes(x=group_size, y=FRmnUgCcd))+
  geom_point(stat = "identity",  color = "blue4", size = 5)+
  scale_x_discrete ("Taxa Groups") +
  scale_y_continuous("Biomass, \U03BCgC "~copepod^-1~d^-1) +
  #scale_y_continuous("Cells"~d^-1) +
  # scale_y_continuous()+
  wimGraph()+
  ggtitle("Ingestion Rates, Biomass
          All Sampling Events")+
  theme(plot.title = element_text(face = "bold", size = (20)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))
#xlab("Taxa Groups with Sizes") +
#ylab("Biomass, \U03BCgC "~copepod^-1~d^-1)
#scale_y_continuous ("Flow, "~m^3~d^-1)
p

### Add Wim's scale break function:
scaleBreak <- function(xvalue, Break, rescale) {
  # Function to set up plotting an axis with a single scale break
  # This is an actual change of scale, unlike what ggplot has
  # Input:
  #   xvalue    The raw data (for either axis)
  #   Break     The value at the break in raw data units
  #   rescale   The divisor (>1 usually) to reduce the scale
  # Use:
  #   Run this for the data to be plotted
  #   Run it again for the axis tick locations  
  #   Then plot the rescaled data with the rescaled tick locations
  #      and the original tick locations for the tick labels
  
  ifelse(xvalue < Break, xvalue, (xvalue-Break)/rescale + Break)
}

Break      <- 3000
rescale    <- 200
ylabels    <- c(-562, 0, 150, 500, 1000, 1500, 2000, 14312)
brks       <- scaleBreak(ylabels, Break, rescale)

FRcpm_Rep_Mn$ySquish <- scaleBreak(FRcpm_Rep_Mn$FRmnCpm, 
                                   Break=Break, rescale=200)

a <- ggplot(data=FRcpm_Rep_Mn, aes(group_size, ySquish)) +
  geom_point(aes(color = FRmnCpm>0), size = 3)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(shape=16, color="navy", size = 4) +
  ##geom_point(stat = "identity",  color = "blue4", size = 4)+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("Ingestion Rates, Cells
          All Sampling Events")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab(bquote('Ingestion rate, cells mL'^-1))+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

load("data/Clearance Rates/Feeding Rates/FrAllOverall_ug.Rdata")

### Wim's scale break function with the Biomass; 
##  see above for scale break function code

Break      <- 1.5
rescale    <- 20
ylabels    <- c(-.3, 0, .1, .2, .5, 1.20, 3.75)
brks       <- scaleBreak(ylabels, Break, rescale)

FRBio_Rep_Mn$ySquish <- scaleBreak(FRBio_Rep_Mn$FRmnUgCcd, 
                                      Break=Break, rescale=20)

a <- ggplot(data=FRBio_Rep_Mn, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnUgCcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("Ingestion Rates, Biomass
          All Sampling Events")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab(bquote('Ingestion rate, µgC L'^-1))+ 
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))

a

###_____________________ PLOTS BY SAMPLING EVENT __________________________###

### Cells (counts)
p <- ggplot(FrGrps, aes(x=group_size, FRmnCpm))+
  geom_bar(stat = "identity", fill = "darksalmon")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10") +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Ingestion Rate, Cells per copepod per day")+
  wimGraph()
p

### Try biomass on a linear scale in a dot plot, because logscale makes the negative
##  ingestion rates disappear
min(FrGrps$FrBpm_ug)
max(FrGrps$FrBpm_ug)
p <- ggplot(FrGrps, aes(x=group_size, FrBpm_ug))+
  geom_point (stat = "identity", fill = "darkseagreen3")+
  scale_x_discrete ("") +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Ingestion Rate, µgC per copepod per day")+
  wimGraph()
p

### ______________FOR PLOTTING THE INDIVIDUAL SAMPLING EVENTS _____________
## Use the code below as a template, changing the file name, and the breaks,
## rescale and ylabels according to the numbers in the sampling event

### Apply Wim's squish function

scaleBreak <- function(xvalue, Break, rescale) {
  # Function to set up plotting an axis with a single scale break
  # This is an actual change of scale, unlike what ggplot has
  # Input:
  #   xvalue    The raw data (for either axis)
  #   Break     The value at the break in raw data units
  #   rescale   The divisor (>1 usually) to reduce the scale
  # Use:
  #   Run this for the data to be plotted
  #   Run it again for the axis tick locations  
  #   Then plot the rescaled data with the rescaled tick locations
  #      and the original tick locations for the tick labels
  
  ifelse(xvalue < Break, xvalue, (xvalue-Break)/rescale + Break)
}

Break      <- 3
rescale    <- 10
ylabels    <- c(0, 1, 2, 3, 18)
brks       <- scaleBreak(ylabels, Break, rescale)

siteBioUgLTotSJR1$ySquish <- scaleBreak(siteBioUgLTotSJR1$tot_bio_ug, 
                                        Break=Break, rescale=10)

a <- ggplot(data=siteBioUgLTotSJR1, aes(group_size, ySquish)) +
  geom_point(shape=16, color="navy", size = 4) +
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("SJR1 Biomass, Site Water Samples")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab(bquote('Biomass, \u00b5gC L'^-1))+
  #ylab(bquote('Cells L'^-1))+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Plot by taxa group; Used the facet_wrap_paginate to get the SEPARATE EVENT PLOTS
### Biomass
p <- ggplot(FrGrps, aes(x=FRmnBpm, group_size))+
  geom_bar(stat = "identity", fill = "darkseagreen3")+
  scale_y_discrete ("") +
  scale_x_continuous(trans = "log10") + #, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  wimGraph()+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 6))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Ingestion Rate, pgC per copepod per day") +
  ylab("Taxa Groups with Sizes")
  #facet_wrap_paginate(~ event, ncol = 2, nrow =2, page = 3)
p
### ncol is how many columns, or individual plots you want on one page
##  nrow doesn't work here, not sure what it's supposed to do
##  page is what page it would be if you put the six plots together with two columns
##  per page, so page 3 would be the last two sampling events in the list
##  You have to run the code separately for each page number
p

### Put the counts on the x-axis and the taxa on the y-axis
### Cells (counts)
p <- ggplot(FrGrps, aes(x=FRmnCpm, group_size))+
  geom_bar(stat = "identity", fill = "darksalmon")+
  scale_y_discrete ("") +
  scale_x_continuous(trans = "log10") + #, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  wimGraph()+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 6))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Ingestion Rate, Cells per copepod per day") +
  ylab("Taxa Groups with Sizes")
p

### Changed to a dot plot; added code that changed the y-axis labels to regular numbers instead
## of scientific notation, but that is moot now that I changed the biomass units to ug per liter
## instead of picograms per milliliter
## Also added the geom_point color = FrBpm_ug>0 to make the negative and positive values different colors
options(scipen = 999)
p <- ggplot(FrAllOverall_ug, aes(x=group_size, y=FrBpm_ug))+
  geom_point(aes(color = FrBpm_ug>0), size = 5)+
  #geom_point(stat = "identity",  color = "blue4", size = 5)+
  scale_x_discrete ("") +
  scale_y_continuous(labels = scales::label_comma(), breaks = c(-34, -15, 0, 25, 150, 300, 500))+
  #scale_y_cut(breaks = c(200000, 450000), which = c(.1,3), scales = c(0.5, 2)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Taxa Groups with Sizes") +
  ylab("Feeding Rate, pgC per copepod per day")+
  wimGraph()
p
### Example plot code

##  NO p <- 		ggplot(FrGrps, aes(x=group)) +
  #geom_bar(aes(y=FrMnCpm))+
  ##p 
#  scale_x_discrete ("") +
#  scale_y_continuous(breaks = seq(-35, 70, by=10))+
#  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
#        strip.text.x = element_text(size = 10))+
#  facet_wrap(~ event, ncol= 2, scales="free") +
#  xlab("Taxa Groups with Sizes") +
#  ylab("Median Clearance Rate, Experimental Samples") +
#  geom_hline(yintercept = 0, color = "blue")+
#  geom_hline(yintercept = 5, color = "cyan")+
#  wimGraph(textSize = 6)

### Use a log10 scale since the range in counts is very wide
p <- ggplot(FrCpmOverall, aes(x=group_size, y=FrCpmAllEvents))+
  geom_bar(stat = "identity",  fill = "chartreuse3")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Taxa Groups with Sizes") +
  ylab("Feeding Rate, Cells per copepod per day")+
  wimGraph()
p  

### This one worked, but it's not good
p <- 		ggplot(FrGrps, aes(x=group_size, FRmnCpm)) +
  geom_point(size=3, color="coral") +
  scale_x_discrete ("") +
  scale_y_log10(limits=c(-10, 5000), breaks = c(-100, 0, 10, 50, 100,500, 2500))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Feeding Rate, Counts per ml") +
  #geom_hline(yintercept = 0, color = "blue")+
  #geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 6)
p

### This is an exmaple, needs to be edited before using
p <- 		ggplot(x, aes(x=seq, CRmed)) +
  geom_point(size=2, color="coral") +
  scale_x_discrete ("") +
  scale_y_continuous(breaks = seq(-35, 70, by=10))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        strip.text.x = element_text(size = 10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Median Clearance Rate, Experimental Samples") +
  geom_hline(yintercept = 0, color = "blue")+
  geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 6)

p


 ### Plot by taxa group; Used the face_wrap_paginate to get the separate event plots
 ### Biomass
 p <- ggplot(FrGrps, aes(x=FRmnBpm, group_size))+
   geom_bar(stat = "identity", fill = "darkseagreen3")+
   scale_y_discrete ("") +
   scale_x_continuous(trans = "log10") + #, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
   wimGraph()+
   theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
         strip.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 6))+
   facet_wrap(~ event, ncol= 2, scales="free") +
   xlab("Ingestion Rate, pgC per copepod per day") +
   ylab("Taxa Groups with Sizes")+
   facet_wrap_paginate(~ event, ncol = 2, nrow =1, page = 3)
 p
 
 