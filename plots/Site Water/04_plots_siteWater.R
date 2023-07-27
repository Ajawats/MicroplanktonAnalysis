##############################################################################
##################### PLOTS SITE WATER SAMPLES ABUNDANCE #####################
##############################################################################

### 3/10/23

library(tidyverse)
library(writexl)
library(ggforce) ## for facet_wrap_paginate
source("scripts/01_function_wimGraph and Palettes.R")

load("data/SiteWater/siteBioUgLTot.Rdata")
load("data/SiteWater/siteCntTot.Rdata")

### above df is from 03_calcs_SiteWaterAbund.R

### DOT PLOT of biomass

p <- ggplot(siteBioUgLTot, aes(x=group_size, tot_bio_ug))+
  geom_point(stat = "identity",  color = "black", size = 3)+
  scale_x_discrete ("") +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab(bquote('Biomass \u00b5gC L'^-1))+
  ggtitle("Biomass Abundance, Site Water Samples")+
  #facet_wrap_paginate(~ samp_ev, ncol = 1, nrow =1, page = 5)+ ## Tried to
  # use paginate, but it's not helpful, since I still need to change the limits
  # and breaks per sampling event
  wimGraph()
p

### DOT PLOT of counts
### Plotting all together on one plot using color = samp_ev and geom_point, position =
##  jitter was not helpful

p <- ggplot(siteCntTot, aes(x=group_size, tot_ct))+
  geom_point(stat = "identity", color = "black", size = 1.5,)+
  scale_x_discrete ("") +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  facet_wrap_paginate(~ samp_ev, ncol = 1, nrow =1, page = 1)+
  xlab("Taxa Groups with Sizes") +
  ylab(bquote('Cells L'^-1))+
  ggtitle("Cell Abundance, Site Water Samples")+
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

## If I want to make the y-axis title bold, so this, but the exponent isn't bold:
##  ylab(bquote(bold('Cells L'^-1)))+
## And if I want to make the x-axis title bold, add this to the theme argument:
##  axis.title = element_text(size=14, face="bold")

### Pasted the above plot code for using without ySquish
a <- ggplot(data=siteBioUgLTotSJR1, aes(group_size, tot_bio_ug)) +
  geom_point(shape=16, color="navy", size = 3) +
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("SJR1 Biomass, Site Water Samples")+
  scale_y_continuous() +
  #ylab(bquote('Cells L'^-1))+
  ylab(bquote('Biomass \u00b5gC L'^-1))+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        legend.position = "none")
a

### Note at end of day 3/20--re do the breaks or scales, something didn't work

### BAR PLOT ###
##  First need to make a df that sums the cpm and biomass for 
##  group_sizes per sampling events
options(scipen = 999)
p <- ggplot(siteCntTot, aes(x=group_size, tot_ct))+
  geom_bar(stat = "identity", fill = "navy")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10") +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Cells per ml")+
  ggtitle("Cell Abundance, Site Water Samples")+
  wimGraph()
p

### Biomass in ugC per L
options(scipen = 999)
p <- ggplot(siteBugLTot, aes(x=group_size, tot_bio_ug))+
  geom_bar(stat = "identity", fill = "navy")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10") +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Biomass, ugC per L")+
  ggtitle("Biomass Abundance, Site Water Samples")+
  wimGraph()
p



