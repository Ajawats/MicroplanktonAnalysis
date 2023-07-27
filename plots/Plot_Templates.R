############################################################################
########################## PLOT TEMPLATES ##################################
############################################################################

### For clearance rates; ingestion rates; cell abundance, including site water
##  See 04_plots_FRgroups.R and scaleBreak function with code to test.R and
##  Plot test Bpm dots.R (this last one not as useful)

### 3/10/23
library(tidyverse)
library(writexl)
library(ggforce) # needed for facet_wrap_paginate
source("scripts/01_function_wimGraph and Palettes.R")

############ PLOTS WITH FACET WRAP FOR INDIVIDUAL SAMPLING EVENT PLOTS##########

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

### Biomass; Used the facet_wrap_paginate to get the separate event plots
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

###_______ PLOTS BY TAXA GROUPS ACROSS ALL SAMPLING EVENTS _________________###

### Plot the overall FRmnCpm and FRmnBpm
load("data/Clearance Rates/Feeding Rates/FrAllOverall.Rdata")
load("data/Clearance Rates/Feeding Rates/FrBpmOverall.Rdata")
load("data/Clearance Rates/Feeding Rates/FrCpmOverall.Rdata")
load("data/Clearance Rates 2/CrIrCntMnTots2.Rdata")

### BAR PLOT
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

### Try it with a dot plot
p <- ggplot(FrCpmOverall, aes(x=group_size, y=FrCpmAllEvents))+
  geom_point(stat = "identity",  color = "blue4", size = 5)+
  scale_x_discrete ("") +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Taxa Groups with Sizes") +
  ylab("Feeding Rate, Cells per copepod per day")+
  wimGraph()
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

Break      <- 1500
rescale    <- 40
ylabels    <- c( 0, 500, 1000, 4000)
brks       <- scaleBreak(ylabels, Break, rescale)

FrCpmOverall$ySquish <- scaleBreak(FrCpmOverall$FrCpmAllEvents, Break=Break, rescale=20)

a <- ggplot(data=FrCpmOverall, aes(group_size, ySquish)) +
  geom_point(shape=16, color="navy", size = 4) +
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("Ingestion Rates Across All Sampling Events")+
  scale_y_continuous("Ingestion rate, cells ml ^-1", breaks=brks, labels=ylabels) +
  theme(plot.title = element_text(face = "bold", size = (25)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        legend.position = "none")+
  wimGraph()
a

### Wim's scale break function with the Biomass; see above for scale break function code

Break      <- 60
rescale    <- 20
ylabels    <- c(-34, 0, 10, 20, 55, 150,500)
brks       <- scaleBreak(ylabels, Break, rescale)

FrAllOverall_ug$ySquish <- scaleBreak(FrAllOverall_ug$FrBpm_ug, Break=Break, rescale=20)

a <- ggplot(data=FrAllOverall_ug, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FrBpm_ug>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("Ingestion Rates Across All Sampling Events")+
  scale_y_continuous("Ingestion rate, µgC L^-1", breaks=brks, labels=ylabels) +
  theme(plot.title = element_text(face = "bold", size = (18)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        legend.position = "none")+
  wimGraph()
a


