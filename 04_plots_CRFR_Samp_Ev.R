##############################################################################
###### INGESTION RATE AND CLEARANCE RATE INDIVIDUAL SAMPLING EVENT PLOTS ######
##############################################################################

library(tidyverse)
library(writexl)
library(ggforce)
source("scripts/01_function_wimGraph and Palettes.R")
source("Wims Text Code.R")
load("data/Clearance Rates/Feeding Rates/FrGrps.Rdata")
load("data/Clearance Rates/CrGrps.Rdata")

labels <- labelFunction
Chl.w <- labelFunction()$Chlw 
cp.day <- labelFunctionA()$cpdday
temp.r <- labelFunction()$temp

##  Make separate plots for each of the sampling events
## Ingestion Rate units are µg C per copepod per day, or cells per copepod per day

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

### ___________________ INGESTION RATES, BIOMASS ____________________###
########################################################################

### LSZ2
FrGrpsLSZ2 <- FrGrps %>% 
  filter(event =="LSZ2")
Break      <- 0.1
rescale    <- 20
ylabels    <- c(0, 0.05, 0.1, 0.3, 1.3)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsLSZ2$ySquish <- scaleBreak(FrGrpsLSZ2$FRmnBul, 
                                        Break=Break, rescale=20)
a <- ggplot(data=FrGrpsLSZ2, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnBul>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+ 
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxa Groups") +
  ggtitle("LSZ2 Mean Ingestion Rates, Biomass")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("\U03BCg C "~copepod^-1~d^-1)+ ## y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### SJR1
FrGrpsSJR1 <- FrGrps %>% 
  filter(event =="SJR1")
Break      <- 0.2
rescale    <- 25
ylabels    <- c(-0.08,0, 0.05, 0.1, 1.2)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsSJR1$ySquish <- scaleBreak(FrGrpsSJR1$FRmnBul, 
                                 Break=Break, rescale=25)
a <- ggplot(data=FrGrpsSJR1, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnBul>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("SJR1 Mean Ingestion Rates, Biomass")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### WLD2
FrGrpsWLD2 <- FrGrps %>% 
  filter(event =="WLD2")
### To plot without ySquish
a <- ggplot(data=FrGrpsWLD2, aes(group_size, FRmnBul)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnBul>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  xlab("Taxa Groups") +
  ggtitle("WLD2 Mean Ingestion Rates, Biomass ")+
  scale_y_continuous()+#limits = c(-8, 46), breaks = c(-7, 0, 5, 20, 45))+
  ylab("\U03BCgC "~copepod^-1~d^-1)+ #y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### SJR2
FrGrpsSJR2 <- FrGrps %>% 
  filter(event =="SJR2")
### To plot without ySquish
a <- ggplot(data=FrGrpsSJR2, aes(group_size, FRmnBul)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnBul>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  xlab("Taxa Groups") +
  ggtitle("SJR2 Mean Ingestion Rates, Biomass ")+
  scale_y_continuous()+#limits = c(-8, 46), breaks = c(-7, 0, 5, 20, 45))+
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP1
FrGrpsYBP1 <- FrGrps %>% 
  filter(event =="YBP1")
Break      <- 0.01
rescale    <- 25
ylabels    <- c(0, .003, 0.008, 0.04, 0.05)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsYBP1$ySquish <- scaleBreak(FrGrpsYBP1$FRmnBul, 
                                 Break=Break, rescale=25)
a <- ggplot(data=FrGrpsYBP1, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnBul>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("YBP1 Mean Ingestion Rates, Biomass")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### To plot without ySquish
a <- ggplot(data=FrGrpsYBP1, aes(group_size, FRmnBul)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnBul>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  xlab("Taxa Groups") +
  ggtitle("YBP1 Mean Ingestion Rates, Biomass ")+
  scale_y_continuous()+#limits = c(-8, 46), breaks = c(-7, 0, 5, 20, 45))+
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP2
FrGrpsYBP2 <- FrGrps %>% 
  filter(event =="YBP2")
Break      <- 0.6
rescale    <- 25
ylabels    <- c(-0.3, 0, 0.2, 0.5, 3.8)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsYBP2$ySquish <- scaleBreak(FrGrpsYBP2$FRmnBul, 
                                 Break=Break, rescale=25)
a <- ggplot(data=FrGrpsYBP2, aes(group_size, ySquish)) +
  geom_point(aes(color = FRmnBul>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(shape=16, color="navy", size = 4) +
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("YBP2 Mean Ingestion Rates, Biomass")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y lable for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### To plot without ySquish
a <- ggplot(data=FrGrpsYBP2, aes(group_size, FRmnBul)) +
  geom_point(shape=16, color="navy", size = 4) +
  xlab("Taxa Groups") +
  ggtitle("YBP2 Mean Ingestion Rates, Biomass ")+
  scale_y_continuous()+#limits = c(-8, 46), breaks = c(-7, 0, 5, 20, 45))+
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### ___________________ INGESTION RATES, CELLS ____________________###
########################################################################

### See what the data looks like by plotting first without ySquish

a <- ggplot(data=FrGrpsYBP1, aes(group_size, FRmnCpm)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  xlab("Taxa Groups") +
  ggtitle("YBP1 Mean Ingestion Rates, Cells ")+
  scale_y_continuous()+#limits = c(-8, 46), breaks = c(-7, 0, 5, 20, 45))+
  #ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### LSZ2
FrGrpsLSZ2 <- FrGrps %>% 
  filter(event =="LSZ2")
Break      <- 150
rescale    <- 100
ylabels    <- c(0,10, 56, 100, 150, 3120, 4890, 6832)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsLSZ2$ySquish <- scaleBreak(FrGrpsLSZ2$FRmnCpm, 
                                 Break=Break, rescale=100)
a <- ggplot(data=FrGrpsLSZ2, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+ 
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxa Groups") +
  ggtitle("LSZ2 Mean Ingestion Rates, Cells")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  #ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### SJR1
FrGrpsSJR1 <- FrGrps %>% 
  filter(event =="SJR1")
Break      <- 600
rescale    <- 25
ylabels    <- c(-180, -78,0, 175, 300, 537, 600, 1900, 2720)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsSJR1$ySquish <- scaleBreak(FrGrpsSJR1$FRmnCpm, 
                                 Break=Break, rescale=25)
a <- ggplot(data=FrGrpsSJR1, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("SJR1 Mean Ingestion Rates, Cells")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### WLD2
FrGrpsWLD2 <- FrGrps %>% 
  filter(event =="WLD2")
Break      <- -300
rescale    <- 1.5
ylabels    <- c(-360,-300,  -42, 0, 20, 45, 65, 192)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsWLD2$ySquish <- scaleBreak(FrGrpsWLD2$FRmnCpm, 
                                 Break=Break, rescale=1.5)
a <- ggplot(data=FrGrpsWLD2, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("WLD2 Mean Ingestion Rates, Cells")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### SJR2
FrGrpsSJR2 <- FrGrps %>% 
  filter(event =="SJR2")
Break      <- 750
rescale    <- 25
ylabels    <- c(0,45, 150, 422, 708, 750, 1690)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsSJR2$ySquish <- scaleBreak(FrGrpsSJR2$FRmnCpm, 
                                 Break=Break, rescale=25)
a <- ggplot(data=FrGrpsSJR2, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("SJR2 Mean Ingestion Rates, Cells")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  #ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP1
FrGrpsYBP1 <- FrGrps %>% 
  filter(event =="YBP1")
Break      <- 200
rescale    <- 300
ylabels    <- c(0, 10, 25,50, 180,200, 5100)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsYBP1$ySquish <- scaleBreak(FrGrpsYBP1$FRmnCpm, 
                                 Break=Break, rescale=300)
a <- ggplot(data=FrGrpsYBP1, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+ # less than or equal to zero is red
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("YBP1 Mean Ingestion Rates, Cells")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  #ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP2
FrGrpsYBP2 <- FrGrps %>% 
  filter(event =="YBP2")
Break      <- 1900
rescale    <- 200
ylabels    <- c(-562, 0,300, 500, 1500, 1900, 14312)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsYBP2$ySquish <- scaleBreak(FrGrpsYBP2$FRmnCpm, 
                                 Break=Break, rescale=200)
a <- ggplot(data=FrGrpsYBP2, aes(group_size, ySquish)) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(shape=16, color="navy", size = 4) +
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("YBP2 Mean Ingestion Rates, Cells")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  #ylab("\U03BCgC "~copepod^-1~d^-1)+ # y label for biomass plots
  ylab("Cells"~copepod^-1~d^-1)+ # y label for cells plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

##############################################################################
### ________________FOR CLEARANCE RATES, mL per copepod per day ______________
##############################################################################

### Use CrGrps.Rdata, from 03_calcs_CrFr_bySizeSmLg.R
load("data/Clearance Rates/CrGrps.Rdata")
load("data/Clearance Rates/CrGrpsReps.Rdata")
##  Make separate plots for each of the sampling events
##  Clearance Rates units are ml per copepod per day

### ______________FOR PLOTTING THE INDIVIDUAL SAMPLING EVENTS _____________
## Use the code below as a template, changing the file name, and the breaks,
## rescale and ylabels according to the numbers in the sampling event

### LSZ2
CrGrpsLSZ2 <- CrGrps %>% 
  filter(event =="LSZ2")
CrGrpsRepsLSZ2 <- CrGrpsReps %>% 
  filter(event =="LSZ2")
a <- ggplot(data=CrGrpsLSZ2, aes(group_size, crMnCpm)) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("LSZ2 Clearance Rates")+
  scale_y_continuous(limits = c(-3, 54), breaks = c(-3, 0, 5, 10, 14, 30, 40, 53)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

###SJR1
CrGrpsSJR1 <- CrGrps %>% 
  filter(event =="SJR1")
### To plot without ySquish
a <- ggplot(data=CrGrpsSJR1, aes(group_size, crMnCpm)) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("SJR1 Clearance Rates")+
  scale_y_continuous(limits = c(-31, 50), breaks = c(-30, -5, 0, 10, 27, 43, 49)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

###SJR2
CrGrpsSJR2 <- CrGrps %>% 
  filter(event =="SJR2")
### To plot without ySquish
a <- ggplot(data=CrGrpsSJR2, aes(group_size, crMnCpm)) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("SJR2 Clearance Rates")+
  scale_y_continuous(limits = c(-8, 40), breaks = c(-7, 0, 10, 18, 25, 30, 39)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### WLD2
CrGrpsWLD2 <- CrGrps %>% 
  filter(event =="WLD2")
### To plot without ySquish
a <- ggplot(data=CrGrpsWLD2, aes(group_size, crMnCpm)) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("WLD2 Clearance Rates")+
  scale_y_continuous(limits = c(-14, 46), breaks = c(-13, -3, 0, 5, 17, 30, 45)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP1
CrGrpsYBP1 <- CrGrps %>% 
  filter(event =="YBP1")
### To plot without ySquish
a <- ggplot(data=CrGrpsYBP1, aes(group_size, crMnCpm)) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("YBP1 Clearance Rates")+
  scale_y_continuous(limits = c(-1, 43), breaks = c(0, 5, 9, 15, 20, 30, 42)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP2
CrGrpsYBP2 <- CrGrps %>% 
  filter(event =="YBP2")
a <- ggplot(data=CrGrpsYBP2, aes(group_size, crMnCpm)) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("YBP2 Clearance Rates")+
  scale_y_continuous(limits = c(-33, 63), breaks = c(-32, 0, 5, 27, 38, 50, 62)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### EXTRA CODE, SAVED IN CASE I NEED SOME OF THE THINGS I DECIDED NOT TO USE ###
### To plot without ySquish
a <- ggplot(data=FrGrpsLSZ2, aes(group_size, FrBpm_ug)) +
  geom_point(shape=16, color="navy", size = 4) +
  #geom_point(stat = "identity",  color = "blue4", size = 4)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxa Groups") +
  ggtitle("LSZ2 Biomass Mean Ingestion Rates")+
  scale_y_continuous()+#limits = c(-8, 46), breaks = c(-7, 0, 5, 20, 45))+
  #name = expression(paste("Cells copepod "["-1""]")))+
  #name = expression(paste("Hydrogen Sulfide Concentration (", mu, "M H" ["2"], "S)"))) +
  ylab("\U03BCgC "~copepod^-1~d^-1)+
  #ylab(bquote('Biomass, '*mu~'g C'~ copepod^-1~d^-1*''))+
  #ylab(bquote('Biomass, \u00b5gC L'^-1))+
  #ylab(bquote('Cells L'^-1))+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
#ylab("Cells copepod \U207B\U00B9 d\U207B\U00B9")
a
