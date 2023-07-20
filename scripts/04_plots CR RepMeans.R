####################################################################################
######### REPS WITH MEANS CLEARANCE RATE INDIVIDUAL SAMPLING EVENT PLOTS  #########
#####################################################################################

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")

### from 03_calcs_CrFr_bySizeSmLg.R
load("data/Clearance Rates/CrGrps.Rdata")
load("data/Clearance Rates/CrGrpsReps.Rdata")

### LSZ2
CrGrpsLSZ2 <- CrGrps %>% 
  filter(event =="LSZ2")
CrGrpsRepsLSZ2 <- CrGrpsReps %>% 
  filter(event =="LSZ2")
write_xlsx(CrGrpsLSZ2, "data/CrGrpsLSZ2.xlsx")

a <- ggplot(data=CrGrpsLSZ2, aes(group_size, crMnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = CrGrpsRepsLSZ2,
                     aes(y=CR),
                   color="orange", size=2)+
#geom_jitter(data = CrGrpsRepsLSZ2,
   #          aes(y=CR),
    #         width = .01,
     #        color="orange", size=2)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  #geom_linerange(data = CrGrpsRepsLSZ2,
                # aes(y=CR, ymin=Cmn, ymax=cpmE),
                 #color="orange", size=3)+
  xlab("Taxon Group") +
  ggtitle("LSZ2 Clearance Rates")+
  scale_y_continuous(limits = c(-11, 66), 
                     breaks = c(-10,-3, 0, 5, 10, 14, 30, 40, 53, 64)) +
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
CrGrpsRepsYBP1 <- CrGrpsReps %>% 
  filter(event =="YBP1")
### Needs Scale Break, see below for Scale Break code, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
### To plot without Scale Break
a <- ggplot(data=CrGrpsYBP1, aes(group_size, crMnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = CrGrpsRepsYBP1,
             aes(y=CR),
             color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("YBP1 Clearance Rates")+
  scale_y_continuous(limits = c(-37, 72), 
                     breaks = c(-36, 0, 5, 9, 15, 20, 30, 42, 71)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a


### YBP2
CrGrpsYBP2 <- CrGrps %>% 
  filter(event =="YBP2")
CrGrpsRepsYBP2 <- CrGrpsReps %>% 
  filter(event =="YBP2")

a <- ggplot(data=CrGrpsYBP2, aes(group_size, crMnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = CrGrpsRepsYBP2,
             aes(y=CR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("YBP2 Clearance Rates")+
  scale_y_continuous(limits = c(-38, 94), 
                     breaks = c(-37, -13, 0, 5, 20, 27, 38, 50, 80, 93)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### WLD2
CrGrpsWLD2 <- CrGrps %>% 
  filter(event =="WLD2")
CrGrpsRepsWLD2 <- CrGrpsReps %>% 
  filter(event =="WLD2")

a <- ggplot(data=CrGrpsWLD2, aes(group_size, crMnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = CrGrpsRepsWLD2,
             aes(y=CR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("WLD2 Clearance Rates")+
  scale_y_continuous(limits = c(-32, 56), 
                     breaks = c(-31, -15, 0, 5, 20, 30, 40, 55)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a


### SJR1
CrGrpsSJR1 <- CrGrps %>% 
  filter(event =="SJR1")
CrGrpsRepsSJR1 <- CrGrpsReps %>% 
  filter(event =="SJR1")

a <- ggplot(data=CrGrpsSJR1, aes(group_size, crMnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = CrGrpsRepsSJR1,
             aes(y=CR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("SJR1 Clearance Rates")+
  scale_y_continuous(limits = c(-39, 62), 
                     breaks = c(-38, -15, 0, 5, 10, 20, 30, 40, 50, 61)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a


### SJR2
CrGrpsSJR2 <- CrGrps %>% 
  filter(event =="SJR2")
CrGrpsRepsSJR2 <- CrGrpsReps %>% 
  filter(event =="SJR2")

a <- ggplot(data=CrGrpsSJR2, aes(group_size, crMnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = CrGrpsRepsSJR2,
             aes(y=CR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("SJR2 Clearance Rates")+
  scale_y_continuous(limits = c(-11, 59), 
                     breaks = c(-10, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 58)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a



### YBP1 Tried to plot with Scale Break, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
###   Saved the code below just in case I need it

#CrGrpsYBP1
#CrGrpsRepsYBP1

Break      <- -30
rescale    <- 25
ylabels    <- c(-36, 0, 5, 9, 15, 20, 30, 42, 71)
brks       <- scaleBreak(ylabels, Break, rescale)
CrGrpsRepsYBP1$ySquish <- scaleBreak(CrGrpsRepsYBP1$CR, 
                                     Break=Break, rescale=25)
a <- ggplot(data=CrGrpsYBP1, aes(group_size, crMnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = crMnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = CrGrpsRepsYBP1, aes(group_size, ySquish),
             color="orange", size=2)+
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