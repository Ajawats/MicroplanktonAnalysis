####################################################################################
######### REPS WITH MEANS INGESTION INDIVIDUAL SAMPLING EVENT PLOTS  #########
#####################################################################################

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")

### from 03_calcs_CrFr_bySizeSmLg.R
load("data/Clearance Rates/Feeding Rates/FrGrps.Rdata")
load("data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCells.Rdata")
load( "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBio.Rdata")

### Cells

### LSZ2
FrGrpsLSZ2 <- FrGrps %>% 
  filter(event =="LSZ2")
FrGrpsRepsCellsLSZ2 <- FrGrpsRepsCells %>% 
  filter(event =="LSZ2")
write_xlsx(FrGrpsLSZ2, "data/FrGrpsLSZ2.xlsx")

a <- ggplot(data=FrGrpsLSZ2, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FrGrpsRepsCellsLSZ2,
             aes(y=FR),
             color="orange", size=2)+
  #geom_jitter(data = FrGrpsRepsCellsLSZ2,
  #          aes(y=FR),
  #         width = .01,
  #        color="orange", size=2)+
  #geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  #geom_linerange(data = FrGrpsRepsCellsLSZ2,
  # aes(y=FR, ymin=Cmn, ymax=cpmE),
  #color="orange", size=3)+
  xlab("Taxon Group") +
  ggtitle("LSZ2 Ingestion Rates, Cells")+
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
FrGrpsYBP1 <- FrGrps %>% 
  filter(event =="YBP1")
FrGrpsRepsCellsYBP1 <- FrGrpsRepsCells %>% 
  filter(event =="YBP1")
### Needs Scale Break, see below for Scale Break code, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
### To plot without Scale Break
a <- ggplot(data=FrGrpsYBP1, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FrGrpsRepsCellsYBP1,
             aes(y=FR),
             color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("YBP1 Ingestion Rates, Cells")+
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
FrGrpsYBP2 <- FrGrps %>% 
  filter(event =="YBP2")
FrGrpsRepsCellsYBP2 <- FrGrpsRepsCells %>% 
  filter(event =="YBP2")
FrGrps_noCenSmYBP2 <- FrGrps %>% 
  filter(event =="YBP2") %>% 
  subset(group_size != "CenDiaSm")
FrGrpsRepsCells_noCenSmYBP2 <- FrGrpsRepsCells %>% 
  filter(event =="YBP2") %>% 
  subset(group_size != "CenDiaSm")

a <- ggplot(data=FrGrps_noCenSmYBP2, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FrGrpsRepsCells_noCenSmYBP2,
             aes(y=FR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("YBP2 Ingestion Rates, Cells (no CenDiaSm)")+
  scale_y_continuous(limits = c(-1400, 3050), 
                     breaks = c(-1345,-500, 0,  260, 500, 800, 1100, 2500, 3013)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (12)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### WLD2
FrGrpsWLD2 <- FrGrps %>% 
  filter(event =="WLD2")
FrGrpsRepsCellsWLD2 <- FrGrpsRepsCells %>% 
  filter(event =="WLD2")

a <- ggplot(data=FrGrpsWLD2, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FrGrpsRepsCellsWLD2,
             aes(y=FR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("WLD2 Ingestion Rates, Cells")+
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
FrGrpsSJR1 <- FrGrps %>% 
  filter(event =="SJR1")
FrGrpsRepsCellsSJR1 <- FrGrpsRepsCells %>% 
  filter(event =="SJR1")

a <- ggplot(data=FrGrpsSJR1, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FrGrpsRepsCellsSJR1,
             aes(y=FR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("SJR1 Ingestion Rates, Cells")+
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
FrGrpsSJR2 <- FrGrps %>% 
  filter(event =="SJR2")
FrGrpsRepsCellsSJR2 <- FrGrpsRepsCells %>% 
  filter(event =="SJR2")

a <- ggplot(data=FrGrpsSJR2, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FrGrpsRepsCellsSJR2,
             aes(y=FR), color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("SJR2 Ingestion Rates, Cells")+
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

#FrGrpsYBP1
#FrGrpsRepsYBP1

Break      <- -30
rescale    <- 25
ylabels    <- c(-36, 0, 5, 9, 15, 20, 30, 42, 71)
brks       <- scaleBreak(ylabels, Break, rescale)
FrGrpsRepsYBP1$ySquish <- scaleBreak(FrGrpsRepsYBP1$CR, 
                                     Break=Break, rescale=25)
a <- ggplot(data=FrGrpsYBP1, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FrGrpsRepsYBP1, aes(group_size, ySquish),
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