####################################################################################
######### REPS WITH MEANS INGESTION RATE INDIVIDUAL SAMPLING EVENT PLOTS  #########
#####################################################################################

### Updated 4/12/23 with new code 03_calcs_CR_FR.R, with new sum calculations,
## because I though the original ones were wrong, but as it turns out, they were not.
### Updated again 4/13/23 with new code again 03_calcs_CR_FR.R, because the
##  µg conversions were wrong

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")

###From 03_calcs_CR_FR.R
load("data/Clearance Rates/Feeding Rates/FRCells_Rep_Mn.Rdata") # Ingestion rate, cells
load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata") # Ingestion rate, biomass


#################### BIOMASS, INDIVIDUAL EVENT PLOTS ####################
#########################################################################
### LSZ2
## Remove Centric Diatoms from plot since they are so high that the others
## can't be seen on the plot
FRBio_Rep_MnLSZ2 <- FRBio_Rep_Mn %>% 
  filter(event =="LSZ2") #%>% 
  #filter(group_size != "CenDiaLg" & group_size !="CenDiaSm")

a <- ggplot(data=FRBio_Rep_MnLSZ2, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRBio_Rep_MnLSZ2,
             aes(y=FRUgCcd),
             color="orange", size=2)+
  xlab("Taxon Group, -centric diatoms") +
  ggtitle("LSZ2 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(-.05, .09), 
                     breaks = c(0.3, 0, .01, .06, .07, .08)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Make a df and plot with just the Centric Diatoms
FRBio_Rep_MnLSZ2_CenDia <- FRBio_Rep_Mn %>% 
  filter(event =="LSZ2") %>% 
  filter(group_size == "CenDiaLg" | group_size =="CenDiaSm")

a <- ggplot(data=FRBio_Rep_MnLSZ2_CenDia, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRBio_Rep_MnLSZ2_CenDia,
             aes(y=FRUgCcd),
             color="orange", size=2)+
  xlab("Taxon Group, centric diatoms") +
  ggtitle("LSZ2 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(.25, 1.3), 
                     breaks = c(.3, .4, 1.2, 1.05, 1.48)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### SJR1
## Remove CenDiaLg
FRBio_Rep_MnSJR1_noCDL <- FRBio_Rep_Mn %>% 
  filter(event =="SJR1") %>% 
  filter(group_size != "CenDiaLg")

a <- ggplot(data=FRBio_Rep_MnSJR1_noCDL, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRBio_Rep_MnSJR1_noCDL,
             aes(y=FRUgCcd),
             color="orange", size=2)+
  xlab("Taxon Group, no centric diatoms") +
  ggtitle("SJR1 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(-.09, .15), 
                     breaks = c (-.08, 0, .01, .02, .14))+
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Make a df and plot with just the Centric Diatoms
FRBio_Rep_MnSJR1_CenDiaLg <- FRBio_Rep_Mn %>% 
  filter(event =="SJR1") %>% 
  filter(group_size == "CenDiaLg")

a <- ggplot(data=FRBio_Rep_MnSJR1_CenDiaLg, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRBio_Rep_MnSJR1_CenDiaLg,
             aes(y=FRUgCcd),
             color="orange", size=2)+
  xlab("Taxon Group, large centric diatoms") +
  ggtitle("SJR1 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(0, 2), 
                     breaks = c(0, 1.08, 1.19)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### SJR2
FRBio_Rep_MnSJR2 <- FRBio_Rep_Mn %>% 
  filter(event =="SJR2")

a <- ggplot(data=FRBio_Rep_MnSJR2, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRBio_Rep_MnSJR2,
             aes(y=FRUgCcd),
             color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("SJR2 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(-.006, .07), 
                     breaks = c(-.007,  0, .01, .02, .06)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### WLD2
FRBio_Rep_MnWLD2 <- FRBio_Rep_Mn %>% 
  filter(event =="WLD2")

a <- ggplot(data=FRBio_Rep_MnWLD2, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRBio_Rep_MnWLD2,
             aes(y=FRUgCcd),
             color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("WLD2 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(-.009, .09), 
                     breaks = c(-.008, -.005, 0, .01, .06, .07, .08)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP2
## Remove Centric Diatoms and large ciliates from plot since they are so high 
## that the others can't be seen on the plot
FRBio_Rep_MnYBP2_NoCenDiaCilLg <- FRBio_Rep_Mn %>% 
  filter(event =="YBP2") %>% 
  filter(group_size != "CenDiaLg" & group_size !="CenDiaSm") %>% 
  filter(group_size != "CilLg")

a <- ggplot(data=FRBio_Rep_MnYBP2_NoCenDiaCilLg, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=.5) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRBio_Rep_MnYBP2_NoCenDiaCilLg,
              aes(y=FRUgCcd),
              color="orange", size=2)+
  xlab("Taxon Group, - centric diatoms and large ciliates") +
  ggtitle("YBP2 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(-.34, .32), 
                     breaks = c(-.33, 0, .05, .09, .25, .31)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Make a df and plot with just the Centric Diatoms and large ciliates
FRBio_Rep_MnYBP2_CDCL <- FRBio_Rep_Mn %>% 
  filter(event =="YBP2") %>% 
  filter(group_size == "CenDiaLg" | group_size =="CenDiaSm" | group_size == "CilLg")

a <- ggplot(data=FRBio_Rep_MnYBP2_CDCL, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRBio_Rep_MnYBP2_CDCL,
             aes(y=FRUgCcd),
             color="orange", size=2)+
  xlab("Taxon Group, centric diatoms and large ciliates") +
  ggtitle("YBP2 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(.24, 6.5), 
                     breaks = c(0, .25, .44,  1.6, 3.75, 6.4)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP1
### Plot only small flagellates and large ciliates
FRBio_Rep_MnYBP1_FlSmCilLg <- FRBio_Rep_Mn %>% 
  filter(event =="YBP1") %>% 
  filter(group_size == "CilLg"| group_size == "FlagSm")

a <- ggplot(data=FRBio_Rep_MnYBP1_FlSmCilLg, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=.5) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRBio_Rep_MnYBP1_FlSmCilLg,
              aes(y=FRUgCcd),
              color="orange", size=2)+
  xlab("Taxon Group, Flag Sm and Cil Lg") +
  ggtitle("YBP1 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(.02, .07), 
                     breaks = c(.025, .04, .05, .06)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

## Remove small flagellates  and large ciliates from plot since they are so high 
## that the others can't be seen on the plot
FRBio_Rep_MnYBP1_noFlSmCilLg <- FRBio_Rep_Mn %>% 
  filter(event =="YBP1") %>% 
  filter(group_size != "FlagSm" & group_size !="CilLg")# %>% 
  #filter(group_size != "CilLg")

a <- ggplot(data=FRBio_Rep_MnYBP1_noFlSmCilLg, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=.5) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRBio_Rep_MnYBP1_noFlSmCilLg,
              aes(y=FRUgCcd),
              color="orange", size=2)+
  xlab("Taxon Group, no Flag Sm and Cil Lg") +
  ggtitle("YBP1 Ingestion Rates, Biomass")+
  scale_y_continuous(limits = c(-.015, .03), 
                     breaks = c(-.01, 0, .02)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a



#################### CELLS, INDIVIDUAL EVENT PLOTS ####################
#######################################################################

### LSZ2
### Adding the reps to the plot makes it mostly illegible, so I'll plot the means
##  and reps separately
FRCells_Rep_MnLSZ2 <- FRCells_Rep_Mn %>% 
  filter(event =="LSZ2")

### CenDia small and large and FlagSm are outliers. Remove from the plot and make a
##  separate plot with them
FRCells_Rep_MnLSZ2_noCenDiaFlSm <- FRCells_Rep_MnLSZ2 %>% 
  filter(group_size !="CenDiaLg" & group_size != "CenDiaSm" & group_size != "FlagSm")

a <- ggplot(data=FRCells_Rep_MnLSZ2_noCenDiaFlSm, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=.5) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRCells_Rep_MnLSZ2_noCenDiaFlSm,
              aes(y=FRCellsCd),
              color="orange", size=2)+
  xlab("Taxon Group, no Flag Sm and CenDia
  UnidSm reps removed") +
  ggtitle("LSZ2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(-4, 201), 
                     breaks = c(0, 15, 50, 100, 200)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Plot LSZ2 with only CenDia small and large and FlagSm 
FRCells_Rep_MnLSZ2_CenDiaFlSm <- FRCells_Rep_MnLSZ2 %>% 
  filter(group_size =="CenDiaLg" | group_size == "CenDiaSm" | group_size == "FlagSm")

a <- ggplot(data=FRCells_Rep_MnLSZ2_CenDiaFlSm, aes(group_size, FRmnCellsCd)) +
  #geom_hline(yintercept=0, color="gray", linewidth=.5) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRCells_Rep_MnLSZ2_CenDiaFlSm,
              aes(y=FRCellsCd),
              color="orange", size=2)+
  xlab("Taxon Group, 
       Only Flag Sm and CenDia Lg and Sm") +
  ggtitle("LSZ2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(2650, 7950), 
                     breaks = c(2662,3120, 4890, 7927)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a


### YBP1
FRCells_Rep_MnYBP1 <- FRCells_Rep_Mn%>% 
  filter(event =="YBP1")
### Remove flag sm
FRCells_Rep_MnYBP1_noFlS <- FRCells_Rep_MnYBP1 %>% 
  filter(group_size != "FlagSm")

a <- ggplot(data=FRCells_Rep_MnYBP1_noFlS, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnYBP1_noFlS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, no Flag Sm
       UnidSm Rep removed") +
  ggtitle("YBP1 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(-74, 180), 
                     breaks = c(-73, 0, 42, 179)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Only Flag Sm
FRCells_Rep_MnYBP1_FlS <- FRCells_Rep_MnYBP1%>% 
  filter(group_size =="FlagSm")

a <- ggplot(data=FRCells_Rep_MnYBP1_FlS, aes(group_size, FRmnCellsCd)) +
  #geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnYBP1_FlS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, only Flag Sm") +
  ggtitle("YBP1 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(3060, 6357), 
                     breaks = c(3076, 5096, 5856, 6356)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### YBP2
FRCells_Rep_MnYBP2 <- FRCells_Rep_Mn%>% 
  filter(event =="YBP2")
### Remove cen dia sm
FRCells_Rep_MnYBP2_noCDS <- FRCells_Rep_MnYBP2 %>% 
  filter(group_size != "CenDiaSm")

a <- ggplot(data=FRCells_Rep_MnYBP2_noCDS, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnYBP2_noCDS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, no CenDiaSm") +
  ggtitle("YBP2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(-1400, 3020), 
                     breaks = c(-1400, -562,0,  300, 1100, 2000, 3020)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Only CenDiaSm
FRCells_Rep_MnYBP2_CDS <- FRCells_Rep_MnYBP2%>% 
  filter(group_size =="CenDiaSm")

a <- ggplot(data=FRCells_Rep_MnYBP2_CDS, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnYBP2_CDS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, only CenDiaSm") +
  ggtitle("YBP2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(8200, 25100), 
                     breaks = c(3076, 8233, 9700, 14312, 25004)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### WLD2
FRCells_Rep_MnWLD2 <- FRCells_Rep_Mn%>% 
  filter(event =="WLD2")
### Remove FlagSm and UnidSm
FRCells_Rep_MnWLD2_noFSUS <- FRCells_Rep_MnWLD2 %>% 
  filter(group_size != "FlagSm" & group_size != "UnidSm")

a <- ggplot(data=FRCells_Rep_MnWLD2_noFSUS, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnWLD2_noFSUS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, 
       no FlagSm and UnidSm") +
  ggtitle("WLD2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(-160, 265), 
                     breaks = c(-157, 0, 50, 100, 200,  261)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Only FlagSm and UnidSm
FRCells_Rep_MnWLD2_FSUS <- FRCells_Rep_MnWLD2%>% 
  filter(group_size =="FlagSm" | group_size == "UnidSm")

a <- ggplot(data=FRCells_Rep_MnWLD2_FSUS, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnWLD2_FSUS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, 
  only FlagSm and UnidSm") +
  ggtitle("WLD2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(-2350, 1840), 
                     breaks = c(-2341, -300, 11, 1833)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### SJR2
FRCells_Rep_MnSJR2 <- FRCells_Rep_Mn%>% 
  filter(event =="SJR2")
### Remove FlagSm and UnidSm
FRCells_Rep_MnSJR2_noFSUSCDS <- FRCells_Rep_MnSJR2 %>% 
  filter(group_size != "CenDiaSm", 
         group_size != "FlagSm" & group_size != "UnidSm")

a <- ggplot(data=FRCells_Rep_MnSJR2_noFSUSCDS, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnSJR2_noFSUSCDS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, no CenDiaSm
    FlagSm and UnidSm") +
  ggtitle("SJR2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(-10, 300), 
                     breaks = c(0, 50, 100, 200, 291)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Only CenDiaSm, FlagSm and UnidSm
FRCells_Rep_MnSJR2_FSUSCDS <- FRCells_Rep_MnSJR2%>% 
  filter(group_size =="FlagSm" | group_size == "UnidSm" | group_size == "CenDiaSm")

a <- ggplot(data=FRCells_Rep_MnSJR2_FSUSCDS, aes(group_size, FRmnCellsCd)) +
  #geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRCells_Rep_MnSJR2_FSUSCDS,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, only 
  CenDiaSm, FlagSm, UnidSm") +
  ggtitle("SJR2 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(170, 2900), 
                     breaks = c(179, 400, 1000,  2884)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a


### SJR1
FRCells_Rep_MnSJR1 <- FRCells_Rep_Mn%>% 
  filter(event =="SJR1")
### Remove CenDiaLg and CenDiaSm
FRCells_Rep_MnSJR1_noCD <- FRCells_Rep_MnSJR1 %>% 
  filter(group_size != "CenDiaSm", 
         group_size != "CenDiaLg")

a <- ggplot(data=FRCells_Rep_MnSJR1_noCD, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRCells_Rep_MnSJR1_noCD,
             aes(y=FRCellsCd),
             color="orange", size=2)+
  xlab("Taxon Group, no CenDiaSm
    and CenDiaLg") +
  ggtitle("SJR1 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(-600, 1250), 
                     breaks = c(-590,-180,  100, 200, 500, 1212)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Only CenDiaSm, CenDiaLg
FRCells_Rep_MnSJR1_CD <- FRCells_Rep_MnSJR1%>% 
  filter(group_size =="CenDiaLg" | group_size == "CenDiaSm")

a <- ggplot(data=FRCells_Rep_MnSJR1_CD, aes(group_size, FRmnCellsCd)) +
  #geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_jitter(data = FRCells_Rep_MnSJR1_CD,
              aes(y=FRCellsCd),
              color="orange", size=2)+
  xlab("Taxon Group, only 
        CenDiaSm, FlagSm") +
  ggtitle("SJR1 Ingestion Rates, Cells")+
  scale_y_continuous(limits = c(1130, 3050), 
                     breaks = c(1154,  1900, 2720, 3030)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Look at the chain diatom group_size to see what the numbers are like
ChnDiaFRBio <- FRBio_Rep_Mn %>% 
  filter(group_size =="ChnDiaLg" | group_size =="ChnDiaSm")
write_xlsx(ChnDiaFRBio, "data/Clearance Rates/Feeding Rates/ChnDiaFRBio.xlsx")

a <- ggplot(data=ChnDiaFRBio, aes(event, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  ggtitle("Chain Diatom Biomass Ingestion Rates")+
  scale_y_continuous()+
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (12)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a


