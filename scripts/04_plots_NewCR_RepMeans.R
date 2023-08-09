####################################################################################
######### REPS WITH MEANS CLEARANCE RATE INDIVIDUAL SAMPLING EVENT PLOTS  #########
#####################################################################################

### Updated 4/12/23 with new code 03_calcs_CR_FR.R, with new sum calculations,
##  which I determined weren't wrong at all.
library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")


###From 03_calcs_CR_FR.R
### From 03_calcs_CR_IR_New_06_03.R
load("data7_24/Clearance Rates/CR_Rep_Mn.Rdata")

### LSZ2
CR_Rep_MnLSZ2 <- CR_Rep_Mn %>% 
  filter(event =="LSZ2")

a <- ggplot(data=CR_Rep_MnLSZ2,  
            aes(x = factor(group_size, 
                           level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
      y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = CrMNmlcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnLSZ2,
             aes(y=CRmlcd),
             color="#E69F00", size=2)+
  xlab(NULL) +
  ggtitle("LSZ2 Clearance Rates")+
  scale_y_continuous(limits = c(-11, 66), 
                     breaks = c(-10,-3, 0, 5, 10, 14, 30, 40, 53, 64)) +
  ylab("mL"~d^-1)+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())+
  wimGraph()
a

### YBP1
CR_Rep_MnYBP1 <- CR_Rep_Mn %>% 
  filter(event =="YBP1")
### Needs Scale Break, see below for Scale Break code, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
### To plot without Scale Break
a <- ggplot(data=CR_Rep_MnYBP1, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = CrMNmlcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnYBP1,
             aes(y=CRmlcd),
             color="#E69F00", size=2)+
  xlab(NULL) +
  ggtitle("YBP1 Clearance Rates")+
  scale_y_continuous(limits = c(-37, 73), 
                     breaks = c(-36, 0, 5, 9, 15, 20, 30, 42, 72)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
a


### YBP2
CR_Rep_MnYBP2 <- CR_Rep_Mn %>% 
  filter(event =="YBP2")

a <- ggplot(data=CR_Rep_MnYBP2, 
            aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", 
                                                 "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                 "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = CrMNmlcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnYBP2,
             aes(y=CRmlcd), color="#E69F00", size=2)+
  xlab(NULL) +
  ggtitle("YBP2 Clearance Rates")+
  scale_y_continuous(limits = c(-38, 94), 
                     breaks = c(-37, -13, 0, 5, 20, 27, 38, 50, 80, 93)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
a

### WLD2
CR_Rep_MnWLD2 <- CR_Rep_Mn %>% 
  filter(event =="WLD2")

a <- ggplot(data=CR_Rep_MnWLD2, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = CrMNmlcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnWLD2,
             aes(y=CRmlcd), color="#E69F00", size=2)+
  xlab(NULL) +
  ggtitle("WLD2 Clearance Rates")+
  scale_y_continuous(limits = c(-32, 56), 
                     breaks = c(-31, -15, 0, 5, 20, 30, 40, 55)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
a

### SJR1
CR_Rep_MnSJR1 <- CR_Rep_Mn %>% 
  filter(event =="SJR1")

a <- ggplot(data=CR_Rep_MnSJR1, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = CrMNmlcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnSJR1,
             aes(y=CRmlcd), color="#E69F00", size=2)+
  xlab(NULL) +
  ggtitle("SJR1 Clearance Rates")+
  scale_y_continuous(limits = c(-39, 62), 
                     breaks = c(-38, -15, 0, 5, 10, 20, 30, 40, 50, 61)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
a


### SJR2
CR_Rep_MnSJR2 <- CR_Rep_Mn %>% 
  filter(event =="SJR2")

a <- ggplot(data=CR_Rep_MnSJR2, 
            aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                 "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                 "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = CrMNmlcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnSJR2,
             aes(y=CRmlcd), color="#E69F00", size=2)+
  xlab(NULL) +
  ggtitle("SJR2 Clearance Rates")+
  scale_y_continuous(limits = c(-11, 59), 
                     breaks = c(-10, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 58)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
a



### YBP1 Tried to plot with Scale Break, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
###   Saved the code below just in case I need it

#CR_Rep_MnYBP1
#CR_Rep_MnYBP1

Break      <- -30
rescale    <- 25
ylabels    <- c(-36, 0, 5, 9, 15, 20, 30, 42, 71)
brks       <- scaleBreak(ylabels, Break, rescale)
CR_Rep_MnYBP1$ySquish <- scaleBreak(CR_Rep_MnYBP1$CR, 
                                     Break=Break, rescale=25)
a <- ggplot(data=CR_Rep_MnYBP1,  
            aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                 "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                 "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = CrmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnYBP1, aes(group_size, ySquish),
             color="#E69F00", size=2)+
  xlab(NULL) +
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