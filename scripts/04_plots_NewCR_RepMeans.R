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

crLSZ2 <- ggplot(data=CR_Rep_MnLSZ2,  
            aes(x = factor(group_size, 
                           level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
      y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
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
crLSZ2

### YBP1
CR_Rep_MnYBP1 <- CR_Rep_Mn %>% 
  filter(event =="YBP1")
### Needs Scale Break, see below for Scale Break code, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
### To plot without Scale Break
crYBP1 <- ggplot(data=CR_Rep_MnYBP1, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
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
crYBP1


### YBP2
CR_Rep_MnYBP2 <- CR_Rep_Mn %>% 
  filter(event =="YBP2")

crYBP2 <- ggplot(data=CR_Rep_MnYBP2, 
            aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", 
                                                 "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                 "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
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
crYBP2

### WLD2
CR_Rep_MnWLD2 <- CR_Rep_Mn %>% 
  filter(event =="WLD2")

crWLD2 <- ggplot(data=CR_Rep_MnWLD2, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
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
crWLD2

### SJR1
CR_Rep_MnSJR1 <- CR_Rep_Mn %>% 
  filter(event =="SJR1")

crSJR1 <- ggplot(data=CR_Rep_MnSJR1, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                     "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                     "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
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
crSJR1


### SJR2
CR_Rep_MnSJR2 <- CR_Rep_Mn %>% 
  filter(event =="SJR2")

crSJR2 <- ggplot(data=CR_Rep_MnSJR2, 
            aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                 "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                 "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                    y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
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
crSJR2

### Experiment with patchwork to put the 6 event CR plots on one panel, since
##  I can't calculate together and use facet_wrap because the ranges are too wide,
## and each individual event plot has different scale break factors
library(patchwork)
### Wim's Example
#  AbunBoth <- AbunPlotFH / AbunPlotHG + plot_layout(ncol = 1)
#  AbunBoth

### Try with two cr plots
crSJR1_2 <- crSJR1 / crSJR2 + plot_layout(ncol = 1)
crSJR1_2
crAll <- crLSZ2 / crSJR1 / crSJR2 / crWLD2 / crYBP1 / crYBP2 + plot_layout(ncol=2)
crAll

crAbSJR2 <- crSJR2 / abSJR2 + plot_layout(ncol = 1)
crAbSJR2
### This works, but the dots and fonts are too big. Re-run the code for all the plots
##  below, with smaller dots and fonts. Copy and paste, then make changes

### LSZ2
CR_Rep_MnLSZ2 <- CR_Rep_Mn %>% 
  filter(event =="LSZ2")

crLSZ2 <- ggplot(data=CR_Rep_MnLSZ2,  
                 aes(x = factor(group_size, 
                                level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                          "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                          "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                     y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnLSZ2,
             aes(y=CRmlcd),
             color="#E69F00", size=1)+
  xlab(NULL) +
  ggtitle("LSZ2 Clearance Rates")+
  scale_y_continuous(limits = c(-11, 66), 
                     breaks = c(-10,-3, 0, 5, 10, 14, 30, 40, 53, 64)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (9)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())

crLSZ2

### YBP1
CR_Rep_MnYBP1 <- CR_Rep_Mn %>% 
  filter(event =="YBP1")
### I can't use  the Scale Break when plotting two variables. 
##  It just squished all the  CR reps together
### To plot without Scale Break
crYBP1 <- ggplot(data=CR_Rep_MnYBP1, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                          "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                          "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                         y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnYBP1,
             aes(y=CRmlcd),
             color="#E69F00", size=1)+
  xlab(NULL) +
  ggtitle("YBP1 Clearance Rates")+
  scale_y_continuous(limits = c(-37, 73), 
                     breaks = c(-36, 0, 5, 9, 15, 20, 30, 42, 72)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (9)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
crYBP1


### YBP2
CR_Rep_MnYBP2 <- CR_Rep_Mn %>% 
  filter(event =="YBP2")

crYBP2 <- ggplot(data=CR_Rep_MnYBP2, 
                 aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", 
                                                      "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                      "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                     y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnYBP2,
             aes(y=CRmlcd), color="#E69F00", size=1)+
  xlab(NULL) +
  ggtitle("YBP2 Clearance Rates")+
  scale_y_continuous(limits = c(-38, 94), 
                     breaks = c(-37, -13, 0, 5, 20, 27, 38, 50, 80, 93)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (9)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
crYBP2

### WLD2
CR_Rep_MnWLD2 <- CR_Rep_Mn %>% 
  filter(event =="WLD2")

crWLD2 <- ggplot(data=CR_Rep_MnWLD2, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                          "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                          "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                         y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnWLD2,
             aes(y=CRmlcd), color="#E69F00", size=1)+
  xlab(NULL) +
  ggtitle("WLD2 Clearance Rates")+
  scale_y_continuous(limits = c(-32, 56), 
                     breaks = c(-31, -15, 0, 5, 20, 30, 40, 55)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (9)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
crWLD2

### SJR1
CR_Rep_MnSJR1 <- CR_Rep_Mn %>% 
  filter(event =="SJR1")

crSJR1 <- ggplot(data=CR_Rep_MnSJR1, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                          "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                          "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                         y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnSJR1,
             aes(y=CRmlcd), color="#E69F00", size=1)+
  xlab(NULL) +
  ggtitle("SJR1 Clearance Rates")+
  scale_y_continuous(limits = c(-39, 62), 
                     breaks = c(-38, -15, 0, 5, 10, 20, 30, 40, 50, 61)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (9)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
crSJR1


### SJR2
CR_Rep_MnSJR2 <- CR_Rep_Mn %>% 
  filter(event =="SJR2")

crSJR2 <- ggplot(data=CR_Rep_MnSJR2, 
                 aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                      "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                      "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                     y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnSJR2,
             aes(y=CRmlcd), color="#E69F00", size=1)+
  xlab(NULL) +
  ggtitle("SJR2 Clearance Rates")+
  scale_y_continuous(limits = c(-11, 59), 
                     breaks = c(-10, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 58)) +
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (9)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
crSJR2

### Test to plot IR with similar code, no scale breaks, w/ facet wrap--yes, it worked!
load("data7_24/Clearance Rates 2/CrIrCntRepMn.Rdata")
plot_IRall <- ggplot(data=CrIrCntRepMn, 
                     aes(x = factor(group_size, 
                                    level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                              "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                              "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                         y=FRUgMn)) +
  geom_hline(yintercept=0, color="gray", linewidth=.5) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=.5)+
  geom_point(aes(color = FRUgMn>0), size = 3)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CrIrCntRepMn,
             aes(y=FRUgCcd),
             color="#E69F00", size=1)+
  xlab(NULL) +
  ggtitle("Ingestion Rates")+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+
  #  limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"))+
  facet_wrap(~event, ncol = 2, scales = "free")+
  #scale_y_continuous(limits = c(-37, 73), 
   #                  breaks = c(-36, 0, 5, 9, 15, 20, 30, 42, 72)) +
  ylab(µgC~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1.5),
        panel.grid.major.y = element_blank())
plot_IRall
### plots saved as 8 x 7 
