#################################################################################
####################### REPS WITH MEANS CLEARANCE RATE ##########################
#################### INDIVIDUAL SAMPLING EVENT PLOTS ############################
##################### WITH PATCHWORK ALL 6 TOGETHER #############################
#################################################################################

### 8/23/23, Plots formatted specifically to plot with patchwork
### Based off of plots created in 04_plots_NewCR_RepMeans.R, but modified with smaller
##  points and font to work better for plotting together with patchwork, and corrected
##  to show all the dots, some of which were missing from previous plots. Added jitter 
##  to some plots so all points are visible; changed outside box to size 1, since the 1.5
##  thick line was cutting off some points on the y-axis

### crAll plot Saved as pdf 8 x 10 Final Final/Clearance/plot_CRall_repMn.pdf

library(tidyverse)
library(writexl)
library(patchwork)
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
                                level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", 
                                          "FlagSm", "ChlLg","ChlSm","ChnDiaLg",
                                          "ChnDiaSm","CyanoLg","CyanoSm",
                                          "DinoLg","FlagLg","PenDiaLg","PenDiaSm",
                                          "UnidLg","UnidSm")),
                     y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnLSZ2,
             aes(y=CRmlcd),
             color="#E69F00", size=1,
             position = position_jitter(width = 0.45))+
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
        panel.background = element_rect(colour = "black", size=1),
        panel.grid.major.y = element_blank())

crLSZ2

### YBP1
CR_Rep_MnYBP1 <- CR_Rep_Mn %>% 
  filter(event =="YBP1")
### I can't use  the Scale Break when plotting two variables. 
##  It just squished all the  CR reps together
### To plot without Scale Break
crYBP1 <- ggplot(data=CR_Rep_MnYBP1, 
                 aes(x = factor(group_size, 
                                level = c("CenDiaLg", "CenDiaSm", "CilLg", 
                                          "CilSm", "FlagSm","ChlLg","ChlSm",
                                          "ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                          "DinoLg","FlagLg","PenDiaLg","PenDiaSm",
                                          "UnidLg","UnidSm")),
                                         y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnYBP1,
             aes(y=CRmlcd),
             color="#E69F00", size=1,
             position = position_jitter(width = 0.45))+
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
        panel.background = element_rect(colour = "black", size=1),
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
        panel.background = element_rect(colour = "black", size=1),
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
             aes(y=CRmlcd), color="#E69F00", size=1,
             position = position_jitter(width = 0.45))+
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
        panel.background = element_rect(colour = "black", size=1),
        panel.grid.major.y = element_blank())
crWLD2

### SJR1
CR_Rep_MnSJR1 <- CR_Rep_Mn %>% 
  filter(event =="SJR1")

crSJR1 <- ggplot(data=CR_Rep_MnSJR1, 
                 aes(x = factor(group_size, 
                                level = c("CenDiaLg", "CenDiaSm", "CilLg", 
                                          "CilSm", "FlagSm","ChlLg","ChlSm",
                                          "ChnDiaLg","ChnDiaSm","CyanoLg",
                                          "CyanoSm", "DinoLg","FlagLg","PenDiaLg",
                                          "PenDiaSm","UnidLg","UnidSm")),
                                         y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_vline(xintercept=  "ChlLg",  color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_color_manual(values=c("FALSE"="#D55E00","TRUE"="#0072B2"))+
  geom_point(data = CR_Rep_MnSJR1,
             aes(y=CRmlcd), color="#E69F00", size=1,
             position = position_jitter(width = 0.45))+
  xlab(NULL) +
  ggtitle("SJR1 Clearance Rates")+
  scale_y_continuous(limits = c(-39, 62), 
                     breaks = c(-38, -15, 0, 5, 10, 20, 30, 40, 50, 61)) +
  scale_x_discrete(expand=c(0.05,0.05))+
  ylab("mL"~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (9)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(colour = "black", size=1),
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
             aes(y=CRmlcd), color="#E69F00", size=1,
             position = position_jitter(width = 0.45))+
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
        panel.background = element_rect(colour = "black", size=1),
        panel.grid.major.y = element_blank())
crSJR2

### Use patchwork to put all 6 in two columns, portrait
crAll <- crLSZ2 / crSJR1 / crSJR2 / crWLD2 / crYBP1 / crYBP2 + plot_layout(ncol=2)
crAll

### crAll plot Saved as pdf 6 x 8 Final Final/Clearance/plot_CRall_repMn.pdf
### 8 x 10 works as well
