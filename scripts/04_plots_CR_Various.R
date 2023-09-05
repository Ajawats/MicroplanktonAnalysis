######## PLOTS probably not meant to be saved, but saving just in case ##########
######### it's been open on the desktop for week, saving on 9/4/23 #############


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
### Needs Scale Break, see below for Scale Break code, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
### To plot without Scale Break
crYBP1 <- ggplot(data=CR_Rep_MnYBP1, aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                                          "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                                          "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                                         y=CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
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

