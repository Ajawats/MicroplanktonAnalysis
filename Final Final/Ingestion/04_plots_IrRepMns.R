####################################################################################
######### REPS WITH MEANS INGESTION RATE INDIVIDUAL SAMPLING EVENT PLOTS  #########
#####################################################################################

### 8/24/23, copied from 04_plots_NewCR_RepMeans.R, which has this IR code at the bottom
library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")

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
### plots saved as 8 x 7 Final Final/Ingestion/plot_IR17all.pdf
