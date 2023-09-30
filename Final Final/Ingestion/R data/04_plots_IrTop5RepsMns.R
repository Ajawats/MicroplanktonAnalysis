######################################################################
############## PLOT TOP 5 IR BIOMASS REPS AND MEANS ####################
######################################################################

### 8/24/23, 9/4/23
library(tidyverse)
library(writexl)
load("Final Final/Ingestion/IrTop5RepMns.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

### Biomass IR, top 5
a <- ggplot(data=IrTop5RepMns, aes(taxaGroup, IrMnµgCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = IrMnµgCd>0), size = 3)+
  scale_y_continuous()+
  ylab("µg C"~copepod^-1~d^-1)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = IrTop5RepMns, aes(y=IRµgCd), color = "orange", 
             size = 1, position = position_jitter(width = 0.25))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab(NULL) +
  ggtitle("Ingestion Rates, Biomass")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size=10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1))
a

### Only ingestion rate values greater than 0
IrTop5RepMns2 <- IrTop5RepMns %>% 
  select(c(event, taxaGroup, IRµgCd, IrMnµgCd))
IrTop5RepMns2 <- subset(IrTop5RepMns2, IRµgCd>0 & IrMnµgCd>0)
save(IrTop5RepMns2, file = "Final Final/Ingestion/IrTop5RepMns2.Rdata")
write_xlsx(IrTop5RepMns2, "Final Final/Ingestion/IrTop5RepMns2.xlsx")

### Biomass IR no 0's, top 5
a <- ggplot(data=IrTop5RepMns2, aes(taxaGroup, IrMnµgCd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = IrMnµgCd>0), size = 3)+
  scale_y_continuous()+
  ylab("µg C"~copepod^-1~d^-1)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = IrTop5RepMns2, aes(y=IRµgCd), color = "orange", 
             size = 1, position = position_jitter(width = 0.25))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab(NULL) +
  ggtitle("Ingestion Rates, Biomass, No Zeros")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size=10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1))
a

### Editing on 9/4 23 stopped here. The below is not done
### Cells, IR top 5
a <- ggplot(data=CpmFRMn5_txGp, aes(taxaGroup, FRmnCellsCd)) +
  #geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCellsCd>0), size = 2)+
  scale_y_continuous()+
  ylab("Cells"~copepod^-1~d^-1)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_jitter(data = CpmFRMn5_txGp,
  #           aes(y=FRmnCellsCd),
  #           color="orange", size=2)+
  #geom_point(data = CrIrAb5Cell, aes(y=TotalCpmI))+
  #scale_y_continuous(sec.axis = sec_axis(~ . + 10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ggtitle("Ingestion Rates, Cells, Top 5 Taxa Groups")+
  #limits = c(-11, 66), 
  #breaks = c(-10,-3, 0, 5, 10, 14, 30, 40, 53, 64)) +
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold")) #color = "red", 
#face = "bold.italic"))
a

### Same as above, but with unused code removed
p <- ggplot(data=CpmFRMn5_txGp, aes(taxaGroup, FRmnCellsCd)) +
  geom_point(aes(color = FRmnCellsCd>0), size = 2)+
  scale_y_continuous()+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Cells"~copepod^-1~d^-1)+
  ggtitle("Ingestion Rates, Cells, Top 5 Taxa Groups")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.position = "none"+
          panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold"))
a


### Clearance Rate, top 5
load("data/Clearance Rates/CR5_IRbio_mn.Rdata")
p <- ggplot(data=CR5_IRbio_mn, aes(taxaGroup, CrMNmlcd)) +
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_y_continuous()+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("mL"~copepod^-1~d^-1)+
  ggtitle("Clearance Rates, Top 5 Taxa Groups")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        #legend.position = "none",
        panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold"))
p

### Clearance Rate, all 17 taxa groups
load("data/Clearance Rates/CR_IRbio_mn.Rdata")
p <- ggplot(data=CR_IRbio_mn, aes(group_size, CrMNmlcd)) +
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_y_continuous()+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("mL"~copepod^-1~d^-1)+
  ggtitle("Clearance Rates")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        #legend.position = "none",
        panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold"))
p

### Ingestion Rate, Cells,  all 17 taxa groups,
load("data/Clearance Rates/CR5_IRbio_mn.Rdata")
p <- ggplot(data=CR5_IRbio_mn, aes(taxaGroup, CrMNmlcd)) +
  geom_point(aes(color = CrMNmlcd>0), size = 2)+
  scale_y_continuous()+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("mL"~copepod^-1~d^-1)+
  ggtitle("Clearance Rates, Top 5 Taxa Groups")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        #legend.position = "none",
        panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold"))
p

### Clearance Rate totals per event
ggplot(sumCRmnEventsOnly, aes(x=event, y=MeanCR))+
  geom_bar(position='stack', stat='identity')+
  #theme_bw()+
  labs(x="",title = "Clearance Rate")+
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()
#theme(axis.text.x = element_text(angle = 90))+
#coord_flip()

### Ingestion Rate, cells, totals per event
load("data/Clearance Rates/Feeding Rates/IRcells_eventsOnly.Rdata")
ggplot(IRcells_eventsOnly, aes(x=event, y=IRcellsMn))+
  geom_bar(position='stack', stat='identity')+
  geom_hline(yintercept=0, color="cyan", linewidth=1) +
  scale_y_continuous(breaks = c(-0, 200, 300, 900, 1300), limits = c(-20, 1350))+
  #theme_bw()+
  labs(x="",title = "Ingestion Rate, Cells")+
  ylab("Cells"~copepod^-1~day^-1)+
  wimGraph()

### Ingestion Rate, biomass, totals per event
load("data/Clearance Rates/Feeding Rates/IRbio_eventsOnly.Rdata")
p<- ggplot(IRbio_eventsOnly, aes(x=event, y=IRbioMn))+
  geom_bar(position='stack', stat='identity')+
  #geom_hline(yintercept=0, color="cyan", linewidth=1) +
  scale_y_continuous(breaks = c(0, .01, .1, .3), limits = c(0, 0.35))+
  #theme_bw()+
  labs(x="",title = "Ingestion Rate, Biomass")+
  ylab("µg C"~copepod^-1~day^-1)+
  wimGraph()
p

