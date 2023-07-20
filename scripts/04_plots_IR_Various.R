#####################################################################################
##################### INGESTION RATE VARIOUS CALCS AND  PLOTS  ######################
#####################################################################################

### IR biomass, rainbow stacked bar plots
##  percentage of sample plots
##  total biomass IR contributed by each taxa group
##  Plots ordered by size, still working on it 5/8/23

### 5/1/23
library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")

### Use FRBio_Rep_Mn.Rdata and CR_Rep_Mn.Rdata for means and reps
load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata")
load("data/Clearance Rates/CR_Rep_Mn.Rdata")
load("data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.Rdata") ## Means only

### Per notes 4/28/23, do a stacked bar plot of IR biomass and CR, 

### IR biomass by taxa group, per each sampling event, rainbow colored bars

p <- ggplot(BugFRMn_GrpSz, aes(fill = group_size, y = FRmnUgCcd,  x=event))+
  #scale_color_viridis_d()+
  geom_bar(position = "fill", stat = "identity")+ #this puts it in a percentage scale
  geom_bar(stat = "identity")+ # this plots the real numbers
  ylab("µg C"~copepod^-1~d^-1)+
  facet_wrap(~ event, ncol= 2, scales="free") +
  wimGraph()
p + scale_fill_viridis_d() ## this changes the color palette, but the gradients
## still bleed too much to distinguish the different taxa groups
  #facet_wrap(~ event, ncol= 2, scales="free")#+
  #theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10))

  #scale_x_discrete () +
  #scale_y_continuous() +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  #+
#  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
 #       axis.title.y = element_text(size = 12),
  #      strip.text.x = element_text(size = 10))+
  #facet_wrap(~ event, ncol= 2, scales="free") +
  #wimGraph()
p


### Calculate the percentage of each taxa group
IRbio_eventTot <- BugFRMn_GrpSz %>%
  pmax(BugFRMn_GrpSz, 0)
IRbio_eventTot$FRmnUgCcd [is.na(IRbio_eventTot$FRmnUgCcd)]<-0

IRbio_eventTot<- IRbio_eventTot %>% 
  group_by(event) %>%
  mutate(event_total = sum(FRmnUgCcd )) %>% 
  ungroup
  #mutate(eventTotal = ifelse(FRmnUgCcd > 0, sum(FRmnUgCcd), NA)
       #mutate(event_tot = sum(FRmnUgCcd)) 

IRbio_percentTot <- IRbio_eventTot %>% 
  group_by(group_size) %>% 
  mutate(percent = FRmnUgCcd/(event_total)) %>% 
  ungroup

### See which taxa groups contributed at least 10% to the total
IRbio_topPercent <- IRbio_percentTot %>% 
  filter(percent >= 0.1)
unique(IRbio_topPercent$group_size)

IRbioPercent_LSZ2 <- IRbio_percentTot %>% 
  filter(event == "LSZ2") %>%
  ungroup
sum(IRbioPercent_LSZ2$percent)


save(IRbio_percentTot, file = "data/Clearance Rates/Feeding Rates/IRbio_percent.Rdata")
write_xlsx(IRbio_percentTot, "data/Clearance Rates/Feeding Rates/IRbio_percent.xlsx")

### Percent of each group_size by sampling event, rainbow colors
p <- ggplot(IRbio_percentTot, aes(fill = group_size,y = percent,  x=event))+
  geom_bar(position = "fill", stat = "identity")+
  ylab("percent µg C"~copepod^-1~d^-1)+
  #coord_flip()+
  facet_wrap(~ event, ncol= 2, scales="free") +
  wimGraph()
p

### Plot percentages of total by group_size, dot plot
p <- ggplot(IRbio_percentTot, aes(x = percent, y = group_size))+
  geom_point()+#stat = "identity",  color = "blue4", size = 2)+
  scale_x_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8))+
  xlab("proportion of total")+
  ylab("Taxa Group")+
  #ylab("percent µg C"~copepod^-1~d^-1)+
  #coord_flip()+
  facet_wrap(~ event, ncol= 2)+#, scales="free") +
  theme(axis.text.y = element_text(size = 6))+
  wimGraph()
p

### Plot percentages of total by group_size, dot plot,
##  but flip the axes and use one column
p <- ggplot(IRbio_percentTot, aes(x = percent, y = group_size))+
  geom_point()+#stat = "identity",  color = "blue4", size = 2)+
  scale_x_continuous(breaks = c(0.0, 0.1, 0.2,0.4, 0.6, 0.8))+
  xlab("proportion of total")+
  ylab("Taxa Group")+
  #ylab("percent µg C"~copepod^-1~d^-1)+
  coord_flip()+
  geom_vline(xintercept=0.05, color="gray", linewidth=0.4) +
  facet_wrap(~ event, ncol= 1)+#, scales="free") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8))+
  theme(axis.text.y = element_text(size = 6))+
  wimGraph()
p

### Plot percentages of total by group_size, as above, but do a bar plot
p <- ggplot(IRbio_percentTot, aes(x = percent, y = group_size))+
  geom_bar(stat = "identity")+ #,  color = "blue4", size = 2)+
  scale_x_continuous(breaks = c(0.0, 0.1, 0.2,0.4, 0.6, 0.8))+
  xlab("proportion of total")+
  ylab("Taxa Group")+
  #ylab("percent µg C"~copepod^-1~d^-1)+
  coord_flip()+
  #geom_vline(xintercept=0.05, color="gray", linewidth=0.4) +
  facet_wrap(~ event, ncol= 1)+#, scales="free") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8))+
  theme(axis.text.y = element_text(size = 6))+
  wimGraph()
p


### Plot to percentages of total 10% or greater by group_size
p <- ggplot(IRbio_topPercent, aes(x = percent, y = group_size))+
  geom_point()+ #stat = "identity"),  color = "blue4", size = 2)+
  scale_x_continuous(breaks = c(0.1, 0.2, 0.4, 0.6, 0.8))+
  xlab("proportion of total")+
  ylab("Taxa Group")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8))+
  coord_flip()+
  facet_wrap(~ event, ncol= 1)+#, scales="free") +
  theme(axis.text.y = element_text(size = 6))+
  wimGraph()
p

p <- ggplot(IRbio_percentTot, aes(x = percent,  y=group_size))+
  geom_bar(stat = "identity")+
  ylab("percent of total biomass ingestion")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  #coord_flip()+
  wimGraph()
p

p <- ggplot(IRbio_percentTot, aes(x = group_size,  y=percent))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept=0, color="red", linewidth=.5) +
  #scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  ylab("percent of total biomass ingestion")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  #coord_flip()+
  wimGraph()
p

### Look at the total biomass IR contributed by each taxa group

IRbyTaxa <- BugFRMn_GrpSz %>% 
  group_by(group_size) %>% 
  summarise(taxaTot = sum(FRmnUgCcd)) 
save(IRbyTaxa, file = "data/Clearance Rates/Feeding Rates/IRbyTaxa.Rdata")
write_xlsx(IRbyTaxa, "data/Clearance Rates/Feeding Rates/IRbyTaxa.xlsx")

p <- ggplot(IRbyTaxa, aes(x = reorder(group_size, -taxaTot), y=taxaTot))+ 
  #this reorders the groupsize by high to low IR
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = c(0, .1, .5, 1,2,3,4))+
  ggtitle("Ingestion Rates Per Taxon 
          Across All Sampling Events")+
  ylab("µg C"~copepod^-1~d^-1)+
  xlab("Taxa Group")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10))+
  wimGraph()
p

#############################################
### Plot biomass ingestion by taxa group size
#############################################
load("data/Clearance Rates/Feeding Rates/FRbio_allEvents.Rdata")
## First without scale break
p <- ggplot(data=FRbio_allEvents, aes(group_size, TotalFRbioMn)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = TotalFRbioMn>0), size = 3)+
  scale_y_continuous()+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("µgC"~copepod^-1~d^-1)+
  ggtitle("Ingestion Rates,Biomass")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.position = "none",
        panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold"))
p

### Need to use scale break
source("scripts/01_function_Scale Break.R")

Break      <- 0.06 
rescale    <- 25
ylabels    <- c(-0.04, 0, .05, .15, .5,.67)
brks       <- scaleBreak(ylabels, Break, rescale)
FRbio_allEvents$ySquish <- scaleBreak(FRbio_allEvents$TotalFRbioMn, 
                                     Break=Break, rescale=25)
a <- ggplot(data=FRbio_allEvents, aes(group_size, TotalFRbioMn)) +
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(data = FRbio_allEvents, aes(group_size, ySquish))+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ggtitle("Biomass Ingestion Rates Per Taxon Across All Sampling Events")+
  ylab("µg C"~copepod^-1~d^-1)+
  xlab("Taxa Group")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        title = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))+
  wimGraph()
a

### Plot cell ingestion by taxa group size
source("scripts/01_function_Scale Break.R")
## Use this dfs:
load("data/Clearance Rates/Feeding Rates/CpmFRMn_GrpSz.Rdata") #Ingestion Rates, cells means

### Use scale break function
Break      <- 600 
rescale    <- 25
ylabels    <- c(-562, -180, 0, 100, 300, 600, 3000, 6000, 14350)
brks       <- scaleBreak(ylabels, Break, rescale)
FR_cell_Size$ySquish <- scaleBreak(FR_cell_Size$FRmnCellsCd, 
                                  Break=Break, rescale=25)
a <- ggplot(data=FR_cell_Size, aes(group_size, FRmnCellsCd)) +
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(data = FR_cell_Size, aes(group_size, ySquish))+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  #geom_point(aes(color = FRmnCellsCd>0), size = 4)+
  #scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  ggtitle("Cell Ingestion Rates Per Taxon 
    Across All Sampling Events")+
  ylab("Cells"~copepod^-1~d^-1)+
  xlab("Taxa Group")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        title = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))+
  wimGraph()
a


### Plot clearance rate by taxa group size
## Use this df:
load("data/Clearance Rates/Feeding Rates/CR_Size.Rdata") #Clearance Rates means
## First without scale break
p <- ggplot(data=CR_allEvents, aes(group_size, TotalCRmn)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = TotalCRmn>0), size = 3)+
  scale_y_continuous()+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("mL"~copepod^-1~d^-1)+
  ggtitle("Clearance Rates")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.position = "none",
        panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold"))
p

### Plot ingestion rate,cells by taxa group size
## Use this df:
load("data/Clearance Rates/Feeding Rates/IRcells_allEvents.Rdata") #Clearance Rates means
## First without scale break
p <- ggplot(data=IRcells_allEvents, aes(group_size, TotalIRcells)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = TotalIRcells>0), size = 3)+
  scale_y_continuous()+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  xlab("Taxa Groups") +
  ylab("Cells"~copepod^-1~d^-1)+
  ggtitle("Ingestion Rates, cells")+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.position = "none",
        panel.background = element_rect(colour = "black", linewidth =1.5),
        strip.text.x = element_text(size = 12, face = "bold"))
p

source("scripts/01_function_Scale Break.R")
### Use scale break function
Break      <- 500 
rescale    <- 25
ylabels    <- c(-79, 0, 30, 75, 200, 300, 400, 1300, 2200, 4000)
brks       <- scaleBreak(ylabels, Break, rescale)
IRcells_allEvents$ySquish <- scaleBreak(IRcells_allEvents$TotalIRcells, 
                                   Break=Break, rescale=25)

p <- ggplot(data=IRcells_allEvents, aes(group_size, ySquish)) +
  geom_point(shape=16, color="navy") +
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxa Group") +
  scale_y_continuous("Cells"~copepod^-1~d^-1, breaks=brks, labels=ylabels) +
  ggtitle("Cell Ingestion Rates ")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        title = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))+
  wimGraph()
p

  ggplot( aes(x=FRmnUgCcd, y=group_size)) +
  #geom_segment( aes(xend=group_size, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip()+
  ggtitle("Ingestion Rates Per Taxon 
          Across All Sampling Events")+
  ylab("µg C"~copepod^-1~d^-1)+
  xlab("Taxa Group")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10))+
  wimGraph()
