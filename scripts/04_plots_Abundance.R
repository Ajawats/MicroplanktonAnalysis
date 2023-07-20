############################################################################
########################## TAXA GROUPS ABUNDANCE   #########################
############################################################################

### 4/21/23
##  Abundance, cells per mL, by sampling events and taxa groups. Use
##  initial samples cpm. First sum the cpm of the groups that make up the
##  group_size groups.
### Reminder that Small  is esd < 15µm, and Large is esd >= 15 µm

### Use volbio_all_cr.Rdata as the base

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")
load("data/Abundance/abundanceI.Rdata")
abundanceI <- volbio_all_cr %>% 
  filter(exp == "I")

### mean cpm per group_size per sampling event
abunISum <- abundanceI %>% 
  group_by(samp_date, samp_ev, group_size) %>% 
  summarise(TotalCpmI = sum(cpm), # sum the counts per ml by group_size
            .groups = 'drop') %>% 
  as.data.frame()

### mean cpm per group_size across all sampling events
abunISumTotals <- abundanceI %>% 
  group_by(group_size) %>% 
  summarise(TotalCpmI = sum(cpm), # sum the counts per ml by group_size
            .groups = 'drop') %>% 
  as.data.frame()
write_xlsx(abunISumTotals, "data/Abundance/abunISumTotals.xlsx")

PgCell <- volbio_all_cr %>% 
  group_by(group_size) %>% 
  summarise(pgCcellMn = mean(biomass_cell_pgC))

save(abundance, file = "data/Abundance/abundance.Rdata")
write_xlsx(abundance, "data/Abundance/abundance.xlsx")
load("data/Abundance/abundance.Rdata")


### Plot, needs help

p <- ggplot(abundance, aes(x = group_size, y = TotalCpmI))+
  geom_bar(stat = "identity")+ #,  color = "blue4", size = 2)+
  #scale_x_continuous(breaks = c(0.0, 0.1, 0.2,0.4, 0.6, 0.8))+
  xlab("Taxa Group")+
  ylab("counts m"~L^-1)+
  #ylab("percent µg C"~copepod^-1~d^-1)+
  #coord_flip()+
  #geom_vline(xintercept=0.05, color="gray", linewidth=0.4) +
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8))+
  theme(axis.text.y = element_text(size = 6))+
  wimGraph()
p


p <- ggplot(abundance, aes(x=group_size, TotalCpmI))+
  geom_bar(stat = "identity", fill = "darksalmon")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10") +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Cells"~ mL^-1)+
  wimGraph()
p

### Plot a subset with rounded numbers of taxa group abundance per event
### example of code to keep the number on the top of the bar from
##   getting cut off:   scale_y_continuous(expand=expansion(mult=c(0,0.15))
abunRound <- abundance %>%
  mutate_at(vars(TotalCpmI), funs(round(., 0)))

p <- ggplot(subset(abunRound, samp_ev %in% "YBP1"), aes(group_size, TotalCpmI))+
  geom_bar(stat = "identity", fill = "grey")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10",
                     expand = expansion(mult = c(0, 0.1)))+ #, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 12))+
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  xlab("Taxa Groups with Sizes") +
  ylab("Cells"~ mL^-1)+
  geom_text(aes(label=TotalCpmI), position = position_dodge(width = 0.9), 
            vjust = -.25, size = 3)+
  wimGraph()
p


### Try a dot plot
p <- ggplot(data=abundance, aes(group_size, TotalCpmI)) +
  #geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = TotalCpmI>0), size = 2)+
  scale_y_continuous()+
  #scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  facet_wrap(~ samp_ev, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Cells"~mL^-1)+
  ggtitle("Abundance")+
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

### Plot taxa groups combined across all events, in Lg Sm order, with scale break
Break      <- 80
rescale    <- 25
ylabels    <- c(0, 10, 35, 75, 250, 864)
brks       <- scaleBreak(ylabels, Break, rescale)
AB_allEvents$ySquish <- scaleBreak(AB_allEvents$TotalCpmI, 
                                     Break=Break, rescale=25)
a <- ggplot(data=AB_allEvents, aes(taxaGroup, TotalCpmI)) +
  #geom_point(aes(color = taxaGroup>0), size = 4)+
  #scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(shape=16, color="navy") +
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  geom_point(data = AB_allEvents, aes(taxaGroup, ySquish),
             color="navy", size=2)+
  xlab("Taxa Groups") +
  ggtitle("Abundance")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  ylab("Cells"~mL^-1)+ # y label for biomass plots
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (16)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 11),
        axis.text.y = element_text(size = 12),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Plot top 5 abundance plus other
load("data/FinalAnalysis/CrIrAb5Cell.Rdata")

p <- ggplot(data=CrIrAb5Cell, aes(taxaGroup, TotalCpmI)) +
  #geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = TotalCpmI>0), size = 2)+
  scale_y_continuous()+
  #scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Cells"~mL^-1)+
  ggtitle("Abundance, Top 5 Taxa Groups")+
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



### Plot single taxa group by sampling event
##  Make a df with just the flagellates

abund_flag <- abundance %>% 
  filter(group_size == "FlagSm" | group_size =="FlagLg") 
abund_flag <- abund_flag %>% 
  group_by(samp_date, samp_ev) %>% 
  summarise(TotalCpmI =sum(TotalCpmI), # sum the counts per ml by group_size
            .groups = 'drop') %>% 
  as.data.frame()

p <- ggplot(abund_flag, aes(x=samp_ev, TotalCpmI))+
  geom_bar(stat = "identity", fill = "gray")+
  scale_x_discrete () +
  scale_y_continuous() +
  ggtitle("HG Flagellate Abundance")+
  xlab("Sampling Event") +
  ylab("Cells"~ mL^-1)+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 12),
        legend.position = "none")+
  wimGraph()
p

### Abundance totals, events only
load("data/Abundance/AbMnEventsOnly.Rdata")
ggplot(AbEventsOnly, aes(x=samp_ev, y=TotalCpmI))+
  geom_bar(position='stack', stat='identity')+
  #theme_bw()+
  labs(x="",title = "Abundance")+
  ylab("Cells"~mL^-1)+
  wimGraph()


### Cells per ml totals per event, with numbers on top of bars
data <- read.csv("data/Abundance/AbEventsOnly.csv")
head(data)

ggplot(data, aes(x=samp_ev, y=TotalCpmI))+
  geom_bar(position='stack', stat='identity')+
  geom_text(aes(label=TotalCpmI), position = position_dodge(width = 0.9), 
            vjust = -.25, size = 3)+
  labs(x="",title = "Abundance")+
  ylab("Cells"~mL^-1)+
  wimGraph()

### Total cells per event with numbers on top of bars
data <- read.csv("data/Abundance/AbTotalsEventsOnly.csv")
head(data)

ggplot(data, aes(x=samp_ev, y=TotalCpmI))+
  geom_bar(position='stack', stat='identity')+
  geom_text(aes(label=TotalCpmI), position = position_dodge(width = 0.9), 
            vjust = -.25, size = 3)+
  labs(x="",title = "Abundance")+
  ylab("Total Cells")+
  wimGraph()

### Total cells per taxa group, abunISumTotals.Rdata
### Round the numbers
abunTaxaRound <- abunISumTotals %>%
  mutate_at(2, round, 0)
ggplot(abunTaxaRound, aes(x=group_size, y=TotalCpmI))+
  geom_bar(position='stack', stat='identity', fill= "gray")+
  geom_text(aes(label=TotalCpmI), position = position_dodge(width = 0.9), 
            vjust = -.25, size = 3)+
  labs(x="",title = "Abundance")+
  scale_y_continuous(trans= "log10", expand = expansion(mult = c(0, 0.1)))+
  ylab("Total Cells" ~mL^-1)+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 12),
        legend.position = "none")+
  wimGraph()
