##############################################################################
############### INTIALS AND EXPERIMENTALS CPM TOTALS #########################
##############################################################################

### Code copied from 03_calcs_SiteWaterAbund.R, that already contained intial
##  and experimental samples cell abundance code.

### For intial samples top 5+other taxa group cell abundance and relative 
##  abundance, see 03_calcs_CellAbunInitTop5.R in Final/Final/Abundance/Cells/Top5

library(tidyverse)
library(writexl)


############## INTIALS CPM ABUNDANCE AND RELATIVE ABUNDANCE #####################

### Make the base df with just initials
load("data7_24/MasterFiles/MasterRFiles/volbio_all_cr.Rdata")
initialsCpm <- volbio_all_cr %>%
  filter(exp == "I") %>% 
  select(samp_ev, exp, rep, Group, type, shp, sa, la, wi, grp_sz, grp_esd, counts, cpm, taxaGroup=group_size)

### take the mean of the 3 replicates of each organism in thr grp_sz column,
##  because that is the most distinct grouping
IcpmTot <- initialsCpm %>% 
  group_by(samp_ev, Group, type, shp, sa, la, wi, grp_sz) %>% 
  mutate(cpmTotMn = mean(cpm))
### Remove the unneeded columns
IcpmTot1 <- IcpmTot %>% 
  subset(select = c(samp_ev, grp_sz, taxaGroup, cpmTotMn))
### Remove the rows that are "duplicates" because I only want to keep one
##  mean value, not the mean value for all three replicates
IcpmTot2 <- IcpmTot1 %>% distinct()
### Now sum all the replicate means in the same taxaGroup
IcpmTot3 <- IcpmTot2 %>% 
  group_by(samp_ev, taxaGroup) %>% 
  mutate(TotCpmTx = sum(cpmTotMn))
### Add a column of total cpm, all taxa, per sampling event
IcpmTot2allTxperEvent <- IcpmTot3 %>% 
  group_by(samp_ev) %>% 
  mutate(cpmTotallTxperEv = sum(cpmTotMn)) %>% 
  ungroup
### test
ybp2TxAllEvTest <- IcpmTot2allTxperEvent %>% 
  filter(samp_ev == "YBP2") %>% 
  summarise(totCpmallTx = sum(cpmTot))
### that's accurate
### Remove the columns again like above
IcpmTot4 <- IcpmTot2allTxperEvent %>% 
  subset(select= c(-grp_sz, -cpmTotMn))
### Remove the "duplicate" rows like above
IcpmTot5 <- IcpmTot4 %>% distinct()
### Calculate proportions
### Calculate the proportion of the total FR UgC that each group_size contributed to total
IcpmProp <- IcpmTot5 %>% 
  mutate(IcpmProp = TotCpmTx/cpmTotallTxperEv)
### Test IcpmProp
testIcpmProp <- IcpmProp %>% 
  filter(samp_ev =="YBP2")
### summed the prop column, it checks out, sums to 1
save(IcpmProp, file = "Final Final/Abundance/Cells/IcpmProp.Rdata")
write_xlsx(IcpmProp, "Final Final/Abundance/Cells/IcpmProp.xlsx")
load("Final Final/Abundance/Cells/IcpmProp.Rdata")
### Plot to compare with site water
source("scripts/01_function_wimGraph and Palettes.R")

### See below for example of how to order the stacks in the plot

plot_IcpmProp <-ggplot(IcpmProp, aes(fill=taxaGroup, y=IcpmProp, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", 
                               "CilLg" = "salmon3", "CilSm" = "salmon1", "FlagLg" = "#85B22C", 
                               "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlLg" = "tan", "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", 
                               "CyanoLg" = "lightgreen", "CyanoSm" = "palegreen", "DinoLg" = "cornsilk4",
                               "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", 
                               "PenDiaLg", "PenDiaSm", "ChlLg","ChlSm", "ChnDiaLg", "CyanoLg",
                               "CyanoSm", "DinoLg", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Counts per mL, Initial Samples")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()
plot_IcpmProp


### Because I wanted to see cyano lg and small, and get ChlSm in the right place
##  in the YBP2 stack because it was on top of the small flagellates.
#specify order of bars (from top to bottom)
df$fill_var <- factor(df$fill_var, levels=c('value1', 'value2', 'value3', ...))

#create stacked bar chart
ggplot(df, aes(x=x_var, y=y_var, fill=fill_var)) + 
  geom_bar(position='stack', stat='identity')

IcpmPropTest <- IcpmProp 
#specify order of bars (from top to bottom)
#convert 'position' to factor and specify level order
IcpmPropTest$taxaGroup <- factor(IcpmPropTest$taxaGroup, 
                                 levels=c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", 
                                          "FlagSm", "FlagLg","PenDiaLg", "PenDiaSm", 
                                          "ChlLg","ChlSm", "ChnDiaLg", "CyanoLg", 
                                          "CyanoSm", "DinoLg", "UnidLg", "UnidSm"))

ggplot(IcpmPropTest, aes(x=samp_ev, y=IcpmProp, fill=taxaGroup)) + 
  geom_bar(position ="stack", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", 
                               "CilLg" = "salmon3", "CilSm" = "salmon1", "FlagLg" = "#85B22C", 
                               "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlLg" = "tan", "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", 
                               "CyanoLg" = "lightgreen", "CyanoSm" = "palegreen", "DinoLg" = "cornsilk4",
                               "UnidLg" = "plum4", "UnidSm" = "plum2")) +
  # limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", 
  #           "PenDiaLg", "PenDiaSm", "ChlLg","ChlSm", "ChnDiaLg", "CyanoLg",
  #          "CyanoSm", "DinoLg", "UnidLg", "UnidSm"),
  #name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Counts per mL, Initial Samples")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()

plot_IcpmProp
