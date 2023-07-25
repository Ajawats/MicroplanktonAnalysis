

##Visualization

## -----------------------------------------------------------------------------------------------------
##load library
library(ggplot2)


## -----------------------------------------------------------------------------------------------------
#load data

data <- read.csv("data/Clearance Rates 2/IrTotAllTaxaKeptProp.csv")
head(data)



## -----------------------------------------------------------------------------------------------------
#clean the data:remove the  rows with N/A values
data <- na.omit(data)
head(data)


## -----------------------------------------------------------------------------------------------------
Taxa.Group <- data$group_size
# plot the data
ggplot(data, aes(fill=Taxa.Group, y=PropIRbioUgC, x=event)) + 
    geom_bar(position="fill", stat="identity")+
xlab(NULL)+
ylab("Proportion of Whole")+
  ggtitle("Taxa Group Relative Biomass Ingestion Rates")+
  theme(plot.title = element_text(hjust = 0.5))


### Above is original from Odinga K on fiverr. Below are variations I made
data <- IrTotAllTaxaKeptProp
### Using above, change colors and add Wim Graph
# this didn't work for me:library(paletteer) 
# this didn't work for me: library(basetheme)
# this didn't work for me: paletteer_d("basetheme::brutal")
source("scripts/01_function_wimGraph and Palettes.R")
ggplot(data, aes(fill=group_size, y=PropIRbioUgC, x=event)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                                "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                                "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Biomass Ingestion Rates")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()

### Plot with Top 5 and Other categories
ggplot(Top5CR_IRbio_mn, aes(fill=Taxa.Group, y=PropIRbioUgC, x=event)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cadetblue", "CenDiaSm" = "cadetblue3", "CilLg" = "#FFCA99", "CilSm" = "#FFE5CC",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "#E57E7E", "PenDiaSm" = "#FFB2B2",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "mediumvioletred", "UnidLg" = "#6551CC", "UnidSm" = "mediumpurple1"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"))+
  xlab(NULL)+
  ylab("Relative Biomass Ingestion")+
  ggtitle("Taxa Group Relative Biomass Ingestion Rates")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()

### Try with proportion of taxa per event
Taxa.Group <- IrTotAllTaxaKeptProp$group_size
# plot the data
ggplot(IrTotAllTaxaKeptProp, aes(fill=group_size, y=PropIRrBuTaxaPerEvent, x=event)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("Sampling Event")+
  ylab("Taxa Group Relative Ingestion Rate per Event")+
  ggtitle("Taxa Group Relative Biomass Ingestion Rates Per Event")+
  theme(plot.title = element_text(hjust = 0.5))

### Bar plot of proportion of total IR bio that each EVENT made up
ggplot(IrTotAllTaxaKeptProp, aes(x=event, y=PropIrBuEvent)) + 
  geom_bar(stat="identity")+
  xlab("Sampling Event")+
  ylab("Relative Ingestion Rate, µg C"~d^-1)+
  ggtitle("Biomass Ingestion Rates By Event")+
  theme(plot.title = element_text(hjust = 0.5))+
  wimGraph()
### Stacked bar plot, need dummy data for x, and need a df with only the event and taxa prop
EventPropofAll <- IrTotAllTaxaKeptProp %>% 
  mutate(NewCol = "Biomass") %>% 
  select(event, PropIrBuEvent, NewCol)
duplicated(EventPropofAll)
EventPropofAll <- EventPropofAll %>% distinct()
ggplot(EventPropofAll, aes(x = NewCol, y = event, fill = PropIrBuEvent))+
  geom_col()+
  xlab("Sampling Event")+
  ylab("Relative Ingestion Rate, µg C"~d^-1)+
  ggtitle("Biomass Ingestion Rates By Event")+
  theme(plot.title = element_text(hjust = 0.5))+
  wimGraph()
ggplot(EventPropofAll, aes(fill=PropIrBuEvent, y=event, x=NewCol)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("Biomass IR Proportion of Total")+
  ylab("Sampling Event")+
  ggtitle("Taxa Group Relative Biomass Ingestion Rates")+
  theme(plot.title = element_text(hjust = 0.5))

### Bar plot of proportion of total IR bio that each TAXA GROUP made up
ggplot(IrTotAllTaxaKeptProp, aes(x=group_size, y=PropIrBuTaxaTot)) + 
  geom_bar(stat="identity")+
  xlab("Taxa Group")+
  ylab("Relative Ingestion Rate, µg C"~copepod^-1~d^-1)+
  ggtitle("Biomass Ingestion Rates By Event")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 6))+
  wimGraph()

### Stacked bar plot, need dummy data for x, and need a df with only the event and taxa prop
TaxaPropofAll <- IrTotAllTaxaKeptProp %>% 
  mutate(NewCol = "Biomass") %>% 
  select(group_size, IrTotUgCTaxa, NewCol)
duplicated(TaxaPropofAll)
TaxaPropofAll <- TaxaPropofAll %>% distinct()
write_xlsx(TaxaPropofAll, "data/Clearance Rates 2/Feeding Rates/TaxaPropofAll.xlsx")
