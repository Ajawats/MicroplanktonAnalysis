####################################################################################
######### PROPORTION OF WHOLE, IR BIO, BY TAXA AND BY EVENTS, SINGLE BAR #########
#####################################################################################
## 7/13/23

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")

### I entered by hand the data from data7_24/Clearance Rates 2/IrTotAllTaxaKeptProp.Rdata,
##  created in 03_calcs_Proportion of TotIR per Taxa.R

### For the proportion of IR bio that each event contributed to the total
EventPropIR <- data.frame(Event = c("LSZ2", "SJR1", "SJR2", "WLD2", "YBP1", "YBP2"),
                              PropIrBuEvent = c(19, 16.3, 2, 1.9, 1.2, 60))
EventPropIR<- EventPropIR %>% 
  mutate(NewCol = "Biomass")

ggplot(EventPropIR, aes(x = NewCol, y = PropIrBuEvent, fill = Event)) +
  geom_col() +
  geom_text(aes(label = paste0(PropIrBuEvent, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Proportion") +
  xlab("Biomass Ingestion Rate")+
  ggtitle("Relative Biomass Ingestion Rates by Event")+
  theme( axis.title.y = element_text(size = 11.5),
         axis.title.x = element_text(size = 11.5),
         plot.title = element_text(hjust = 0.5, size = (13)))

### For the proportion of IR bio that each taxa group contributed to the total
TaxaGroupPropIR <- data.frame(TaxaGroup = c("CenDiaLg", "CenDiaSm", "ChlLg","ChlSm", "ChnDiaLg", "CilLg", "CilSm",
                                            "FlagLg", "FlagSm", "PenDiaLg","PenDiaSm", "UnidLg", "UnidSm"),
                              IrTotUgTaxa = c(29, 9, 0, 0.1, 0.6, 41,
                                              3, 2.5, 1.9, 1, 0.2,
                                              0.03, 0.6))
TaxaGroupPropIR<- TaxaGroupPropIR %>% 
  mutate(NewCol = "Biomass")

ggplot(TaxaGroupPropIR, aes(x = NewCol, y = IrTotUgTaxa, fill = TaxaGroup)) +
  geom_col() +
  geom_text(aes(label = paste0(IrTotUgTaxa, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 10) +
  ylab("Proportion") +
  xlab("Biomass Ingestion Rate")+
  ggtitle("Relative Biomass Ingestion Rates by Taxa Group")+
  theme(plot.title = element_text(hjust = 0.5))


#rTotUgTaxa = c(2.919515236, 0.949897624, 0.000000000, 0.011077528, 0.069381708, 4.079082912,
              # 0.300737544, 0.252415913, 0.194649114, 0.099935246, 0.024637879,
             #  0.003391618, 0.060215009))


#c(0.190876060 ,0.163076718, 0.019688080, 0.008772464, 0.012594842, 0.602266095))
sum(0.190876060 ,0.163076718, 0.019688080, 0.008772464, 0.012594842, 0.602266095)
sum(2.919515236, 0.949897624, 0.000000000, 0.011077528, 0.069381708, 4.079082912,
    0.300737544, 0.252415913, 0.194649114, 0.099935246, 0.024637879,
    0.003391618, 0.060215009)
