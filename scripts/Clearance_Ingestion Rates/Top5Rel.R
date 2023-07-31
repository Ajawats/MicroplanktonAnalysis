
##Data Analysis


## -----------------------------------------------------------------------------------------------------
## Packages

# Load required libraries
library(tidyverse)
library(ggplot2)


## -----------------------------------------------------------------------------------------------------
## Dataset

##load data

df <- read.csv("baseTop5kept.csv")
head(df)
 df <- read.csv("data7_24/Clearance Rates 2/IrTotAllTaxaKeptPropNew.csv")

## -----------------------------------------------------------------------------------------------------
##  Data Cleaning

##remove missing values

df <- na.omit(df)

head(df)


## -----------------------------------------------------------------------------------------------------
## Analysis


###  Consolidating "Other" taxaGroups and calculating "PropIRBuTaxaPerEvent"



# Consolidate "Other" taxaGroups and calculate "PropIRBuTaxaPerEvent"
df2 <- df %>%
  group_by(event, taxaGroup) %>%
  summarise(TotalFRUgMn = sum(FRUgMn),
            IrTotUgCEvent = sum(IrTotUgCEvent)) %>%
  mutate(taxaGroup = ifelse(taxaGroup %in% c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"), as.character(taxaGroup), "Other"),
         PropIRBuTaxaPerEvent = TotalFRUgMn / IrTotUgCEvent)

df2


## -----------------------------------------------------------------------------------------------------
### Visualization

# Defining colors for the taxaGroups
taxa_colors <- c("CenDiaLg" = "cornflowerblue",
                 "CenDiaSm" = "lightskyblue",
                 "CilLg" = "salmon3",
                 "CilSm" = "salmon1",
                 "FlagSm" = "#c3E57E",
                 "Other" = "peachpuff")

# Creating the stacked bar plot
ggplot(df, aes(x = event, y = PropIRrBuTaxaPerEvent, fill = taxaGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = taxa_colors) +
  ggtitle("Taxa Group Relative Biomass Ingestion Rates")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Event",
       y = "PropIRBuTaxaPerEvent",
       fill = "Taxa Group")


