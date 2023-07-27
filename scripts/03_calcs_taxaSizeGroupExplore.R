####################################################################
################ TAXA SIZE GROUPINGS EXPLORATION ###################
####################################################################
### 12/15/22
### Now that I've done the taxa groups distribution plots as well as the plot of all
## grp_typ groups together in 03_calcs_GroupsCollapse.R, 
##  and found they all fit roughly into these three categories:

# Small: 		3 - 11.54 µm
# Medium:		14 – 24   µm
# Large:		26 – 120  µm

##  Wim said I should make the boundaries whole numbers and maybe multiples of 5,
##  see my references in "How To Sort Size Categories.docx", so this code is to
##  experiment with size groups of <12, 12-24, >24, or usign 15 and 25, 
##  and see how the distribution of organisms changes all of the major groups

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

### Use the file taxaGrp.Rdata and the code from 03_calcs_GroupsCollapse.R as the source

load("data/TopTen/MainGroups/taxaGrp.Rdata")

taxaSzGrpExp <- taxaGrp
taxaSzGrpExp$szGroup <- with(taxaSzGrpExp, ifelse(esd < 12, 'small',
                              ifelse(esd > 24, 'large',
                                     ifelse(esd >= -12 & esd <= 24, "medium", "WHAT?"))))

p <- ggplot(taxaSzGrpExp, aes(x= factor(szGroup, levels=c("small", "medium", "large")),
                              totalMnCPM, color=type)) +
  geom_point(size=1) +
  # this color scale below for plotting grp_typ
  scale_color_manual(values = c("blueviolet", "blueviolet", "deeppink", "deeppink", "chartreuse3", "firebrick", 
                                "firebrick", "firebrick", "dodgerblue3", "dodgerblue3", "dodgerblue3", "orange1"))+
  # choose which jittering amount you want
  #geom_jitter(position = position_jitter(width = 0.5, height = 0.1))+
  #geom_jitter()+
  #scale_x_log10 (n.breaks=10) +
  #scale_y_log10()+
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))

p + ggtitle("Taxa Main group Types by Sm, Med, Lg")+
  theme(plot.title = element_text(size = 15))

### Do same graph with new df, taxaSzGrpExp, but graph the grp_typs separately:
##  Add the "ifelse" arguments to the individual graphs in 03_calcs_GroupsCollapse.R

