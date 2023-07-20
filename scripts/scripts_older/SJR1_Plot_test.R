###  8/26/22
###  Experimenting with plotting the SJR1 ciliate data
##   See Ct_Plots.R for all the libraries and other code

###  8/29/22

###  Due to problems figuring out how to plot the SJR 1 ciliate data, I
###   made a new data frame today, in Bmass_Mn_Ct.R and will try with it
###   See notes in Thesis Project and Analysis Notes.doc
### The new dataframe, sjr1_categories, (in Bmass_Mn_Ct.R) has just ciliates from SJR1, but 
### as opposed to sjr1_cil_fin, I went about it a different way, so that
### one of the columns would be exp, to see if I can use it to fill the bars


library(tidyverse)
library(readxl)
library(ggtext)
library(RColorBrewer)
library(ggbreak)
library(ggrepel)
#load("sjr1_cil_fin.Rdata") 

#load("ct_bmn_fin_exp")
## Don't need this now? load("all_sta_categories")
## This allows me to change the order of the samples

load("sjr1_categories")
## Don't remember if it was necessary to set the exp as factor?
sjr1_categories$exp <- factor(sjr1_categories$exp, levels=c("IC", "FC", "T24"))


ggplot(sjr1_categories)+
  (aes(fill = exp, y= tot_ct, x = category)) +
  geom_col(position = position_dodge())+
  ylim(0, 300) +
  geom_label_repel(aes(label = tot_ct), size = 3, vjust = 1.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(y = "Total Counts",
       x = "Organism Category",
       title = "SJR1 Ciliates",
       fill = "Experimental Sample") 
  #scale_y_cut(breaks = c(13, 125), which = c(1,3), scales = c(0.5, 3))
# not sure what the which and scales part does, will play with it later
# found the ggbreaks package here https://stackoverflow.com/questions/7194688/using-ggplot2-can-i-insert-a-break-in-the-axis
         
  

