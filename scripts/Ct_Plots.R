###  8/14/22  Plot the counts per organism group
##  Plot # in IC vs. # in FC
##       # in IC vs. # in T24
##       # in FC vs. # in T24

## This file also contains a lot of experimenting with how to
## get it in the format needed to be able to plot.

library(tidyverse)
library(readxl)
library(ggtext)
library(RColorBrewer)
library(writexl)
library(formattable) #found here https://www.tutorialspoint.com/how-to-format-all-decimal-places-in-an-r-vector-and-data-frame

# Install wimPalettes and wimGraphs
wimPalettes <- function (pal="point", ncolors=12, 
                         red=seq(0,1, length=ncolors), 
                         green=0, blue=seq(1,0,length=ncolors))
{
  # Selections are "gradient","point","line", "slide" as of 10/17/2016
  # Added "month" for monthly colors from linear interpolation 
  #     of blue,green,red,orange, and back to blue (January April July)
  # Gradient uses rgb for a selected number of colors ncolors.
  # Gradient also uses the red, green, and blue values above
  # Default gradient is blue to red through purple. 
  require(grDevices)
  nc <- ceiling(ncolors/7)
  palG <- rgb(red=red, green=green, blue=blue)
  palP <- rep(c("black","red","green4","blue","magenta","cyan","orange","gray"),nc)
  palL <- rep(c("red","black","green4","magenta","blue","gray","cyan","orange"),nc)
  palM <- c("#0000ff", "#0055aa", "#00aa55", "#00ff00", "#55aa00", "#aa5500", 
            "#ff0000", "#ff4400", "#ff8800", "#ffcc00", "#aa8855", "#5544aa")  # Blue..Green..Red..Orange..(blue)
  palO <- c("seagreen4", "red","darkgoldenrod4", "royalblue4","coral","gray50", "lightblue1","turquoise4","darkblue")
  palS <- rep(c("yellow","green","cyan","magenta","gray90","pink","orange"),nc)
  switch(tolower(substring(pal,1,1)), "g"=palG, "p"=palP, "l"=palL,"m"=palM, "o"=palO, "s"=palS, palP)
}

wimGraph <- function(background="white", panelBackground=NA,
                     panelBorder = "black",
                     textCol="black", titleCol="black", 
                     axisCol="black",gridCol="gray90",
                     stripCol="black", stripFill="white",
                     title.rel=1.2, axis.title.rel=1.6, axis.text.rel=1.0,
                     strip.text.rel=0.7, textSize=10)
{ 
  theme(panel.border = element_rect(colour = panelBorder, fill=NA, size=0.5),
        plot.background = element_rect(fill = background, linetype=0),
        panel.background = element_rect(fill = panelBackground, linetype=1, color=axisCol),
        legend.key = element_rect(fill = NA),
        legend.background=element_rect(fill=NA),
        text= element_text(color=textCol, size=textSize),
        panel.grid.major = element_line(color=gridCol, size=0.2),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color=stripCol, fill=stripFill),
        strip.text = element_text(color=textCol, size=rel(strip.text.rel)),
        plot.title = element_text(size=rel(title.rel), color=titleCol),
        axis.title  = element_text(size=rel(axis.title.rel), color=titleCol, vjust=0),
        axis.text = element_text(size=rel(axis.text.rel),color=axisCol),
        axis.line.x =  element_line(color=axisCol),			# Changed these two
        axis.line.y =  element_line(color=axisCol),
        axis.ticks=element_line(color=axisCol))
}

## Load the file that has the counts data
load("ct_bmn_fin_exp") # Originally created in Bmass_Mn_Ct.R
ct_bmn_fin_exp$bmn_pgcml<-formattable(ct_bmn_fin_exp$bmn_pgcml,
                                      format="f",digits=0)

## Experiment with just one sampling event
ct_plot_SJR1 <- ct_bmn_fin_exp %>% 
  filter(samp_ev == "SJR1") %>% 
  select(- samp_date, -bmn_pgcml)

write_xlsx(ct_plot_SJR1, "ct_plot_SJR1.xlsx")

## Make separate data frames with each experimental sample type
ct_plot_SJR1_IC <- ct_plot_SJR1 %>%  filter(exp == "IC")
ct_plot_SJR1_FC <- ct_plot_SJR1 %>%  filter(exp == "FC")
ct_plot_SJR1_T24 <- ct_plot_SJR1 %>%  filter(exp == "T24")

## Join them together. This joined them, but didn't align the entries
## in one experiment with the entries in another. Not all organisms are
## found in all experiment samples
#sjr1_join_if <-  left_join(ct_plot_SJR1_IC, ct_plot_SJR1_FC, by = "group", "type")
#sjr1_join_it <- left_join(ct_plot_SJR1_IC, ct_plot_SJR1_T24, by = "group")
#sjr1_join_ft <- left_join(ct_plot_SJR1_FC, ct_plot_SJR1_T24, by = "group")
#names(ct_plot_SJR1)

## Per meeting with Wim 8/15, try another kind of join
## see full_join: includes all rows in x or y
##  or inner_join : includes all rows in x and y
## inner_join duplicates a whole bunch of rows, and so does full_join
##  Try this code below from https://stackoverflow.com/questions/50476717/i-want-to-align-match-two-unequal-columns

sjr1ic <- ct_plot_SJR1_IC %>%
  mutate(ID1 = exp) %>%
  group_by(exp) %>%
  mutate(ID2 = row_number()) %>%
  ungroup()

sjr1fc <- ct_plot_SJR1_FC %>% 
  mutate(ID1 = exp) %>%
  group_by(exp) %>%
  mutate(ID2 = row_number()) %>%
  ungroup()

sjr1_join_if <- full_join(sjr1ic, sjr1fc, by = c("ID1", "ID2")) %>%
  select(-starts_with("ID")) %>%
  arrange(exp.y)

sjr1_join_if <- full_join(ct_plot_SJR1_IC, ct_plot_SJR1_FC, by = "group", "type")
#sjr1_join_it <- full_join(ct_plot_SJR1_IC, ct_plot_SJR1_T24, by = "group")
#sjr1_join_ft <- full_join(ct_plot_SJR1_FC, ct_plot_SJR1_T24, by = "group")

## Not sure if this is working, but see what happens when I pivot wider
## and combine columns into one "category" column as below
### Combine the group, type, sa, la, wi columns into one
##  Can't combine the columns at this point, because each column was 
##    duplicated and has a .x or .y added to it, unless I want to include
##    all those column names in the arguement

#sjr1_join_if$category <- paste(sjr1_join_if$group, sjr1_join_if$type,
     #                          sjr1_join_if$sa, sjr1_join_if$la)
### Get rid of the columns I no longer need

sjr1_cil_wid <- pivot_wider(sjr1_cil, id_cols = NULL, names_from = exp, 
                            values_from = tot_ct)
sjr1_cil_fin <- select(sjr1_cil_wid, -group, -type, -sa, -la, -wi) %>% 
  select(category, IC, FC, T24)

## Experiment by just taking the ciliates out of SJR1
sjr1_cil <- ct_plot_SJR1 %>% 
  filter(group == "ciliate")

## Try pivot_wider to get the IC, FC, and T24 entries on the same row.
## This worked. Now need to figure out how to translate that to everything
sjr1_cil_wid <- pivot_wider(sjr1_cil, id_cols = NULL, names_from = exp, 
                            values_from = tot_ct)
write_xlsx(sjr1_cil_wid, "sjr1_cil_wid.xlsx")

### Combine the group, type, sa, la, wi columns into one
sjr1_cil_wid$category <- paste(sjr1_cil_wid$group, sjr1_cil_wid$type,
                               sjr1_cil_wid$sa, sjr1_cil_wid$la)
### Get rid of the columns I no longer need
sjr1_cil_fin <- select(sjr1_cil_wid, -group, -type, -sa, -la, -wi) %>% 
  select(category, IC, FC, T24)
save(sjr1_cil_fin, file = "sjr1_cil_fin.Rdata")

write_xlsx(sjr1_cil_fin, "sjr1_cil_fin.xlsx")

### Experiment with plotting sjr1_cil_fin
## This looks helpful https://www.youtube.com/watch?v=wo6FSaz9AlE
ggplot(sjr1_cil_fin) +
  geom_bar(aes(fill = IC, FC, T24, y= category, x = ))+
  facet_wrap(~a)

table(sjr1_cil_wid)
table(ct_plot_SJR1$group)
table(ct_plot_SJR1$bmn_pgcml)




## Here's an example of a ggplot from the growth rates

# ggplot(growthCalculations$sampleMedians, aes((days), ugC)) +
#   geom_point(color="blue", alpha=0.5, size=1) +
#   geom_smooth(method = "lm", se = TRUE, color="red", lwd=0.2) +
#   facet_wrap(~ sample, ncol = 2, strip.position = "left") +
#   scale_y_log10(limits=c(0.05, 1), breaks=c(0.1, 0.2,.5,1)) +
#   scale_color_manual(values=wimPalettes()) +
#   geom_text(data=outAll, 
#             aes(x=0, y=1, label=paste("slope =", label), group=NULL), size=2.5, color=2, adj=0, nudge_y = -.25) +
#   xlab("Time, d") +
#   ylab(expression(paste("Carbon, ", mu,"g"))) +	
# #annotate(geom = "text", x = 1.9, y = 1.5, label = outAll$slope, fontface = 2, hjust=0) +
#   wimGraph(textSize = 10)

#