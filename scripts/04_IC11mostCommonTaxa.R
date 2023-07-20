####################################################################
############# 10 MOST COMMON TAXA GROUPS IN THE INITIALS############
####################################################################

library(tidyverse)
library(writexl)
load("data/volbio_all.Rdata")
wimGraph()
wimPalettes()
library(patchwork)

### First create the IC file from volbio_all
grpCtml <- subset(volbio_all,
                   select = c(samp_ev, exp, grp_typ, counts_per_ml)) %>% 
  group_by(exp, grp_typ) %>% 
  summarise(tot_ct_ml = sum(counts_per_ml)) %>% 
  arrange (desc(tot_ct_ml))
t24 <- filter(grpCtml, exp == "T24")

### Make a dot plot
x <- fc
head(x) # shows first six rows
x <- select(x, group=grp_typ, totalCPM = tot_ct_ml) #change the names
x$seq <- factor(1:nrow(x), ordered=T, labels=abbreviate(x$group)) #makes a
# column with the grp_typ names abbreviated so they fit better on the plot
#write_xlsx(x, "data/taxaPic2abbrev.xlsx")  
p <- ggplot(x, aes(x=seq, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_discrete ("") +
  scale_y_log10("Total Counts per Ml, FC") +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("All Control Samples, Organism Group Totals")+
  theme(plot.title = element_text(size = 15))

### Make a bar plot of the top 11

library(RColorBrewer)

coul <- brewer.pal(10, "Set3")
fcTen <- fc %>% 
  slice(1:10) %>% 
  select(group=grp_typ, totalCPM = tot_ct_ml)

p <-ggplot(fcTen, aes(x=reorder(group, -totalCPM),y=totalCPM, fill=group))+
  #geom_point()+
  geom_bar(position='stack', stat='identity')+
  #theme_minimal()+
  labs(x="", title = "Controls Top 10 Total CPM")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(axis.title.y = element_text(size = 10))+
  wimGraph()
p
