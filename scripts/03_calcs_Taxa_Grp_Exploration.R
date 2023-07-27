####################################################################
############### SEE WHAT ARE THE MOST COMMON TAXA GROUPS############
####################################################################

### 11/20/22
### From Wim's email reply 11/19/22: For the whole data frame, use the standard
##  tidyverse method for grouping data by two variables, samp_ev and taxon,
##  summing the counts/ml for each event and taxon group, using arrange to sort
## by the taxa (decreasing order), and see what comes out.

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")
library(patchwork)

taxa_pic <- subset(volbio_all,
                   select = c(samp_ev, grp_typ, counts_per_ml)) %>% 
  group_by(samp_ev, grp_typ) %>% 
  summarise(tot_ct_ml = sum(counts_per_ml)) %>% 
  arrange (desc(tot_ct_ml))
save(taxa_pic, file = "data/TopTen/taxa_pic.Rdata")

#na_rows <- taxa_pic[!complete.cases(taxa_pic$grp_typ), ]

### 12/1/22 Recreate the list of all taxa totals I made and can't find now
# remove the sampling event column
taxaPicAll <- subset(taxa_pic, select = c(grp_typ, tot_ct_ml)) 
#na_rows <- taxaPicAll[!complete.cases(taxaPicAll$grp_typ), ]

# add up all the total counts per ml for each grp_typ organism
taxaPicAll <- aggregate(taxaPicAll$tot_ct_ml, list(taxaPicAll$grp_typ), sum)
# rename the columns
colnames(taxaPicAll) <- c("Group", "totalCPM")
# reduce the number of decimal places
taxaPicAll$totalCPM<-formattable(taxaPicAll$totalCPM,format="f",digits=2)
# arrange in descending order by total counts per milliliter
taxaPicAll <- arrange(taxaPicAll, desc(totalCPM))
save(taxaPicAll, file = "data/taxaPicAllTotCPM.Rdata")
write_xlsx(taxaPicAll, "data/taxaPicAllTotCPM.xlsx")

### Stacked bar graph of the total counts per ml, by samp_ev, filled
##  with the grp_typ organisms
ggplot(data=taxa_pic, aes(samp_ev, tot_ct_ml, fill = grp_typ))+
  geom_bar(position='fill', stat='identity')+
  #theme_bw()+
  labs(x="", title = "Taxa Grps by Samp_ev ESD")+
  scale_y_continuous(limits = )
 

### Look only at taxa and total counts per ml, in descending order
taxa_pic_2 <- subset(taxa_pic, select = c(grp_typ, tot_ct_ml)) %>% 
  group_by( grp_typ) %>% 
  summarise(total_cts_per_ml = sum(tot_ct_ml)) %>% 
  arrange(desc(total_cts_per_ml))
write_xlsx(taxa_pic_2, "data/taxa_pic_2.xlsx")

### From meeting with Wim 11/22/22, graph taxa_pic_2 this way (this worked
##  really well):
x <- taxa_pic_2
head(x) # shows first six rows
x <- select(x, group=grp_typ, totalCPM = total_cts_per_ml) #change the names
x$seq <- factor(1:nrow(x), ordered=T, labels=abbreviate(x$group)) #makes a
# column with the grp_typ names abbreviated so they fit better on the plot
write_xlsx(x, "data/taxaPic2abbrev.xlsx")  
p <- ggplot(x, aes(x=seq, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_discrete ("") +
  scale_y_log10("Total Counts per Ml, all samples") +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
### Note: On 11/26 I used "options(scipen = 10)"+" to prevent the 
## y axis numbers from being in scientific notation. When I ran it on
##  11/28,this caused an error even though it worked on 11/26. 
## : Error in `ggplot_add()`:
#! Can't add `o` to a <ggplot> object. So I removed that line of code
## and it worked just fine. Could have been a conflict in packages?
p

p +  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                      vjust = 0.2, size = 8))
  #theme(axis.text.x = element_text(hjust = .1))
  #theme(axis.text.x = element_text(vjust=.5))+ If I use angle=60, I need
  ## to use vjust so the labels aren't over the x-axis line, but if I use 
  ## angle=90, they don't overlap, so vjust isn't necessary
  

#________________________________-
### Graph using facet wrap so that there are separate graphs for each sampling event
#define order for plots
taxa_pic <- within(taxa_pic, class <- factor(class, 
                                             levels = c("SJR1", "LSZ2", "YBP2",
                                                        "YBP1", "WLD2","SJR2")))
ggplot(taxa_pic, aes(grp_typ, tot_ct_ml))+
  geom_bar(position='fill', stat='identity')+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~ samp_ev, nrow = 2, scales = "free_y")+
  theme_bw()
  #scale_y_continuous(limits =  c(0, 400))
  #theme_bw()+
  #labs(x="", title = "Taxa Grps by Samp_ev ESD")

### Trying again
x <- taxa_pic
head(x) # shows first six rows
x <- select(x, group=grp_typ, totalCPM = tot_ct_ml, sampEvent = samp_ev)
#change the names
x$seq <- factor(1:nrow(x), ordered=T, labels=abbreviate(x$group))
ggplot(x, aes(group, totalCPM))+
  geom_point()+
  facet_wrap(vars(sampEvent), scales="free")+
  theme_bw()
#scale_y_continuous(limits =  c(0, 400))
#theme_bw()+
#labs(x="", title = "Taxa Grps by Samp_ev ESD")a
write_csv(x, "data/x.csv")

ggplot(taxa_pic, aes(samp_ev, tot_ct_ml, group=grp_typ))+
  geom_col(aes(fill=grp_typ))+
  facet_wrap(~ samp_ev, nrow = 3, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90))+
  theme_bw()

#scale_y_continuous(limits =  c(0, 400))
#theme_bw()+
#labs(x="", title = "Taxa Grps by Samp_ev ESD")
### From Wim's email, try to graph using facet_wrap, see his Longfin smelt
##  data and graphs

#ggplot(datasetname, aes(xvariable, yvariable) ) +
 # geom_...  +
 # geom...    +
  #facet_wrap (~eventVariable, nrow=1, scales="free_y") +

plotGrp <-  ggplot(taxa_pic, aes(samp_ev, grp_typ, color=rainbow)) +
  geom_bar(position='fill', stat='identity')#+
  #xlab("Slope of X2 relationship, km\U207B\U00B9") +
  
plotCt <-  ggplot(taxa_pic, aes(samp_ev, tot_ct_ml, color=rainbow)) +
  geom_bar(position='fill', stat='identity')

library(patchwork)

plotOK <- plotGrp + plotCt
plotOK 
  
### Add up all the taxa groups tot_ct_ml
#taxa_tot = taxa_pic_2
#taxa_tot <- group_by( grp_typ) %>% 
  #summarise(totals = sum(tot_ct_ml))
#taxa_pic_3 <- arrange(taxa_pic_2, desc(totals))
### Now just look at the taxa and their total counts per ml
#taxa_pic_3 <- arrange(taxa_pic_2, desc(totals))


### An example of a bar chart, geom_bar, or a point-line chart, geom_point
#ggplot(ic_taxa_mn_10, aes(x=grp_esd, y=mn_ct_ml))+
 # geom_point()+
  #geom_bar(position='stack', stat='identity')+
 # theme_bw()+
 # labs(x="", title = "IC Top 10 MnCts/mL Taxa Grps ESD")+
 # theme(axis.text.x = element_text(angle = 90))+
 # coord_flip()
### With some other handy things
#theme(axis.text.x = element_text(), 
#axis.text.y = element_text(size=10),
#title = element_text(size=10))

##________________________________
### 12/2/22
### Create a file that lists all the taxa/size groups, with a corresponding
##  column for esd

taxa_list <- 
  subset(volbio_all,
         select = c( grp_sz, esd, counts_per_ml, 
                     bio_per_vol_pgc_ml))
### Round the ESD numbers to 2 decimal places
library(formattable) #found here https://www.tutorialspoint.com/how-to-format-all-decimal-places-in-an-r-vector-and-data-frame
taxa_list$esd<-formattable(taxa_list$esd,format="f",digits=2)

### Put the esd measurement into the grp_sz column so I can see them together
taxa_list$Group <- paste(taxa_list$grp_sz, taxa_list$esd)

## Add up the counts per ml for each row
taxa_list <- group_by(taxa_list, Group, counts_per_ml) %>% 
  summarise(tot_ct_ml = sum(counts_per_ml))%>% 
  arrange (desc(tot_ct_ml))

### Combine all the matching Group organisms and add up their totals
taxa_list <- aggregate(taxa_list$tot_ct_ml, list(taxa_list$Group), sum) 
unique(taxa_list$Group.1)
# this line below, I thought I could also do the same for biomass, but
## it didn't work
#aggregate(taxa_list$bmass_pgCml, list(bmass_pgCml$Group), sum)

# Need to rename the columns afterwards because it changes them 
colnames(taxa_list) <- c("Group", "totalCPM")

### Get rid of the rows that have 0 counts
taxa_list <-subset(taxa_list, totalCPM!="0")

### Make another list in descending order of counts
taxa_list_byCount <- arrange(taxa_list, (desc(totalCPM)))

#________________________________________________________________
### SINGLE OUT SOME TAXA

# dolichospermum
taxaListDolicho <- group_by(volbio_all, samp_ev, exp, rep, grp_typ)
taxaListDolicho <- subset(taxaListDolicho, 
                          grp_typ == "cyanobacteria dolichospermum")
taxaListDolicho <- subset(taxaListDolicho,counts !=0)
taxaListDol <- subset(taxaListDolicho, 
                      select = c(samp_ev, exp, rep, grp_typ, 
                                 counts_per_ml))
write_xlsx(taxaListDol, "data/TopTen/taxaListDol.xlsx")

#pennate diatoms
taxaPen <- subset(volbio_all, select = c(samp_ev, exp, rep, mag, grp_sz, esd, 
                                         counts_per_ml, bio_per_org_pgC))
taxaPen$Group <- paste(taxaPen$grp_sz, taxaPen$esd)
taxaPen <- filter(taxaPen, grepl('pennate', Group))
taxaPen <- subset(taxaPen,counts_per_ml !=0)
taxaPen <- subset(taxaPen, 
                      select = c(samp_ev, exp, rep, mag, Group, esd,  
                                 counts_per_ml, bio_per_org_pgC))
taxaPen$counts_per_ml<-formattable(taxaPen$counts_per_ml,format="f",digits=2)
taxaPen$bio_per_org_pgC<-formattable(taxaPen$bio_per_org_pgC,format="f",digits=2)

## 12/6/22 take a look at the individual pennate entries

pennateLook <- subset(volbio_all, 
                      select = c(samp_ev, exp, rep, mag, grp_sz, esd, 
                                 counts_per_ml, vol_per_org_um3, bio_per_org_pgC)) %>% 
  filter(grepl('pennate', grp_sz)) #%>% 
pennateLook <- subset(pennateLook, counts_per_ml !=0)
penPlot <- subset(pennateLook, 
                  select = c(esd, counts_per_ml)) %>% 
  select(esd, totalCPM=counts_per_ml)
#penPlot$totalCPM<-formattable(penPlot$totalCPM,format="f",digits=2)

#sum(taxaPen$counts_per_ml)
write_xlsx(taxaPen, "data/TopTen/taxaPen.xlsx")

### Add up the counts per ml for each distinct pennate size/esd name 
##  but keep the esd  and biomass columns
taxaPenLump <- group_by(taxaPen, Group, esd, mag, bio_per_org_pgC) %>% 
  summarise(totalCPM =sum(counts_per_ml)) %>% 
  summarise(totalBioM =sum(bio_per_org_pgC))

unique(taxaPenLump$esd)
write_xlsx(taxaPenLump, "data/TopTen/taxaPenLump.xlsx")
save(taxaPenLump, file = "data/TopTen/taxaPenLump.Rdata")

# This below dropped all the other columns
#taxaPenLump <- aggregate(taxaPen$counts_per_ml, list(taxaPen$Group), sum)+
#colnames(taxaPenLump) <- c("Group", "totalCPM")  

#group_by(taxaPen, grp_typ, esd, counts_per_ml) %>% 
  #summarise(tot_ct_ml = sum(counts_per_ml))%>% 
  #arrange (desc(tot_ct_ml))

#taxaCen 

write_xlsx(taxa_list, "data/taxa_list.xlsx")
write_xlsx(taxa_list_byCount, "data/taxa_list_byCount.xlsx")

#______________________________
### Make a dot plot of esd and counts

x <- penPlot
p <- ggplot(x, aes(x=esd, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  #scale_y_discrete("Total Counts per mL") +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Pennate Diatoms by ESD")+
  theme(plot.title = element_text(size = 15))



