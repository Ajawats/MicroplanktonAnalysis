####################################################################
############### CENTRIC DIATOM EXPLORATION #########################
####################################################################
### 11/28/22

### Use code from 03_Taxa_Grp_Exploration.R as source code
### Use centric diatom file from 03_Taxa_Groups_ESD.R as source file

load("data/ctsum_rep_mnesd_no0.Rdata")
diacen_esd <- ctsum_rep_mnesd_no0[grep("diatom centric", 
                                       ctsum_rep_mnesd_no0$grp_esd), ]
### Select only the sampling event, organisms and mean counts, and arrange
##  in descending order
diacenPic <- subset(diacen_esd, select = c(samp_ev, grp_esd, mn_ct_ml)) %>% 
  group_by(samp_ev, grp_esd) %>% 
  arrange (desc(mn_ct_ml))

### Sum up counts by
diacenPic <- subset(diacen_esd, select = c(samp_ev, grp_esd, mn_ct_ml)) %>% 
  group_by(samp_ev, grp_esd) %>% 
  arrange (desc(mn_ct_ml))


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
arrange (desc(tot_ct_ml))