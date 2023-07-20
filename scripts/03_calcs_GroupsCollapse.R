####################################################################
############################ GROUPS COLLAPSE #######################
####################################################################
### 12/6/22
### For consolidating the organisms of the same group and type but with 
##  different dimensions into the same category, such as three size groups
##  of pennate diatoms, instead of the 200+ in the raw data. To be done with
##  all of the major groups.

### Based on previous data exploration, the main organism groups are:

## Centric diatoms
## Pennate diatoms
## Flagellate cryptomonas
## Flagellate other
## Ciliates, cone and round
## Cyanobacateria, all
## Unidentified round


library(tidyverse)
library(writexl)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")
#install.packages("plyr")
#library(plyr)

### To look at the major groups that I will want to consolidate, look at this file:
# taxaPicAll.Rdata in 03_calcs_Taxa_Grp_Exploration.R

### First add the szGroup column to the volbio_all source file:

volbio_all$szGroup <- with(volbio_all, ifelse(esd < 12, 'small',
                                                 ifelse(esd > 24, 'large',
                                                        ifelse(esd >= -12 & esd <= 24, "medium", "WHAT?"))))
#_____________________________
### Pennate Diatoms
taxaPen <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, mag,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml, szGroup)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaPen$szesd <- paste(taxaPen$grp_sz, taxaPen$esd)
taxaPen <- filter(taxaPen, grepl('pennate', szesd))
taxaPen <- subset(taxaPen,totalMnCPM !=0)
taxaPen <- subset(taxaPen, 
                  select = c(samp_ev, exp, rep, mag, szesd, esd, szGroup,  
                             totalMnCPM, totalMnBPM))

### Look at the individual pennate entries

pennateLook <- subset(volbio_all, 
                      select = c(samp_ev, exp, rep, mag, grp_sz, esd, 
                                 counts_per_ml, vol_per_org_um3, bio_per_org_pgC)) %>% 
  filter(grepl('pennate', grp_sz)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
pennateLook <- subset(pennateLook, totalMnCPM !=0)

penPlot <- subset(pennateLook, 
                  select = c(esd, totalMnCPM)) %>% 
  select(esd, totalMnCPM)
#penPlot$totalCPM<-formattable(penPlot$totalCPM,format="f",digits=2)

#sum(taxaPen$totalMnCPM)
write_xlsx(taxaPen, "data/TopTen/taxaPen.xlsx")

### Add up the counts per ml for each distinct pennate size/esd name 
##  but keep the esd  and biomass columns

taxaPenLumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                          data = taxaPen, FUN = sum, na.rm =TRUE)

taxaPenLumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaPen, FUN = sum, na.rm =TRUE)

taxaPenLump <- merge(taxaPenLumpC, taxaPenLumpB, by="szesd")

taxaPenLump<- subset(taxaPenLump, 
                     select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaPenLump)[1] = "grpSzEsd"
colnames(taxaPenLump)[2] = "esd"

write_xlsx(taxaPenLump, "data/TopTen/taxaPenLump.xlsx")
save(taxaPenLump, file = "data/TopTen/taxaPenLump.Rdata")

### Make a dot plot of esd and counts

p <- ggplot(taxaPen, aes(x=esd, totalMnCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  #scale_y_discrete("Total Counts per mL") +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Pennate Diatoms by ESD")+
  theme(plot.title = element_text(size = 15))

#______________________________
### Centric Diatoms
taxaCen <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, mag,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaCen$szesd <- paste(taxaCen$grp_sz, taxaCen$esd)
taxaCen <- filter(taxaCen, grepl('centric', szesd))
taxaCen <- subset(taxaCen,totalMnCPM !=0)
taxaCen <- subset(taxaCen, 
                  select = c(samp_ev, exp, rep, mag, szesd, esd,  
                             totalMnCPM, bio_per_org_pgC))
taxaCen$totalMnCPM<-formattable(taxaCen$totalMnCPM,
                                   format="f",digits=2)
taxaCen$totalMnBPM<-formattable(taxaCen$totalMnBPM,
                                     format="f",digits=2)

### Look at the individual centric diatom entries

centricLook <- subset(volbio_all, 
                      select = c(samp_ev, exp, rep, mag, grp_sz, esd, 
                                 counts_per_ml, vol_per_org_um3, bio_per_org_pgC)) %>% 
  filter(grepl('centric', grp_sz)) #%>% 
centricLook <- subset(centricLook, counts_per_ml !=0)
write_xlsx(centricLook, "data/TopTen/centricDiatomsESD.xlsx")

cenPlot <- subset(centricLook, 
                  select = c(esd, counts_per_ml)) %>% 
  select(esd, totalCPM=counts_per_ml)
#penPlot$totalCPM<-formattable(penPlot$totalCPM,format="f",digits=2)

#sum(taxaCen$counts_per_ml)
write_xlsx(taxaCen, "data/TopTen/taxaCen.xlsx")

### Add up the counts per ml for each distinct centric diatom
##  size/esd name but keep the esd  and biomass columns
taxaCenLumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                          data = taxaCen, FUN = sum, na.rm =TRUE)

taxaCenLumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaCen, FUN = sum, na.rm =TRUE)

taxaCenLump <- merge(taxaCenLumpC, taxaCenLumpB, by="szesd")

taxaCenLump<- subset(taxaCenLump, 
                     select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaCenLump)[1] = "grpSzEsd"
colnames(taxaCenLump)[2] = "esd"
write_xlsx(taxaCenLump, "data/TopTen/taxaCenLump.xlsx")
save(taxaCenLump, file = "data/TopTen/taxaCenLump.Rdata")

### Make a dot plot of esd and counts

x <- cenPlot
p <- ggplot(x, aes(x=esd, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  #scale_y_discrete("Total Counts per mL") +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Centric Diatoms by ESD")+
  theme(plot.title = element_text(size = 15))

#______________________________
### Flagellate Cryptomonas
taxaFlagC <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, mag,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaFlagC$szesd <- paste(taxaFlagC$grp_sz, taxaFlagC$esd)
taxaFlagC <- filter(taxaFlagC, grepl('cryptomonas', szesd))
taxaFlagC <- subset(taxaFlagC,totalMnCPM !=0)
taxaFlagC <- subset(taxaFlagC, 
                  select = c(samp_ev, exp, rep, mag, szesd, esd,  
                             totalMnCPM, totalMnBPM))
taxaFlagC$totalMnCPM<-formattable(taxaFlagC$totalMnCPM,
                                   format="f",digits=2)
taxaFlagC$bio_per_org_pgC<-formattable(taxaFlagC$totalMnBPM,
                                     format="f",digits=2)

### Look at the individual Flagellate Cryptomonas entries

flagClook <- subset(volbio_all, 
                      select = c(samp_ev, exp, rep, mag, grp_sz, esd, 
                                 counts_per_ml, vol_per_org_um3, bio_per_org_pgC)) %>% 
  filter(grepl('cryptomonas', grp_sz)) 
flagClook <- subset(flagClook, counts_per_ml !=0)
write_xlsx(centricLook, "data/TopTen/flagCrypESD.xlsx")

flagCPlot <- subset(flagClook, 
                  select = c(esd, counts_per_ml)) %>% 
  select(esd, totalCPM=counts_per_ml)

#write_xlsx(taxaFlagC, "data/TopTen/taxaFlagC.xlsx")

### Add up the counts per ml for each distinct flagellate cryptomonas
##  size/esd name but keep the esd  and biomass columns
taxaFlagCLumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                          data = taxaFlagC, FUN = sum, na.rm =TRUE)

taxaFlagCLumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaFlagC, FUN = sum, na.rm =TRUE)

taxaFlagCLump <- merge(taxaFlagCLumpC, taxaFlagCLumpB, by="szesd")

taxaFlagCLump<- subset(taxaFlagCLump, 
                     select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaFlagCLump)[1] = "grpSzEsd"
colnames(taxaFlagCLump)[2] = "esd"

write_xlsx(taxaFlagClump, "data/TopTen/taxaFlagClump.xlsx")
save(taxaFlagClump, file = "data/TopTen/taxaFlagClump.Rdata")

### Make a dot plot of esd and counts
flagCPlot <- subset(flagClook, 
                    select = c(esd, counts_per_ml)) %>% 
  select(esd, totalCPM=counts_per_ml)
x <- flagCPlot
p <- ggplot(x, aes(x=esd, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Flagellate Cryptomonas by ESD")+
  theme(plot.title = element_text(size = 15))

#______________________________
### FLAGELLATE OTHER: Run this again 12/9, now that I corrected some of the
##  flagellate raw data entries on 12/8. Done on 12/11

taxaFlagO <- subset(volbio_all, 
                    select = c(samp_ev, exp, rep, mag,
                               grp_sz, type, esd, counts_per_ml, 
                               bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)

taxaFlagO$szesd <- paste(taxaFlagO$grp_sz, taxaFlagO$esd)

taxaFlagO <- filter(taxaFlagO, grepl('flagellate', szesd))

taxaFlagO <-filter(taxaFlagO, !str_detect(szesd, "cryptomonas"))

taxaFlagO <-filter(taxaFlagO, !str_detect(szesd, "dinoflagellate"))

taxaFlagO <- subset(taxaFlagO,counts_per_ml !=0)

taxaFlagO <- subset(taxaFlagO, 
                    select = c(samp_ev, exp, rep, mag, szesd, type, esd,  
                               totalMnCPM, totalMnBPM))
taxaFlagO$totalMnCPM<-formattable(taxaFlagO$totalMnCPM,
                                     format="f",digits=2)
taxaFlagO$totalMnBPM<-formattable(taxaFlagO$totalMnBPM,
                                       format="f",digits=2)
write_xlsx(taxaFlagO, "data/TopTen/taxaFlagO.xlsx")

### Look at the individual Flagellate Other entries
### 12/12/22 I think this data frame below might be unneccessary. It seems like
##  I could just have used taxaFlagO to filter the same info.
flagOlook <- subset(volbio_all, 
                    select = c(samp_ev, exp, rep, mag, grp_sz, esd, 
                               counts_per_ml, vol_per_org_um3, bio_per_vol_pgc_ml)) %>% 
  filter(grepl('flagellate', grp_sz)) 
flagOlook <-filter(flagOlook, !str_detect(grp_sz, "cryptomonas"))
flagOlook <-filter(flagOlook, !str_detect(grp_sz, "dinoflagellate"))
flagOlook <- subset(flagOlook, counts_per_ml !=0)


### Add up the counts per ml for each distinct flagellate 
##  size/esd name but keep the esd  and biomass columns
taxaFlagOLumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                            data = taxaFlagO, FUN = sum, na.rm =TRUE)

taxaFlagOLumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                            data = taxaFlagO, FUN = sum, na.rm =TRUE)

taxaFlagOLump <- merge(taxaFlagOLumpC, taxaFlagOLumpB, by="szesd")

taxaFlagOLump<- subset(taxaFlagOLump, 
                       select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaFlagOLump)[1] = "grpSzEsd"
colnames(taxaFlagOLump)[2] = "esd"

write_xlsx(taxaFlagOLump, "data/TopTen/taxaFlagOLump.xlsx")
save(taxaFlagOlump, file = "data/TopTen/Flagellates/taxaFlagOlump.Rdata")

### Make a dot plot of esd and counts
#flagOPlot <- subset(flagOlook, 
 #                   select = c(esd, counts_per_ml)) %>% 
#  select(esd, totalCPM=counts_per_ml)

### Jitter the points and color them by type; use taxaFlagO as the data source since
##  it includes the "type" column
p <- ggplot(taxaFlagO, aes(x=esd, counts_per_ml, color = type)) +
  geom_point(size=1) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Flagellate Other by ESD")+
  theme(plot.title = element_text(size = 15))

#______________________________
### CILIATES
# Use master file volbio_all and choose the columns I want, including counts per ml
# and biomass per ml
taxaCil <- subset(volbio_all, 
                    select = c(samp_ev, exp, rep, mag,
                               grp_sz, esd, counts_per_ml, 
                               bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
# Create the column that combines the group name, dimensions and esd
taxaCil$szesd <- paste(taxaCil$grp_sz, taxaCil$esd)
# Filter out only the ciliates
taxaCil <- filter(taxaCil, grepl('ciliate', szesd))
# Remove the 0 count rows
taxaCil <- subset(taxaCil,totalMnCPM !=0)
# Choose the columns I want to keep
taxaCil <- subset(taxaCil, 
                    select = c(samp_ev, exp, rep, mag, szesd, esd,  
                               totalMnCPM, totalMnBPM))
# Format the numbers to 2 decimal places
taxaCil$totalMnCPM<-formattable(taxaCil$totalMnCPM,
                                     format="f",digits=2)
taxaCil$totalMnBPM<-formattable(taxaCil$totalMnBPM,
                                       format="f",digits=2)
# Save as an excel document
write_xlsx(taxaCil, "data/TopTen/Ciliates/taxaCil.xlsx")

### Add up the counts per ml for each distinct cilate 
##  size/esd name but keep the esd  and biomass columns
# This one has "C" in the name becasue it's for counts per ml
taxaCillumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                          data = taxaCil, FUN = sum, na.rm =TRUE)
# This one has "B" in the name because it's for biomass per ml
taxaCillumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaCil, FUN = sum, na.rm =TRUE)
# Merge counts with biomass into one data frame
taxaCillump <- merge(taxaCillumpC, taxaCillumpB, by="szesd")
# Choose 
taxaCillump<- subset(taxaCillump, 
                     select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaCillump)[1] = "grpSzEsd"
colnames(taxaCillump)[2] = "esd"

write_xlsx(taxaCillump, "data/TopTen/Ciliates/taxaCillump.xlsx")
save(taxaCillump, file = "data/TopTen/Ciliates/taxaCillump.Rdata")

### Make a dot plot of esd and counts
CilPlot <- subset(taxaCil, 
                    select = c(esd, counts_per_ml)) %>% 
  select(esd, totalCPM=counts_per_ml)

p <- ggplot(CilPlot, aes(x=esd, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Ciliates by ESD")+
  theme(plot.title = element_text(size = 15))

### Do more lumping. Put ciliate cones and ciliate rounds together;

taxaCnRn <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, mag, Group, type,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaCnRn$szesd <- paste(taxaCnRn$grp_sz, taxaCnRn$esd)
taxaCnRn <- filter(taxaCnRn, type=='cone'| type =='round', Group=='ciliate')
taxaCnRn <- subset(taxaCnRn,totalMnCPM !=0)
taxaCnRn <- subset(taxaCnRn, 
                  select = c(samp_ev, exp, rep, mag, szesd, type, esd, szesd, 
                             totalMnCPM, totalMnBPM))
taxaCnRn$totalMnCPM<-formattable(taxaCnRn$totalMnCPM,
                                   format="f",digits=2)
taxaCnRn$totalMnBPM<-formattable(taxaCnRn$totalMnBPM,
                                     format="f",digits=2)

write_xlsx(taxaCnRn, "data/TopTen/Ciliates/taxaCnRn.xlsx")

### Add up the counts per ml for each distinct cilate 
##  size/esd name but keep the esd  and biomass columns
taxaCnRnlumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                          data = taxaCnRn, FUN = sum, na.rm =TRUE)
taxaCnRnlumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaCnRn, FUN = sum, na.rm =TRUE)
taxaCnRnlump <- merge(taxaCnRnlumpC, taxaCnRnlumpB, by="szesd")

taxaCnRnlump<- subset(taxaCnRnlump, 
                     select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaCnRnlump)[1] = "grpSzEsd"
colnames(taxaCnRnlump)[2] = "esd"

write_xlsx(taxaCnRnlump, "data/TopTen/Ciliates/taxaCnRnlump.xlsx")
save(taxaCnRnlump, file = "data/TopTen/Ciliates/taxaCnRnlump.Rdata")

### Make a dot plot of esd and counts
CnRnPlot <- subset(taxaCnRn, 
                  select = c(esd, totalCPM))

p <- ggplot(CnRnPlot, aes(x=esd, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cone and Round Ciliates by ESD")+
  theme(plot.title = element_text(size = 15))


#__________________________________________________________________
### UNIDENTIFIED ROUND

taxaUnRn <- subset(volbio_all, 
                   select = c(samp_ev, exp, rep, mag, Group, type,
                              grp_sz, esd, counts_per_ml, 
                              bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaUnRn$szesd <- paste(taxaUnRn$grp_sz, taxaUnRn$esd)
taxaUnRn <- filter(taxaUnRn, type=='round', Group=='unidentified')
taxaUnRn <- subset(taxaUnRn,totalMnCPM !=0)
taxaUnRn <- subset(taxaUnRn, 
                   select = c(samp_ev, exp, rep, mag, szesd, type, esd, szesd, 
                              totalMnCPM, totalMnBPM))
taxaUnRn$totalMnCPM<-formattable(taxaUnRn$totalMnCPM,
                                    format="f",digits=2)
taxaUnRn$totalMnBPM<-formattable(taxaUnRn$totalMnBPM,
                                         format="f",digits=2)

write_xlsx(taxaUnRn, "data/TopTen/Unidentified/taxaUnRn.xlsx")

### Add up the counts per ml for each distinct cilate 
##  size/esd name but keep the esd  and biomass columns
taxaUnRnlumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                           data = taxaUnRn, FUN = sum, na.rm =TRUE)
taxaUnRnlumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                           data = taxaUnRn, FUN = sum, na.rm =TRUE)
taxaUnRnlump <- merge(taxaUnRnlumpC, taxaUnRnlumpB, by="szesd")

taxaUnRnlump<- subset(taxaUnRnlump, 
                      select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaUnRnlump)[1] = "grpSzEsd"
colnames(taxaUnRnlump)[2] = "esd"

write_xlsx(taxaUnRnlump, "data/TopTen/Unidentified/taxaUnRnlump.xlsx")
save(taxaUnRnlump, file = "data/TopTen/Unidentified/taxaUnRnlump.Rdata")

### Make a dot plot of esd and counts
UnRnPlot <- subset(taxaUnRn, 
                   select = c(esd, totalMnCPM))

p <- ggplot(UnRnPlot, aes(x=esd, totalMnCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Unidentified Round by ESD")+
  theme(plot.title = element_text(size = 15))

### Make a dot plot of sm, med,lg, and counts
p <- ggplot(taxaCyAll, aes(x= factor(szGroup, levels=c("small", "medium", "large")),
                          totalCPM, color=type)) +
  geom_point(size=1) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
  #scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cyano Aphanizo and Dolicho Sm, Med, Lg ESD")+
  theme(plot.title = element_text(size = 15))


#__________________________________________________________________
### CYANOBACTERIA APHANIZOMENON

taxaCyA <- subset(volbio_all, 
                   select = c(samp_ev, exp, rep, mag, Group, type,
                              grp_sz, esd, counts_per_ml, 
                              bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaCyA$szesd <- paste(taxaCyA$grp_sz, taxaCyA$esd)
taxaCyA <- filter(taxaCyA, type =='aphanizomenon' )
taxaCyA <- subset(taxaCyA,totalCPM !=0)
taxaCyA <- subset(taxaCyA, 
                   select = c(samp_ev, exp, rep, mag, Group, type, esd, szesd, 
                              totalCPM, totalMnBPM))
taxaCyA$totalCPM<-formattable(taxaCyA$totalCPM,
                               format="f",digits=2)
taxaCyA$totalMnBPM<-formattable(taxaCyA$totalMnBPM,
                                         format="f",digits=2)

write_xlsx(taxaCyA, "data/TopTen/Cyanobacteria/taxaCyA.xlsx")

### Add up the counts per ml for each distinct ciliate 
##  size/esd name but keep the esd  and biomass columns
taxaCyAlumpC <- aggregate(totalCPM ~ szesd +esd, 
                           data = taxaCyA, FUN = sum, na.rm =TRUE)
taxaCyAlumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                           data = taxaCyA, FUN = sum, na.rm =TRUE)
taxaCyAlump <- merge(taxaCyAlumpC, taxaCyAlumpB, by="szesd")

taxaCyAlump<- subset(taxaCyAlump, 
                      select = c(szesd, esd.x, totalCPM, totalMnBPM))
colnames(taxaCyAlump)[1] = "Group"
colnames(taxaCyAlump)[2] = "esd"
#colnames(taxaCyAlump)[3] = "totalCPM"
colnames(taxaCyAlump)[4] = "totalBPM"

write_xlsx(taxaCyAlump, "data/TopTen/Cyanobacteria/taxaCyAlump.xlsx")
save(taxaCyAlump, file = "data/TopTen/Cyanobacteria/taxaCyAlump.Rdata")

### Make a dot plot of esd and counts
CyAPlot <- subset(taxaCyA, 
                   select = c(esd, totalCPM))

p <- ggplot(CyAPlot, aes(x=esd, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cyanobacteria Aphanizomenon by ESD")+
  theme(plot.title = element_text(size = 15))

#__________________________________________________________________
### CYANOBACTERIA DOLICHOSPERMUM

taxaCyD <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, mag, Group, type,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaCyD$szesd <- paste(taxaCyD$grp_sz, taxaCyD$esd)
taxaCyD <- filter(taxaCyD, type =='dolichospermum' )
taxaCyD <- subset(taxaCyD,totalCPM !=0)
taxaCyD <- subset(taxaCyD, 
                  select = c(samp_ev, exp, rep, mag, Group, type, esd, szesd, 
                             totalCPM, totalMnBPM))
taxaCyD$totalCPM<-formattable(taxaCyD$totalCPM,
                              format="f",digits=2)
taxaCyD$totalMnBPM<-formattable(taxaCyD$totalMnBPM,
                                        format="f",digits=2)

write_xlsx(taxaCyD, "data/TopTen/Cyanobacteria/taxaCyD.xlsx")

### Add up the counts per ml for each distinct cilate 
##  size/esd name but keep the esd  and biomass columns
taxaCyDlumpC <- aggregate(totalCPM ~ szesd +esd, 
                          data = taxaCyD, FUN = sum, na.rm =TRUE)
taxaCyDlumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaCyD, FUN = sum, na.rm =TRUE)
taxaCyDlump <- merge(taxaCyDlumpC, taxaCyDlumpB, by="szesd")

taxaCyDlump<- subset(taxaCyDlump, 
                     select = c(szesd, esd.x, totalCPM, totalMnBPM))
colnames(taxaCyDlump)[1] = "Group"
colnames(taxaCyDlump)[2] = "esd"
#colnames(taxaCyDlump)[3] = "totalCPM"
colnames(taxaCyDlump)[4] = "totalBPM"

write_xlsx(taxaCyDlump, "data/TopTen/Cyanobacteria/taxaCyDlump.xlsx")
save(taxaCyDlump, file = "data/TopTen/Cyanobacteria/taxaCyDlump.Rdata")

### Make a dot plot of esd and counts
CyDPlot <- subset(taxaCyD, 
                  select = c(esd, totalCPM))

p <- ggplot(CyDPlot, aes(x=esd, totalCPM)) +
  geom_point(size=1, color="blue") +
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cyanobacteria Dolichospermum by ESD")+
  theme(plot.title = element_text(size = 15))

#__________________________________________________________________
### CYANOBACTERIA, All APHANIZOMENON, DOLICHOSPERMUM, AND OTHER

taxaCyAll <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, mag, Group, type,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)


taxaCyAll$szesd <- paste(taxaCyAll$grp_sz, taxaCyAll$esd)

taxaCyAll <- filter(taxaCyAll, grepl('cyanobacteria', Group))

taxaCyAll <- subset(taxaCyAll,totalMnCPM !=0)

taxaCyAll <- subset(taxaCyAll, 
                  select = c(samp_ev, exp, rep, mag, Group, type, esd, szesd, 
                             totalMnCPM, totalMnBPM))

taxaCyAll$totalMnCPM<-formattable(taxaCyAll$totalMnCPM,
                              format="f",digits=2)
taxaCyAll$totalMnBPM<-formattable(taxaCyAll$totalMnBPM,
                                        format="f",digits=2)

write_xlsx(taxaCyAll, "data/TopTen/Cyanobacteria/taxaCyAll.xlsx")

### Add up the counts per ml for each distinct cilate 
##  size/esd name but keep the esd  and biomass columns
taxaCyAllLumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                          data = taxaCyAll, FUN = sum, na.rm =TRUE)

taxaCyAllLumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaCyAll, FUN = sum, na.rm =TRUE)

taxaCyAllLump <- merge(taxaCyAllLumpC, taxaCyAllLumpB, by="szesd")

taxaCyAllLump<- subset(taxaCyAllLump, 
                     select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaCyAllLump)[1] = "grpSzEsd"
colnames(taxaCyAllLump)[2] = "esd"

taxaCyAllLump$grpSzEsd <- with(taxaCyAllLump, ifelse(esd < 15, 'small',
                                                 ifelse(esd > 25, 'large',
                                                        ifelse(esd >= 15 & esd <= 245, "medium", "WHAT?"))))
taxaCyAllLumpAgg <- aggregate(totalMnCPM ~ grpSzEsd, 
                            data = taxaCyAllLump, FUN = sum, na.rm =TRUE)

write_xlsx(taxaCyAllLump, "data/TopTen/Cyanobacteria/taxaCyAllLump.xlsx")
save(taxaCyAllLump, file = "data/TopTen/Cyanobacteria/taxaCyAllLump.Rdata")

### Make a dot plot of esd and counts

p <- ggplot(taxaCyAll, aes(x=esd, totalMnCPM, color=type)) +
  geom_point(size=1) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cyanobacteria, All, by ESD")+
  theme(plot.title = element_text(size = 15))

### Make a dot plot of sm, med,lg, and counts
p <- ggplot(taxaCyAll, aes(x= factor(szGroup, levels=c("small", "medium", "large")),
                                    totalCPM, color=type)) +
  geom_point(size=1) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
  #scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cyano Aphanizo and Dolicho Sm, Med, Lg ESD")+
  theme(plot.title = element_text(size = 15))

#__________________________________________________________________
### CYANOBACTERIA OTHER

taxaCyO <- subset(volbio_all, 
                    select = c(samp_ev, exp, rep, mag, Group, type,
                               grp_sz, esd, counts_per_ml, 
                               bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)

taxaCyO$szesd <- paste(taxaCyO$grp_sz, taxaCyO$esd)
taxaCyO <- filter(taxaCyO, grepl('cyanobacteria', Group))
taxaCyO <- filter(taxaCyO, type =='other' )

taxaCyO <- subset(taxaCyO,totalMnCPM !=0)

taxaCyO <- subset(taxaCyO, 
                    select = c(samp_ev, exp, rep, mag, Group, type, esd, szesd, 
                               totalMnCPM, totalMnBPM))

taxaCyO$totalMnCPM<-formattable(taxaCyO$totalMnCPM,
                                  format="f",digits=2)
taxaCyO$totalMnBPM<-formattable(taxaCyO$totalMnBPM,
                                  format="f",digits=2)

write_xlsx(taxaCyO, "data/TopTen/Cyanobacteria/taxaCyO.xlsx")

### Add up the counts per ml for each distinct cyano 
##  size/esd name but keep the esd  and biomass columns
taxaCyOLumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                            data = taxaCyO, FUN = sum, na.rm =TRUE)

taxaCyOLumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                            data = taxaCyO, FUN = sum, na.rm =TRUE)

taxaCyOLump <- merge(taxaCyOLumpC, taxaCyOLumpB, by="szesd")

taxaCyOLump<- subset(taxaCyOLump, 
                       select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaCyOLump)[1] = "grpSzEsd"
colnames(taxaCyOLump)[2] = "esd"

### Put the cyano other  in small, medium and large, alternately with 12 and 24, then
##  15 and 25 as the break points, and with the individually determined break points
## in 03_calcs_GroupCollapse.R
taxaCyOLump$grpSzEsd <- with(taxaCyOLump, ifelse(esd < 15, 'small',
                                                   ifelse(esd > 25, 'large',
                                                          ifelse(esd >= 15 & esd <= 25, "medium", "WHAT?"))))
taxaCyOLumpAgg <- aggregate(totalMnCPM ~ grpSzEsd, 
                             data = taxaCyOLump, FUN = sum, na.rm =TRUE)

write_xlsx(taxaCyOLump, "data/TopTen/Cyanobacteria/taxaCyOLump.xlsx")
save(taxaCyOLump, file = "data/TopTen/Cyanobacteria/taxaCyOLump.Rdata")

### Make a dot plot of esd and counts

p <- ggplot(taxaCyO, aes(x=esd, totalMnCPM, color=type)) +
  geom_point(size=1) +
  #geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
  scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cyanobacteria, Other by ESD")+
  theme(plot.title = element_text(size = 15))

### Make a dot plot of sm, med,lg, and counts
p <- ggplot(taxaCyO, aes(x= factor(szGroup, levels=c("small", "medium", "large")),
                           totalCPM, color=type)) +
  geom_point(size=1) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
  #scale_x_log10 (n.breaks=10) +
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Cyano Aphanizo and Dolicho Sm, Med, Lg ESD")+
  theme(plot.title = element_text(size = 15))


###______________________________________________________________
### PLOT ALL THESE GROUPS together: 
## centric diatoms, pennates, flag cryp unid rnd, flag other, 
##  ciliate round and cone, cyano aph and dol

##  Jitter them and make the groups different colors so I can compare the
##  distribution of sizes

taxaGrp <- subset(volbio_all, 
                   select = c(samp_ev, exp, rep, mag, Group, type, grp_typ,
                              grp_sz, esd, counts_per_ml, 
                              bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaGrp$szesd <- paste(taxaGrp$grp_sz, taxaGrp$esd)
taxaGrp <- filter(taxaGrp, Group %in% c("diatom", "cyanobacteria", "ciliate", 
                                        "flagellate", "unidentified")) %>% 
  filter(type %in% c("cone", "other", "round", "aphanizomenon", "dolichospermum",
                     "centric", "pennate", "pennate fragillaria","pennate pleurosigma",
                     "colonial","cryptomonas"))
taxaGrp <- filter(taxaGrp, !grp_typ %in%  c("ciliate other", "unidentified other", "cyanobacteria other",
                                            "diatom other"))
taxaGrp <- subset(taxaGrp,totalMnCPM !=0)
taxaGrp <- subset(taxaGrp, 
                   select = c(samp_ev, exp, rep, mag, Group, type, grp_typ, esd, szesd, 
                              totalMnCPM, totalMnBPM))
taxaGrp$totalMnCPM<-formattable(taxaGrp$totalMnCPM,
                               format="f",digits=2)
taxaGrp$totalMnBPM<-formattable(taxaGrp$totalMnBPM,
                                         format="f",digits=2)

write_xlsx(taxaGrp, "data/TopTen/MainGroups/taxaGrp.xlsx")
save(taxaGrp, file = "data/TopTen/MainGroups/taxaGrp.Rdata")


### Add up the counts per ml for each distinct cilate 
##  size/esd name but keep the esd  and biomass columns
taxaGrplumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                           data = taxaGrp, FUN = sum, na.rm =TRUE)
taxaGrplumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                           data = taxaGrp, FUN = sum, na.rm =TRUE)
taxaGrplump <- merge(taxaGrplumpC, taxaGrplumpB, by="szesd")

taxaGrplump<- subset(taxaGrplump, 
                      select = c(szesd, esd.x, totalMnCPM, totalMnBPM))
colnames(taxaGrplump)[1] = "Group"
colnames(taxaGrplump)[2] = "esd"
#colnames(taxaGrplump)[3] = "totalMnCPM"
#colnames(taxaGrplump)[4] = "totalMnBPM"

#write_xlsx(taxaGrplump, "data/TopTen/Cyanobacteria/taxaGrplump.xlsx")
#save(taxaGrplump, file = "data/TopTen/Cyanobacteria/taxaGrplump.Rdata")

### Make a dot plot of esd and counts
# see this for color scale http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

p <- ggplot(taxaGrp, aes(x=esd, totalMnCPM, color=grp_typ)) +
  geom_point(size=1) +
  # this color scale below for plotting grp_typ
  scale_color_manual(values = c("blueviolet", "blueviolet", "deeppink", "deeppink", "chartreuse3", "firebrick", 
                              "firebrick", "firebrick", "dodgerblue3", "dodgerblue3", "dodgerblue3", "orange1"))+
  # choose which jittering amount you want
  geom_jitter(position = position_jitter(width = 0.5, height = 0.1))+
  #geom_jitter()+
  scale_x_log10 (n.breaks=10) +
  #scale_y_log10()+
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Taxa Main group Types by ESD (0.5 jitter)")+
  theme(plot.title = element_text(size = 15))

### Plot with small,medium, and large size groups
taxaSzGrpExp$szGroup <- with(taxaSzGrpExp, ifelse(esd < 12, 'small',
                                                  ifelse(esd > 24, 'large',
                                                         ifelse(esd >= -12 & esd <= 24, "medium", "WHAT?"))))
p <- ggplot(taxaSzGrpExp, aes(x=szGroup, totalMnCPM, color=grp_typ)) +
  geom_point(size=1) +
  # this color scale below for plotting grp_typ
  scale_color_manual(values = c("blueviolet", "blueviolet", "deeppink", "deeppink", "chartreuse3", "firebrick", 
                                "firebrick", "firebrick", "dodgerblue3", "dodgerblue3", "dodgerblue3", "orange1"))+
  wimGraph()+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, 
                                   vjust = 0.2, size = 8))
p + ggtitle("Taxa Main group Types by Sm, Med, Lg")+
  theme(plot.title = element_text(size = 15))
#diatomSearch <- filter(volbio_all, grepl("diatom", Group))
#diatomSearch  <-subset(diatomSearch, type="")
#unique(diatomSearch$type)                       
                       