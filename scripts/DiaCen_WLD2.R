### Updated 7/12/22, see below for first version. This version uses the code
##   below to filter just centric diatoms from the controls and experimental
##  samples for one station, WLD2, but uses the updated file that contains the
##   biomass per volume in ug/L. But I removed the "Organism" column from
##   bio_per_vol_all_ord, so I'll grepl by group

## Update 7/22/22, using the source file, bio_per_vol_all_ord, that I updated today
##   to include number of bugs and experiment time

## First, double check the column names, since I edited them

names(bio_per_vol_all_ord)

## choose only the diatoms (group) and only  centric (type) and WLD2 experimental
##  samples only, no site water samples
## Note this way seems more efficient than the one I did on 7/1 below.
diacen_wld2 <- bio_per_vol_all_ord[grepl("^diatom", bio_per_vol_all_ord$group), ] %>% 
  filter("centric" == type) %>% 
  filter(str_detect(sample, "WLD2")) %>% 
  filter(!str_detect(sample, "site"))

### Now calculate the means of the replicates, use code pasted below
## to get started
# diacen_mn <-  (summarize, diacen_wld2, (dimension_count = n(), total_count = sum(counts)
                       #biomass_total = sum(biomass))

mean(diacen_wld2$bio_per_vol_ugl)

#dia_exp_ord <- dia_exp  [with(dia_exp, order(sample)), ]

## Save the file
write_xlsx(diacen_wld2, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/CentricDiatoms_WLD2_Exp.xlsx")


### 7/1/22
##  This is for filtering just centric diatoms from the controls and experimental
##  samples for one station, WLD2 using bio_all_no0, because that's the file
## that has the 100x and 400x raw counts combined
## Source file, bio_all_no0 is from 100_400_All_Bio.R

names(bio_all_no0)
diatom_all_table <- bio_all_no0[grepl("^Diatom", bio_all_no0$Organism), ] %>% 
  group_by(sample, Group, type, sa, la, wi) %>%
  summarize(dimension_count = n(), total_count = sum(counts), 
            biomass_total = sum(biomass)) 

## Now filter out the centric diatoms, this didn't work
#diacen_all_table <- bio_all_no0[grepl("^centric", bio_all_no0$type )] %>% 
 # group_by(Group, type, sa, la, wi) %>%
  #summarize(dimension_count = n(), total_count = sum(counts), 
            #biomass_total = sum(biomass))

### This worked:
diacen_all_table <- filter(bio_all_no0, grepl("Diatom", Organism),
                           type == "centric")
names(diacen_all_table)
### Now filter out just WLD2
dia_wld2 <- filter(diacen_all_table, grepl("WLD2", sample)) %>% 
  select(sample, group = Group, type, sa, la, counts, tot_vol_um3 = volume, 
         tot_biomass_pgC = biomass)

# This didn't work either                  
# dia_wld2_ctrl <- dia_wld2[grepl("^WLD2 IC-1", dia_wld2$sample)]
#  This did nothing
## dia_wld2_ctrl <- filter(dia_wld2,(sample != "WLD2 site" | sample !="^WLD2 IC"))


### Now get ride of site water samples
## Notes from https://suzan.rbind.io/2018/02/dplyr-tutorial-3/
# remove <- c("Rodentia", "Carnivora", "Primates")
# msleep %>% 
  # select(order, name, sleep_total) %>% 
  # filter(!order %in% remove)
## This worked
remove <- c("WLD2 site")
dia_exp <- dia_wld2 %>% 
  select(sample, group, type, sa, la, counts, tot_vol_um3, tot_biomass_pgC) %>% 
  filter(!sample %in% remove)

## Note 7/11. Not sure why I did this below, since the abov code removed the
##  sample column already. Maybe it was a second version I was trying?
#diafilter_test <- dia_wld2 %>% 
 # select(sample, group = Group, type, sa, la, counts, tot_vol_um3, tot_biomass_pgC) %>% 
#  filter(sample != "WLD2 site")

## Now arrange by sample? Alphabetically?
dia_exp_ord <- dia_exp  [with(dia_exp, order(sample)), ]

## Save the file
write_xlsx(dia_exp_ord, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/CentricDiatoms_WLD2.xlsx")


