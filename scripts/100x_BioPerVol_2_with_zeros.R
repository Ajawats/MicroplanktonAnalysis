### 9/1/22 Using the code from 100x_BioPerVol.R to rerun everything with zeros to see
###   if it will then include in later analyses the zero counts for organisms
###   that weren't found in the T24 samples.
### 7/11/22
### This code contains the biomass per volume calculations of 100x counts
##  It uses the raw counts file and merges with it the headers file that
##  includes the preservation factor and volume of sample settled.
## It also includes separating out the sample names into three columns,
## one for sampling event, one for experiment type, and one for replicate.
## This is necessary for when calculating the mean biomass of FC samples,
##  and when summing the total counts of organisms per experiment across reps.

library(tidyverse)
library(writexl)
getwd()

#Load the excel csv data file

raw100 <- read_csv("data/100x_RawCount_R.csv")

#Convert to long format

raw100_long <- pivot_longer(data = raw100,
                            cols = c(9:68),
                            names_to = "sample",
                            values_to = "counts")

## Rearrange the column orders to have the station/sample column first, 
##  using the select function, and then change the NAs to 0s using the 
##  mutate function.

## When running raw100_fin below, "error in select... unused arguments" 
## occurred because the select() function from the MASS package clashes 
## with the select() function from the dplyr package. 

## The easiest way to fix this error is to explicitly tell R to use the 
## select() function from the dplyr package by adding "dplyr::" before 
## function, like this: dplyr::select

## Update on 8/8/22 But this only happens if Wim's "segmented" package has been 
##   uploaded, and MASS is in that.
## Since I don't  need to use Bquick, I don't need to upload segmented,
##   so this is no longer an issue

raw100_fin <- mutate(raw100_long, counts = ifelse(is.na(counts), 0, counts)) %>% 
  select(sample, Organism, Group, type, name, shp, sa, la, wi, counts)

##  Run the volume function

vol_func <- function(diameter, height, width, shape, counts) {
  pi= 3.1415
  volume = 0
  if(grepl("cone1", shape, ignore.case = TRUE)){
    volume = (pi/12) * diameter^2 * height
  }
  if(grepl("cones2", shape, ignore.case = TRUE)){
    volume = 2*(pi/12) * diameter^2 * height
  }
  if(grepl("sph", shape, ignore.case = TRUE)){
    volume = (pi/6) * diameter^3
  } 
  if(grepl("prosph", shape, ignore.case = TRUE)){
    volume = (pi/6) * diameter^2 * height
  }
  if(grepl("cyl", shape, ignore.case = TRUE)){
    volume = (pi/4) * diameter^2 * height
  }
  if(grepl("ellips", shape, ignore.case = TRUE)){
    volume = (pi/6) * diameter * height * width
  }
  if(grepl("recbox", shape, ignore.case = TRUE)){
    volume = diameter * height * width
  }
  if(grepl("prisell", shape, ignore.case = TRUE)){
    volume = (pi/4) * diameter * height * width
  }
  if(grepl("prispar", shape, ignore.case = TRUE)){
    volume = 0.5 * diameter * height * width
  }
  if(volume == 0){
    return(NA)
  } else{
    return(volume * counts)
  }
}

##  Calculate volumes

vol100 <- rowwise(raw100_fin) %>% 
  mutate(volume = vol_func(diameter = sa, height = la, width = wi, 
                           shape = shp, counts = counts))


##  If you want to add a column that just calculates volume of one organism 
##  and not mulitplying it by the counts, see the file 100x_Corrected_06_17 
##  in the 100x June 2022 file.

#  Add the biomass function

biomass_func <- function(Group, volume) {
  biomass = 0
  if(grepl("ciliate", Group, ignore.case = TRUE)){
    biomass = .23 * volume^.984
  }
  if(grepl("tintinnid", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("chlorophyte", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("diatom", Group, ignore.case = TRUE)){
    biomass = .117 * volume^.881
  }
  if(grepl("dinoflagellate", Group, ignore.case = TRUE)){
    biomass = .760 * volume^.819
  }
  if(grepl("flagellate", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("ochrophyte", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("unidentified", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("cyanobacteria", Group, ignore.case = TRUE)){
    biomass = .181 * volume
  }
  if(biomass == 0){
    return(NA)
  } else{
    return(biomass)
  }
}


## Calculate Biomass  

bio100 <- rowwise(vol100) %>% 
  mutate(biomass = biomass_func(volume = volume, Group = Group))


###  Here's the change from the orginial file: do not
###  run this bio100_no0, instead run the rest of the code with bio100
### But that means I have to do the same for bio400, which joins to bio100
### Eliminate all the rows that have zero occurrences

# bio100_no0 <- bio100 %>% 
#  rowwise() %>% 
#  filter(counts != 0)

## Add the csv doc that has all the header information, i.e., preservative
### factor, volume of sample settled in mL, sampling dates, count dates
###  on 7/22, I added number of bugs and experiment time
### on 8/8, think about deleting the count date, (not sure why I need it),
## and maybe this is where I can separate out the sample ID info
headers_100 <- read_csv("data/100xHeaders.csv")
view(headers_100)
## Join the two datasets by the sample column
### Rename the columns
### Delete the columns I don't need--Organism, name and shp;
### Reorder the columns
### Add a column that calculates biomass per volume:
###  total biomass in pgC divided by volume, which is preservative factor
###  times volume of sample settled: 
###  tot_biomass_pgC/(pres_fact*vol_set_ml)
###  the last mutate argument is to convert pgC per mL to ugC per L
###  Below, I put "hd" in the name to indicate that I added the headers that
##     include the number of bugs, preservative factor and volume of sample 
##     settled, etc., as noted above

bio100_hdv2 <- left_join(bio100, headers_100, by = "sample") %>% 
  rename(tot_vol_um3 = volume, tot_biomass_pgC = biomass, group = Group) %>% 
  select(-Organism, -name, -shp) %>% 
  select(samp_date,  everything()) %>% 
  mutate(bio_per_vol = tot_biomass_pgC/(pres_fact*vol_set_ml)) %>% 
  rename(bio_per_vol_pgc_ml = bio_per_vol) %>% 
  mutate(bio_per_vol_ugl = bio_per_vol_pgc_ml/1000)

## Save bio100_hd as an Rdata file in the 100xJune 2022 folder so I can
## load it without having to run all this code each time

save(bio100_hdv2, file = "bio100_hdv2.Rdata")
## Below combines all the 100x and 400x data, if I've loaded bio400_hdv2
## Note: I have to put bio400_hdv2 in the 100x directory
load("bio400_hdv2.Rdata")
bio_per_vol_all_v2 <- rbind(bio100_hdv2, bio400_hdv2)
view(bio400_hdv2)

### Change the order of the rows to chronologically by sample date
biovolall_ord_v2 <- bio_per_vol_all_v2  [with(bio_per_vol_all_v2, order(samp_date)), ]
#names(bio100_hd_v2)

### Two lines of code below are copied from "Biomass_per_volume_All.R,
## as other options
## Order the data frame first by sample date, then alphabetically by sample
bio_per_vol_all_ord_v2 <- bio_per_vol_all_v2 [with(bio_per_vol_all_v2, order(samp_date, sample, group, type, sa)), ]

##  This removes the site water samples
bio_vol_ctrl_exp_v2 <- filter(bio_per_vol_all_ord_v2, !str_detect(sample, "site"))

## Add three columns to separate out the sample names so
## sampling event, experimental and replicate have their own columns
## Note this code belwo uses the previous version, biovolall_ord_v2,
##  which does have the site water samples. Same code applied below
##   to bio_vol_ctrl_exp_v2 that excludes site water

bmass_sampev_v2 <- mutate(biovolall_ord_v2, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                       ifelse(grepl("WLD2", sample), "WLD2", 
                                                              ifelse(grepl("YBP1", sample), "YBP1",
                                                                     ifelse(grepl("LSZ2", sample), "LSZ2",
                                                                            ifelse(grepl("SJR2", sample), "SJR2",
                                                                                   ifelse(grepl("YBP2", sample), "YBP2", "??")))))))

bmass_exp_v2 <- mutate(bmass_sampev_v2, exp = ifelse(grepl("site", sample), "site",
                                               ifelse(grepl("IC", sample), "IC", 
                                                      ifelse(grepl("FC", sample), "FC",
                                                             ifelse(grepl("T24", sample), "T24", "??")))))

bmass_rep_v2 <- mutate(bmass_exp_v2, rep = ifelse(grepl("1", sample), "1",
                                            ifelse(grepl("2", sample), "2", 
                                                   ifelse(grepl("3", sample), "3",
                                                          "??"))))

## Reorder the columns and remove count dates, total volumes, total biomass
bmass_fin_v2 <- subset(bmass_rep_v2, select=c(1,18,19,20,3,4,5,6,7,8,9,10,12,13,14,15,16,17))
#save(bmass_fin, file = "bmass_fin")
## Correct the date format. See https://epirhandbook.com/en/working-with-dates.html
Sys.Date()
class(bmass_fin_v2$samp_date)
bmass_fin_dt_v2 <- bmass_fin_v2 %>% 
  mutate(samp_date = as.Date(samp_date, format ="%m/%d/%y"))
class(bmass_fin_dt_v2$samp_date)

#bmasbmass_rep_exps_fin_ <- bmass_fin %>% 
  mutate(samp_date = lubridate::dmy(samp_date))


##Create the file as an excel document
##write_xlsx(outAll, "add filepath and name here.xlsx")

#write_xlsx(bio400_hd, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/400x June 2022/BioPerVol400.xlsx")

## Repeat all this below using 
#bio_vol_ctrl_exp <- filter(bio_per_vol_all_ord, !str_detect(sample, "site"))
## So I can have a data frame without the site water samples

bmass_sampev_exp_v2 <- mutate(bio_vol_ctrl_exp_v2, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                              ifelse(grepl("WLD2", sample), "WLD2", 
                                                                     ifelse(grepl("YBP1", sample), "YBP1",
                                                                            ifelse(grepl("LSZ2", sample), "LSZ2",
                                                                                   ifelse(grepl("SJR2", sample), "SJR2",
                                                                                          ifelse(grepl("YBP2", sample), "YBP2", "??")))))))

bmass_exp_exp_v2 <- mutate(bmass_sampev_exp_v2, exp = ifelse(grepl("site", sample), "site",
                                                       ifelse(grepl("IC", sample), "IC", 
                                                              ifelse(grepl("FC", sample), "FC",
                                                                     ifelse(grepl("T24", sample), "T24", "??")))))

bmass_rep_exp_v2 <- mutate(bmass_exp_exp_v2, rep = ifelse(grepl("1", sample), "1",
                                                    ifelse(grepl("2", sample), "2", 
                                                           ifelse(grepl("3", sample), "3",
                                                                  "??"))))

## Reorder the columns and remove count dates, total volumes, total biomass
bmass_fin_exp_v2 <- subset(bmass_rep_exp_v2, select=c(1,18,19,20,3,4,5,6,7,8,9,10,12,13,14,15,16,17))
#save(bmass_fin_exp, file = "bmass_fin_exp")
