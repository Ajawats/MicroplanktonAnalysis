### 9/2/22
### This code contains the biomass per volume calculations for all samples
##  See 100x_BioPerVol.R for the original code and notes. This is
##  the cleaned up version.

### Updated 9/21/22 to include a column that calculates the counts
##   per milliliter (preservative factor * volume of sample settled)

library(tidyverse)
library(writexl)

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


##  Eliminate all the rows (organisms) that have zero occurrences

bio100_no0 <- bio100 %>% 
  rowwise() %>% 
  filter(counts != 0)

## Add the csv doc that has all the header information, i.e., preservative
### factor, volume of sample settled in mL, sampling dates, count dates


headers_100 <- read_csv("data/100xHeaders.csv")
view(headers_100)
names(headers_100)
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

bio100_hd <- left_join(bio100_no0, headers_100, by = "sample")%>% 
  rename(tot_vol_um3 = volume, tot_biomass_pgC = biomass, group = Group) %>% 
  select(-Organism, -name, -shp) %>% 
  select(samp_date,  everything()) %>% 
  mutate(bio_per_vol = tot_biomass_pgC/(pres_fact*vol_set_ml)) %>% 
  rename(bio_per_vol_pgc_ml = bio_per_vol) %>% 
  mutate(bio_per_vol_ugl = bio_per_vol_pgc_ml/1000) %>% 
  mutate(counts_per_ml = counts/(pres_fact*vol_set_ml))

### Make a test file of adding the counts per milliliter to the bio100_hd
## bio100_hd_ct_ml <- mutate(bio100_hd,counts_per_ml = counts/(pres_fact*vol_set_ml))

## Save bio100_hd as an Rdata file in the 100xJune 2022 folder so I can
## load it without having to run all this code each time

save(bio100_hd, file = "bio100_hd.Rdata")

## Below combines all the 100x and 400x data, if I've loaded bio400_hd
load("data/bio100_hd.Rdata")
load("data/bio400_hd.Rdata")
bio_per_vol_all <- rbind(bio100_hd, bio400_hd)
view(bio400_hd)

### Change the order of the rows to chronologically by sample date
biovolall_ord <- bio_per_vol_all  [with(bio_per_vol_all, order(samp_date)), ]
#names(bio100_hd)

### Two lines of code below are copied from "Biomass_per_volume_All.R,
## as other options
## Order the data frame first by sample date, then alphabetically by sample
#bio_per_vol_all_ord <- bio_per_vol_all [with(bio_per_vol_all, order(samp_date, sample, group, type, sa)), ]

##  This removes the site water samples
#bio_vol_ctrl_exp <- filter(bio_per_vol_all_ord, !str_detect(sample, "site"))

## Add three columns to separate out the sample names so
## sampling event, experimental and replicate have their own columns
## Note that in bmass_exp, I neeeded to create a new column that had all the exp
##  with the rep numbers so that I could extract the rep numbers from there. Otherwise
##  R would search for the #s 1, 2 and 3 in the sampling event name, such as the 1 in
##  SJR1, and assign the rep with that number, so that the rep numbers would be incorrect.

bmass_sampev <- mutate(biovolall_ord, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                       ifelse(grepl("WLD2", sample), "WLD2", 
                                                              ifelse(grepl("YBP1", sample), "YBP1",
                                                                     ifelse(grepl("LSZ2", sample), "LSZ2",
                                                                            ifelse(grepl("SJR2", sample), "SJR2",
                                                                                   ifelse(grepl("YBP2", sample), "YBP2", "??")))))))

bmass_exp <- mutate(bmass_sampev, exp = ifelse(grepl("site", sample), "site",
                                               ifelse(grepl("IC-1", sample), "IC-1",
                                                      ifelse(grepl("IC-2", sample), "IC-2",
                                                             ifelse(grepl("IC-3", sample), "IC-3",
                                                                    ifelse(grepl("FC-1", sample), "FC-1",
                                                                           ifelse(grepl("FC-2", sample), "FC-2",
                                                                                  ifelse(grepl("FC-3", sample), "FC-3",
                                                                                         ifelse(grepl("T24-1", sample), "T24-1",
                                                                                                ifelse(grepl("T24-2", sample), "T24-2",
                                                                                                       ifelse(grepl("T24-3", sample), "T24-3","??")))))))))))

bmass_rep <- mutate(bmass_exp, rep = ifelse(grepl("1", exp), "1",
                                            ifelse(grepl("2", exp), "2", 
                                                   ifelse(grepl("3", exp), "3",
                                                          "??"))))

bmass_rep["exp"][bmass_rep["exp"] == "IC-1"] <- "IC"
bmass_rep["exp"][bmass_rep["exp"] == "IC-2"] <- "IC"
bmass_rep["exp"][bmass_rep["exp"] == "IC-3"] <- "IC"
bmass_rep["exp"][bmass_rep["exp"] == "FC-1"] <- "FC"
bmass_rep["exp"][bmass_rep["exp"] == "FC-2"] <- "FC"
bmass_rep["exp"][bmass_rep["exp"] == "FC-3"] <- "FC"
bmass_rep["exp"][bmass_rep["exp"] == "T24-1"] <- "T24"
bmass_rep["exp"][bmass_rep["exp"] == "T24-2"] <- "T24"
bmass_rep["exp"][bmass_rep["exp"] == "T24-3"] <- "T24"

names(bmass_rep)
## Reorder the columns and remove count dates, and sample
bmass_fin <- subset(bmass_rep, select=c(1,19,20,21,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18))
save(bmass_fin, file = "bmass_fin")
## Correct the date format. See https://epirhandbook.com/en/working-with-dates.html
Sys.Date()
class(bmass_fin$samp_date)
bmass_final <- bmass_fin %>% 
  mutate(samp_date = as.Date(samp_date, format ="%m/%d/%y"))
class(bmass_fin_dt$samp_date)
names(bmass_fin)
save(bmass_fin, file = "data/bmass_fin.Rdata")

## 9/2/22 This creates a data frame with all the same columns, but not including
##   site water samples.
bmass_final_exp <-subset(bmass_final, exp!="site")
save(bmass_final_exp, file = "data/bmass_final_exp.Rdata")
write_xlsx(bmass_final_exp, "biomass_final_no_sitewater.xlsx")
