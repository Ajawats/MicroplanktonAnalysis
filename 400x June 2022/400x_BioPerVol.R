### 7/11/22
### This code contains the biomass per volume calculations of 400x counts
##  It uses the raw counts file and merges with it the headers file that
##  includes the preservation factor and volume of sample settled.

### Updated on 9/21/22 to include a column that has the counts
## per milliliter (preservative factor * volume of sample settled)


library(tidyverse)
library(writexl)
getwd()

#Load the excel csv data file

raw400 <- read_csv("400x_RawCount_R.csv")

#Convert to long format


raw400_long <- pivot_longer(data = raw400,
                            cols = c(9:68),
                            names_to = "sample",
                            values_to = "counts")

raw400_long_noNA <- raw400_long %>% 
  rowwise() %>% 
  filter(counts != "NA")

sum(raw400_long_noNA$counts)

### The two sets of code below were part of me trying to see if I 
##  made an error in the raw400_long_noNA and raw100_long_noNA 
##  calculations, since, they both came out as having exactly 952 obs.,
## in other words, 952 entries of distinct organism/sizes
raw400_long_noNA_noCil <- raw400_long_noNA %>% 
  rowwise() %>% 
  filter(Group != "ciliate")

raw400_long_noNA_noColon <- raw400_long_noNA %>% 
  rowwise() %>% 
  filter(type != "colonial")

## Rearrange the column orders to have the station/sample column first, 
##  using the select function, and then change the NAs to 0s using the 
##  mutate function.

raw400_fin <- mutate(raw400_long, counts = ifelse(is.na(counts), 0, counts)) %>% 
  select(sample, Organism, Group, type, name, shp, sa, la, wi, counts)

write_xlsx(raw400_fin, "raw400_fin_test.xlsx")
### Test to see how many rows there are when I remove all the
#  organisms that have count=0
raw400_fin_no0 <- raw400_fin %>% 
  rowwise() %>% 
  filter(counts != 0)


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

vol400 <- rowwise(raw400_fin) %>% 
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

bio400 <- rowwise(vol400) %>% 
  mutate(biomass = biomass_func(volume = volume, Group = Group))


##  Eliminate all the rows that have zero occurrences

bio400_no0 <- bio400 %>% 
  rowwise() %>% 
  filter(counts != 0)

## Add the csv doc that has all the header information, i.e., preservative
### factor, volume of sample settled in mL, sampling dates and count dates
headers_400 <- read_csv("400xHeaders.csv")

## Join the two datasets by the sample column
### Rename the columns
### Delete the columns I don't need--Organsim, name and shp;
### Reorder the columns
### Add a column that calculates biomass per volume
###  total biomass in pgC divided by (preservative factor times volume of
###  sample settled)
###  tot_biomass_pgC/(pres_fact*vol_set_ml)

bio400_hd <- left_join(bio400_no0, headers_400, by = "sample") %>% 
  rename(tot_vol_um3 = volume, tot_biomass_pgC = biomass, group = Group) %>% 
  select(-Organism, -name, -shp) %>% 
  select(samp_date, everything()) %>% 
  mutate(bio_per_vol = tot_biomass_pgC/(pres_fact*vol_set_ml)) %>% 
  rename(bio_per_vol_pgc_ml = bio_per_vol) %>% 
  mutate(bio_per_vol_ugl = bio_per_vol_pgc_ml/1000) %>% 
  mutate(counts_per_ml = counts/(pres_fact*vol_set_ml))
view(bio400_hd)
## Save bio400_hd, then move it to  the 100x June 2022 folder so I can easily combine it with this
## data frame to get the total biomass per volume, 100x + 400x

save(bio400_hd, file = "bio400_hd.Rdata")
save(bio400_hd, file = "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/bio400_hd.Rdata")
#names(bio400_hd)
load("bio400_hd.Rdata")
##Create the file as an excel document
##write_xlsx(outAll, "add filepath and name here.xlsx")

write_xlsx(bio400_hd, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/400x June 2022/BioPerVol400.xlsx")



