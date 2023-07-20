### 7/1/22
### This code is for volume and the biomass calculations 
##  of both the 100x and 400x raw counts


library(tidyverse)
library(writexl)
getwd()

## Load the excel csv data file
rawcount_all<- read_csv("100_400_RawCount_R.csv")

## Convert to long format
rawcount_all_long <- pivot_longer(data = rawcount_all,
                                cols = c(9:68),
                                names_to = "sample",
                                values_to = "counts")


## Rearrange the column orders using the select function, and then change 
## the NAs to 0s using the mutate function. Then rearrange the rows so that
## all the organism names from both 100x and 400x are together, in alphbetical
## order.
rawcount_all_final <- mutate(rawcount_all_long, counts = ifelse(is.na(counts), 0, counts)) %>% 
  select(sample, Organism, Group, type, name, shp, sa, la, wi, counts) %>% 
  arrange(Organism)

## Run the volume function
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
vol_all <- rowwise(rawcount_all_final) %>% 
  mutate(volume = vol_func(diameter = sa, height = la, width = wi, shape = shp, counts = counts))


## Add the biomass function
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
bio_all <- rowwise(vol_all) %>% 
  mutate(biomass = biomass_func(volume = volume, Group = Group))

## Eliminate all the rows that have zero occurrences
bio_all_no0 <- bio_all %>% 
  rowwise() %>% 
  filter(counts != 0)

## Test to see how to order the rows by organism and then by size. 
## bio_all_ord ordered them alphabetically by Organsim, and then by size 
###  within that alphabetization.
## bio_all_sample ordered them first by sample then by organism--which it
### might have done anyway?
##  bio_all_exp removed all samples that contained the word "site" in it
###  I wanted to have only the experimental samples, not site water.
bio_all_ord <- bio_all_no0  [with(bio_all_no0, order(Organism, sa)), ]
bio_all_sample <- bio_all_no0  [with(bio_all_no0, order(sample, Organism)), ]
bio_all_exp <- bio_all_sample [!grepl("site", bio_all_sample$sample),]
# Test by ordering only by sa.
# Didn't do what I wanted it to.
# bio_all_ord_sa <- bio_all_no0  [with(bio_all_no0, order(sa, Organism)), ]

## Create the file as an excel document
#  write_xlsx(outAll, "add filepath and name here.xlsx")

write_xlsx(bio_all_no0, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/100_400_All_Bio.xlsx")
write_xlsx(bio_all_ord, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/100_400_All_Bio_Ord.xlsx")
write_xlsx(bio_all_exp, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/100_400_All_Bio_Exp.xlsx")
