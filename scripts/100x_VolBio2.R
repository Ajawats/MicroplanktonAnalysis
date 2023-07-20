### 7/1/22
### This code contains only the volume and the biomass calculations 
##  for the 100x raw counts
## Copied from 100x_Corrected_06_17.Rmd,  which also contains the code for 
##creating files filtered by individual organism groups, sucha as chlorophytes, 
## ciliates, diatoms, etc., and also has code for calculating the volume and
##  biomass of just one individual organism.

library(tidyverse)
library(writexl)
getwd()

## Load the excel csv data file
volbio100_cor <- read_csv("100x_Corrected_06_17.csv")

## Convert to long format
volbio_long_cor <- pivot_longer(data = volbio100_cor,
                                cols = c(9:68),
                                names_to = "sample",
                                values_to = "counts")


## Rearrange the column orders using the select function, and then change 
## the NAs to 0s using the mutate function.
volbio_final_cor <- mutate(volbio_long_cor, counts = ifelse(is.na(counts), 0, counts)) %>% 
  select(sample, Organism, Group, type, name, shp, sa, la, wi, counts)

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
volbio_fin_cn2 <- rowwise(volbio_final_cor) %>% 
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
volbio_all <- rowwise(volbio_fin_cn2) %>% 
  mutate(biomass = biomass_func(volume = volume, Group = Group))

## Eliminate all the rows that have zero occurrences
volbio_all_no0 <- volbio_all %>% 
  rowwise() %>% 
  filter(counts != 0)

## Create the file as an excel document
#  write_xlsx(outAll, "add filepath and name here.xlsx")

#  write_xlsx(volbio_all_no0, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/VolBioAllNo0_test.xlsx")


