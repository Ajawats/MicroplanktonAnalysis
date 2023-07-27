### 7/1/22
### This code contains only the volume and the biomass calculations 
##  for the 400x raw counts
## Copied from 400x_VolBio.Rmd, which also contains the code for creating 
## files filtered by individual organism groups, sucha as chlorophytes, 
## ciliates, diatoms, etc.



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

## Rearrange the column orders to have the station/sample column first, 
##  using the select function, and then change the NAs to 0s using the 
##  mutate function.

raw400_fin <- mutate(raw400_long, counts = ifelse(is.na(counts), 0, counts)) %>% 
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


##Create the file as an excel document
##write_xlsx(outAll, "add filepath and name here.xlsx")

#write_xlsx(bio400_no0, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/400x June 2022/Bio400_no0.xlsx")



