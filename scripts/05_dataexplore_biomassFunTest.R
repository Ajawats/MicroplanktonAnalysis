####################################################################
################# BIOMASS EDIT TEST ###################
####################################################################

### Make a test biomass function to see if I can add the second condition
## of the size of the organism to the biomass calculations



### This version uses the aloricate ciliate equation from 
# Menden_Deuer & Lessard 2000 (MDL).
### Need to do another one with the Putt & Stoecker 1989 ratio.
### Might also try with the MDL protist plankton equation
### Need to separate diatoms by size and do two equations according to MDL. 
### Here I used the equation for diatoms > 3000 µm^3, since the first dataset
#  I'm working with is the 100x samples

### Units: equation results in picograms of carbon per cell, because it is 
##  based on the C: vol data deteremined in the above referenced studies
##  volume is in cubic micrometers

biotest_func <- function(Group, vol_org) {
  biomass = 0
  if(grepl("diatom", Group, ignore.case = TRUE) & (vol_org > 3000)){
    biomass = .117 * vol_org^.881
  }
  if(grepl("diatom", Group, ignore.case = TRUE) & (vol_org <= 3000)){
    biomass = .287 * vol_org^.811
  }
  if(biomass == 0){
    return(NA)
  } else{
    return(biomass)
  }
}


### This returned errors, which disappeared once I removed the entries that were NA

#biotest_func <- function(Group, volume) {
 # biomass = 0
  #if(grepl("diatom", Group, ignore.case = TRUE)){
   # biomass = .117 * volume^.881
#  }
 # if(biomass == 0){
  #  return(NA)
  #} else{
   # return(biomass)
  #}
#}
#0.287 * volume0.811


