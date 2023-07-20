####################################################################
################# BIOMASS CALCULATIONS FUNCTION ###################
####################################################################

### Volume:carbon conversions are from Menden_Deuer & Lessard 2000 (MDL).
## MDL small diatom and large diatom equations are used for diatoms;
##  MDL protist plankton equation is used for all others

### Units: equation results in picograms of carbon per cell, because it is 
##  based on the C: vol data determined in the above referenced studies
##  volume is in cubic micrometers

biomass_func <- function(Group, volume) {
  biomass = 0
  if(grepl("ciliate", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("tintinnid", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("chlorophyte", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
  }
  if(grepl("diatom", Group, ignore.case = TRUE) & (volume > 3000)){
    biomass = .117 * volume^.881
  }
  if(grepl("diatom", Group, ignore.case = TRUE) & (volume <= 3000)){
    biomass = .287 * volume^.811
  }
  if(grepl("dinoflagellate", Group, ignore.case = TRUE)){
    biomass = .216 * volume^.939
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
    biomass = .216 * volume^.939
  }
  if(biomass == 0){
    return(NA)
  } else{
    return(biomass)
  }
}
