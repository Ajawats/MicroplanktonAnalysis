### Biomass function for diatom experiment
### Uses biomass conversion for all protist plankton, MD&L 2000,
##  Note that Val Greene uses this one for all diatoms in her thesis

biomass2_func <- function(Group, volume) {
  biomass = 0
  if(grepl("diatom", Group, ignore.case = TRUE) & (volume > 3000)){
    biomass = .216 * volume^.939
  }
  if(grepl("diatom", Group, ignore.case = TRUE) & (volume <= 3000)){
    biomass = .216 * volume^.939
  }
  if(biomass == 0){
    return(NA)
  } else{
    return(biomass)
  }
}
