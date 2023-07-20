####################################################################
################ FEEDING RATE CALCULATIONS FUNCTION ##############
####################################################################

### Also referred to as ingestion rate or consumption rate
##  Ref: Frost, B. W. (1972). Effects of size and concentration of food particles on 
##   the feeding behavior of the marine planktonic copepod Calanus pacificus. 
##  Limnology and Oceanography, 17(6), 805–815. 
##  https://doi.org/10.4319/lo.1972.17.6.0805 

### Equation according to Frost is I = F * mean counts per ml of initial samples,
##  I = feeding rate, F = clearance rate
##  Resulting units are cells per copepod per day, or
##  biomass, pg C or µg C per copepod per day, depending on if I used
##  pg or µg in the calculations

fr_func <- function(CR, initialMnCt) {
  fr = (CR*initialMnCt) 
  if(is.na(CR)){
    return(NA)
  } else{
    return(fr)
  }
  if(is.na(initialMnCt)){
    return(NA)
  } else{
    return(fr)
  } 
}

save(fr_func, file = "scripts/01_function_feedingRate.Rdata")

### In the function, CR = clearance rate; intialMnCt = mean counts per ml or 
#biomass pg C per ml, in the initial samples