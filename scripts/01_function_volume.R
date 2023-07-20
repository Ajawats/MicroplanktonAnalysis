####################################################################
################## VOLUME CALCULATIONS FUNCTION ####################
####################################################################

### Note: units of diameter and height are micrometers
### Calculation returns volume per cell in cubic micrometers (um^3)
### Note 2/6/23, originally the function included mulitiplying by
##  counts to generate the total volume. But today I edited so the
##  function returns volume per cell. This seemed to fix an issue
##  with R returning NaN when calculating volume per cell of zero count
##  organisms when dividing volume by total counts to get volume per cell.
##  Also, I hash-tagged out the pi= row, because when I originally wrote the
##  function, I put pi = 3.1415. But when I manually calculated some 
##  volume and other things on a calculator to double check, the numbers
##  didn't match. I realized it was due to pi having four decimal place, when
##  the calculator uses nine. R uses 3.14593.

vol_func <- function(diameter, height, width, shape) {
  #pi= 3.141593
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
    return(volume)
  }
}
