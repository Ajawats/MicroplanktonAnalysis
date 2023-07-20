####################################################################
####### EQUIVALENT SPHERICAL DIAMETER CALCULATIONS FUNCTION ########
####################################################################

esd_func <- function(vol_per_cell_um3){
  #pi= 3.141593
  esd = ((6*vol_per_cell_um3)/pi)^(1/3)
  return(esd)
}

### Note that the equation I originally had here is also correct:

### esd = 2*(((0.75 * vol_per_cell_um3)/pi)^(1/3)))
