####################################################################
################ CLEARANCE RATE CALCULATIONS FUNCTION ##############
####################################################################


cr_func <- function(V=595, controlMnCt, expCt, numBugs=24, T=1) {
  cr = (V/T) * ((log(controlMnCt) - log(expCt))/numBugs)
  if(expCt == 0 | controlMnCt == 0){
    return(NA)
  } else{
    return(cr)
  }
}

### Error code on above:
#Error in `mutate()`:
#  ! Problem while computing `CR = cr_func(controlMnCt = Cmn, expCt = cpm)`.
#ℹ The error occurred in row 1.
#Caused by error in `cr_func()`:
#  ! object 'cpm' not found
### Note on above on 1/16/23: put ExpCt in place of cpm

## Original function, for referring to so I don't forget what it is.
#cr_func <- function(V=595, controlMnCt, expCt, numBugs=24, T=1) {
#  cr = (V/T) * ((log(controlMnCt) - log(expCt))/numBugs)
 # if(Cmn == 0){
 #   return(NA)
 # } else{
 #   return(cr)
 # }
#}
### Note on 1/4/23: trying to fix an error, I changed if(cr == 0)
##  to if(controlMnCt == 0)  Doing this got rid of the error code
## but something's not right. Will check on 1/5/23

### Note on 1/12/23 following up on above note: per discussion with Wim,
## if Ctrl and Exp both = 0, then Cmn needs to = NA
## if Ctrl = 0 and Exp = #, then Cmn needs to = NA
## if Ctrl = # and Exp = 0, then Cmn needs to = NA
## So change to if (Cmn == 0){ return}


### Note:controlMnCt is the mean count of the three replicates of the control samples; 
##  expCt is the counts of each of the three replicates of the experimental samples; 
##  numBugs is the number of copepods that were in the experimental sample 
##  bottles--in the Hunger Games project, it was 24 in all bottles.
##  V = total volume in ml of the sample bottle; 
##  T = time in days, 1 day for the Hunger Games project

### Note 1/20/23, I had previously added the condition controlMnCt <= expCt
## to get rid of the CR that had negative numbers, but Wim said to keep the
## negative numbers: if the experimentals had less than the controls, that might
## mean that those taxa wer released from predation and were not eaten by the
## copepods, which is useful information.
## So I removed that condition from the function. Below is the function with it:

#cr_func <- function(V=595, controlMnCt, expCt, numBugs=24, T=1) {
#  cr = (V/T) * ((log(controlMnCt) - log(expCt))/numBugs)
#  if(expCt == 0 | controlMnCt == 0 | controlMnCt <= expCt){
#    return(NA)
#  } else{
#    return(cr)
#  }
#}

### Note 1/20/23 Per conversation with Wim, I removed the condition
## controlMnCt <= expCt. When the control means were less than the 
## experimentals, it resulted in a negative number. That is usefu information
##  as it might inicated taxa that are released from predation and not eaten,
## so I removed that condition from the code.

  
  