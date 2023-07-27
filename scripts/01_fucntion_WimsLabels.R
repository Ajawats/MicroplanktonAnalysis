############################################################################
########################### WIM'S LABEL FUNCTION ###########################
############################################################################

### 3/31/23 Wim gave me this so I can make a label with superscript
##  of mL (or other unit) per copepod per day
## I need to create a new label following his example below that has
## both copepod and day together with the superscript

labelFunctionA <- function () #  Returns a list containing the labels below for text using Unicode
{
  dm1 <- "d\U207B\U00B9"            # d^-1
  Lm1 <- "L\U207B\U00B9"            # L^-1
  fm1 <- "\U2640\U207B\U00B9"       # (Female sign)^-1
  mm3 <- "m\U207B\U00B3"       	    # m^-3
  allLabels   <-     list(dm1 = dm1, L1 = Lm1, fm1 = fm1, mm3 = mm3,
                          Chlw     = paste("Chlorophyll, \U03BCg",Lm1),
                          Chlw.2   = paste("Chlorophyll\n\U03BCg",Lm1),
                          Chl5     = paste("Chlorophyll > 5 \U03BCm, \U03BCg" ,Lm1),
                          Chl5.2   = paste("Chlorophyll\n > 5 \U03BCm, \U03BCg",Lm1),
                          temp     = "Temperature,\u00b0C",
                          sal      = "Salinity",
                          turb     = "Turbidity, NTU",
                          growth   = paste("Growth Rate, ", dm1),
                          growth22 = paste("Growth Rate at 22 \u00b0C, ", dm1),
                          epr      = paste("Egg Production, Eggs ", fm1, dm1),
                          sepr     = paste("Specific Egg Production Rate, ", dm1), 
                          abunm3   = paste("Abundance,", mm3),
                          abunm3p10= paste("Abundance + 10,", mm3),
                          abunm3p1 = paste("Abundance + 1,", mm3),
                          abunL    = paste("Abundance,", Lm1),
                          cpday    ="copepod\U207B\U00B9 day\U207B\U00B9")
  allLabels
}
