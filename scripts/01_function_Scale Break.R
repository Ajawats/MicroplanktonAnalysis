####################################################################
################ SCALE BREAK FUNCTION ############################
####################################################################

### From Wim
##  To use on the clearance rate and ingestion rate plots where
##  there are some very high and very low numbers, to help
##  them fit on the plot in a way that is easy to read

scaleBreak <- function(xvalue, Break, rescale) {
  # Function to set up plotting an axis with a single scale break
  # This is an actual change of scale, unlike what ggplot has
  # Input:
  #   xvalue    The raw data (for either axis)
  #   Break     The value at the break in raw data units
  #   rescale   The divisor (>1 usually) to reduce the scale
  # Use:
  #   Run this for the data to be plotted
  #   Run it again for the axis tick locations  
  #   Then plot the rescaled data with the rescaled tick locations
  #      and the original tick locations for the tick labels
  
  ifelse(xvalue < Break, xvalue, (xvalue-Break)/rescale + Break)
}

### Originally saved in ~/Downloads/scaleBreak function with code to test.R

#Break      <- 5
#rescale    <- 5
#ylabels    <- c(0:5, 10, 15, 20)
#brks       <- scaleBreak(ylabels, Break, rescale)

#N <- 20
#x <- data.frame(x= seq(0, 3, length=N ), y = exp(seq(0, 3, length=N)) )
#plot(x$x, x$y)

#x$ySquish <- scaleBreak(x$y, Break=Break, rescale=5)

#ggplot(data=x, aes(x, ySquish)) +
#  geom_point(shape=16, color="navy") +
#  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
#  xlab("X value") +
#  scale_y_continuous("Scaled y value", breaks=brks, labels=ylabels) 

