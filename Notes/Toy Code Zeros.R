############################################################################
## TOY CODE FOR SEEING THE EFFECT OF RANDOM SAMPLING WITH SMALL NUMBERS  ##
############################################################################


mnval <- 5                        # Pick any mean value (need not be integer)
samp <- rpois(n = 10000, mnval)	  # Take a random sample of selected size
#   from a Poisson distribution with that mean	
mean(samp)			  # Get the mean of the sample (close to selected value)
var(samp)			  # Get the variance and compare (close to mean)
hist(samp, breaks=0:max(samp)) 	  # Plot a histogram of the samples

You will see that the number of zeros will decrease as the mean increases.

mnval <- 15                       # Pick any mean value (need not be integer)
samp <- rpois(n = 10000, mnval)	  # Take a random sample of selected size
#   from a Poisson distribution with that mean	
mean(samp)			  # Get the mean of the sample (close to selected value)
var(samp)			  # Get the variance and compare (close to mean)
hist(samp, breaks=0:max(samp)) 	  # Plot a histogram of the samples
