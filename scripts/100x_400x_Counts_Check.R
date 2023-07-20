#########################################################################################
############################### TEST of 100 + 400 Obs. ##################################
#########################################################################################

### This script combines the 100x and 400x datasets into one, and then performs the
## data cleaning code, including removing the instances where no organisms/sizes were
##  found in a particular sample, so removes all the NAs. The purpose of doing this is
## to verify that the results from the individual data frames, 100x and 400x, were correct,
## because they both have the exact same number of observations, 952, which seems
## unlikely. But I double checked many other things already.

library(tidyverse)

raw100400 <- read_csv("data/100_400x_RawCount_R.csv")

#Convert to long format

raw100400_long <- pivot_longer(data = raw100400,
                            cols = c(9:68),
                            names_to = "sample",
                            values_to = "counts")

raw100400_long_noNA <- raw100400_long %>% 
  rowwise() %>% 
  filter(counts != "NA")

sum(raw100400_long_noNA$counts)
