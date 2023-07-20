### 8/2/22 , 8/3 and 8/4  This file is for testing out using Wim's Bquick function
##   for calcuating the means of FC and IC, from the R file

### As of 8/5/22, I'm not using Bquick, as I couldn't get it to work with my data. 
##   bio_vol_ctrl_exp

library(tidyverse)
library(readxl)
library(writexl)
load("BioPerVolCtrlExp.Rdata")
read_xlsx("carbio.xlsx")
carbio <- read_excel("carbio.xlsx")
load("carbio.xlsx")
getwd()

### Remove the count date column from the bio_vol_ctrl_exp dataframe. This also
##   removes all the other columns besides the ones selected below.
#names(bio_vol_ctrl_exp)
#carbio <- select(bio_vol_ctrl_exp, samp_date, sample, group, type, sa, la,
  #               wi,counts, bmass_ugCl=bio_per_vol_ugl)

names(carbio)
### Add Bquick function since it is used below
## Wim's notes are those within the function below
## My notes are that x is my data frame; ind is the number of index columns.
## Index columns are the columns that have the information you need to keep that
## identify the things you are calculating. In my case it will be samp_date, sample,
## group, type, sa, la, wi, counts.
## FUN is the name of the mathematical calculation I want to do, in my case, mean
## The three dots, or ellipses are further things you want to to with the calculation

#Bquick <- function(x, ind, FUN,  ...)
# {
  # Uses aggregate but converts index columns back to same mode as source
  # Revised 6/15/02 to fix output from aggregate, which converts indices to factors or character
  # x is a data frame
  # ind is the number of columns (starting from the left) over which aggregation occurs
  # In either case factors in the input are returned as factors
  #   and ordered factors are returned as ordered factors
  # The ellipses ... can mean any argument passed to fun
 # if(ind > 1.) w <- as.list(x[, 1.:ind])
#  else 
  #{
 #   w <- vector("list", length=1)
   # w[[1]] <- x[, 1.]
#  }
#  xd <- data.frame(x[, (ind + 1.):dim(x)[2.]], stringsAsFactors = F)
#  y <- aggregate(xd, w, FUN, ...)
#  for(i in 1.:ind) {
#    if(is.ordered(x[, i]))
#      y[, i] <- ordered(y[, i], levels = levels(x[, i]))
#    else if(is.factor(x[, i]))
#      y[, i] <- factor(y[, i], levels = levels(x[, i]))
#    else {
#      y[, i] <- as.character(y[, i])  	
#      if(is.numeric(x[,i])) y[, i] <- as.numeric(y[,i])
#    }}
  
 # for(i in seq(ind, 1., -1.))
#    y <- y[order(y[, i]),  ]
#  names(y) <- names(x)
#  row.names(y) <- 1.:dim(y)[1.]
#  y
# }

### After putting the Bquick function in, this is how I apply it to my data, see
  ## example below of line of code from the growth rate file
### I will replace the x with the name of my dataframe, and replace the names in quotes
  ## with the names of the columns I want to have in the new dataframe. The number 3 below
  ## is telling R that the first three columns are the index columns. The last three
  ## columns in the list are the data columns, i.e., the columns that have the numbers
  ## that I want R to use to make the calculations. I will replace the word, median,
  ## with the word, mean, since I want to take the means.

### Calculate medians of volume and carbon
##   xmn <- Bquick(x[,c("sample","days","rep","volmm3", "LnC","ugC")], 
#              3, median)

# xmn <- Bquick(carbio[,c("samp_date", "sample", "group", "type", "sa", "la", "wi", "counts", "bmass_ugCl")], 
 #            8, mean)
# names(xmn)
# write_xlsx(xmn, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/xmn.xlsx")
# write_xlsx(carbio, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/carbio.xlsx")
# names(carbio)
# xmn <- Bquick(carbio[,c("samp_date", "samp_ev", "exp", "rep", "group", "type", "sa", "la", "wi", "counts", "bmass_ugCl")], 
  #                        10, mean)
## This is still not working. It's only outputting some of the groups/types organisms
## Plus, I should have it sum the counts first
## Try to sum the counts before doing the means

# count_sum <- Bquick(carbio[,c("samp_date", "samp_ev", "exp", "rep", "group", "type", "sa", "la", "wi", "counts", "bmass_ugCl")], 
       #       9, sum)
### Nope, above didn't work. Try using grepl or group_by
  

### This below was successful in summing up the counts of reps 1, 2 and 3 of each organism 
##  and its specific size. I needed to take the "rep" column out of the group_by command so 
## that it wouldn't look for the separate rep numbers. That allowed it to add up the organisms by every
## other category and ignore the rep number, so that in essence, it added the counts in all the reps per
## organism, size sampling event, and experimental sample
names(carbio)
count_sum_rep <- carbio %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(tot_ct = sum(counts)) %>% 
  ungroup()
write_xlsx(count_sum_rep, "count_sum_rep.xlsx")
## I just learned that I don't need to write the entire filepath name
##   when saving a data frame as an excel file, presumably because
##   it will automatically save it in the working directory.

## Now I need to figure out how to get it to take the means of the biomass of those same groups
## of organisms that it combined the counts of. Maybe try to do it separately and then combine?

bmean_rep <- carbio %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(bmn = mean(bmass_ugCl)) %>% 
  ungroup()
write_xlsx(bmean_rep, "bmean_rep.xlsx")
## This above did work just like the counts summary

## Now I think I should try to do a right join to join the data frame
## so that both total counts and mean biomass are on the same data frame

ct_bmn <- left_join(count_sum_rep, bmean_rep)
write_xlsx(ct_bmn, "ct_bmn.xlsx")
