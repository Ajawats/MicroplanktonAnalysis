####################################################################
########################## ADD SIZE CATEGORIES ######################
####################################################################
### 1/27/23
### For adding a column of which size category the organisms fall into 
## There are two:< 15µm and >= 15 µm

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
###volbio_all_cr has the experiment sample names changed:  
##  T24 to E (for experimental), FC to C (for control),
##  IC to I, (for initials), site to S (for site)
source("scripts/01_function_wimGraph and Palettes.R")

### Using the code from 03_calcs_GroupsCollapse.R, but changing to the two
##  size categories of <15 µm and >= 15 µm. See note in 
## Table of All Top Organism Groups.docx

volbio_all_sz <- volbio_all_cr
volbio_all_cr$szGroup  <-with(volbio_all_cr,
                              ifelse(esd < 15, 'small',
                                     ifelse(esd >= 15, 'large',
                                            ifelse( "WHAT?"))))

### Other examples I was considering before I remembered that I had already
## done it.
### Example from https://www.marsja.se/r-add-column-to-dataframe-based-on-other-columns-conditions-dplyr/
#depr_df %>%
 # mutate(Status = case_when(
#    endsWith(ID, "R") ~ "Recovered",
 #   endsWith(ID, "S") ~ "Sick"
#  ))

### And this example:
#depr_df %>% mutate(Group =
#                     case_when(DeprIndex <= 15 ~ "A", 
#                               DeprIndex <= 20 ~ "B",
#                               DeprIndex >= 21 ~ "C")
#)
