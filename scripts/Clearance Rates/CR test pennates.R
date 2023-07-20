###__________________USING OTHER DATA_______________________________
### Create a new df with only the FC (Controls) counts per ml, and
##  calculate the FC counts per ml means across the three replicates

### Start with new base df, volbio_all_cr, that already has the experiment
## sample names changed:  T24 to E (for experimental), FC to C (for control),
##  IC to I, (for initials), site to S (for site)

load("data/Clearance Rates/volbio_all_cr.Rdata")

### Try with pennate diatoms
diapen <- volbio_all_cr %>%
  filter(Group == "diatom",
         type == "pennate",
         str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

diapenC <- diapen %>% 
  filter(exp == "C")

diapenC <-diapenC  %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Make a df with only centric diatoms experimental samples
diapenE <- diapen %>% 
  filter(exp == "E") 

### Join the above df with the df that has the T24 (experimental) counts
## per ml
diapenCR <- 	left_join(diapenE, diapenC, by = "grp_sz")

names(diapenCR)
### Remove unneeded columns
diapenCR <- 	subset(diapenCR, select = c(-samp_ev.y, -exp.y))


### Rename and re-order columns
diapenCR <- select(diapenCR, 
                   event = samp_ev.x,
                   sample = exp.x,
                   rep,
                   group = grp_sz,
                   cpm = counts_per_ml,
                   Cmn)


### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

diapenSJR1cr <- rowwise(diapenCR) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
