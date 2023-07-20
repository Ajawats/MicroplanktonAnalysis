

CenDiaSmVolBio <- volbio_all_cr %>% 
  filter(samp_ev == "YBP2" &  group_size == "CenDiaSm")

CenDiaSmAll <- CenDiaSmVolBio %>% 
  filter(exp !="S")
sum(CenDiaSmAll$cpm)

CenDiaSmC <- CenDiaSmVolBio %>% 
  filter(exp == "C")
sum(CenDiaSmC$cpm)
mean(CenDiaSmC$cpm)
  
CenDiaSmE <- CenDiaSmVolBio %>% 
  filter(exp == "E")
sum(CenDiaSmE$cpm)
mean(CenDiaSmE$cpm)

CenDiaSmI <- CenDiaSmVolBio %>% 
  filter(exp == "I")
sum(CenDiaSmI$cpm)
mean(CenDiaSmI$cpm)

diaTestSumVol <- CenDiaSmVolBio %>% 
  aggregate(counts ~ samp_ev+exp+rep+Group+size, FUN = sum)
#CenDiaSmVolBio <- select(group_size, counts, cpm, vol_per_cell_um3, biomass_cell_pgC, )