### make a file so I can look at cyano dimensions

load("data/Clearance Rates/volbio_all_cr.Rdata")

cyanoLook2 <- volbio_all_cr %>% 
  filter(group_size %in% c("CyanoLg", "CyanoSm")) %>% 
  select(group_size, sa, la, wi, counts, esd) %>% 
  group_by(group_size, sa, la, esd) %>% 
  summarise(ctPerSaLa = sum(counts)) %>% 
  ungroup
duplicated(cyanoLook)

cyanoLook <- cyanoLook %>% distinct()
write_xlsx(cyanoLook,"data/cyanoLook.xlsx")
write_xlsx(cyanoLook2,"data/cyanoLook2.xlsx")
