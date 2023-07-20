### 9/9/22
##  This file is copied from Bmean_Ct_GrpTyp.R, whose data inlcudes site water
##  samples. This file uses ct_bmn_fin_exp as its base, which excludes
##  site water samples and has only IC, T24 and FC 
library(tidyverse)
library(readxl)
library(writexl)

load("ct_bmn_fin.exp")
# ct_bmn_fin and ct_bmn_fin_exp are found in Bmass_Mn_Ct.R
# ct_bmn_fin includes all samples, including site water
# ct_bmn_fin_exp included only the experimentals samples, not site water.

### Combine group and type as one column because that's the level at which
##    I will group the different sizes

## First make a copy of the ct_bmn_fin data file to use for combining group and type
ct_bmn_grp_typ_exp <- ct_bmn_fin_exp
#  Combine group and type into one column and call it "grp_typ"
ct_bmn_grp_typ_exp$grp_typ <- (paste(ct_bmn_grp_typ_exp$group, ct_bmn_grp_typ_exp$type))
## Remove the columns I don't want and reorder the ones I keep
##  And then arrange the data frame alphabetically by grp_typ
##  so that all the same organisms are together and I can see the list
##  of them by their sizes
ctbmn_grptyp_exp <- select(ct_bmn_grp_typ_exp, samp_ev, exp, grp_typ, sa, la, wi, tot_ct, bmn_pgcml) %>% 
  arrange(grp_typ)
save(ctbmn_grptyp_exp, file = "ctbmn_grptyp_exp")
write_xlsx(ctbmn_grptyp_exp, "ctbmn_grptyp_exp.xlsx")


# Get a sum of the total counts of the same organisms of the same size
ctbmn_grptyp_exp_sumct <- ctbmn_grptyp_exp %>%
  group_by(grp_typ, sa, la, wi) %>%
  summarise(tot_ct = sum(tot_ct))
# Get a mean of the means, biomass of the same organisms of the same size
ctbmn_grptyp_exp_bmn <- ctbmn_grptyp_exp %>%
  group_by(grp_typ, sa, la, wi) %>%
  summarise(tot_bmn = mean(bmn_pgcml))

# Join the above two together so I can see the total counts and total
## mean biomass r
ctbm_grptyp_exp_grp <- cbind(ctbmn_grptyp_exp_sumct, ctbmn_grptyp_exp_bmn)
# Remove the extra columns it created that 
## I don't need and rename the columns
ctbm_grptyp_exp_grp2 <- select(ctbm_grptyp_exp_grp, -c(6,7,8,9)) %>% 
  rename( Organism= grp_typ...1, sa = sa...2, la =la...3, wi=wi...4, 
          Total_Count = tot_ct, Mean_Biomass_PgCMl= tot_bmn)

save(ctbm_grptyp_exp_grp2, file = "ctbm_grptyp_exp_grp2")
write_xlsx(ctbm_grptyp_exp_grp2, "ctbm_grptyp_exp_grp2.xlsx")

## Filter for each of the distinct grp-typ's so I can compare the sizes
## These lines below use the original ctbmn_grptyp file as their source
## which is the one with the site water samples. Replace it with 
## ctbmn_grptyp_exp if you want to run them with only experimental samples
## but I can't see why I would do that as there are easier ways.
grptyp_chl_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte other")
write_xlsx(ctbmn_grptyp, "grptyp_chl_oth.xlsx")
grptyp_chl_scen <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte scenedesmus")
write_xlsx(ctbmn_grptyp, "grptyp_chl_scen.xlsx")
grptyp_cil_cn <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate cone")
write_xlsx(ctbmn_grptyp, "grptyp_cil_cn.xlsx")
grptyp_cil_odd <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate other odd shape")
write_xlsx(ctbmn_grptyp, "grptyp_cil_odd.xlsx")
grptyp_cil_rnd <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate round")
write_xlsx(ctbmn_grptyp, "grptyp_cil_rnd.xlsx")
grptyp_cyan_aph <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria aphanizomenon")
write_xlsx(ctbmn_grptyp, "grptyp_cyan_aph.xlsx")
grptyp_cyan_dol <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria dolichospermum")
write_xlsx(ctbmn_grptyp, "grptyp_cyan_dol.xlsx")
grptyp_dia_cen <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom centric")
write_xlsx(ctbmn_grptyp, "grptyp_dia_cen.xlsx")
grptyp_dia_chn <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom chain")
write_xlsx(ctbmn_grptyp, "grptyp_dia_chn.xlsx")
grptyp_dia_pen <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom pennate")
write_xlsx(ctbmn_grptyp, "grptyp_dia_pen.xlsx")
grptyp_dia_pen_pleuro <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom pennate pleurosigma")
write_xlsx(ctbmn_grptyp, "grptyp_dia_pen_pleuro.xlsx")
grptyp_fla_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate other")
write_xlsx(ctbmn_grptyp, "grptyp_fla_oth.xlsx")
grptyp_fla_na <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate NA")
write_xlsx(ctbmn_grptyp, "grptyp_fla_na.xlsx")
grptyp_och_syn <- ctbmn_grptyp %>% 
  filter(grp_typ == "ochrophyte synura")
write_xlsx(ctbmn_grptyp, "grptyp_och_syn.xlsx")
grptyp_unid_flachl <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified flagchloro")
write_xlsx(ctbmn_grptyp, "grptyp_unid_flachl.xlsx")
grptyp_unid_rot <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified rotifer")
write_xlsx(ctbmn_grptyp, "grptyp_unid_rot.xlsx")
grptyp_unid_rnd <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified round")
write_xlsx(ctbmn_grptyp, "grptyp_unid_rnd.xlsx")
grptyp_unid_na <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified NA")
write_xlsx(ctbmn_grptyp, "grptyp_unid_na.xlsx")
grptyp_chl_col <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte colonial")
write_xlsx(ctbmn_grptyp, "grptyp_chl_col.xlsx")
grptyp_chl_act <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte actinastrum")
write_xlsx(ctbmn_grptyp, "grptyp_chl_act.xlsx")
grptyp_cil_obl <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate oblong")
write_xlsx(ctbmn_grptyp, "grptyp_cil_obl.xlsx")
grptyp_fla_col <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate colonial")
write_xlsx(ctbmn_grptyp, "grptyp_fla_col.xlsx")
grptyp_fla_cryp <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate cryptomonas")
write_xlsx(ctbmn_grptyp, "grptyp_fla_cryp.xlsx")
grptyp_fla_cn <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate in a cone")
write_xlsx(ctbmn_grptyp, "grptyp_fla_cn.xlsx")
grptyp_tin_ag <- ctbmn_grptyp %>% 
  filter(grp_typ == "tintinnid agglutinated")
write_xlsx(ctbmn_grptyp, "grptyp_tin_ag.xlsx")
grptyp_cil_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate other")
write_xlsx(ctbmn_grptyp, "grptyp_cil_oth.xlsx")
grptyp_cyan_na <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria NA")
write_xlsx(ctbmn_grptyp, "grptyp_cyan_na.xlsx")
grptyp_dia_egg <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom egg shaped")
write_xlsx(ctbmn_grptyp, "grptyp_dia_egg.xlsx")
grptyp_dia_pen_cyl <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom cylindrotheca pennate")
write_xlsx(ctbmn_grptyp, "grptyp_dia_pen_cyl.xlsx")
grptyp_dino_per <- ctbmn_grptyp %>% 
  filter(grp_typ == "dinoflagellate peridinium")
write_xlsx(ctbmn_grptyp, "grptyp_dino_per.xlsx")
grptyp_tin_hya <- ctbmn_grptyp %>% 
  filter(grp_typ == "tintinnid hyaline")
write_xlsx(ctbmn_grptyp, "grptyp_tin_hya.xlsx")
grptyp_dia_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom other")
write_xlsx(ctbmn_grptyp, "grptyp_dia_oth.xlsx")
grptyp_cyan_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria other")
write_xlsx(ctbmn_grptyp, "grptyp_cyan_oth.xlsx")
grptyp_flag_eug <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate euglenid")
write_xlsx(ctbmn_grptyp, "grptyp_flag_eug.xlsx")
grptyp_dia_pen_frag <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom pennate fragillaria")
write_xlsx(ctbmn_grptyp, "grptyp_dia_pen_frag.xlsx")
grptyp_chl_ped <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte pediastrum")
write_xlsx(ctbmn_grptyp, "grptyp_chl_ped.xlsx")

summarise(grptyp_chl_col, sum(tot_ct))         
summarise(grptyp_chl_col, mean(filter(sa, la, bmn_pgml)))          

