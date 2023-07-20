#######################################################
######## PLOT FOR ESD CR DATA ########################
#######################################################

### 5/26/23
source("scripts/01_function_wimGraph and Palettes.R")
load("data/EsdCpm_crmn")
ggplot(subset(EsdCpm_crmn, Group %in% "centricDiatom"), aes(esd, CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size =2.5)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  wimGraph()+
  ylab("ml"~copepod^-1~d^-1)+
  ggtitle("Centric Diatoms Clearance Rate by ESD")+
  guides(color = guide_legend("Clearance Rate > 0"))

ggplot(subset(EsdCpm_crmn, Group %in% "ciliate"), aes(esd, CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size =2.5)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  wimGraph()+
  ylab("ml"~copepod^-1~d^-1)+
  ggtitle("Ciliates Clearance Rate by ESD")+
  guides(color = guide_legend("Clearance Rate > 0"))

ggplot(subset(EsdCpm_crmn, Group %in% "flagellate"), aes(esd, CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size =2.5)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  wimGraph()+
  ylab("ml"~copepod^-1~d^-1)+
  ggtitle("Flagellates Clearance Rate by ESD")+
  guides(color = guide_legend("Clearance Rate > 0"))


### Try to plot with facet_wrap
ggplot(data=EsdCpm_crmn, aes(esd, CrMNmlcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1)+
  geom_point(aes(color = CrMNmlcd>0), size =2.5)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  wimGraph()+
  ylab("ml"~copepod^-1~d^-1)+
  ggtitle("Clearance Rate by ESD")+
  guides(color = guide_legend("Clearance Rate > 0"))

