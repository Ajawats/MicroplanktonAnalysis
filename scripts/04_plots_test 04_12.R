### LSZ2
### Adding the reps to the plot makes it mostly illegible, so I'll plot the means
##  and reps separately
FRcpm_Rep_MnLSZ2 <- FRcpm_Rep_Mn %>% 
  filter(event =="LSZ2")
### Means, with Scale Break
Break      <- 350
rescale    <- 20
ylabels    <- c(-218,  0, 56, 120, 3120, 4890, 6832, 8000)
brks       <- scaleBreak(ylabels, Break, rescale)

FRcpm_Rep_MnLSZ2$ySquish <- scaleBreak(FRcpm_Rep_MnLSZ2$FR, 
                                       Break=Break, rescale=20)

a <- ggplot(data=FRcpm_Rep_MnLSZ2, aes(group_size, ySquish)) +
  #geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(color = "orange", size = 2)+
  #scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  #geom_point(data = FRcpm_Rep_MnLSZ2,
   #        aes(y=FRmnCpm),
   #        color= "blue", size=2)+
  xlab("Taxon Group") +
  ggtitle("LSZ2 Ingestion Rates Reps")+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  #scale_y_continuous(limits = c(-3, 6900), 
  #                  breaks = c(-2, 0, 5, 15, 20, 50, 300, 3000, 4000, 6832)) +
  ylab("mL"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

FRcpm_Rep_MnLSZ2 <- FRcpm_Rep_Mn %>% 
  filter(event =="LSZ2")

### Needs Scale Break, see below for Scale Break code, but I can't use
##  the Scale Break when plotting two variables. It just squished all the
##  CR reps together
### To plot without Scale Break
a <- ggplot(data=FRcpm_Rep_MnLSZ2, aes(group_size, FRmnCpm)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnCpm>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_point(data = FRcpm_Rep_MnLSZ2,
             aes(y=FR),
             color="orange", size=2)+
  xlab("Taxon Group") +
  ggtitle("LSZ2 Ingestion Rates, Means and Reps")+
  scale_y_continuous(limits = c(-219, 8100), 
                     breaks = c(-218, 0, 50, 200, 300, 2500, 6000, 8000)) +
  ylab("Cells"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a
