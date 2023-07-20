########  TRY TO MAKE AN AXIS SCALE BREAK ON Y AXIS ######################
### For the biomass ingestion rates, because they go from negative numbers to
##  very large numbers and I can't use lot scale b/c the negatives go away

### Try this package
library(plotrix)
### With this argument: gap.plot

### S
options(scipen = 999)
p <- ggplot(FrAllOverall_ug, aes(x=group_size, y=FrBpm_ug))+
  geom_point(aes(color = FrBpm_ug>0), size = 5)+
#geom_point(stat = "identity",  color = "blue4", size = 5)+
  scale_x_discrete ("") +
  scale_y_continuous(labels = scales::label_comma(), breaks = c(-34, -15, 0, 25, 150, 300, 500))+
  #scale_y_cut(breaks = c(200000, 450000), which = c(.1,3), scales = c(0.5, 2)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Taxa Groups with Sizes") +
  ylab("Feeding Rate, pgC per copepod per day")+
  wimGraph()
p
#p1 <- p + scale_y_break(c(200000, 475000))

p1### Not working but try to get rid of scientific notation
### Create axis marks?
marks <- c(-1, 0, 10000, 100000, 200000, 500000, 600000)

### Wim's scale break code

scaleBreak <- function(xvalue, Break, rescale) {
  # Function to set up plotting an axis with a single scale break
  # This is an actual change of scale, unlike what ggplot has
  # Input:
  #   xvalue    The raw data (for either axis)
  #   Break     The value at the break in raw data units
  #   rescale   The divisor (>1 usually) to reduce the scale
  # Use:
  #   Run this for the data to be plotted
  #   Run it again for the axis tick locations  
  #   Then plot the rescaled data with the rescaled tick locations
  #      and the original tick locations for the tick labels
  
  ifelse(xvalue < Break, xvalue, (xvalue-Break)/rescale + Break)
}


### Replace with my data
Break      <- 60
rescale    <- 20
ylabels    <- c(-34, 0, 10, 20, 55, 150,500)
brks       <- scaleBreak(ylabels, Break, rescale)

FrAllOverall_ug$ySquish <- scaleBreak(FrAllOverall_ug$FrBpm_ug, Break=Break, rescale=20)

a <- ggplot(data=FrAllOverall_ug, aes(group_size, ySquish)) +
  #geom_point(shape=16, color="navy", size = 4) +
  geom_point(aes(color = FrBpm_ug>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("Taxon Group") +
  ggtitle("Ingestion Rates Across All Sampling Events")+
  scale_y_continuous("Ingestion rate, µgC L^-1", breaks=brks, labels=ylabels) +
  theme(plot.title = element_text(face = "bold", size = (18)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        legend.position = "none")+
  wimGraph()
a
a +  scale_color_manual(guide=FALSE)

scaleBreak <- function(xvalue, Break, rescale) {
  # Function to set up plotting an axis with a single scale break
  # This is an actual change of scale, unlike what ggplot has
  # Input:
  #   xvalue    The raw data (for either axis)
  #   Break     The value at the break in raw data units
  #   rescale   The divisor (>1 usually) to reduce the scale
  # Use:
  #   Run this for the data to be plotted
  #   Run it again for the axis tick locations  
  #   Then plot the rescaled data with the rescaled tick locations
  #      and the original tick locations for the tick labels
  
  ifelse(xvalue < Break, xvalue, (xvalue-Break)/rescale + Break)
}



Break      <- 5
rescale    <- 5
ylabels    <- c(0:5, 10, 15, 20)
brks       <- scaleBreak(ylabels, Break, rescale)

N <- 20
x <- data.frame(x= seq(0, 3, length=N ), y = exp(seq(0, 3, length=N)) )
plot(x$x, x$y)

x$ySquish <- scaleBreak(x$y, Break=Break, rescale=5)

ggplot(data=x, aes(x, ySquish)) +
  geom_point(shape=16, color="navy") +
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  xlab("X value") +
  scale_y_continuous("Scaled y value", breaks=brks, labels=ylabels) 


