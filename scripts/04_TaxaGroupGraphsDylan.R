####################################################################
############### TAXA GROUP GRAPHS FROM DYLAN 11/29 #################
####################################################################



load("data/x.Rdata")
library(tidyverse)
library(viridis) # R package for colors

load("data/taxa_pic.Rdata")
taxa_pic <- select(taxa_pic, totalCPM = tot_ct_ml, group = grp_typ, 
                   sampEvent = samp_ev)
# Save taxa_pic as "x"
# base R
#Split data into different groups

x <- taxa_pic %>% filter(totalCPM != 0)
# colors for graph
x$cols = as.numeric(as.factor(x$group))
x <- x[order(x$cols,decreasing = TRUE),]
x$cols_vir = rep(viridis(39), each= 6)
# filter out 0s
x = x %>% filter(totalCPM != 0)

# subset into groups
SJR2 = subset(x, sampEvent == "SJR2")
YBP1 = subset(x, sampEvent == "YBP1")
LSZ2 = subset(x, sampEvent == "LSZ2")
SJR1 = subset(x, sampEvent == "SJR1")
YBP2 = subset(x, sampEvent == "YBP2")
WLD2 = subset(x, sampEvent == "WLD2")


# order all of the datasets
SJR1 <- SJR1[order(SJR1$totalCPM,decreasing = TRUE),]
SJR2 <- SJR2[order(SJR2$totalCPM,decreasing = TRUE),]
YBP2 <- YBP2[order(YBP2$totalCPM,decreasing = TRUE),]
YBP1 <- YBP1[order(YBP1$totalCPM,decreasing = TRUE),]
LSZ2 <- LSZ2[order(LSZ2$totalCPM,decreasing = TRUE),]
WLD2 <- WLD2[order(WLD2$totalCPM,decreasing = TRUE),]

#pdf("C:/Users/dylan/OneDrive/Desktop/Copepods/Copepods/Figures/Samp Event Barplot.pdf",
   # height = 10,
   # width = 20)
# now plot all of them
par(mfrow = c(2,4))
# SJR1
barplot(SJR1$totalCPM,names.arg = SJR1$seq, col = SJR1$cols_vir, las =2, cex.lab=2, cex.axis =1, 
        cex.names = 1.2, col.axis="black", ylim = c(0,301), main="SJR1")
# LSZ2
barplot(LSZ2$totalCPM,names.arg = LSZ2$seq, col = LSZ2$cols_vir, las =2, cex.lab=1, cex.axis =1, 
        cex.names = 1.2, col.axis="black", ylim = c(0,350), main="LSZ2")
# YBP2
barplot(YBP2$totalCPM,names.arg = YBP2$seq, col = YBP2$cols_vir, las =2, cex.lab=1, cex.axis =1, 
        cex.names = 1, col.axis="black", ylim = c(0,250), main="YBP2")
# legend only 1 time
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n', xlab="", ylab="")
legend("center", legend = unique(x$group), pch=16, col= unique(x$cols_vir), ncol=2)
# SJR2
barplot(SJR2$totalCPM,names.arg = SJR2$seq, col = SJR2$cols_vir, las =2, cex.lab=1, cex.axis =1, 
        cex.names = 1.2, col.axis="black", ylim = c(0, 120), main= "SJR2")
# WLD2
barplot(WLD2$totalCPM,names.arg = WLD2$seq, col = WLD2$cols_vir, las =2, cex.lab=1, cex.axis =1, 
        cex.names = 1.2, col.axis="black", ylim = c(0,140), main="WLD2")
# YBP1
barplot(YBP1$totalCPM,names.arg = YBP1$seq, col = YBP1$cols_vir, las =2, cex.lab=1, cex.axis =1, 
        cex.names = 1.2, col.axis="black", ylim= c(0, 400), main="YBP1")
#par(mfrow = c(1,1))
dev.off()


