load("data/ctbmn_grptyp_mnct.Rdata")

YBP1 = subset(ctbmn_grptyp_mnct, samp_ev == "YBP1")
YBP1FC = subset(YBP1, exp == "FC")
YBP1T24 = subset(YBP1, exp == "T24")
YBP1exp = subset(YBP1, !(exp == "IC"))
YBP1_list <-  YBP1[, c("samp_ev", "exp", "category", "mn_ct")]


ggplot(data= YBP1, aes(x= reorder(category, -mn_ct, FUN = sum), y=mn_ct, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP1 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#999999','#56B4E9','#E69F00', '#6600cc'))+
  scale_y_log10(name = "Mean, Total Counts, log scale")
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1250, 50))+

ggplot(data= YBP1FC, aes(x= reorder(category, -mn_ct, FUN = sum), y=mn_ct, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP1FC Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#800000'))+
  scale_y_log10(name = "Mean, Total Counts, log scale")

ggplot(data= YBP1T24, aes(x= reorder(category, -mn_ct, FUN = sum), y=mn_ct, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP1T24 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#E69F00'))+
  scale_y_log10(name = "Mean, Total Counts, log scale")

ggplot(data= YBP1exp, aes(x= reorder(category, -mn_ct, FUN = sum), y=mn_ct, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP1 FC and T24 Mean Counts")+
  theme(axis.text.x = element_text(angle = 45,  vjust = 1, hjust=1), axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#800000','#E69F00'))+
  scale_y_log10(name = "Mean, Total Counts, log scale")




# List of the organisms in FC that were not in T24
load("data/RawcountComb.csv")
# FC
FC <- RawcountComb[grepl("FC", colnames(RawcountComb))]
FC <- cbind(FC, total = rowSums(FC, na.rm=T))
FC <- cbind(FC, RawcountComb$name)
FC <- data.frame(FC$`RawcountComb$name`, FC$total)
names(FC)[1] <- "org"
names(FC)[2] <- "total"
FC[FC==0]<-NA
FC <- FC[complete.cases(FC),]
FC <- FC$org
# T24
T24 <- RawcountComb[grepl("T24", colnames(RawcountComb))]
T24 <- cbind(T24, total = rowSums(T24, na.rm=T))
T24 <- cbind(T24, RawcountComb$name)
T24 <- data.frame(T24$`RawcountComb$name`, T24$total)
names(T24)[1] <- "org"
names(T24)[2] <- "total"
T24[T24==0]<-NA
T24 <- T24[complete.cases(T24),]
T24 <- T24$org
# Now there are two lists return the list of unmatched
print(FC[!(FC %in% T24)])
# 106 species



