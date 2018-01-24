data = read.csv(file.choose())

pdpdata = data[data$Year<2002,]
str(pdpdata)
pdpdata$RD = pdpdata$RS - pdpdata$RA
head(pdpdata)
plot(pdpdata$RD,pdpdata$W)

model=lm(pdpdata$W~pdpdata$RD,data=pdpdata)
summary(model)

80.881375 + 99*0.105766 #FE1

model2=lm(pdpdata$RS~pdpdata$OBP+pdpdata$SLG,data=pdpdata)
summary(model2)
-804.63+2737.77*0.311+0.405*1584.91 #FE2

model3=lm(pdpdata$RA~pdpdata$OOBP+pdpdata$OSLG,data=pdpdata)
summary(model3)
-837.38+2913.60*0.297+0.370*1514.29 #FE2

-804.63+2737.77*0.361+0.5*1584.91 #FE4
-804.63+2737.77*0.369+0.374*1584.91 #FE4

#FE5
teamRank = c(1,2,3,3,4,4,4,4,5,5)

wins2012=c(94,88,95,88,93,94,98,97,93,94)
wins2013=c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)
