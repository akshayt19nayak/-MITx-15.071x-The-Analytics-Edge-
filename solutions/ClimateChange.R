data<-read.csv(file.choose())

#Problem1
set.seed(4)
train=data[data$Year<2007,]
test=data[data$Year>=2007,]
str(data)
model1=lm(Temp~MEI+CO2+CH4+N2O+Aerosols+CFC.11+CFC.12+TSI,data=train)
summary(model1)

#Problem2
cor(train)
pairs(train)

#Problem3
model2=lm(Temp~MEI+N2O+Aerosols+TSI,data=train)
summary(model2)

#Problem4
model3=step(model1)
summary(model3)

#Problem5
predictions=predict(model3,test)
SSE=sum((test$Temp-predictions)^2)
SST=sum((test$Temp-mean(train$Temp))^2)

R2 = (SST-SSE)/SST
R2
