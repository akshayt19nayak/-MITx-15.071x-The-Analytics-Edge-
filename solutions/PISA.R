pisa<-read.csv(file.choose())
test<-read.csv(file.choose())

#1.2
str(pisa)
tapply(pisa$readingScore,pisa$male,mean,na.rm=TRUE)

#1.3
summary(pisa)

#1.4
pisa=na.omit(pisa)
test=na.omit(test)

#2.1
as.factor(pisa$grade)

#3.1
pisa$raceeth=relevel(pisa$raceeth,'White')
test$raceeth=relevel(test$raceeth,'White')
model1=lm(readingScore~.,data=pisa)
summary(model1)

#3.2
model1$residuals
RMSE1=sqrt((sum((model1$residuals)^2))/nrow(pisa))

#3.3 and 3.5
summary(model1)

#4.1
pred = predict(model1,test)
summary(pred)
max(pred)-min(pred)

#4.2
SSE=sum((test$readingScore-pred)^2)
RMSE2=sqrt(SSE/nrow(test))

#4.3
baseline=mean(pisa$readingScore)
TSS.test = sum((baseline-test$readingScore)^2) 
TSS.test

#4.4
R.squared=(TSS.test-SSE)/TSS.test
R.squared
