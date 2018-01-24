flu<-read.csv(file.choose())

#1.1
summary(flu)
head(flu)
flu[flu$ILI==max(flu$ILI),]
flu[flu$Queries==max(flu$Queries),]

#1.2
hist(flu$ILI,breaks = 100)

#1.3
plot(flu$Queries,log(flu$ILI))
plot(flu$Queries,flu$ILI)

#2.2
model1=lm(log(ILI)~Queries,data=flu)
summary(model1)

#2.3
corr = cor(log(flu$ILI),flu$Queries)
R.squared=0.709
corr^2

#3.1
flutest<-read.csv(file.choose())
pred = exp(predict(model1,flutest))
flutest[flutest$Week=='2012-03-11 - 2012-03-17',]
pred[11]

#3.2
(flutest$ILI[11]-pred[11])/flutest$ILI[11]

#3.3
RMSE=sqrt((sum((pred-flutest$ILI)^2))/nrow(flutest))
RMSE

#4.1
install.packages('zoo')
library(zoo)
ILILag2=lag(zoo(flu$ILI),-2,na.pad=TRUE)
flu$ILILag2=coredata(ILILag2)
summary(flu)

#4.2
plot(log(flu$ILILag2),log(flu$ILI))

#4.3
model2=lm(log(ILI)~log(ILILag2)+Queries,data=flu)
summary(model2)

#5.1
ILITestlag2=lag(zoo(flutest$ILI),-2,na.pad=TRUE)
flutest$ILILag2=coredata(ILITestlag2)

summary(flutest)

#5.3
head(flutest)
flutest$ILILag2[1]=flu$ILI[nrow(flu)-1]
flutest$ILILag2[2]=flu$ILI[nrow(flu)]

tail(flu,2)
head(flutest,2)

#5.4
pred2=exp(predict(model2,newdata=flutest))
RMSE2=sqrt(sum((pred2-flutest$ILI)^2)/nrow(flutest))
RMSE2
