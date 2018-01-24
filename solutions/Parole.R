parole=read.csv(file.choose())

#1.2
summary(parole)
nrow(parole[parole$violator==1,])

#2.2
str(parole)
parole$crime=as.factor(parole$crime)
parole$state=as.factor(parole$state)
table(parole$crime)
summary(parole$crime)

#3.1
set.seed(144)
library(caTools)
split=sample.split(parole$violator,SplitRatio = 0.7)
train=subset(parole,split==TRUE)
test=subset(parole,split==FALSE)

#3.2
head(train)

#4.1
model=glm(violator~.,data=train,family=binomial)
summary(model)

#4.2
exp(1.61)

#4.3
logit=-4.2411574+0.3869904*1+0.8867192*1+50*(-0.0001756)+3*(-0.1238867)+12*0.0802954+0.6837143*1
odds=exp(logit)
odds
probability=exp(logit)/(1+exp(logit))
probability

#5.1
predTest=predict(model,type='response',newdata=test)
max(predTest)

#5.2
table(test$violator,predTest>0.5)
12/(12+11)
167/(167+12)
(12+167)/(12+12+167+11)

#5.3
table(test$violator)
179/(179+23)

#5.5
table(test$violator,predTest>0.3)
179/(179+10+13)
174/(9+19+174)

#5.6
library('ROCR')
ROCRpred=prediction(predTest,test$violator)
Auc=as.numeric(performance(ROCRpred,'auc')@y.values)
Auc
