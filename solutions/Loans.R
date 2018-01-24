loans=read.csv(file.choose())
summary(loans)

#1.1
(nrow(loans[loans$not.fully.paid==1,]))/(nrow(loans))

#1.3
subset(loans,is.na(loans$pub.rec)==1)
names(loans)
subset(loans,is.na(loans$pub.rec)|is.na(log.annual.inc)| is.na(days.with.cr.line) |
is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs))

#1.4
library(mice)
set.seed(144)
vars.for.imputation=setdiff(names(loans),'not.fully.paid')
imputed=complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation]=imputed
summary(loans)
head(loans,11)
loansimputed=read.csv(file.choose())
loans=loansimputed

#2.1
set.seed(144)
split=sample.split(loansimputed$not.fully.paid,SplitRatio = 0.7)
train=subset(loansimputed,split==TRUE)
test=subset(loansimputed,split==FALSE)

model=glm(not.fully.paid~.,data=train,family=binomial)
summary(model)
summary(model)$coeff[,4]

#2.2
(-9.317e-03*700)-(-9.317e-03*710)
exp(-9.317e-03*700)/exp(-9.317e-03*710)

#2.3
pred=predict(model,newdata = test,type='response')
test$predicted.risk=pred

confusionmat=table(test$not.fully.paid,pred>0.5)
accuracy=(confusionmat[1,1]+confusionmat[2,2])/sum(confusionmat)
base=table(loans$not.fully.paid)
basaccuracy=(base[1])/sum(base)
basaccuracy

#2.4
library(ROCR)
ROCRpred=prediction(pred,test$not.fully.paid)
AUCtest=as.numeric(performance(ROCRpred,'auc')@y.values)
AUCtest

#3.1
model2=glm(not.fully.paid~int.rate,data=train,family=binomial())
summary(model2)

#3.2
pred2=predict(model2,newdata=test,type='response')
max(pred2)
table(test$not.fully.paid,pred2>0.5)

#3.3
library(ROCR)
ROCRp2=prediction(pred2,test$not.fully.paid)
AUC2=as.numeric(performance(ROCRp2,'auc')@y.values)
AUC2

#4.1
10*exp(3*0.06)


#5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)

#6.1
highint=test[test$int.rate>=0.15,]
mean(highint$profit)
(nrow(highint[highint$not.fully.paid==1,]))/nrow(highint)
summary(highint)

#6.2
cutoff=sort(highint$predicted.risk,decreasing = FALSE)[100]
bestloans=subset(highint,highint$predicted.risk<=cutoff)[1:100,]
sum(bestloans$profit)
nrow(bestloans[bestloans$not.fully.paid==1,])
