#1
bank = read.csv(file.choose())
str(bank)
mean(bank$age)

#2
sort(tapply(bank$duration,bank$job,mean))

#3
cor(bank[c('emp.var.rate','cons.conf.idx','cons.price.idx','euribor3m','nr.employed')])

#4
library(caTools)
set.seed(201)
split=sample.split(bank$y,SplitRatio = 0.7)

train=subset(bank,split==TRUE)
test=subset(bank,split==FALSE)

#5
str(bank)
logreg=glm(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+
             campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,
           data=train,family=binomial)
summary(logreg)

#6
exp(1.286)

#7
logpred=predict(logreg,newdata=test,type='response')
table(test$y,logpred>0.5)

table(test$y)
table(logpred>0.5)

#8
library(ROCR)
ROCRpred=prediction(logpred,test$y)
AUC=as.numeric(performance(ROCRpred,'auc')@y.values)
AUC

#11
ROCRpred
plot(performance(ROCRpred,'tpr','fpr'),colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=
       c(0.2,1.7))

#13
set.seed(201)
library(caret)
library(e1071)
numFolds=trainControl(method='cv',number=10)
cpGrid=expand.grid(.cp=seq(0.001,0.05,0.001))
train$y=as.factor(train$y)
train(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+
        campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,
      data=train,method='rpart',trControl=numFolds,tuneGrid=cpGrid)

#14
tree2=rpart(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+
              campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,
            data=train,cp=0.016,method='class')
prp(tree2)

#15
predtree2=predict(tree2,newdata=test,type='class')
table(test$y,predtree2)
(1293+37)/1500
