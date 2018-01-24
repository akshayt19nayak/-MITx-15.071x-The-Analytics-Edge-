census=read.csv(file.choose())

#1.1
library(caTools)
set.seed(2000)
split=sample.split(census$over50k,SplitRatio = 0.6)
train=subset(census,split==TRUE)
test=subset(census,split==FALSE)
LogModel=glm(over50k~.,data=train,family=binomial)
summary(LogModel)

#1.2
predLog=predict(LogModel,newdata=test,type='response')
table(test$over50k,predLog>0.5)
(9051+1888)/nrow(test)

#1.3
summary(test$over50k)
9713/(9713+3078)

#1.4
library(ROCR)
ROCRpredlog=prediction(predLog,test$over50k)
AUC=as.numeric(performance(ROCRpredlog,'auc')@y.values)
AUC
perflog=performance(ROCRpredlog,'tpr','fpr')
plot(perflog)

#2.1
library(rpart)
library(rpart.plot)
Tree=rpart(over50k~.,data=train,method='class')
prp(Tree)

#2.4
predTree=predict(Tree,test,type='class')
table(test$over50k,predTree)
(9243+1596)/(470+9243+1482+1596)

#2.5
pred=predict(Tree,test)
ROCRpred=prediction(pred[,2],test$over50k)
perf=performance(ROCRpred,'tpr','fpr')
plot(perf)

#2.6
AUC=as.numeric(performance(ROCRpred,'auc')@y.values)
AUC

#3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
rfmodel=randomForest(over50k~.,data=trainSmall)
predrf=predict(rfmodel,newdata=test,type='class')
table(test$over50k,predrf)
(9584+1090)/nrow(test)

#3.2
vu = varUsed(rfmodel, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rfmodel$forest$xlevels[vusorted$ix]))

#3.3
varImpPlot(rfmodel)

#4.1
set.seed(2)
library('caret')
library('e1071')
numFolds=trainControl(method='cv',number=10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
train(over50k~.,data=train,method='rpart',trControl=numFolds,tuneGrid=cartGrid)

#4.2
cartcp=rpart(over50k~.,cp=0.002,method='class',data=train)
predbest=predict(cartcp,newdata = test,type='class')
table(test$over50k,predbest)
(9178+1838)/nrow(test)

#4.3
prp(cartcp)

