stevens<-read.csv(file.choose())
str(stevens)

#Splitting data into train and test
library(caTools)
set.seed(3000)
split=sample.split(stevens$Reverse,SplitRatio = 0.7)
train=subset(stevens,split==TRUE)
test=subset(stevens,split==FALSE)

#Building a tree model
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
StevensTree=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                  data=train,method='class',minbucket=25)
prp(StevensTree)

#Making predictions
PredictTree=predict(StevensTree,newdata=test,type='class')
table(test$Reverse,PredictTree)

#ROC Curves
library('ROCR')
ROCRpred=predict(StevensTree,newdata=test)
ROCRpred
pred=prediction(ROCRpred[,2],test$Reverse)
perf=performance(pred,'tpr','fpr')
plot(perf)

AUC=as.numeric(performance(pred,'auc')@y.values)
AUC

#Looking at the trees for different value of minbucket
StevensTree5=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                  data=train,method='class',minbucket=5)
StevensTree100=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                  data=train,method='class',minbucket=100)
prp(StevensTree5)
prp(StevensTree100)

#Building a model using random forest
install.packages('randomForest')
library('randomForest')
StevensForest=randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt
                           +Unconst,data=train,nodesize=25,ntree=200)
train$Reverse=as.factor(train$Reverse)
test$Reverse=as.factor(test$Reverse)
PredictForest=predict(StevensForest,newdata=test)
table(test$Reverse,PredictForest)
(40+74)/(19+37+40+74)

#Using different values of seed
set.seed(100)
StevensForest100=randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt
                              +Unconst,data=train,nodesize=25,ntree=200)
PredictForest100=predict(StevensForest100,newdata=test)
table(test$Reverse,PredictForest100)
(43+74)/(43+74+34+19)

set.seed(200)
StevensForest200=randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt
                              +Unconst,data=train,nodesize=25,ntree=200)
PredictForest200=predict(StevensForest200,newdata=test)
table(test$Reverse,PredictForest200)
(44+76)/(33+17+44+76)

#Cross Validation
library('caret')
library('e1071')
numFolds=trainControl(method='cv',number=10)
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt
      +Unconst,data=train,method='rpart',trControl=numFolds,tuneGrid=cpGrid)

TreeCV=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt
             +Unconst,data=train,cp=0.18,method='class')
prp(TreeCV)
