data<-read.csv(file.choose())
install.packages('caTools')

#Splitting The Data
library('caTools')
set.seed(88)
split=sample.split(data$PoorCare,SplitRatio = 0.75)
qualityTrain=subset(data,split==TRUE)
qualityTest=subset(data,split==FALSE)

#Building the first model
QualityLog=glm(PoorCare~OfficeVisits+Narcotics,data=qualityTrain,family=binomial)
summary(QualityLog)

#Making predictions
predictTrain=predict(QualityLog,type='response')

#Building the second model
model2=glm(PoorCare~StartedOnCombination+ProviderCount,data=qualityTrain,
           family=binomial)
summary(model2)


#Confusion Matrix and Varying The Threshold
table(qualityTrain$PoorCare,predictTrain>0.5)
table(qualityTrain$PoorCare,predictTrain>0.2)
table(qualityTrain$PoorCare,predictTrain>0.7)


#ROC Curves
install.packages('ROCR')
library('ROCR')
ROCRpred=prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf=performance(ROCRpred,'tpr','fpr')
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1))

#Making predictions on test set
predictTest=predict(QualityLog,newdata=qualityTest,type='response')
table(qualityTest$PoorCare,as.numeric(predictTest>0.3))
ROCpredtest=prediction(predictTest,qualityTest$PoorCare)
AUC=as.numeric(performance(ROCpredtest,'auc')@y.values)
AUC
