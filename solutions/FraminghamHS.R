data=read.csv(file.choose())
str(data)

#Splitting the data
library('caTools')
set.seed(1000)
split=sample.split(data$TenYearCHD,SplitRatio = 0.65)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)

#Building the model
TrainModel=glm(TenYearCHD~.,data=train,family=binomial)
summary(TrainModel)

#Making predictions
predictTest=predict(TrainModel,type='response',newdata=test)

#Confusion Matrix
CM=table(test$TenYearCHD,as.numeric(predictTest>0.5))
CM
Accuracy=(1069+11)/(1069+187+6+11)
BaselineAccuracy=(1069+6)/(187+1069+11+6)
Sensitivity=11/(11+187)
Specificity=1069/(1069+6)
Sensitivity
Specificity

#ROC Curves
library(ROCR)
ROCRpred=prediction(predictTest,test$TenYearCHD)
OOSAUC=as.numeric(performance(ROCRpred,'auc')@y.values)

