stocks=read.csv(file.choose())

#1.2
table(stocks$PositiveDec)/nrow(stocks)

#1.3
sort(cor(stocks))

#1.4
summary(stocks)

#2.1
set.seed(144)
library(caTools)
split=sample.split(stocks$PositiveDec,SplitRatio = 0.7)
stocksTrain=subset(stocks,split==TRUE)
stocksTest=subset(stocks,split==FALSE)

model=glm(PositiveDec~.,family=binomial,data=stocksTrain)
predtrain=predict(model,stocksTrain,type='response')$PositiveDec,predtrain>0.5)
cm=table(stocksTrain)
(cm[1,1]+cm[2,2])/nrow(stocksTrain)

#2.2
predtest=predict(model,newdata = stocksTest,type='response')
cmtest=table(stocksTest$PositiveDec,predtest>0.5)
(cmtest[1,1]+cmtest[2,2])/nrow(stocksTest)

#2.3
table(stocksTest$PositiveDec)/nrow(stocksTest)

#3.1
limitedTrain=stocksTrain
limitedTrain$PositiveDec=NULL
limitedTest=stocksTest
limitedTest$PositiveDec=NULL


#3.2
library(caret)
preproc=preProcess(limitedTrain)
normTrain=predict(preproc,limitedTrain)
normTest=predict(preproc,limitedTest)
summary(normTrain)
summary(normTest)

#3.4
set.seed(144)
km=kmeans(normTrain,centers=3)
str(km)

#3.5
library(flexclust)
km.kcca=as.kcca(km,normTrain)
clustertrain=predict(km.kcca)
clustertest=predict(km.kcca,newdata=normTest)
sum(clustertest==2)

#4.1
summary(stocksTrain[clustertrain==1,])
summary(stocksTrain[clustertrain==2,])
summary(stocksTrain[clustertrain==3,])

stocksTest[clustertrain==1,]
stocksTest[clustertrain==2,]
stocksTest[clustertrain==3,]

#4.2
model1=glm(PositiveDec~.,family=binomial,data=stocksTrain[clustertrain==1,])
model2=glm(PositiveDec~.,family=binomial,data=stocksTrain[clustertrain==2,])
model3=glm(PositiveDec~.,family=binomial,data=stocksTrain[clustertrain==3,])
summary(model1)
summary(model2)
summary(model3)

#4.3
predtest1=predict(model1,newdata = stocksTest[clustertest==1,],type='response')
cmtest1=table((stocksTest[clustertest==1,])$PositiveDec,predtest1>0.5)
(cmtest1[1,1]+cmtest1[2,2])/nrow(stocksTest[clustertest==1,])

predtest2=predict(model2,newdata = stocksTest[clustertest==2,],type='response')
cmtest2=table((stocksTest[clustertest==2,])$PositiveDec,predtest2>0.5)
(cmtest2[1,1]+cmtest2[2,2])/nrow(stocksTest[clustertest==2,])

predtest3=predict(model3,newdata = stocksTest[clustertest==3,],type='response')
cmtest3=table((stocksTest[clustertest==3,])$PositiveDec,predtest3>0.5)
(cmtest3[1,1]+cmtest3[2,2])/nrow(stocksTest[clustertest==3,])

#4.4
AllPredictions = c(predtest1, predtest2, predtest3)
AllOutcomes = c(stocksTest[clustertest==1,]$PositiveDec,
                stocksTest[clustertest==2,]$PositiveDec, 
                stocksTest[clustertest==3,]$PositiveDec)
cmoverall=table(AllOutcomes,AllPredictions>0.5)
(cmoverall[1,1]+cmoverall[2,2])/nrow(stocksTest)
