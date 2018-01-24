letters<-read.csv(file.choose())

#1.1
library('caTools')
set.seed(1000)
letters$isB=as.factor(letters$letter=='B')
split=sample.split(letters$isB,SplitRatio = 0.5)
train=subset(letters,split==TRUE)
test=subset(letters,split==FALSE)
summary(test$isB)
1175/(1175+383)

#1.2
library('rpart')
library('rpart.plot')
CARTb = rpart(isB ~. -letter, data=train, method="class")
predb=predict(CARTb,newdata=test,type='class')
table(test$isB,predb)
(340+1118)/(1118+57+43+340)

#1.3
library(randomForest)
set.seed(1000)
model1=randomForest(isB~.-letter,data=train)
predRF=predict(model1,newdata=test)
table(test$isB,predRF)
(374+1165)/(374+1165+10+9)

#2.1
letters$letter = as.factor(letters$letter) 
set.seed(2000)
split1=sample.split(letters$letter,SplitRatio = 0.5)
trainlet=subset(letters,split1==TRUE)
testlet=subset(letters,split1==FALSE)
summary(testlet$letter)
401/(395+401+383+379)

#2.2
CARTlet=rpart(letter~.-isB,data=trainlet,method='class')
predlet=predict(CARTlet,newdata=testlet,type='class')
table(testlet$letter,predlet)
(348+318+363+340)/nrow(test)

#2.3
model2=randomForest(letter~.-isB,data=trainlet)
predRF2=predict(model2,newdata=testlet)
table(testlet$letter,predRF2)
(390+380+393+369)/nrow(test)


