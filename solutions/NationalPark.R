visits=read.csv(file.choose())

#1
visits2016jul=subset(visits,visits$Month==7&visits$Year==2016)
visits2016jul[(visits2016jul$logVisits)==max(visits2016jul$logVisits),]
table(visits2016jul$ParkType)

#2
visits2016jul$Region=as.factor(visits2016jul$Region)
sort(tapply(visits2016jul$logVisits,visits2016jul$Region,mean))

#3
cor(visits2016jul$cost,visits2016jul$logVisits)

#4
ys=subset(visits,visits$ParkName=='Yellowstone NP')
ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)
plot(ys_ts)

#5
visits = visits[rowSums(is.na(visits)) == 0, ]

#6
visits$Month = as.factor(visits$Month)
train=visits[visits$Year<2015,]
test=visits[visits$Year>2014,]
mod=lm(logVisits~laglogVisits,data=train)
summary(mod)
predmod=predict(mod,newdata=test)
RSS=sum((predmod-test$logVisits)^2)
TSS=(sum((test$logVisits-mean(train$logVisits))^2))
(TSS-RSS)/TSS
mean(train$logVisits)

#7
mod2=lm(logVisits~laglogVisits+laglogVisitsYear+Year+Month+ParkType+Region+cost,data=train)
summary(mod2)
predmod2=predict(mod2,newdata=test)
RSS2=sum((predmod2-test$logVisits)^2)
(TSS-RSS2)/TSS

#9
library(rpart)
library(rpart.plot)
tree=rpart(logVisits~laglogVisits+laglogVisitsYear+Year+Month+ParkType+Region+cost,data=train,cp=0.05)
prp(tree)
predtree=predict(tree,newdata=test)
RSStree=sum((predtree-test$logVisits)^2)
(TSS-RSStree)/(TSS)

#10
library(caret)
library(e1071)
set.seed(201)
numFolds=trainControl(method='cv',number=10)
cpGrid=expand.grid(.cp=seq(0.0001,0.005,0.0001))
train=na.omit(train)
train(logVisits~laglogVisits+laglogVisitsYear+Year+Month+ParkType+Region+cost,data=train,
      method='rpart',trControl=numFolds,tuneGrid=cpGrid)
0.0001

#11
tree=rpart(logVisits~laglogVisits+laglogVisitsYear+Year+Month+ParkType+Region+cost,data=train,
           cp=0.0001)
predtree2=predict(tree,newdata = test)
RSS2TREE=sum((predtree2-test$logVisits)^2)
(TSS-RSS2TREE)/TSS

#12
library(randomForest)
set.seed(201)
rf=randomForest(logVisits~laglogVisits+laglogVisitsYear+Year+Month+ParkType+Region+cost,data=train)
predrf=predict(rf,newdata=test)
RSSrf=sum((predrf-test$logVisits)^2)
(TSS-RSSrf)/TSS
