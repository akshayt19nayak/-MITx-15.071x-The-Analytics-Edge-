claims<-read.csv(file.choose())
str(claims)
table(claims$bucket2009)/nrow(claims)

#Splitting the data into train and test
library('caTools')
set.seed(88)
spl=sample.split(claims$bucket2009,SplitRatio = 0.6)
train=subset(claims,spl==TRUE)
test=subset(claims,spl==FALSE)

mean(train$age)
table(claims$diabetes)/nrow(claims)

#Baseline
base=table(test$bucket2009,test$bucket2008)
base
basaccuracy=(base[1,1]+base[2,2]+base[3,3]+base[4,4]+base[5,5])/sum(base)
basaccuracy

#Penalty error
PenMat=matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE,nrow = 5)
PenMat
PenErr=as.matrix(base)*PenMat
sum(PenErr)/nrow(test)
PenErr 

#If we were to predict the majority class in test$bucket2009 as the baseline
table(test$bucket2009)
122978/nrow(test)
sum(PenMat[,1]*table(test$bucket2009))/sum(table(test$bucket2009))

#Building a model
library('rpart')
library('rpart.plot')
Tree=rpart(bucket2009~.-reimbursement2009,data=train,method='class',cp=0.00005)
prp(Tree)

#Making predictions
pred=predict(Tree,newdata=test,type='class')
pt=table(test$bucket2009,pred)
(pt[1,1]+pt[2,2]+pt[3,3]+pt[4,4]+pt[5,5])/sum(pt)
sum(as.matrix(pt)*PenMat)/nrow(test)

#Thus we can see that although our accuracy has gone up, the penalty error has increased
Tree=rpart(bucket2009~.-reimbursement2009,data=train,method='class',cp=0.00005,
           parms=list(loss=PenMat))
#Running the above codes again we can see that both the cacuracy and penalty errors
#have gone down
94310/sum(pt)
