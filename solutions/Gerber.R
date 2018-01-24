vote<-read.csv(file.choose())

#1.1
mean(vote$voting)

#1.2
table(vote$voting,vote$hawthorne)/nrow(vote)
table(vote$voting,vote$civicduty)/nrow(vote)
table(vote$voting,vote$neighbors)/nrow(vote)
table(vote$voting,vote$self)/nrow(vote)
table(vote$voting,vote$control)/nrow(vote)

#1.3
model1=glm(voting~hawthorne+civicduty+neighbors+self,family=binomial,data=vote)
summary(model1)
pred=predict(model1,type='response')

#1.4
cm=table(vote$voting,pred>0.3)
(cm[1,1]+cm[2,2])/nrow(vote)

#1.5
table(vote$voting,pred>0.5)

#1.6
library('ROCR')
ROCRpred=prediction(pred,vote$voting)
AUC=as.numeric(performance(ROCRpred,'auc')@y.values)
AUC

#2.1
library('rpart')
library('rpart.plot')
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote)
prp(CARTmodel)

#2.2
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote,cp=0.0)
prp(CARTmodel)

#2.3
CARTsex =rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=vote,cp=0.0)
prp(CARTsex)

#3.1
CARTcontrol=rpart(voting ~ control, data=vote,cp=0.0)
prp(CARTcontrol,digits=6)
0.34-0.296638

#3.2
CARTcs=rpart(voting ~ control+sex, data=vote,cp=0.0)
prp(CARTcs,digits=6)

#3.3
model2=glm(voting~sex+control,data=vote,family = binomial)
summary(model2)

#3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model2, newdata=Possibilities, type="response")
0.290806-0.290456

#3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=vote, family="binomial")
summary(LogModel2)

#3.6
predict(LogModel2, newdata=Possibilities, type="response")
0.2904558-0.290456
