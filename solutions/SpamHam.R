emails=read.csv(file.choose(),stringsAsFactors = FALSE)

#1.2
table(emails$spam)

#1.3
emails$text[1]

#1.5
max(nchar(emails$text))

#1.6
emails[nchar(emails$text)==min(nchar(emails$text)),]

#2.1
corpus=VCorpus(VectorSource(emails$text))
corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,stopwords('english'))
corpus=tm_map(corpus,stemDocument)
dtm=DocumentTermMatrix(corpus)
dtm

#2.2
spdtm=removeSparseTerms(dtm,0.95)
spdtm

#2.3
emailsSparse=as.data.frame(as.matrix(spdtm))
colnames(emailsSparse)=make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))

#2.4
emailsSparse$spam=emails$spam
table(sort(colSums(emailsSparse[emailsSparse$spam==0,]))>=5000)

#2.5
table(sort(colSums(emailsSparse[emailsSparse$spam==1,]))>=1000)

#3.1
emailsSparse$spam=as.factor(emailsSparse$spam)
set.seed(123)
split=sample.split(emailsSparse$spam,SplitRatio = 0.7)
train=subset(emailsSparse,split==TRUE)
test=subset(emailsSparse,split==FALSE)

library(randomForest)
spamLog=glm(spam~.,family=binomial,data=train)
spamCART=rpart(spam~.,method='class',data=train)
set.seed(123)
spamRF=randomForest(spam~.,data=train)

trainLog=predict(spamLog)
sum(trainLog<0.00001)
sum(trainLog>0.99999)
sum(trainLog>=0.00001 & trainLog<=0.99999)

#3.2
sum(summary(spamLog)$coeff[,4]<=0.05)

#3.3
prp(spamCART)

#3.4
table(train$spam,trainLog>0.5)
(3052+954)/nrow(train)

#3.5
library(ROCR)
predROCRlog=prediction(trainLog,train$spam)
as.numeric(performance(predROCRlog,'auc')@y.values)

#3.6
trainCART=predict(spamCART)
table(train$spam,trainCART[,2]>0.5)
(2885+894)/nrow(train)

#3.7
predROCRcart=prediction(trainCART[,2],train$spam)
as.numeric(performance(predROCRcart,'auc')@y.values)

#3.8
trainRF=predict(spamRF,type='prob')[,2]
table(train$spam,trainRF>0.5)
(3016+918)/nrow(train)

#3.9
predROCRrf=prediction(trainRF,train$spam)
as.numeric(performance(predROCRrf,'auc')@y.values)

#4.1
testlog=predict(spamLog,type='response',newdata = test)
table(test$spam,testlog>=0.5)
(1257+376)/nrow(test)

#4.2
testROCRlog=prediction(testlog,test$spam)
as.numeric(performance(testROCRlog,'auc')@y.values)

#4.3
testcart=predict(spamCART,newdata = test)
table(test$spam,testcart[,2]>=0.5)
(1228+386)/nrow(test)

#4.4
testROCRcart=prediction(testcart[,2],test$spam)
as.numeric(performance(testROCRcart,'auc')@y.values)

#4.5
testrf=predict(spamRF,newdata = test,type='prob')[,2]
table(test$spam,testrf>=0.5)
(1291+390)/nrow(test)

#4.6
testROCRrf=prediction(testrf,test$spam)
as.numeric(performance(testROCRrf,'auc')@y.values)

