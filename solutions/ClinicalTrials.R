trials<-read.csv(file.choose(),stringsAsFactors = FALSE)

#1.1
str(trials)
max(nchar(trials$abstract))

#1.2
nrow(trials[nchar(trials$abstract)==0,])

#1.3
trials[nchar(trials$title)==min(nchar(trials$title)),]
strwrap(trials[1258,])

#2.1
library(tm)
library(SnowballC)

corpustitle=VCorpus(VectorSource(trials$title))
corpusabstract=VCorpus(VectorSource(trials$abstract))

corpustitle=tm_map(corpustitle,content_transformer(tolower))
corpusabstract=tm_map(corpusabstract,content_transformer(tolower))

corpustitle=tm_map(corpustitle,removePunctuation)
corpusabstract=tm_map(corpusabstract,removePunctuation)

corpustitle=tm_map(corpustitle,removeWords,stopwords('english'))
corpusabstract=tm_map(corpusabstract,removeWords,stopwords('english'))

corpustitle=tm_map(corpustitle,stemDocument)
corpusabstract=tm_map(corpusabstract,stemDocument)

dtmtitle=DocumentTermMatrix(corpustitle)
dtmabstract=DocumentTermMatrix(corpusabstract)

dtmtitle=removeSparseTerms(dtmtitle,0.95)
dtmabstract=removeSparseTerms(dtmabstract,0.95)

dtmtitle=as.data.frame(as.matrix(dtmtitle))
dtmabstract=as.data.frame(as.matrix(dtmabstract))

#2,3
sort(colSums(dtmabstract))

#3.1
colnames(dtmtitle) = paste0("T", colnames(dtmtitle))
colnames(dtmabstract) = paste0("A", colnames(dtmabstract))

#3.2
dtm = cbind(dtmtitle, dtmabstract)
dtm$trials=trials$trial

#3.3
library(caTools)
set.seed(144)
split=sample.split(dtm$trials,SplitRatio = 0.7)
train=subset(dtm,split==TRUE)
test=subset(dtm,split==FALSE)
table(test$trials)/nrow(test)

#3.4
trialCART=rpart(trials~.,method='class',data=train)
prp(trialCART)

#3.5
predtree=predict(trialCART)[,2]
max(predtree)

#3.6
max(predict(trialCART,newdata=test)[,2])
prob=levels(as.factor(predict(trialCART)[,2]))
prob

#3.7
table(train$trials,predtree>=0.5)
(631+441)/(631+441+131+99)
441/(441+131)
631/(631+99)

#4.1
predtest=predict(trialCART,newdata=test)
table(test$trials,predtest[,2]>=0.5)
(261+162)/(261+162+52+83)

#4.2
ROCRpred=prediction(predtest[,2],test$trials)
performance(ROCRpred,'auc')@y.values

