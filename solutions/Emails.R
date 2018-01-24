emails=read.csv(file.choose(),stringsAsFactors = FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1])

#Convert everything to a corpus
library(tm)
library(SnowballC)
corpus=Corpus(VectorSource(emails$email))

#Pre-processing
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus, removeWords, stopwords('english'))
corpus=tm_map(corpus,stemDocument)

#DocumentTermMatrix
dtm=DocumentTermMatrix(corpus)
dtm
dtm=removeSparseTerms(dtm,0.97)
labeledTerms=as.data.frame(as.matrix(dtm))
labeledTerms$responsive=emails$responsive
str(labeledTerms)

#Splitting the data
library(caTools)
set.seed(144)
spl=sample.split(labeledTerms$responsive, SplitRatio = 0.7)
train=subset(labeledTerms,spl==TRUE)
test=subset(labeledTerms, spl==FALSE)

#Building a model
library(rpart)
library(rpart.plot)
emailCART=rpart(responsive~.,data=train,method = 'class')
prp(emailCART)
predCART=predict(emailCART,newdata=test)
predCART

pred.prob=predCART[,2]
table(test$responsive,pred.prob>=0.5)
table(test$responsive)/nrow(test)

library(ROCR)
predROCR=prediction(pred.prob,test$responsive)
perfROCR=performance(predROCR,'tpr','fpr')
plot(perfROCR,colorize=TRUE)
as.numeric(performance(predROCR,'auc')@y.values)
#Thus this model can differentiate between a positive and negative example 80% of the times
