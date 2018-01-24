tweets<-read.csv(file.choose(),stringsAsFactors = FALSE)
str(tweets)

#Percentage of Negative tweets 
tweets$Negative=as.factor(tweets$Avg<=-1)
table(tweets$Negative)

#Libraries for text mining
install.packages('tm')
install.packages('SnowballC')
library(tm)
library(SnowballC)

#To convert everything to a corpus
corpus=Corpus(VectorSource((tweets$Tweet)))

#To convert everything in a corpus to lowercase
corpus[[1]]$content
corpus=tm_map(corpus,tolower)
corpus[[1]]$content

#To remove all punctuation marks in the corpus 
corpus=tm_map(corpus,removePunctuation)
corpus[[1]]$content

#To remove the stopwords
stopwords('english')[1:10]
corpus=tm_map(corpus,removeWords,c('apple',stopwords('english')))
corpus[[1]]$content

#And lastly for stemming
corpus=tm_map(corpus,stemDocument)
corpus[[1]]$content

#Generating a DocumentTermMatrix
frequencies=DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])

#Finding the more frequent terms
freq=findFreqTerms(frequencies,lowfreq=20)
freq
sparse=removeSparseTerms(frequencies,0.995)
sparse
tweetsSparse=as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
tweetsSparse$Negative=tweets$Negative

#Splitting the data
split=sample.split(tweetsSparse$Negative,SplitRatio = 0.7)
trainSparse=subset(tweetsSparse,split==TRUE)
testSparse=subset(tweetsSparse,split==FALSE)

#Building models
library(rpart)
library(rpart.plot)
library(randomForest)

#CART
tweetCART=rpart(Negative~.,data=trainSparse,method='class')
prp(tweetCART)
predictCART=predict(tweetCART,newdata = testSparse,type='class')
cmCART=table(testSparse$Negative,predictCART)
(cmCART[1,1]+cmCART[2,2])/nrow(testSparse)

#RandomForests
tweetRF=randomForest(trainSparse$Negative~.,data=trainSparse)
predictRF=predict(tweetRF,newdata=testSparse)
cmRF=table(testSparse$Negative,predictRF)
(cmRF[1,1]+cmRF[2,2])/nrow(testSparse)

#Logistic Regression
tweetLogReg=glm(Negative~.,family=binomial,data=trainSparse)
predictLogReg=predict(tweetLogReg,newdata=testSparse,type='response')
cmLogReg=table(testSparse$Negative,predictLogReg>0.5)
(cmLogReg[1,1]+cmLogReg[2,2])/nrow(testSparse)
