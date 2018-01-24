wiki<-read.csv(file.choose(),stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal=as.factor(wiki$Vandal)
Sys.setlocale("LC_ALL", "C")

#1.1
table(wiki$Vandal)

#1.2
library(tm)
library(SnowballC)
corpusAdded=VCorpus(VectorSource(wiki$Added))
corpusAdded=tm_map(corpusAdded,removeWords,stopwords('english'))
corpusAdded=tm_map(corpusAdded,stemDocument)
dtmAdded=DocumentTermMatrix(corpusAdded)
dtmAdded

#1.3
sparseAdded=removeSparseTerms(dtmAdded,0.997)
sparseAdded

#1.4
wordsAdded=as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved=VCorpus(VectorSource(wiki$Removed))
corpusRemoved=tm_map(corpusRemoved,removeWords,stopwords('english'))
corpusRemoved=tm_map(corpusRemoved,stemDocument)
dtmRemoved=DocumentTermMatrix(corpusRemoved)
sparseRemoved=removeSparseTerms(dtmRemoved,0.997)
wordsRemoved=as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#1.5
library(caTools)
wikiWords = cbind(wordsAdded, wordsRemoved) 
wikiWords$Vandal=wiki$Vandal
set.seed(123)
split=sample.split(wikiWords$Vandal,SplitRatio = 0.7)
train=subset(wikiWords,split==TRUE)
test=subset(wikiWords, split==FALSE)

table(test$Vandal)/nrow(test)

#1.6
library(rpart)
library(rpart.plot)
tree=rpart(Vandal~.,data=train,method='class')
predtree=predict(tree,type='class',newdata = test)
table(test$Vandal,predtree)
(618+12)/nrow(test)

#1.7
prp(tree)

#2.1
wikiWords2=wikiWords
wikiWords2$HTTP=ifelse(grepl('http',wiki$Added,fixed=TRUE),1,0)
table(wikiWords2$HTTP)/nrow(wikiWords2)

#2.2
wikiTrain2=subset(wikiWords2,split==TRUE)
wikiTest2=subset(wikiWords2,split==FALSE)
tree2=rpart(Vandal~.,data=wikiTrain2,method='class')
predtree2=predict(tree2,type='class',newdata = wikiTest2)
table(wikiTest2$Vandal,predtree2)
(609+57)/(9+488+609+57)

#2.3
wikiWords2$NumWordsAdded=rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved=rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

#2.4
wikiwordstrain2=subset(wikiWords2,split==TRUE)
wikiwordstest2=subset(wikiWords2,split==FALSE)
tree3=rpart(Vandal~.,data=wikiwordstrain2,method='class')
predtree3=predict(tree3,type='class',newdata = wikiwordstest2)
table(wikiwordstest2$Vandal,predtree3)
(514+248)/(514+104+297+248)

#3.1
wikiwords3=wikiWords2
wikiwords3$Minor=wiki$Minor
wikiwords3$Loggedin=wiki$Loggedin
wikiwordstrain3=subset(wikiwords3,split==TRUE)
wikiwordstest3=subset(wikiwords3,split==FALSE)
tree4=rpart(Vandal~.,data=wikiwordstrain3,method='class')
predtree4=predict(tree4,type='class',newdata = wikiwordstest3)
table(wikiwordstest3$Vandal,predtree4)
(595+241)/(595+241+304+23)

#3.2
prp(tree4)
