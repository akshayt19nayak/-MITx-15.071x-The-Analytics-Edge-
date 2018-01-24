tweets=read.csv(file.choose(),stringsAsFactors = FALSE)

#1.1
library(tm)
library(SnowballC)
corpus=VCorpus(VectorSource(tweets$Tweet))
corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,stopwords('english'))
dtm=DocumentTermMatrix(corpus)
allTweets=as.data.frame(as.matrix(dtm))

#2.1
install.packages('wordcloud')
library(wordcloud)
?wordcloud
colSums(allTweets)

#2.3
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25))
tail(sort(colSums(allTweets)))

#2.4 - Removing 'apple' as it is the most freqently occuring word
tweets=read.csv(file.choose(),stringsAsFactors = FALSE)
corpus=VCorpus(VectorSource(tweets$Tweet))
corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,c('apple',stopwords('english')))
dtm=DocumentTermMatrix(corpus)
allTweets=as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25))

#3.1
tweets$Negative=as.factor(tweets$Avg<=-1)
allTweets$Negative=tweets$Negative

negTweets=allTweets[allTweets$Negative==TRUE,]
negTweets$Negative=NULL
allTweets$Negative=NULL

wordcloud(colnames(negTweets),colSums(negTweets),scale=c(2,0.25))

#3.3
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),random.order = FALSE)

#3.4
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),
          rot.per = 0.5 )

#3.5
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),random.color = TRUE)

#4.1
display.brewer.all()

#4.3
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),
          colors=brewer.pal(9,'Blues')[c(5,6,7,8,9)])










