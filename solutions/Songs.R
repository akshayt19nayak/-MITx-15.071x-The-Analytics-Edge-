songs=read.csv(file.choose())

#1.1
summary(songs)
str(songs)
nrow(songs[songs$year==2010,])

#1.2
nrow(songs[songs$artistname=='Michael Jackson',])

#1.3
songs[songs$artistname=='Michael Jackson'&songs$Top10==1,]

#1.4
levels(as.factor(songs$timesignature))
table(as.factor(songs$timesignature))

#1.5
songs[songs$tempo==max(songs$tempo),]

#2.1
set.seed(100)
summary(as.factor(songs$year))
train=subset(songs,songs$year<=2009)
test=subset(songs,songs$year==2010)

#2.2
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[,!(names(train) %in% nonvars)]
test = test[,!(names(test) %in% nonvars)]
model=glm(Top10~.,family=binomial,data=train)
summary(model)

#2.3, 2.4 and 2.5
summary(model)

#3.1
cor(train$loudness,train$energy)

#3.2
model2 = glm(Top10 ~ . - loudness, data=train, family=binomial)
summary(model2)

#3.3
model3 = glm(Top10 ~ . - energy, data=train, family=binomial)
summary(model3)

#4.1
predTest=predict(model3,type='response',newdata=test)
table(test$Top10,predTest>0.45)
accuracy=(309+19)/(309+19+5+40)
accuracy

#4.2
table(test$Top10)
BaseAccuracy=(314/(314+59))
BaseAccuracy

#4.3
table(test$Top10,predTest>0.45)

#4.4
Sensitivity=19/(19+40)
Specificity=309/(309+5)
Sensitivity
Specificity

#4.5
table(test$Top10,predTest>0.65)
table(test$Top10,predTest>0.35)
