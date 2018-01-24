polling<-read.csv(file.choose())
str(polling)
table(polling$Year)
summary(polling)

# Filling in missing values
install.packages('mice')
library('mice')
simple=polling[c('Rasmussen','SurveyUSA','PropR','DiffCount')]
set.seed(144)
imputed=complete(mice(simple))
polling$Rasmussen=imputed$Rasmussen
polling$SurveyUSA=imputed$SurveyUSA

summary(polling)

#Splitting the data into test and train
Train=subset(polling,Year==2004|Year==2008)
Test=subset(polling,Year==2012)

#Building a sophisticated baseline model
table(Train$Republican)
table(sign(Train$Rasmussen))
table(Train$Republican,sign(Train$Rasmussen))

#Exploratory Data Analyis
cor(Train[c('Rasmussen','SurveyUSA','DiffCount','PropR','Republican')])
summary(Train)

#Building a one variable model
model=glm(Republican~PropR,data=Train,family=binomial)
summary(model)

#Making predictions
pred1=predict(model,type='response')
table(Train$Republican,pred1>=0.5)

#Building a 2 variable model
model2=glm(Republican~PropR+SurveyUSA,data=Train,family=binomial)
summary(model2)

#Test Set Baseline
table(Test$Republican,sign(Test$Rasmussen)) 

#Making predictions
TestPrediction=predict(model2,newdata=Test,type='response')
table(Test$Republican,TestPrediction>=0.5)
