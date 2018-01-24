library(maps)
library(ggplot2)
library(ggmap)

#1.1
statesMap=map_data('state')
str(statesMap)
levels(as.factor(statesMap$group))

#1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")

#2.1
polling=read.csv(file.choose())
train=polling[polling$Year!=2012,]
test=polling[polling$Year==2012,]
mod2=glm(Republican~SurveyUSA+DiffCount,data=train,family = binomial)
TestPrediction=predict(mod2,newdata=test,type='response')
TestPredictionBinary=as.numeric(TestPrediction>0.5)

predictiondf=data.frame(TestPrediction,TestPredictionBinary,test$State)
table(TestPredictionBinary)
mean(TestPrediction)

#2.2
predictiondf$region=tolower(predictiondf$test.State)
predictionMap=merge(statesMap,predictiondf,by='region')
predictionMap=predictionMap[order(predictionMap$order),]

#2.4
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=TestPredictionBinary))+
  geom_polygon(color='black')
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=TestPredictionBinary))+
  geom_polygon(color='black')+scale_fill_gradient(low='blue',high='red',breaks=c(0,1),
                                                  guide='legend',labels=c('Democratic',
                                                                          'Republic'),
                                                  name='Predictions 2012')

ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=TestPrediction))+
  geom_polygon(color='black')+scale_fill_gradient(low='blue',high='red',
                                                  guide='legend',
                                                  name='Predictions 2012')

#3.1
predictiondf[predictiondf$region=='florida',]

#4.1
?geom_polygon
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=TestPredictionBinary))+
  geom_polygon(color='black',linetype=2)+scale_fill_gradient(low='blue',high='red',breaks=c(0,1),
                                                  guide='legend',labels=c('Democratic',
                                                                          'Republic'),
                                                  name='Predictions 2012')


ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=TestPredictionBinary))+
  geom_polygon(color='black',alpha=0.3)+scale_fill_gradient(low='blue',high='red',breaks=c(0,1),
                                                  guide='legend',labels=c('Democratic',
                                                                          'Republic'),
                                                  name='Predictions 2012')








