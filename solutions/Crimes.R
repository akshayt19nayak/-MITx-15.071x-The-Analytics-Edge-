mvt=read.csv(file.choose(),stringsAsFactors = FALSE)

#Seeing the structure of the data
str(mvt)

#Converting the 'Date' variable into its appropriate format
mvt$Date=strptime(mvt$Date,format='%m/%d/%y %H:%M')
mvt$Weekdays=weekdays(mvt$Date)
mvt$Hour=mvt$Date$hour
table(mvt$Weekdays)
table(mvt$Weekdays,mvt$Hour)
Weekdays=as.data.frame(table(mvt$Weekdays))
DayHourCounts=as.data.frame(table(mvt$Weekdays,mvt$Hour))

#Visualising the data frame with the help of ggplot2
ggplot(Weekdays,aes(x=Var1,y=Freq))+geom_line(aes(group=1))

#To display it in chronological order
Weekdays$Var1=factor(Weekdays$Var1,ordered=TRUE,levels=c('Sunday','Monday','Tuesday',
                                                         'Wednesday','Thursday','Friday',
                                                         'Saturday'))
ggplot(Weekdays,aes(x=Var1,y=Freq))+geom_line(aes(group=1),alpha=0.3)

#Visualising the crimes per day according to the number of the hours
str(DayHourCounts)
DayHourCounts$Hour=as.numeric(DayHourCounts$Var2)
ggplot(DayHourCounts,aes(x=Hour,y=Freq))+geom_line(aes(group=Var1,color=Var1),size=2)

#To make the heatmap
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday",
                                                                      "Wednesday","Thursday", 
                                                                       "Friday", "Saturday",
                                                                       "Sunday"))
ggplot(DayHourCounts,aes(x=Hour,y=Var1))+geom_tile(aes(fill=Freq))+
  scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())

#Changing the highs and lows
ggplot(DayHourCounts,aes(x=Hour,y=Var1))+geom_tile(aes(fill=Freq))+
  scale_fill_gradient(name="Total MV Thefts",high = 'black',low = 'white') +
  theme(axis.title.y = element_blank())

#Maps
install.packages('maps')
install.packages('ggmap')
library(maps)
library(ggmap)
chicago=get_map(location='chicago',zoom = 11)
ggmap(chicago)
ggmap(chicago)+geom_point(data=mvt[1:100,],aes(x=Longitude,y=Latitude))

LatLonCounts=as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Lat=as.numeric(as.character(LatLonCounts$Var2))
LatLonCounts$Long=as.numeric(as.character(LatLonCounts$Var1))

#Plotting only those places that have a frequency of crime greater than 0
ggmap(chicago)+geom_point(data=LatLonCounts[LatLonCounts$Freq>0,],
                          aes(x=Long,y=Lat,color=Freq,size=Freq))+
  scale_colour_gradient(low='yellow',high='red')

#Heatmap
ggmap(chicago)+geom_tile(data=LatLonCounts[LatLonCounts$Freq>0,],
                          aes(x=Long,y=Lat))+scale_fill_gradient(high='red',low='yellow')
