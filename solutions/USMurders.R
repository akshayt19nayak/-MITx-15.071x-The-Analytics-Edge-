murders=read.csv(file.choose())
str(murders)

#Plotting the states
statesMap=map_data('state') #US Map
str(statesMap)
ggplot(statesMap,aes(x=long,y=lat,group=group))+geom_polygon(fill='white',color='black')
murders$region=tolower(murders$State)
murderMap=merge(statesMap,murders,by='region')

#Plotting the data
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Murders))+geom_polygon(color='Black')+
  scale_fill_gradient(low='black',high='red',guide='legend')
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Population))+geom_polygon(color='Black')+
  scale_fill_gradient(low='black',high='red',guide='legend')

murderMap$Rate=(murderMap$Murders/murderMap$Population)*100000
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=GunOwnership))+
  geom_polygon(color='Black')+scale_fill_gradient(low='black',high='red',guide='legend')
       