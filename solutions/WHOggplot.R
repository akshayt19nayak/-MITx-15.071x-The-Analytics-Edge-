who=read.csv(file.choose())
str(who)
library('ggplot2')
scatterplot=ggplot(who, aes(x=GNI, y=FertilityRate))
myplot=scatterplot+geom_point(color='blue',shape=17,size=3)+ggtitle(
  'Fertility Rate vs Gross National Income')
pdf('MyPlot.pdf')
print(myplot)
dev.off()
ggplot(who, aes(x=log(FertilityRate),y=Under15))+geom_point()+stat_smooth(method='lm',
                                                                          levels=0.99)

ggplot(who, aes(x = FertilityRate, y = Under15,color=Region)) + geom_point()
       