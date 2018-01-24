airlines=read.csv(file.choose())

#1.1
summary(airlines)

#1.3
library(caret)
preproc=preProcess(airlines)
airlinesNorm=predict(preproc,airlines)
summary(airlinesNorm)

#2.1
distances=dist(airlinesNorm,method='euclidean')
clusterairlines=hclust(distances,method='ward.D')
plot(clusterairlines)
rect.hclust(clusterairlines,k=7,border='red')

#2.2
clustergroups=cutree(clusterairlines,k=5)
nrow(airlinesNorm[clustergroups==1,])

#2.3, 2.4, 2.5, 2.6, 2.7
summary(airlines[clustergroups==1,])
summary(airlines[clustergroups==2,])
summary(airlines[clustergroups==3,])
summary(airlines[clustergroups==4,])
summary(airlines[clustergroups==5,])

#3.1
set.seed(88)
k=5
kMeans=kmeans(airlinesNorm,centers=k,iter.max=1000)
str(kMeans)

#3.2
summary(airlines[clustergroups==1,])
summary(airlines[kMeans$cluster==1,])
