dailykos=read.csv(file.choose())

#1.1
distances=dist(dailykos,method='euclidean')
clusterdocs=hclust(distances,method='ward.D')

#1.2
plot(clusterdocs)
rect.hclust(clusterdocs,6,border='red')

#1.4
clustergroups=cutree(clusterdocs,k=7)
nrow(dailykos[clustergroups==1,])
nrow(dailykos[clustergroups==2,])
nrow(dailykos[clustergroups==3,])
nrow(dailykos[clustergroups==4,])
nrow(dailykos[clustergroups==5,])
nrow(dailykos[clustergroups==6,])
nrow(dailykos[clustergroups==7,])

#1.5
tail(sort(colMeans(dailykos[clustergroups==1,])))

#1.6
tail(sort(colMeans(dailykos[clustergroups==2,])))
tail(sort(colMeans(dailykos[clustergroups==3,])))
tail(sort(colMeans(dailykos[clustergroups==4,])))
tail(sort(colMeans(dailykos[clustergroups==5,])))
tail(sort(colMeans(dailykos[clustergroups==6,])))
tail(sort(colMeans(dailykos[clustergroups==7,])))

#2.1
set.seed(1000)
k=7
kMeans=kmeans(dailykos,centers = k)
str(kMeans)

#2.2
tail(sort(colMeans(dailykos[kMeans$cluster==1,])))
tail(sort(colMeans(dailykos[kMeans$cluster==2,])))
tail(sort(colMeans(dailykos[kMeans$cluster==3,])))
tail(sort(colMeans(dailykos[kMeans$cluster==4,])))
tail(sort(colMeans(dailykos[kMeans$cluster==5,])))
tail(sort(colMeans(dailykos[kMeans$cluster==6,])))
tail(sort(colMeans(dailykos[kMeans$cluster==7,])))

#2.3, 2.4
table(clustergroups,kMeans$cluster)

#2.5
123/nrow(dailykos[kMeans$cluster==7,])






