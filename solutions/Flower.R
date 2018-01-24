flower=read.csv(file.choose(),header = FALSE)
flower=as.matrix(flower)
flowervector=as.vector(flower)

#Converting the data from a dataframe to a matrix and then to a vector is a crucial step
distance=dist(flowervector,method = 'euclidean')
clusterintensity=hclust(distance,method='ward.D')
plot(clusterintensity)

#To draw boxes on the dendogram
rect.hclust(clusterintensity,k=3,border='red')
clustergroups=cutree(clusterintensity,k=3)
tapply(flowervector,clustergroups,mean)

#Visualising the image
dim(clustergroups)=c(50,50)
image(clustergroups,axes=FALSE)
image(flower,axes=FALSE,col=grey(seq(0,1,length=256)))
