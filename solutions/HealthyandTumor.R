healthy=read.csv(file.choose(),header=FALSE)
healthy=as.matrix(healthy)
str(healthy)

#Visualising the image
image(healthy,axes=FALSE,col = grey(seq(0,1,length=256)))

#Convert healthy matrix to a vector
healthyvector=as.vector(healthy)
#As the number of elements is large, we cannot use hierarchial clustering

#K-Means clustering
k=5
set.seed(1)
KMC=kmeans(healthyvector,iter.max = 1000, centers = k)
healthyclusters=KMC$cluster
dim(healthyclusters)=c(nrow(healthy),ncol(healthy))
image(healthyclusters,axes=FALSE, col=rainbow(k))

#Tumor
tumor=read.csv(file.choose(),header = FALSE)
tumor=as.matrix(tumor)
tumorvector=as.vector(tumor)
install.packages('flexclust')
library(flexclust)
KMC.kcca=as.kcca(KMC,healthyvector)
tumorclusters=predict(KMC.kcca,newdata=tumorvector)
dim(tumorclusters)=c(nrow(tumor),ncol(tumor))
image(tumorclusters,axes=FALSE,col=rainbow(k))
