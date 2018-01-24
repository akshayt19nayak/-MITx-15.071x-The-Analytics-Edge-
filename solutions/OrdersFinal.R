orders=read.csv(file.choose())

#1
sort(table(orders$order_hour_of_day))
mean(orders$days_since_prior_order)

#2
cor(orders$fresh.fruits,orders$fresh.vegetables)
table(orders$frozen.pizza)
261/5000

#3
orders.aisle = orders[, 5:ncol(orders)] 
library(caret)
preproc = preProcess(orders.aisle)
ordersNorm = predict(preproc, orders.aisle)

max(ordersNorm$frozen.dessert)
min(ordersNorm$soft.drinks)

#4
distances <- dist(ordersNorm, method = "euclidean")
ClusterProducts <- hclust(distances, method = "ward.D")
plot(ClusterProducts, labels = FALSE)

#5
set.seed(200)
k=4
clusterorders=kmeans(ordersNorm,centers=k,iter.max=1000)
summary(clusterorders)
nrow(orders[clusterorders$cluster==1,])

#6
summary(orders[clusterorders$cluster==4,])

#7
summary(orders.aisle[clusterorders$cluster==3,])

#8
mean(rowSums(orders[clusterorders$cluster==1,]))


