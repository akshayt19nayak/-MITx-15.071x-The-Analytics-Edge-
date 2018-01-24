edges=read.csv(file.choose())
users=read.csv(file.choose())
head(edges)

#1.1
summary(users)
head(users)
count=0
mean(degree(g))
292/59

#1.2
table(users$locale)

#1.3
table(users$school,users$gender)

#2.1
install.packages('igraph')
library(igraph)
g=graph.data.frame(edges,FALSE,users)

#2.2
plot(g,vertex.size=5,vertex.label=NA)

#2.3
degree(g)
V(g)$size=degree(g)/2+2
plot(g, vertex.label=NA)

#2.4
V(g)$size

#3,1
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

#3.2
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

#3.3
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

#4
plot(g, vertex.label=NA,edge.width=1)










