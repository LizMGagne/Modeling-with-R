#load the datasets library to access the iris data set
library(datasets)

#load and view the iris dataset
data(iris)
iris

#load ggplot 2 for plotting
#install.packages("ggplot2")
library(ggplot2)
#install.packages("GGally")
library(GGally)

ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) + geom_point()

set.seed(13)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 35)
irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()

ggpairs(iris, columns = 1:5, mapping=aes(color=Species))

#find the errors
not.error <- unclass(iris$Species) == irisCluster$cluster
iris$noerror <- not.error

#See errors 
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) +
  geom_point(size = 3, alpha = 0.5, aes(shape = iris$noerror))
