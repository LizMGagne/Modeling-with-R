#load the datasets library to access the iris data set
library(datasets)

#load and view the iris dataset
data(iris)
head(iris)

#load ggplot 2 for plotting
#install.packages("ggplot2")
library(ggplot2)
#install.packages("GGally")
library(GGally)

ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) + geom_point()

set.seed(13)

#Since we know there are 3 species, we expect k=3 to give the best results
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 35)
irisCluster

table(irisCluster$cluster, iris$Species)

#We can see that petal length and width are similar within the same species, but vary between different species
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()

#Sepal length and width are not as easily distinguished between species 
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

ggpairs(iris, columns = 1:5, mapping=aes(color=Species))

#find the errors
not.error <- unclass(iris$Species) == irisCluster$cluster
iris$noerror <- not.error

#See errors 
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) +
  geom_point(size = 3, alpha = 0.5, aes(shape = iris$noerror))


#We can also look at other k values, just to see how that affects the clustering
#Since the starting assignments are random, we specify nstart = 35. 
#This means that R will try 35 different random starting assignments and then select the one with the lowest within cluster variation.
irisCluster2 <- kmeans(iris[, 3:4], 2, nstart = 35)
irisCluster2

table(irisCluster2$cluster, iris$Species)

#We can see that petal length and width are similar within the same species, but vary between different species
irisCluster2$cluster <- as.factor(irisCluster2$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster2$cluster)) + geom_point()

#Sepal length and width are not as easily distinguished between species 
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

ggpairs(iris, columns = 1:5, mapping=aes(color=Species))

#find the errors
not.error <- unclass(iris$Species) == irisCluster2$cluster
iris$noerror <- not.error

#See errors 
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster2$cluster)) +
  geom_point(size = 3, alpha = 0.5, aes(shape = iris$noerror))

irisCluster5 <- kmeans(iris[, 3:4], 5, nstart = 35)
irisCluster5

table(irisCluster5$cluster, iris$Species)

#We can see that petal length and width are similar within the same species, but vary between different species
irisCluster5$cluster <- as.factor(irisCluster2$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster5$cluster)) + geom_point()

#Sepal length and width are not as easily distinguished between species 
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

ggpairs(iris, columns = 1:5, mapping=aes(color=Species))

#find the errors
not.error <- unclass(iris$Species) == irisCluster5$cluster
iris$noerror <- not.error

#See errors 
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster5$cluster)) +
  geom_point(size = 3, alpha = 0.5, aes(shape = iris$noerror))


# Let's see what the total distance is between data points and cluster centers

# initialize distance to zero
csum = 0

# for each data point: add the distance between its point and its cluster center
for (i in 1:nrow(iris)) {
  csum = csum + dist(rbind(iris[i,1:4],irisCluster$centers[irisCluster$cluster[i],]))
}

# get the total
csum[1]


#Now, let's compare the clusters with the species.
#We can only do this because we happen to know the species of each data point, usually this isn't an option
table(irisCluster$cluster, iris$Species)

#Let's try using only petal length and petal width
irisClusterPET <- kmeans(iris[,3:4], 2, nstart = 35)

#Again, we can compare the clusters with the species
table(irisClusterPET$cluster, iris$Species)

#We can see the cluster centroids, the clusters that each data point was assigned to, and the within cluster variation.
# Here's an example using a 3-cluster solution with just the petal factors
irisClusterPET

ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterPET$cluster)) + geom_point()