# 2a
iris.log <- log(iris[,1:4])
iris.stand <- scale(iris.log, center=TRUE)
iris.pca <- prcomp(iris.stand)
iris.final <- predict(iris.pca)[,1:2]

# 2b
clusters = kmeans(iris.final, 3, iter.max = 100)
plot(iris.final, col = clusters$cluster)
points(clusters$centers, col = 1:3, pch = 8, cex = 3)

# 2c
plot(iris.final, col = iris$Species)