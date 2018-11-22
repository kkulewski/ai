
# zad 1
# 2.7, 6  versicolor
# 5, 7  virginica
# 7, 3.5 virginica


# z lab 4
iris.log = iris
for (col in 1:4) { iris.log[, col] = log(iris[, col]) }

iris.log.scale = iris.log
for (col in 1:4) { iris.log.scale[, col] = scale(iris.log.scale[, col]) }

iris.log.scaleWithoutSpecies = iris.log.scale[,-5]

iris.pca <- prcomp(iris.log.scaleWithoutSpecies)

iris.final = predict(iris.pca)[,1:2]
iris.final


# na lab7
iris.log = log(iris[,1:4])
iris.scale = scale(iris.log, center=TRUE)
iris.pca = prcomp(iris.scale)
iris.final = predict(iris.pca)[,1:2]