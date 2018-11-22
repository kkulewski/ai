iris
#zad1


# 1-najbliższego sąsiada
# 2.7,6 versicolor
# 5,7 virginica
# 7,3.5 virginica


# 3-najbliższego sąsiada
# 2.7,6 versicolor ???
# 5,7 virginica
# 7,3.5 versicolor

#iris.log <- log(iris[,1:4])
#iris.scale <- scale(iris.log, center = TRUE)
#iris.pca < prcomp(iris.scale)
#iris.final < predict(iris.pca)[,1:2]

fnorm = function(x){
  return ((x-min(x))/ (max(x)-min(x)))
}

nSL = fnorm(data.matrix(iris$Sepal.Length))
nSW = fnorm(data.matrix(iris$Sepal.Width))
nPL = fnorm(data.matrix(iris$Petal.Length))
nPW = fnorm(data.matrix(iris$Petal.Width))

irisNorm <- as.data.frame.matrix(nSL)

irisNorm <- cbind(irisNorm,nSW)
irisNorm <- cbind(irisNorm,nPL)
irisNorm <- cbind(irisNorm,nPW)
irisNorm <- cbind(irisNorm,iris$Species)
irisNormT <- irisNorm

install.packages("class")
library(class)
knn.3 <-knn(irisNormT[,1:4], irisNorm[,1:4], cl=irisNormT[,5], k = 3, prob=FALSE)

predicted <-knn.3
real <-irisNorm[,5]
conf.matrix <-table(predicted,real)
accuracy <-sum(diag(conf.matrix))/sum(conf.matrix)
accuracy #tak, lepszy

# zad2
# kupi (0.02678)


# zad 3

# a
install.packages("e1071")
library(e1071)

# irisT = iris[1:5]
# irisTest = iris[1:5]

ind = sample(2,nrow(irisNorm),replace=TRUE,prob=c(0.67,0.33))
irisT = iris[ind==1,1:5] # dane treningowe
irisTest = iris[ind==2,1:5] # dane testowe

# b
model <- naiveBayes(Species ~ ., data = irisT)
model

pred = predict(model, newdata = irisTest)
conf.matrix <-table(pred,irisTest$Species)
conf.matrix

accuracy = sum(diag(conf.matrix))/sum(conf.matrix)
accuracy


