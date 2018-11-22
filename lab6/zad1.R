
# zad 1
# 2.7, 6  versicolor
# 5, 7  virginica
# 7, 3.5 virginica


# z lab 4
fnorm = function(x)
{
  return ((x-min(x))/(max(x)-min(x)))
}

iris.num = iris[,1:4]
sl = fnorm(data.matrix(iris.num[,"Sepal.Length"]))
sw = fnorm(data.matrix(iris.num[,"Sepal.Width"]))
pl = fnorm(data.matrix(iris.num[,"Petal.Length"]))
pw = fnorm(data.matrix(iris.num[,"Petal.Width"]))

iris.norm = as.data.frame.matrix(sl)
iris.norm = cbind(iris.norm, sw)
iris.norm = cbind(iris.norm, pl)
iris.norm = cbind(iris.norm, pw)
iris.norm = cbind(iris$Species)
iris.normT = iris.norm
iris.normT

#set.seed(1234) # ziarno dla RNG
#ind = sample(2, nrow(iris.norm), replace=TRUE, prob=c(0.67, 0.33)) # stworzenie labelek dla probek
#iris.norm.training = iris.norm[ind==1,1:5] # dane treningowe
#iris.norm.test = iris.norm[ind==2,1:5] # dane testowe

# iris.norm.test

install.packages("class")
library("class")
knn.3 <- knn(iris.normT[,1:4], iris.norm[,1:4], cl=iris.normT[,5], k = 3, prob=FALSE)
