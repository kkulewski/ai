# zad 1

MyPredictRow = function(sl, sw, pl, pw)
{
  if (pw <= 0.6)
  {
    return ("setosa")
  }
  else
  {
    if (pl >= 4.8)
    {
      return ("virginica")
    }
    else
    {
      return ("versicolor")
    }
  }
}

MyPredict = function(data)
{
  correct = 0.0
  total = length(data[[1]])
  for (i in 1:total)
  {
    if (MyPredictRow(data[i, 1], data[i, 2], data[i, 3], data[i, 4]) == iris[i, 5])
    {
      correct = correct + 1
    }
  }
  
  return (correct / total)
}

MyPredict(iris) # 95%



# zad 2

install.packages("party")
library("party")

set.seed(1234) # ziarno dla RNG
ind = sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33)) # stworzenie labelek dla probek
iris.training = iris[ind==1,1:5] # dane treningowe
iris.test = iris[ind==2,1:5] # dane testowe

iris.ctree = ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris.training) # wyrazenie

print(iris.ctree)
plot(iris.ctree)
plot(iris.ctree, type="simple")

labels = iris.test$Species
result = predict(iris.ctree, iris.test[,1:4])

total = length(iris.test$Species)
correct = 0
for (i in 1:total)
{
  if (result[i] == labels[i])
  {
    correct = correct + 1
  }
}

total
correct
correct / total # accuracy

predicted = predict(iris.ctree, iris.test[,1:4])
real = iris.test[,5]
table(predicted,real) # porownanie ile dobrze przypisano

