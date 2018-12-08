# zad 1

fct.act <- function(x) {
  return ( 1 / (1 + exp(-x)))
}

forwardPass <- function(wiek,waga,wzrost) {
  hidden1 <- fct.act((wiek * -0.46122 + waga * 0.97314 + wzrost * -0.39203) + 0.80109)
  hidden2 <- fct.act((wiek * 0.78548 + waga * 2.10584 + wzrost * -0.57847) + 0.43529)
  output <-  hidden1 * -0.81546 + hidden2 * 1.03775 + -0.2368
  return(output)
}

forwardPass(23,75,176)
forwardPass(48,97,178)



# zad 2

install.packages("neuralnet")
library(neuralnet)

norm <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

iris.norm <- data.frame(norm(iris[1]), norm(iris[2]), norm(iris[3]), norm(iris[4]), iris[5])
iris.norm

set.seed(1234)
# split into train and test sample
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.train <- iris.norm[ind==1,1:5]
iris.test <- iris.norm[ind==2,1:5]
iris.test

# add new columns
iris.train$Setosa <- 0
iris.train$Versicolor <- 0
iris.train$Virginica <- 0
iris.train

# input 0 or 1 in setosa/versicolor/virginica columns using Species
for (row in 1:nrow(iris.train)) {
  if (iris.train[row,]["Species"] == "setosa")
  {
    iris.train[row,]["Setosa"] = 1
  }
  if (iris.train[row,]["Species"] == "versicolor")
  {
    iris.train[row,]["Versicolor"] = 1
  }
  if (iris.train[row,]["Species"] == "virginica")
  {
    iris.train[row,]["Virginica"] = 1
  }
}
iris.train

# remove Species column
# iris.train <- subset(iris.train, select = -c(Species))
iris.train

iris.neuralnet <- neuralnet(Setosa + Versicolor + Virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris.train, hidden=4)
iris.pred <- compute(iris.neuralnet, iris.test[,1:4])
iris.pred

plot(iris.neuralnet)

res <- as.data.frame(iris.pred$net.result)
colnames(res) < c ("Setosa","Versicolor","Virginica")

pred  <- colnames(res)[max.col(res)]
names <- iris.test$Species

conf.matrix <-table(pred, names)
print(conf.matrix)