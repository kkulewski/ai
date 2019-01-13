# mnist
# read in data
library(readr)

data.train = read_csv("data/mnist_train2.csv")
data.test = read_csv("data/mnist_test2.csv")

labels.train = data.train[, 1]
labels.train = as.factor(labels.train$label)
summary(labels.train)


library(randomForest)
numTrees = 50

startTime <- proc.time()
rf <- randomForest(data.train[-1], labels.train, xtest=data.test[-1], ntree=numTrees)
proc.time() - startTime

rf

cm = rf$confusion[, 1:10]
sum(diag(cm))/sum(cm)

