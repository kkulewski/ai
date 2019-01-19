###
### 0. LOAD DATA FROM CSV
###

library(readr)

data.train = read_csv("data/mnist_train.csv")
data.test = read_csv("data/mnist_test.csv")

# Create label vectors
data.train.labels = data.train[, 1]
data.train.labels = as.factor(data.train.labels$label)
data.test.labels = data.test[, 1]
data.test.labels = as.factor(data.test.labels$label)

## Fix illegal column names
names(data.train) = make.names(names(data.train))
names(data.test) = make.names(names(data.test))



###
### 1. RANDOM FOREST
###

library(randomForest)

model.rf5 = randomForest(label ~ ., data = data.train, ntree = 5)
model.rf15 = randomForest(label ~ ., data = data.train, ntree = 15)
model.rf50 = randomForest(label ~., data = data.train, ntree = 50)