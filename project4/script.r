###
### 0. LOAD DATA FROM CSV
###

library(readr)

train = read_csv("data/mnist_train.csv")
test = read_csv("data/mnist_test.csv")

# Create label vectors
train.labels = train[, 1]
train.labels = as.factor(train.labels$label)
test.labels = test[, 1]
test.labels = as.factor(test.labels$label)

# Create data tables
train.data = as.matrix(train[, -1])
test.data = as.matrix(test[, -1])

## Fix illegal column names
names(train.data) = make.names(names(train.data))
names(test.data) = make.names(names(test.data))

###
### 1. RANDOM FOREST
###

library(randomForest)

model.rf10n10 = randomForest(train.data, train.labels, ntree = 10, maxnodes = 10)
model.rf10n50 = randomForest(train.data, train.labels, ntree = 10, maxnodes = 50)
model.rf50n10 = randomForest(train.data, train.labels, ntree = 50, maxnodes = 10)
model.rf50n50 = randomForest(train.data, train.labels, ntree = 50, maxnodes = 50)
model.rf150n150 = randomForest(train.data, train.labels, ntree = 150, maxnodes = 150)

pred.model.rf10n10 = predict(model.rf10n10, test.data, type = "class")
pred.model.rf10n50 = predict(model.rf10n50, test.data, type = "class")
pred.model.rf50n10 = predict(model.rf50n10, test.data, type = "class")
pred.model.rf50n50 = predict(model.rf50n50, test.data, type = "class")
pred.model.rf150n150 = predict(model.rf150n150, test.data, type = "class")

cm.rf10n10 = table(pred.model.rf10n10, test.labels)
cm.rf10n50 = table(pred.model.rf10n50, test.labels)
cm.rf50n10 = table(pred.model.rf50n10, test.labels)
cm.rf50n50 = table(pred.model.rf50n50, test.labels)
cm.rf150n150 = table(pred.model.rf150n150, test.labels)

acc.rf10n10 = sum(diag(cm.rf10n10))/sum(cm.rf10n10)
acc.rf10n50 = sum(diag(cm.rf10n50))/sum(cm.rf10n50)
acc.rf50n10 = sum(diag(cm.rf50n10))/sum(cm.rf50n10)
acc.rf50n50 = sum(diag(cm.rf50n50))/sum(cm.rf50n50)
acc.rf150n150 = sum(diag(cm.rf150n150))/sum(cm.rf150n150)


cm.rf10n10
acc.rf10n10

cm.rf10n50
acc.rf10n50

cm.rf50n10
acc.rf50n10

cm.rf50n50
acc.rf50n50

cm.rf150n150
acc.rf150n150
