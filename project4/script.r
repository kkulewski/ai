###
### 0. LOAD DATA FROM CSV
###

install.packages("readr")
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

install.packages("randomForest")
library(randomForest)

model.rf10n10 = randomForest(train.data, train.labels, ntree = 10, maxnodes = 10)
model.rf10n50 = randomForest(train.data, train.labels, ntree = 10, maxnodes = 50)
model.rf50n10 = randomForest(train.data, train.labels, ntree = 50, maxnodes = 10)
model.rf500n500 = randomForest(train.data, train.labels, ntree = 500, maxnodes = 500)

pred.model.rf10n10 = predict(model.rf10n10, test.data, type = "class")
pred.model.rf10n50 = predict(model.rf10n50, test.data, type = "class")
pred.model.rf50n10 = predict(model.rf50n10, test.data, type = "class")
pred.model.rf500n500 = predict(model.rf500n500, test.data, type = "class")

cm.rf10n10 = table(pred.model.rf10n10, test.labels)
cm.rf10n50 = table(pred.model.rf10n50, test.labels)
cm.rf50n10 = table(pred.model.rf50n10, test.labels)
cm.rf500n500 = table(pred.model.rf500n500, test.labels)

acc.rf10n10 = sum(diag(cm.rf10n10))/sum(cm.rf10n10)
acc.rf10n50 = sum(diag(cm.rf10n50))/sum(cm.rf10n50)
acc.rf50n10 = sum(diag(cm.rf50n10))/sum(cm.rf50n10)
acc.rf500n500 = sum(diag(cm.rf500n500))/sum(cm.rf500n500)


cm.rf10n10
acc.rf10n10

cm.rf10n50
acc.rf10n50

cm.rf50n10
acc.rf50n10

cm.rf500n500
acc.rf500n500



###
### 2. Gradient Boosted Trees
###

install.packages("gbm")
library(gbm)

model.gbt10i2 = gbm.fit(train.data,  train.labels, distribution="multinomial", n.trees=10, interaction.depth=2)
model.gbt10i8 = gbm.fit(train.data,  train.labels, distribution="multinomial", n.trees=10, interaction.depth=8)
model.gbt50i2 = gbm.fit(train.data,  train.labels, distribution="multinomial", n.trees=50, interaction.depth=2)
model.gbt50i8 = gbm.fit(train.data,  train.labels, distribution="multinomial", n.trees=50, interaction.depth=8)
model.gbt10i40 = gbm.fit(train.data,  train.labels, distribution="multinomial", n.trees=10, interaction.depth=40)

pred.model.gbt10i2 = apply(predict(model.gbt10i2, test.data, n.trees=model.gbt10i2$n.trees), 1, which.max) - 1L
pred.model.gbt10i8 = apply(predict(model.gbt10i8, test.data, n.trees=model.gbt10i8$n.trees), 1, which.max) - 1L
pred.model.gbt50i2 = apply(predict(model.gbt50i2, test.data, n.trees=model.gbt50i2$n.trees), 1, which.max) - 1L
pred.model.gbt50i8 = apply(predict(model.gbt50i8, test.data, n.trees=model.gbt50i8$n.trees), 1, which.max) - 1L
pred.model.gbt10i40 = apply(predict(model.gbt10i40, test.data, n.trees=model.gbt10i40$n.trees), 1, which.max) - 1L

cm.gbt10i2 = table(pred.model.gbt10i2, test.labels)
cm.gbt10i8 = table(pred.model.gbt10i8, test.labels)
cm.gbt50i2 = table(pred.model.gbt50i2, test.labels)
cm.gbt50i8 = table(pred.model.gbt50i8, test.labels)
cm.gbt10i40 = table(pred.model.gbt10i40, test.labels)

acc.gbt10i2 = sum(diag(cm.gbt10i2))/sum(cm.gbt10i2)
acc.gbt10i8 = sum(diag(cm.gbt10i8))/sum(cm.gbt10i8)
acc.gbt50i2 = sum(diag(cm.gbt50i2))/sum(cm.gbt50i2)
acc.gbt50i8 = sum(diag(cm.gbt50i8))/sum(cm.gbt50i8)
acc.gbt10i40 = sum(diag(cm.gbt10i40))/sum(cm.gbt10i40)


cm.gbt10i2
acc.gbt10i2

cm.gbt10i8
acc.gbt10i8

cm.gbt50i2
acc.gbt50i2

cm.gbt50i8
acc.gbt50i8

cm.gbt10i40
acc.gbt10i40



###
### 3. KNN clustering
###

install.packages("FNN")
library(FNN)

model.knn1 = knn(train.data, test.data, train.labels, k = 1)
model.knn3 = knn(train.data, test.data, train.labels, k = 3)
model.knn5 = knn(train.data, test.data, train.labels, k = 5)
model.knn7 = knn(train.data, test.data, train.labels, k = 7)
model.knn9 = knn(train.data, test.data, train.labels, k = 9)
model.knn2 = knn(train.data, test.data, train.labels, k = 2)
model.knn4 = knn(train.data, test.data, train.labels, k = 4)

pred.model.knn1 = model.knn1
pred.model.knn3 = model.knn3
pred.model.knn5 = model.knn5
pred.model.knn7 = model.knn7
pred.model.knn9 = model.knn9
pred.model.knn2 = model.knn2
pred.model.knn4 = model.knn4

cm.knn1 = table(pred.model.knn1, test.labels)
cm.knn3 = table(pred.model.knn3, test.labels)
cm.knn5 = table(pred.model.knn5, test.labels)
cm.knn7 = table(pred.model.knn7, test.labels)
cm.knn9 = table(pred.model.knn9, test.labels)
cm.knn2 = table(pred.model.knn2, test.labels)
cm.knn4 = table(pred.model.knn4, test.labels)

acc.knn1 = sum(diag(cm.knn1))/sum(cm.knn1)
acc.knn3 = sum(diag(cm.knn3))/sum(cm.knn3)
acc.knn5 = sum(diag(cm.knn5))/sum(cm.knn5)
acc.knn7 = sum(diag(cm.knn7))/sum(cm.knn7)
acc.knn9 = sum(diag(cm.knn9))/sum(cm.knn9)
acc.knn2 = sum(diag(cm.knn2))/sum(cm.knn2)
acc.knn4 = sum(diag(cm.knn4))/sum(cm.knn4)

cm.knn1
acc.knn1

cm.knn3
acc.knn3

cm.knn5
acc.knn5

cm.knn7
acc.knn7

cm.knn9
acc.knn9

cm.knn2
acc.knn2

cm.knn4
acc.knn4



###
### 4. Ridge regression
###
