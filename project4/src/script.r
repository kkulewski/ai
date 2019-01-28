###
### 0. LOAD DATA FROM CSV
###

install.packages("readr")
library(readr)

train = read_csv("../data/mnist_train.csv")
test = read_csv("../data/mnist_test.csv")

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

install.packages("glmnet")
library(glmnet)

model.rrf03 = cv.glmnet(train.data[1:1000,], train.labels[1:1000], lambda = NULL, nfolds = 3, family = "multinomial")
model.rrf10 = cv.glmnet(train.data[1:1000,], train.labels[1:1000], lambda = NULL, nfolds = 10, family = "multinomial")
model.rrf30 = cv.glmnet(train.data[1:1000,], train.labels[1:1000], lambda = NULL, nfolds = 30, family = "multinomial")
model.rr = cv.glmnet(train.data[1:10000,], train.labels[1:10000], lambda = NULL, nfolds = 3, family = "multinomial")

pred.model.rrf03 = apply(predict(model.rrf03, test.data, s = model.rrf03$lambda.min, type="response"), 1, which.max) - 1L
pred.model.rrf10 = apply(predict(model.rrf10, test.data, s = model.rrf10$lambda.min, type="response"), 1, which.max) - 1L
pred.model.rrf30 = apply(predict(model.rrf30, test.data, s = model.rrf30$lambda.min, type="response"), 1, which.max) - 1L
pred.model.rr = apply(predict(model.rr, test.data, s = model.rr$lambda.min, type="response"), 1, which.max) - 1L

cm.rrf03 = table(pred.model.rrf03, test.labels)
cm.rrf10 = table(pred.model.rrf10, test.labels)
cm.rrf30 = table(pred.model.rrf30, test.labels)
cm.rr = table(pred.model.rr, test.labels)

acc.rrf03 = sum(diag(cm.rrf03))/sum(cm.rrf03)
acc.rrf10 = sum(diag(cm.rrf10))/sum(cm.rrf10)
acc.rrf30 = sum(diag(cm.rrf30))/sum(cm.rrf30)
acc.rr = sum(diag(cm.rr))/sum(cm.rr)

cm.rrf03
acc.rrf03

cm.rrf10
acc.rrf10

cm.rrf30
acc.rrf30

cm.rr
acc.rr



###
### 5. Support Vector Machine
###

install.packages("e1071")
library(e1071)

model.svm1linear = svm(train.data[1:5000, ], train.labels[1:5000], kernel = "linear", cost = 1, scale = FALSE)
model.svm1radial = svm(train.data[1:5000, ], train.labels[1:5000], kernel = "radial", cost = 1, scale = FALSE)
model.svm1polyno = svm(train.data[1:5000, ], train.labels[1:5000], kernel = "polynomial", cost = 1, scale = FALSE)
model.svm = svm(train.data, train.labels, kernel = "polynomial", cost = 1, scale = FALSE)

pred.model.svm1linear = predict(model.svm1linear, test.data)
pred.model.svm1radial = predict(model.svm1radial, test.data)
pred.model.svm1polyno = predict(model.svm1polyno, test.data)
pred.model.svm = predict(model.svm, test.data)

cm.svm1linear = table(pred.model.svm1linear, test.labels)
cm.svm1radial = table(pred.model.svm1radial, test.labels)
cm.svm1polyno = table(pred.model.svm1polyno, test.labels)
cm.svm = table(pred.model.svm, test.labels)

acc.svm1linear = sum(diag(cm.svm1linear))/sum(cm.svm1linear)
acc.svm1radial = sum(diag(cm.svm1radial))/sum(cm.svm1radial)
acc.svm1polyno = sum(diag(cm.svm1polyno))/sum(cm.svm1polyno)
acc.svm = sum(diag(cm.svm))/sum(cm.svm)


cm.svm1linear
acc.svm1linear

cm.svm1radial
acc.svm1radial

cm.svm1polyno
acc.svm1polyno

cm.svm
acc.svm



###
### 6. Deep Neural Network
###

# Custom source
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
library(mxnet)


# Format required by MXNet
train.matrix = data.matrix(train)
train.matrix.y = train.matrix[,1]
train.matrix.x = train.matrix[,-1]
train.matrix.x = train.matrix.x / 255

test.matrix = data.matrix(test)
test.matrix.y = test.matrix[,1]
test.matrix.x = test.matrix[,-1]
test.matrix.x = test.matrix.x / 255

##
## 6.1 NN with 1 hidden layer and 10 rounds
##

dat = mx.symbol.Variable("data")
fc1 = mx.symbol.FullyConnected(data, name = "fc1", num_hidden = 10)
smx = mx.symbol.SoftmaxOutput(fc1, name = "smx")

mx.set.seed(0)
model.nn1r10 = mx.model.FeedForward.create(
  smx,
  X = train.matrix.x,
  y = train.matrix.y,
  ctx = mx.cpu(),
  num.round = 10,
  array.batch.size = 100,
  learning.rate = 0.07,
  momentum = 0.9,
  eval.metric = mx.metric.accuracy,
  initializer = mx.init.uniform(0.07),
  epoch.end.callback = mx.callback.log.train.metric(100),
  array.layout = "rowmajor")

pred.model.nn1r10 = predict(model.nn1r10, test.matrix.x, array.layout = "rowmajor")
pred.model.nn1r10.label = max.col(t(pred.model.nn1r10))
cm.nn1r10 = table(pred.model.nn1r10.label, test.matrix.y)
acc.nn1r10 = sum(diag(cm.nn1r10))/sum(cm.nn1r10)


##
## 6.2 NN with 4 hidden layers and 10 rounds
##

dat = mx.symbol.Variable("data")
fc1 = mx.symbol.FullyConnected(dat, name = "fc1", num_hidden = 256)
ac1 = mx.symbol.Activation(fc1, name = "relu1", act_type = "relu")
fc2 = mx.symbol.FullyConnected(ac1, name = "fc2", num_hidden = 128)
ac2 = mx.symbol.Activation(fc2, name = "relu2", act_type = "relu")
fc3 = mx.symbol.FullyConnected(ac2, name = "fc3", num_hidden = 64)
ac3 = mx.symbol.Activation(fc3, name = "relu3", act_type = "relu")
fc4 = mx.symbol.FullyConnected(ac3, name = "fc4", num_hidden = 10)
smx = mx.symbol.SoftmaxOutput(fc4, name = "smx")

mx.set.seed(0)
model.nn4r10 = mx.model.FeedForward.create(
  smx,
  X = train.matrix.x,
  y = train.matrix.y,
  ctx = mx.cpu(),
  num.round = 10,
  array.batch.size = 100,
  learning.rate = 0.07,
  momentum = 0.9,
  eval.metric = mx.metric.accuracy,
  initializer = mx.init.uniform(0.07),
  epoch.end.callback = mx.callback.log.train.metric(100),
  array.layout = "rowmajor")

pred.model.nn4r10 = predict(model.nn4r10, test.matrix.x, array.layout = "rowmajor")
pred.model.nn4r10.label = max.col(t(pred.model.nn4r10))
cm.nn4r10 = table(pred.model.nn4r10.label, test.matrix.y)
acc.nn4r10 = sum(diag(cm.nn4r10))/sum(cm.nn4r10)


##
## 6.3 NN with 4 hidden layers and 25 rounds
##

mx.set.seed(0)
model.nn4r25 = mx.model.FeedForward.create(
  smx,
  X = train.matrix.x,
  y = train.matrix.y,
  ctx = mx.cpu(),
  num.round = 25,
  array.batch.size = 100,
  learning.rate = 0.07,
  momentum = 0.9,
  eval.metric = mx.metric.accuracy,
  initializer = mx.init.uniform(0.07),
  epoch.end.callback = mx.callback.log.train.metric(100),
  array.layout = "rowmajor")

pred.model.nn4r25 = predict(model.nn4r25, test.matrix.x, array.layout = "rowmajor")
pred.model.nn4r25.label = max.col(t(pred.model.nn4r25))
cm.nn4r25 = table(pred.model.nn4r25.label, test.matrix.y)
acc.nn4r25 = sum(diag(cm.nn4r25))/sum(cm.nn4r25)


##
## 6.4 NN with 4 hidden layers and 50 rounds
##

mx.set.seed(0)
model.nn4r50 = mx.model.FeedForward.create(
  smx,
  X = train.matrix.x,
  y = train.matrix.y,
  ctx = mx.cpu(),
  num.round = 50,
  array.batch.size = 100,
  learning.rate = 0.07,
  momentum = 0.9,
  eval.metric = mx.metric.accuracy,
  initializer = mx.init.uniform(0.07),
  epoch.end.callback = mx.callback.log.train.metric(100),
  array.layout = "rowmajor")

pred.model.nn4r50 = predict(model.nn4r50, test.matrix.x, array.layout = "rowmajor")
pred.model.nn4r50.label = max.col(t(pred.model.nn4r50))
cm.nn4r50 = table(pred.model.nn4r50.label, test.matrix.y)
acc.nn4r50 = sum(diag(cm.nn4r50))/sum(cm.nn4r50)


##
## Summary
##

cm.nn1r10
acc.nn1r10

cm.nn4r10
acc.nn4r10

cm.nn4r25
acc.nn4r25

cm.nn4r50
acc.nn4r50
