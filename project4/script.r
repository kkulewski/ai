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



###
### 2. Gradient Boosted Trees
###

install.packages("gbm")
library(gbm)

model.gbt10i2 = gbm.fit(train.data,  factor(train.labels), distribution="multinomial", n.trees=10, interaction.depth=2)
model.gbt10i8 = gbm.fit(train.data,  factor(train.labels), distribution="multinomial", n.trees=10, interaction.depth=8)
model.gbt50i2 = gbm.fit(train.data,  factor(train.labels), distribution="multinomial", n.trees=50, interaction.depth=2)
model.gbt50i8 = gbm.fit(train.data,  factor(train.labels), distribution="multinomial", n.trees=50, interaction.depth=8)
model.gbt10i20 = gbm.fit(train.data,  factor(train.labels), distribution="multinomial", n.trees=10, interaction.depth=20)

pred.model.gbt10i2 = apply(predict(model.gbt10i2, data.test, n.trees=model.gbt10i2$n.trees), 1, which.max) - 1L
pred.model.gbt10i8 = apply(predict(model.gbt10i8, data.test, n.trees=model.gbt10i8$n.trees), 1, which.max) - 1L
pred.model.gbt50i2 = apply(predict(model.gbt50i2, data.test, n.trees=model.gbt50i2$n.trees), 1, which.max) - 1L
pred.model.gbt50i8 = apply(predict(model.gbt50i8, data.test, n.trees=model.gbt50i8$n.trees), 1, which.max) - 1L
pred.model.gbt10i20 = apply(predict(model.gbt10i20, data.test, n.trees=model.gbt10i20$n.trees), 1, which.max) - 1L

cm.gbt10i2 = table(pred.model.gbt10i2, test.labels)
cm.gbt10i8 = table(pred.model.gbt10i8, test.labels)
cm.gbt50i2 = table(pred.model.gbt50i2, test.labels)
cm.gbt50i8 = table(pred.model.gbt50i8, test.labels)
cm.gbt10i20 = table(pred.model.gbt10i20, test.labels)

acc.gbt10i2 = sum(diag(cm.gbt10i2))/sum(cm.gbt10i2)
acc.gbt10i8 = sum(diag(cm.gbt10i8))/sum(cm.gbt10i8)
acc.gbt50i2 = sum(diag(cm.gbt50i2))/sum(cm.gbt50i2)
acc.gbt50i8 = sum(diag(cm.gbt50i8))/sum(cm.gbt50i8)
acc.gbt10i20 = sum(diag(cm.gbt10i20))/sum(cm.gbt10i20)


cm.gbt10i2
acc.gbt10i2

cm.gbt10i8
acc.gbt10i8

cm.gbt50i2
acc.gbt50i2

cm.gbt50i8
acc.gbt50i8

cm.gbt10i20
acc.gbt10i20



###
### 3. KNN clustering
###
