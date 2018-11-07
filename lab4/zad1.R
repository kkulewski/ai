# lab4

install.packages("editrules")
install.packages("deducorrect")
install.packages("Hmisc")
library(editrules)



# zad1
dirty.iris <- read.csv("dirty_iris.csv", header=TRUE, sep=",")
nrow(subset(dirty.iris, is.finite(Sepal.Length) & is.finite(Sepal.Width) & is.finite(Petal.Length) & is.finite(Petal.Width)))

E <-editset(c("Sepal.Length <= 30"))
E

ve <- violatedEdits(E, dirty.iris)
ve

summary(ve)
plot(ve)


E <- editset(c("Sepal.Length <= 30",
               "Species %in% c('setosa', 'versicolor', 'virginica')",
               "Sepal.Length > Petal.Length",
               "Sepal.Width >= 0",
               "Sepal.Length >= 0",
               "Petal.Length >= 0",
               "Petal.Width >= 0",
               "Petal.Length >= 2 * Petal.Width"))
E

ve <- violatedEdits(E, dirty.iris)
ve

summary(ve)
plot(ve)



# zad 2

library(deducorrect)

R = NULL
R <- correctionRules("corrects")
R

cor = correctWithRules(R, dirty.iris)
cor

clean.iris = cor[[1]]



# zad 3

library(Hmisc)

clean.iris.mean = data.frame(clean.iris)
for (col in 1:4)
{
  clean.iris.mean[, col] = impute(clean.iris.mean[, col], mean)
}
clean.iris.mean

# clean.iris.knn = knn(data.frame(clean.iris))
# clean.iris.knn



# zad 4

iris

iris.log = iris
for (col in 1:4)
{
  iris.log[, col] = log(iris[, col])
}
iris.log 

iris.log.scale = iris.log
for (col in 1:4)
{
  iris.log.scale[, col] = scale(iris.log.scale[, col])
}
iris.log.scale



# zad 5

iris.log.scaleWithoutSpecies = iris.log.scale[,-5]
iris.log.scaleWithoutSpecies

iris.pca <- prcomp(iris.log.scaleWithoutSpecies)
iris.pca # standard deviations: 1.7124583 0.9523797 0.3647029 0.1656840
# P1 i P2

iris.pca[1] # odchylenie
iris.pca[2]$rotation # przepis na współczynniki liniowe

iris.predict <- predict(iris.pca)
iris.predict # baza irysów po zadziałaniu macierzą rotation na naszą bazę iris.slog.scale

iris.predict2 = iris.predict[, -3]
iris.predict2 = iris.predict[, -4]
iris.predict2 = iris.predict[, c(-3, -4)]
iris.predict2

iris.predict2 = data.frame(iris.predict2)
iris.predict2[3] = iris.log.scale[, 5]
iris.predict2



# zad 6
pc1 = iris.predict2[, 1]
pc2 = iris.predict2[, 2]
spe = iris.predict2[, 3]
col = c("blue", "red", "green")
col.pairs = col[spe] # nice
col.pairs
plot(pc1, pc2, col=col.pairs, xlab="PC1", ylab="PC2")
legend(x="topleft", legend=unique(spe), pch=1, col=col)
