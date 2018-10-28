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


# zad 3

library(Hmisc)

srednia <- impute(dirty.iris, fun = mean)  # mean imputation
srednia
knn <- impute(x, fun = median)  # median imputation
