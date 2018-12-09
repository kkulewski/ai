## PROJEKT 2 - Zglebianie danych
##
## 1. Wybrana baza
## Pima Indians Diabetes Database
## https://www.kaggle.com/uciml/pima-indians-diabetes-database
##
## 2. Zawartosc bazy
## Baza opisuje wystepowanie cukrzycy wsrod zenskich Indian z plemienia Pima
##
## 3. Atrybuty
## [int] Pregnancies - liczba ciazy
## [int] Glucose - poziom glukozy 2 godziny po zazyciu dawki testowej gkukozy
## [int] BloodPressure - rozkurczowe cisnienie krwi (mm Hg)
## [int] SkinThickness - grubosc faldu skornego na tricepsie (mm)
## [int] Insulin - poziom insuliny 2 godziny po zazyciu dawki testowej glukozy2 (mu U/ml)
## [num] BMI - wskaznik BMI
## [num] DiabetesPedigreeFunction - miara genetycznego wplywu (cukrzyca u krewnych i poziom ich pokrewienstwa)
## [int] Age - wiek
## [bool] Outcome - wynik



### 1. Wczytujemy baze + westepne ogledziny
db <- read.csv('diabetes.csv', header=TRUE, sep=',')
#db
#summary(db)



### 2. Obrobka danych

# Mozemy dostrzec, ze brakujace wartosci zostaly prawdopodobnie zastapione zerami.
# Wyliczamy srednie z niezerowych wartosci w kolumnach
glucose.mean = mean(db$Glucose[which(db$Glucose>0)])
bloodPressure.mean = mean(db$BloodPressure[which(db$BloodPressure>0)])
skinThickness.mean = mean(db$SkinThickness[which(db$SkinThickness>0)])
insulin.mean = mean(db$Insulin[which(db$Insulin>0)])
bmi.mean = mean(db$BMI[which(db$BMI>0.0)])
dpf.mean = mean(db$DiabetesPedigreeFunction[which(db$DiabetesPedigreeFunction>0.0)])
age.mean = mean(db$Age[which(db$Age>0)])

# Zamieniamy zerowe wartosci wyliczonymi srednimi
db.nz = db
for (i in 1:nrow(db))
{
  if (db$Glucose[i] == 0) db.nz$Glucose[i] = round(glucose.mean)
  if (db$BloodPressure[i] == 0) db.nz$BloodPressure[i] = round(bloodPressure.mean)
  if (db$SkinThickness[i] == 0) db.nz$SkinThickness[i] = round(skinThickness.mean)
  if (db$Insulin[i] == 0) db.nz$Insulin[i] = round(insulin.mean)
  if (db$BMI[i] == 0) db.nz$BMI[i] = round(bmi.mean, digits = 1)
  if (db$DiabetesPedigreeFunction[i] == 0) db.nz$DiabetesPedigreeFunction[i] = round(dpf.mean, digits = 3)
  if (db$Age[i] == 0) db.nz$Age[i] = round(age.mean)
}

# Trzeba jeszcze zmienic wartosci outcome 0 i 1 na "healthy" i "sick"
db.nz$Outcome = factor(db.nz$Outcome, level=0:1, labels=c("healthy", "sick"))

db.nz


### 3. Klasyfikacja

# Normalizacja danych

fnorm = function(x) { return ((x-min(x))/(max(x)-min(x))) }
db.nz.norm = db.nz
db.nz.norm$Pregnancies = fnorm(db.nz$Pregnancies)
db.nz.norm$Glucose = fnorm(db.nz$Glucose)
db.nz.norm$BloodPressure = fnorm(db.nz$BloodPressure)
db.nz.norm$SkinThickness = fnorm(db.nz$SkinThickness)
db.nz.norm$Insulin = fnorm(db.nz$Insulin)
db.nz.norm$BMI = fnorm(db.nz$BMI)
db.nz.norm$DiabetesPedigreeFunction = fnorm(db.nz$DiabetesPedigreeFunction)
db.nz.norm$Age = fnorm(db.nz$Age)

# Podzial na grupe treningowa i testowa
set.seed(1234)
ind <- sample(2, nrow(db.nz.norm), replace=TRUE, prob=c(0.8, 0.2))
db.training <- db.nz.norm[ind==1, 1:9]
db.test <- db.nz.norm[ind==2, 1:9]

## 3.1 Klasyfikacja drzewami
#install.packages("party")
library("party")

db.ctree = ctree(Outcome ~ ., data = db.training)

# Macierz bledu i dokladnosc
ctr.predicted = predict(db.ctree, db.test[,1:8])
ctr.real = db.test[,9]
ctr.conf.matrix = table(ctr.predicted, ctr.real)
ctr.accuracy = sum(diag(ctr.conf.matrix)) / sum(ctr.conf.matrix)


## 3.2 Klasyfikacja KNN
#install.packages("class")
library("class")

db.knn3 = knn(db.training[,1:8], db.test[,1:8], cl=db.training[,9], k = 3, prob=FALSE)

# Macierz bledu i dokladnosc
knn.predicted = db.knn3
knn.real = db.test[,9]
knn.conf.matrix = table(knn.predicted, knn.real)
knn.accuracy = sum(diag(knn.conf.matrix)) / sum(knn.conf.matrix)

## 3.3 Klasyfikacja Naive-Bayes
#install.packages("e1071")
library("e1071")

db.naiveBayes = naiveBayes(db.training[,1:8], db.training[,9])

# Macierz bledu i dokladnosc
nbs.predicted = predict(db.naiveBayes, db.test[,1:8])
nbs.real = db.test[,9]
nbs.conf.matrix = table(nbs.predicted, nbs.real)
nbs.accuracy = sum(diag(nbs.conf.matrix)) / sum(nbs.conf.matrix)


## 3.4 Klasyfikacja Lasy Losowe
#install.packages("randomForest")
library("randomForest")

db.rfo = randomForest(Outcome ~ ., data=db.training)

# Macierz bledu i dokladnosc
rfo.predicted = predict(db.rfo, db.test[,1:8])
rfo.real = db.test[,9]
rfo.conf.matrix = table(rfo.predicted, rfo.real)
rfo.accuracy = sum(diag(rfo.conf.matrix)) / sum(rfo.conf.matrix)

## 3.5 Zestawienie
ctr.conf.matrix
knn.conf.matrix
nbs.conf.matrix
rfo.conf.matrix
ctr.accuracy
knn.accuracy
nbs.accuracy
rfo.accuracy

# TPR, FNR
P = nrow(subset(db.test,Outcome == "healthy"))
N = nrow(subset(db.test,Outcome == "sick"))

# CTR
ctr.TP = ctr.conf.matrix[1,1]
ctr.FN = ctr.conf.matrix[2,1]
ctr.TPR = (ctr.TP/(ctr.TP+ctr.FN))
ctr.FP = ctr.conf.matrix[1,2]
ctr.TN = ctr.conf.matrix[2,2]
ctr.FPR = (ctr.FP/(ctr.FP+ctr.TN))
ctr.TPR
ctr.FPR

# KNN
knn.TP = knn.conf.matrix[1,1]
knn.FN = knn.conf.matrix[2,1]
knn.TPR = (knn.TP/(knn.TP+knn.FN))
knn.FP = knn.conf.matrix[1,2]
knn.TN = knn.conf.matrix[2,2]
knn.FPR = (knn.FP/(knn.FP+knn.TN))
knn.TPR
knn.FPR

# NBS
nbs.TP = nbs.conf.matrix[1,1]
nbs.FN = nbs.conf.matrix[2,1]
nbs.TPR = (nbs.TP/(nbs.TP+nbs.FN))
nbs.FP = nbs.conf.matrix[1,2]
nbs.TN = nbs.conf.matrix[2,2]
nbs.FPR = (nbs.FP/(nbs.FP+nbs.TN))
nbs.TPR
nbs.FPR

# RFO
rfo.TP = rfo.conf.matrix[1,1]
rfo.FN = rfo.conf.matrix[2,1]
rfo.TPR = (rfo.TP/(rfo.TP+rfo.FN))
rfo.FP = rfo.conf.matrix[1,2]
rfo.TN = rfo.conf.matrix[2,2]
rfo.FPR = (rfo.FP/(rfo.FP+rfo.TN))
rfo.TPR
rfo.FPR

# accuracy
barplot(c(ctr.accuracy, knn.accuracy, nbs.accuracy, rfo.accuracy),
        main = "Dokładność klasyfikatorów",
        names.arg = c("Drzewa dec.", "KNN", "Naive-Bayes", "Lasy los."),
        ylim = c(0.0, 1.0),
        col = c("red", "green", "blue", "purple"))

# ROC plot
roc = data.frame(
  "Name" = c("DC", "KNN", "NB", "RF", "Ideal"),
  "FPR" = c(ctr.FPR, knn.FPR, nbs.FPR, rfo.FPR, 0.0),
  "TPR" = c(ctr.TPR, knn.TPR, nbs.TPR, rfo.TPR, 1.0)
  )

roc

plot(
  roc$FPR,
  roc$TPR,
  xlab = "False Positive Rate",
  ylab = "True Positive Rate",
  main = "ROC Space",
  xlim=c(0.0, 1.0),
  ylim=c(0.0, 1.0),
  cex = 1.5,
  pch = 18,
  col=c("red", "green", "purple", "blue", "gray")
  )

text(
  roc$FPR,
  roc$TPR,
  roc$Name,
  cex= 0.7,
  pos = 1)



### 4. Grupowanie

#install.packages("editrules)
library("editrules")


db.pca = prcomp(db.nz.norm[,1:8])
db.pca.predict = predict(db.pca)
db.kmeans = kmeans(db.pca.predict, 2)

plot(db.pca.predict, col = db.kmeans[["cluster"]], main = "Metoda k-średnich (2 klastry)")
points(db.kmeans[["centers"]], col = 1:8, pch = 16, cex = 1.8)

# liczymy odsetek zdrowych i chorych osob w klastrach
first.cluster.healthy = 0
first.cluster.sick = 0
second.cluster.healthy = 0
second.cluster.sick = 0

for (i in 1:nrow(db.nz.norm))
{
  if (db.kmeans$cluster[i] == 1)
  {
    if (db.nz.norm$Outcome[i] == "healthy") first.cluster.healthy = first.cluster.healthy + 1
    else first.cluster.sick = first.cluster.sick + 1
  }
  else
  {
    if (db.nz.norm$Outcome[i] == "healthy") second.cluster.healthy = second.cluster.healthy + 1
    else second.cluster.sick = second.cluster.sick + 1
  }
}

# ilosc 
healthy = nrow(subset(db.nz.norm,Outcome == "healthy"))
sick = nrow(subset(db.nz.norm,Outcome == "sick"))

healthy
first.cluster.healthy
first.cluster.healthy.ratio = first.cluster.healthy / healthy
second.cluster.healthy
second.cluster.healthy.ratio = second.cluster.healthy / healthy

sick
first.cluster.sick
first.cluster.sick.ratio = first.cluster.sick / sick
second.cluster.sick
second.cluster.sick.ratio = second.cluster.sick / sick


first.cluster.healthy.ratio
second.cluster.healthy.ratio
first.cluster.sick.ratio
second.cluster.sick.ratio


### 