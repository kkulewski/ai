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
db
summary(db)



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
db.nz$Outcome



### 3. Klasyfikacja

# Normalizacja danych

fnorm = function(x) { return ((x-min(x))/(max(x)-min(x))) }

db.nz$Pregnancies = fnorm(db.nz$Pregnancies)
db.nz$Glucose = fnorm(db.nz$Glucose)
db.nz$BloodPressure = fnorm(db.nz$BloodPressure)
db.nz$SkinThickness = fnorm(db.nz$SkinThickness)
db.nz$Insulin = fnorm(db.nz$Insulin)
db.nz$BMI = fnorm(db.nz$BMI)
db.nz$DiabetesPedigreeFunction = fnorm(db.nz$DiabetesPedigreeFunction)
db.nz$Age = fnorm(db.nz$Age)

# Podzial na grupe treningowa i testowa
set.seed(1234)
ind <- sample(2, nrow(db.nz), replace=TRUE, prob=c(0.8, 0.2))
db.training <- db.nz[ind==1, 1:9]
db.test <- db.nz[ind==2, 1:9]

## 3.1 Klasyfikacja drzewami
#install.packages("party")
#library("party")

db.ctree = ctree(Outcome ~ ., data = db.training)

# Macierz bledu i dokladnosc
ctr.predicted = predict(db.ctree, db.test[,1:8])
ctr.real = db.test[,9]
ctr.conf.matrix = table(ctr.predicted, ctr.real)
ctr.accuracy = sum(diag(ctr.conf.matrix)) / sum(ctr.conf.matrix)


## 3.2 Klasyfikacja Naive-Bayes
#install.packages("class")
#library("class")

db.knn3 = knn(db.training[,1:8], db.test[,1:8], cl=db.training[,9], k = 2, prob=FALSE)

# Macierz bledu i dokladnosc
knn.predicted = db.knn3
knn.real = db.test[,9]
knn.conf.matrix = table(knn.predicted, knn.real)
knn.accuracy = sum(diag(knn.conf.matrix)) / sum(knn.conf.matrix)
knn.conf.matrix