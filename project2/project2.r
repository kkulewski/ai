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


# wczytujemy baze
db <- read.csv('diabetes.csv', header=TRUE, sep=',')


# wstepne ogledziny
db
summary(db)

# szybko mozemy dostrzec, ze brakujace wartosci zostaly prawdopodobnie zastapione zerami
glucose.mean = mean(db$Glucose[which(db$Glucose>0)])
bloodPressure.mean = mean(db$BloodPressure[which(db$BloodPressure>0)])
skinThickness.mean = mean(db$SkinThickness[which(db$SkinThickness>0)])
insulin.mean = mean(db$Insulin[which(db$Insulin>0)])
bmi.mean = mean(db$BMI[which(db$BMI>0.0)])
dpf.mean = mean(db$DiabetesPedigreeFunction[which(db$DiabetesPedigreeFunction>0.0)])
age.mean = mean(db$Age[which(db$Age>0)])

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

db.nz
