### zad3

## a
osoby = read.csv("osoby.csv", header=TRUE, sep = ",")
osoby
#wyplata = sample(2000:5000,7)
wyplata = round(runif(7, 2000.0, 5000.0), 2)
place = cbind(osoby, wyplata)
place


## b
nowy = list("Kowalski", "Jan", "m", 30, round(runif(1, 2000.0, 5000.0), 2))
nowy
# typeof(place)
fplace = as.data.frame.matrix(place)
fplace2 = rbind(fplace, nowy)
fplace2


## c
srednia = mean(fplace2[,"wyplata"])
srednia

odchylenie = sd(fplace2[,"wyplata"])
odchylenie

max = max(fplace2[,"wyplata"])
max

min = min(fplace2[,"wyplata"])
min


## d
fstand = function(x)
{
  return ((x-mean(x))/sd(x))
}

place_stand = fstand(fplace2[,"wyplata"])
place_stand = as.matrix(as.data.frame(place_stand))
place_stand
fplace3 = cbind(fplace2, place_stand=place_stand)
fplace3

mean(fplace3[,"place_stand"])
sd(fplace3[,"place_stand"])


## e
fnorm = function(x)
{
  return (x-min(x))/(max(x)-min(x))
}

place_norm = fnorm(data.matrix(fplace3[,"wyplata"]))
place_norm = as.data.frame.matrix(place_norm)
fplace4 = cbind(fplace3, place_norm)
fplace4

min(fplace4[,"V1"])
max(fplace4[,"V1"])


## f
summary(fplace4)
# avg age = 40.88