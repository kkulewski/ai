### 2.

## a
getwd()


## b
osoby = read.csv("osoby.csv", header=TRUE, sep = ",")
osoby


## c
osoby[,2]


## d
subset(osoby, plec == "k")


## e
klub50 = subset(osoby, plec == "m" & wiek > 50)
write.csv(klub50, file = "osoby2.csv")

