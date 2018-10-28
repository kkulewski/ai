# lab3
install.packages("genalg")
library(genalg)

#1a
duzyProblemPlecakowy <- data.frame(wartosc = sample(10:100,30), waga = sample(10:100,30))
duzyLimit <- 600

fitnessFunc2 <- function(chr)
{
  calkowita_wartosc_chr <- chr %*% duzyProblemPlecakowy$wartosc
  calkowita_waga_chr <- chr %*% duzyProblemPlecakowy$waga
    
  if (calkowita_waga_chr > duzyLimit)
    return(0) else return(-calkowita_wartosc_chr)
}

duzyPlecakGenAlg <- rbga.bin(size = 30, popSize = 200, iters = 50, mutationChance = 0.03, elitism = T, evalFunc = fitnessFunc2)

duzyPlecakGenAlg

#1b
chartData <- data.frame(srednia = -duzyPlecakGenAlg$mean, maksymalne = -duzyPlecakGenAlg$best)


#1c-e
wykres = plot(chartData$maksymalne, main = "Dzialanie alg. genetycznego", xlab = "pokolenie", ylab = "fitness", type="l", col="red")
lines(chartData$srednia, type="l", col="blue")
legend("bottomright", legend = c("srednia", "maksymalnie"), col = c("blue", "red"), lty = 1)
wykres


###### zad2
t = c()
for (items in 1:5)
{
  duzyProblemPlecakowy <- data.frame(wartosc = sample(10:100, replace = TRUE, items * 30), waga = sample(10:100, replace = TRUE, items * 30))
  duzyLimit <- items * 30 * 20
  t[items] <- system.time(rbga.bin(size = items * 30, popSize = 200, iters = 50, mutationChance = 0.03, elitism = T, evalFunc = fitnessFunc2))[1]
}

wykr = plot(c(30, 60, 90, 120, 150), t, main = "Czas dzialania a dlugosc chromosomu", xlab = "dlugosc chromosomu", ylab = "czas (s)", type="l", col="blue")

