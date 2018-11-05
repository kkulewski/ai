# main

# 3/3 satisfable
chr1 = c(1, 0, 1)
cnf1 = list()
cnf1[[1]] = c(1, 2, -3)
cnf1[[2]] = c(1, -2, -3)
cnf1[[3]] = c(-1, 3)
CnfFitness(cnf1, chr1)

# 3/5 satisfable
chr2 = c(1, 0, 1)
cnf2 = list()
cnf2[[1]] = c(1, 2, -3)
cnf2[[2]] = c(-1, 2, -3)
cnf2[[3]] = c(1, -2, -3)
cnf2[[4]] = c(-1)
cnf2[[5]] = c(-1, 3)
CnfFitness(cnf2, chr2)


# using CNF file
chr3 = c(1, 0, 1)
cnf3 = CnfParse("../cnf/1.cnf")
CnfFitness(cnf3, chr3)

chr4 = rep(1, 100)
cnf4 = CnfParse("../cnf/CBS_k3_n100_m403_b10_0.cnf")
CnfFitness(cnf4, chr4)



# GA 
install.packages("genalg")
library(genalg)

CnfFitnessGa = function(chromosome)
{
  return (CnfFitness(cnf4, chromosome))
}

gaResult = rbga.bin(size = 100, 
                    popSize = 25, 
                    iters = 50, 
                    mutationChance = 0.05, 
                    elitism = T, 
                    evalFunc = CnfFitnessGa)

bestChromosome = gaResult$population[which.min(gaResult$evaluations),]

CnfFitness(cnf4, bestChromosome)



## iter / pop : time / score

sampleRangeEnd = 50

scorIte = matrix(NA, 10, sampleRangeEnd)
timeIte = matrix(NA, 10, sampleRangeEnd)

scorPop = matrix(NA, 10, sampleRangeEnd)
timePop = matrix(NA, 10, sampleRangeEnd)

sampleRange = 1:sampleRangeEnd

for (i in 1:10)
{
  for (j in sampleRange)
  {
    timeIte[i, j] = system.time(rbga.bin(size = 100, popSize = 25, iters = 10 * j, mutationChance = 0.02, elitism = T, evalFunc = CnfFitnessGa))[3]
    iteGa = rbga.bin(size = 100, popSize = 25, iters = 10 * j, mutationChance = 0.02, elitism = T, evalFunc = CnfFitnessGa)
    scorIte[i, j] = min(iteGa$evaluations)
      
    timePop[i, j] = system.time(rbga.bin(size = 100, popSize = 10 * j, iters = 25, mutationChance = 0.02, elitism = T, evalFunc = CnfFitnessGa))[3]
    popGa = rbga.bin(size = 100, popSize = 10 * j, iters = 25, mutationChance = 0.02, elitism = T, evalFunc = CnfFitnessGa)
    scorPop[i, j] = min(popGa$evaluations)
  }
}


meanScorIte = c()
meanTimeIte = c()

meanScorPop = c()
meanTimePop = c()

for (i in sampleRange)
{
  meanScorIte[i] = mean(scorIte[, i])
  meanTimeIte[i] = mean(timeIte[, i])
  
  meanScorPop[i] = mean(scorPop[, i])
  meanTimePop[i] = mean(timePop[, i])
}


scorPlotIte = plot(sampleRange * 10, meanScorIte, main = "Liczba iteracji a wynik", xlab = "Liczba iteracji", ylab = "niespełnione klauzule", type="l", col="blue")
timePlotIte = plot(sampleRange * 10, meanTimeIte, main = "Liczba iteracji a czas", xlab = "Liczba iteracji", ylab = "czas (sekundy)", type="l", col="blue")

scorPlotPop = plot(sampleRange * 10, meanScorPop, main = "Wielkość populacji a wynik", xlab = "Wielkość populacji", ylab = "niespełnione klauzule", type="l", col="green")
timePlotPop = plot(sampleRange * 10, meanTimePop, main = "Wielkość populacji a czas", xlab = "Wielkość populacji", ylab = "czas (sekundy)", type="l", col="green")



## mutation chance : score

scorMut = matrix(NA, 20, 26)

for (i in 1:20)
{
  for (j in 0:25)
  {
    mutGa = rbga.bin(size = 100, popSize = 50, iters = 50, mutationChance = j/100, elitism = T, evalFunc = CnfFitnessGa)
    scorMut[i, j + 1] = min(mutGa$evaluations)
  }
}

meanScorMut = c()

for (i in 1:26)
{
  meanScorMut[i] = mean(scorMut[, i])
}

scorPlotPop = plot(0:25, meanScorMut, main = "Szansa mutacji a wynik", xlab = "% szansy mutacji", ylab = "niespełnione klauzule", type="l", col="violet")



## elitism : score

scorEli = matrix(NA, 20, 2)

for (i in 1:20)
{
  eliOfGa = rbga.bin(size = 100, popSize = 50, iters = 50, mutationChance = 0.02, elitism = F, evalFunc = CnfFitnessGa)
  scorEli[i, 1] = min(eliOfGa$evaluations)
  eliOnGa = rbga.bin(size = 100, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = CnfFitnessGa)
  scorEli[i, 2] = min(eliOnGa$evaluations) 
}

meanEliOf = mean(scorEli[, 1])
meanEliOn = mean(scorEli[, 2]) 
100 - meanEliOf
100 - meanEliOn



## instance size : time

cnf050 = CnfParse("../cnf/prog/uf50-01.cnf")
cnf075 = CnfParse("../cnf/prog/uf75-01.cnf")
cnf100 = CnfParse("../cnf/prog/uf100-01.cnf")
cnf125 = CnfParse("../cnf/prog/uf125-01.cnf")
cnf150 = CnfParse("../cnf/prog/uf150-01.cnf")
cnf175 = CnfParse("../cnf/prog/uf175-01.cnf")
cnf200 = CnfParse("../cnf/prog/uf200-01.cnf")
cnf225 = CnfParse("../cnf/prog/uf225-01.cnf")
cnf250 = CnfParse("../cnf/prog/uf250-01.cnf")

ga050 = function(chromosome) { return (CnfFitness(cnf050, chromosome)) }
ga075 = function(chromosome) { return (CnfFitness(cnf075, chromosome)) }
ga100 = function(chromosome) { return (CnfFitness(cnf100, chromosome)) }
ga125 = function(chromosome) { return (CnfFitness(cnf125, chromosome)) }
ga150 = function(chromosome) { return (CnfFitness(cnf150, chromosome)) }
ga175 = function(chromosome) { return (CnfFitness(cnf175, chromosome)) }
ga200 = function(chromosome) { return (CnfFitness(cnf200, chromosome)) }
ga225 = function(chromosome) { return (CnfFitness(cnf225, chromosome)) }
ga250 = function(chromosome) { return (CnfFitness(cnf250, chromosome)) }

timeVar = matrix(NA, 5, 9)

for (i in 1:5)
{
  timeVar[i, 1] = system.time(rbga.bin(size = 50, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga050))[3]
  timeVar[i, 2] = system.time(rbga.bin(size = 75, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga075))[3]
  timeVar[i, 3] = system.time(rbga.bin(size = 100, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga100))[3]
  timeVar[i, 4] = system.time(rbga.bin(size = 125, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga125))[3]
  timeVar[i, 5] = system.time(rbga.bin(size = 150, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga150))[3]
  timeVar[i, 6] = system.time(rbga.bin(size = 175, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga175))[3]
  timeVar[i, 7] = system.time(rbga.bin(size = 200, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga200))[3]
  timeVar[i, 8] = system.time(rbga.bin(size = 225, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga225))[3]
  timeVar[i, 9] = system.time(rbga.bin(size = 250, popSize = 50, iters = 50, mutationChance = 0.02, elitism = T, evalFunc = ga250))[3]
}


meanTimeVar = c()

for (i in 1:9)
{
  meanTimeVar[i] = mean(timeVar[, i])
}

timePlotIte = plot(c(50, 75, 100, 125, 150, 175, 200, 225, 250), meanTimeVar, main = "Liczba zmiennych a czas", xlab = "Liczba zmiennych", ylab = "czas (sekundy)", type="l", col="blue")
