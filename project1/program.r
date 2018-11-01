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
cnf3 = CnfParse("./cnf/1.cnf")
CnfFitness(cnf3, chr3)

chr4 = rep(1, 100)
cnf4 = CnfParse("./cnf/CBS_k3_n100_m403_b10_0.cnf")
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



timeIte = matrix(NA, 10, 10)
timePop = matrix(NA, 10, 10)
for (i in 1:10)
{
  for (j in 1:10)
  {
    timeIte[i, j] = system.time(rbga.bin(size = 100, popSize = 10, iters = 10 * j, mutationChance = 0.10, elitism = T, evalFunc = CnfFitnessGa))[3]
    timePop[i, j] = system.time(rbga.bin(size = 100, popSize = 10 * j, iters = 10, mutationChance = 0.10, elitism = T, evalFunc = CnfFitnessGa))[3]
  }
}

meanIte = c()
meanPop = c()
for (i in 1:10)
{
  meanIte[i] = mean(timeIte[, i])
  meanPop[i] = mean(timePop[, i])
}


timePlot = plot(1:10 * 10, meanIte, main = "How number of iterations affects time", xlab = "Iterations", ylab = "time (s)", type="l", col="blue")
