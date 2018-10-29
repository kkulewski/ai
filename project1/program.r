# main

chr1 = c(1, 0, 1)

# 3/3 satisfable with chr1
cnf1 = list()
cnf1[[1]] = c(1, 2, -3)
cnf1[[2]] = c(1, -2, -3)
cnf1[[3]] = c(-1, 3)

# 3/5 satisfable with chr1
cnf2 = list()
cnf2[[1]] = c(1, 2, -3)
cnf2[[2]] = c(-1, 2, -3)
cnf2[[3]] = c(1, -2, -3)
cnf2[[4]] = c(-1)
cnf2[[5]] = c(-1, 3)


CnfFitness(cnf1, chr1)
CnfFitness(cnf2, chr1)

# using CNF file

cnf3 = CnfParse("./cnf/1.cnf")
CnfFitness(cnf3, chr1)
