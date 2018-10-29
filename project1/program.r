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