########
# Libs #
########

install.packages("splitstackshape")
library(splitstackshape)



########
# Load #
########

setwd("C:/Users/Krzysztof/Documents/ai/project3/data")

# load files into data frames list
data.raw = list()
for (fileNumber in 1:99)
{
  data.raw[[fileNumber]] = read.csv(toString(fileNumber), header=TRUE, sep=",")
}

# bind data frames into single one
data.merged = data.raw[[1]]
for (fileNumber in 2:99)
{
  data.merged = rbind(data.merged, data.raw[[fileNumber]])
}

# split invalid first column, drop first part, fix second part header, save
data.fixed = concat.split(data = data.merged, split.col = "Main.js.143.myTank.x", sep = " ", drop = TRUE)
data.fixed = data.fixed[, c(51, 1:49)]
colnames(data.fixed)[colnames(data.fixed) == "Main.js.143.myTank.x_2"] = "myTank.x"
write.csv(data.fixed, file = "data-fixed.csv")



###########
# Process #
###########

