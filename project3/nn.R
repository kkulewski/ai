########
# Libs #
########

install.packages("splitstackshape")
library(splitstackshape)

install.packages("neuralnet")
library(neuralnet)



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
write.csv(data.merged, file = "0-01-data-merged.csv")

# split invalid first column, drop first part, fix second part header, save
data.fixed = concat.split(data = data.merged, split.col = "Main.js.143.myTank.x", sep = " ", drop = TRUE)
data.fixed = data.fixed[, c(51, 1:49)]
colnames(data.fixed)[colnames(data.fixed) == "Main.js.143.myTank.x_2"] = "myTank.x"
write.csv(data.fixed, file = "0-02-data-fixed.csv")



###########
# Process #
###########

data.stripped = data.fixed[, c(10:16, 50, 1:9, 29:36)]
write.csv(data.stripped, file = "0-03-data-stripped.csv")

# INPUT
# 01 myTank.controls.turnLeft
# 02 myTank.controls.turnRight
# 03 myTank.controls.goForward
# 04 myTank.controls.goBack
# 05 myTank.controls.shoot
# 06 myTank.controls.cannonLeft
# 07 myTank.controls.cannonRight

# TIME
# 08 currentGameTime

# MY TANK
# 09 myTank.x
# 10 myTank.y
# 11 myTank.rotation
# 12 myTank.cannonRotation
# 13 myTank.velocityX
# 14 myTank.velocityY
# 15 myTank.accelerationX
# 16 myTank.accelerationY
# 17 myTank.shootCooldown

# ENEMY TANK
# 18 enemyTank.x
# 19 enemyTank.y
# 20 enemyTank.rotation
# 21 enemyTank.cannonRotation
# 22 enemyTank.velocityX
# 23 enemyTank.velocityY
# 24 enemyTank.accelerationX
# 25 enemyTank.accelerationY



#############
# Normalize #
#############

norm <- function(x)
{
  (x-min(x)) / (max(x)-min(x))
}

data.norm = norm(data.stripped)
write.csv(data.norm, file = "0-04-data-normalized.csv")


##############
# Neural Net #
##############

# prepare formula
out.names = names(data.norm[, 1:7])
inp.names = names(data.norm[, 8:25])
formula = paste(paste(out.names, collapse = " + "), paste(inp.names, collapse = " + "), sep = " ~ ")

# create NN
hidden.amount = 7
nn.result <- neuralnet(formula, data.norm, hidden = hidden.amount, threshold = 0.1, stepmax = 1e6)

plot(nn.result)



###########
# Extract #
###########

inp.amount = length(inp.names)
out.amount = length(out.names)
hid.amount = hidden.amount

bias1 = round(nn.result[["weights"]][[1]][[1]][1,], 6)
bias2 = round(nn.result[["weights"]][[1]][[2]][1,], 6)
weig1 <- round(nn.result[["weights"]][[1]][[1]][2:(inp.amount+1),], 8)
weig2 <- round(nn.result[["weights"]][[1]][[2]][2:(hid.amount+1),], 8)



########
# Copy #
########

bias1.string = c()
bias1.string[1] = "["
bias1.string[2] = toString(bias1)
bias1.string[3] = "]"
bias1.text = paste(bias1.string, collapse = '')
write(bias1.text, "z_bias1.txt")

bias2.string = c()
bias2.string[1] = "["
bias2.string[2] = toString(bias2)
bias2.string[3] = "]"
bias2.text = paste(bias2.string, collapse = '')
write(bias2.text, "z_bias2.txt")

write.table(weig1, file="z_weig1.txt", row.names = FALSE)
weig1.lines = scan("z_weig1.txt", what = character(), sep = '\n')
weig1.lines
weig1.text = c()
for (line in 2:length(weig1.lines))
{
  weig1.string = c()
  weig1.string[1] = "["
  weig1.string[2] = gsub(" ", ", ", weig1.lines[line])
  weig1.string[3] = "],"
  weig1.text[line-1] = paste(weig1.string, collapse = '')
}
weig1.text
conn = file("z_weig1.txt")
writeLines(weig1.text, conn)
close(conn)

write.table(weig2, file="z_weig2.txt", row.names = FALSE)
weig2.lines = scan("z_weig2.txt", what = character(), sep = '\n')
weig2.text = c()
for (line in 2:length(weig2.lines))
{
  weig2.string = c()
  weig2.string[1] = "["
  weig2.string[2] = gsub(" ", ", ", weig2.lines[line])
  weig2.string[3] = "],"
  weig2.text[line-1] = paste(weig2.string, collapse = '')
}
conn = file("z_weig2.txt")
writeLines(weig2.text, conn)
close(conn)

conn = file("zz_full.txt")
writeLines(weig1.text, conn)
writeLines("", conn)
writeLines(bias1.text, conn)
writeLines("", conn)
writeLines(weig2.text, conn)
writeLines("", conn)
writeLines(bias2.text, conn)
close(conn)
