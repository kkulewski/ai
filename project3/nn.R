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

data.stripped = data.fixed[, c(10:16, 50, 1:9, 29:36)]
write.csv(data.stripped, file = "data-stripped.csv")

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


