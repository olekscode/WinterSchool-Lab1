require(GGally)

credit <- read.csv("data/credit.csv", sep = ";", dec = ",")
ggpairs(data = credit)
