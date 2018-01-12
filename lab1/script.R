require(GGally)

# Reading the credit score data
credit <- read.csv("data/credit.csv", sep = ";", dec = ",")

# 1. Import the data and make appropriate visualisation (pairs, ggpairs)
pairs(credit)
ggpairs(credit)

# 2. Split the data randomly into training and test data set (sample).

#Sample Indexes
indexes = sample(1:nrow(credit), size=0.2*nrow(credit))

# Split data
test = credit[indexes,]
dim(test)  # 100 8
train = credit[-indexes,]
dim(train) # 400 11
