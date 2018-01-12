# The objective is to forecast the credit score of bank clients
# relying on several explanatory variables.

require(GGally)

# Reading the credit score data
credit <- read.csv("data/credit.csv", sep = ";", dec = ",")

# 1. Import the data and make appropriate visualisation (pairs, ggpairs)
pairs(credit)
ggpairs(credit)

# 2. Split the data randomly into training and test data set (sample).

# Sample indexes
indexes <- sample(1:nrow(credit), size=0.2*nrow(credit))

# Splitting the data
credit.test <- credit[indexes,]
dim(credit.test)  # 100 8

credit.train <- credit[-indexes,]
dim(credit.train) # 400 11

# 3. Run the regression using training data set. Assess the fit.
model <- lm(score ~ ., credit.train)
summary(model)
plot(model)

# 4. Compute the confidence intervals for the parameters.
confint(model)

# 5. Compute the forecasts (point, prediction and confidence intervals)
# for the test data set (predict, forecast).
pred <- predict(model, newdata = credit.test, interval = "confidence")
