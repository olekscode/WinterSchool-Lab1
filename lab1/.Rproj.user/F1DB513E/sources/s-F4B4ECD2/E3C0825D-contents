# The objective is to forecast the credit score of bank clients
# relying on several explanatory variables.

require(GGally)
require(forecast)
require(Metrics)
require(margins)

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
matplot(c(1:100), pred, type="p", pch=15, col=c(1,2,2), cex=0.8)
pred <- predict(model, newdata = credit.test, interval = "prediction")
forec <- forecast(model, newdata = credit.test)

# 6. Compute forecast errors and analyse them.
mse(credit.test$score, forec$mean)
mae(credit.test$score, forec$mean)
mape(credit.test$score, forec$mean)

# 7. Make appropriate data transformation and repeat the above steps
# for the new model.
model <- lm(score ~ savings + I(income ^ 2) + log(savings + 1), credit.train)
summary(model)
confint(model)

# 8. Compute the marginal effects for the original and the transformed
# models (margins).
model <- lm(score ~ savings + income, credit.train)
margins(model)

# 9. Compare the forecasting ability of the two models using loss functions
# and using appropriate tests.

# 10. The score variable is slightly skewed. Takings logs shall overcome
# this problem. Check the forecasting ability of the log-score model.
