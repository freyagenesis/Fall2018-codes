####################################################################################
##
## Problem 3
####################################################################################

library("ggplot2")
library("leaps")
library("glmnet")
library("ISLR")
library("pls")
set.seed(12345)

# A data set with p = 20 features, n = 1, 000 observations
n <- 1000
p <- 20

# Y = X.Beta + Bias
# Main data set as a data frame X
dataSample <- data.frame(replicate(p, sample(0:1000, n, replace = TRUE)))
# Beta Coefficients
coeffs <- rexp(20,rate=7)
# setting some beta coeffcients to 0
coeffs[c(2,5,7,9,11)] <- 0
hist(coeffs)
# Bias values
Bias <- sample(0:n, n, replace = TRUE)
# Y = X.Beta + Bias
X <- as.matrix(dataSample)
dataSample$Y <- (X %*% coeffs) + Bias

train_index <- sample(1:nrow(dataSample), 100)

data_train <- dataSample[train_index, ]
data_test <- dataSample[-train_index, ]

# Exhaustive subset selection
sub_select <- regsubsets(data_train$Y~., data = data_train, nvmax = 20, 
                       method = "exhaustive")
sub_select_summ <- summary(sub_select)
sub_select_summ
par(mfrow = c(2,2))
plot(sub_select_summ$cp, xlab = "Predictors", ylab = "CP", type = "l")
plot(sub_select_summ$bic, xlab = "Predictors", ylab = "BIC", type = "l")
plot(sub_select_summ$rss, xlab = "Predictors", ylab = "RSS", type = "l")
plot(sub_select_summ$adjr2, xlab = "Predictors", ylab = "Adjusted R^2", type = "l")

#They both agree model with 8 variables is the best.
which(sub_select_summ$cp == min(sub_select_summ$cp))
which(sub_select_summ$bic == min(sub_select_summ$bic))
which(sub_select_summ$rss == min(sub_select_summ$rss))
which(sub_select_summ$adjr2 == max(sub_select_summ$adjr2))

# Creating Sub Models and Plotting Test Train MSE

#Train MSE
regfit.full <- regsubsets(data_train$Y ~ ., data = data_train, nvmax = p)
train.mat <- model.matrix(data_train$Y ~ ., data = data_train, nvmax = p)
val.errors.train <- rep(NA, p)
min_train_error = 1000000000000
min_train_val_of_predictors = 0
for (i in 1:p) {
  coefi <- coef(regfit.full, id = i)
  pred <- as.matrix(train.mat[, names(coefi)]) %*% coefi
  val.errors.train[i] <- mean((pred - data_train$Y)^2)
  if(val.errors.train[i] < min_error)
  {
    min_train_error = val.errors.train[i]
    min_train_val_of_predictors = i
  }
}


#Test MSE
test.mat <- model.matrix(data_test$Y ~ ., data = data_test, nvmax = 20)
val.errors.test <- rep(NA, p)
min_test_error = 1000000000000
min_test_val_of_predictors = 0
for (i in 1:p) {
  coefi <- coef(regfit.full, id = i)
  pred <- as.matrix(test.mat[, names(coefi)]) %*% coefi
  val.errors.test[i] <- mean((pred - data_test$Y)^2)
  if(val.errors.test[i] < min_test_error)
  {
    min_test_error = val.errors.test[i]
    min_test_val_of_predictors = i
  }
}
par(mfrow = c(1,2))

plot(val.errors.train, type = "l", xlab = "Number of predictors", ylab = "Training MSE", main = "Training set MSE
associated with the best model of each size")  
plot(val.errors.test, type = "l", xlab = "Number of predictors", ylab = "Test MSE", main = "Test set MSE
     associated with the best model of each size") 

#For which model size does the test set MSE take on its minimum value?
# min_train_error
# min_train_val_of_predictors
min_test_error 
min_test_val_of_predictors 

