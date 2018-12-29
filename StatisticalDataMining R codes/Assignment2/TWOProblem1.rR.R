####################################################################################
##
## Problem 1
####################################################################################

#install.packages("ggplot2")
#install.packages("ISLR")
#install.packages("pls")
#install.packages("mclust")

library("ggplot2")
library("leaps")
library("glmnet")
library(ISLR)
library("pls")
library("mclust")


?College

# Number of n vs p = 777 x 18
dim(College)
# Names of the predictors
names(College)

# na.omit() is used to get rid of missing data
colg_data <- na.omit(College[,1:18])

# Data transformation to avoid coercing errors
colg_data$Private <- ifelse(colg_data$Private == "Yes", 1, 0)


# Response variable = Number of applications recd
# Y = Apps
# X = Rest of the predictors
colg_data_without_apps <- colg_data
colg_data_without_apps$Apps <- NULL
X <- as.matrix(colg_data[3:18])
Y <- colg_data$Apps

train_index <- sample(1:nrow(colg_data), 0.7 * nrow(colg_data))

colg_train <- colg_data[train_index, ]
colg_test <- colg_data[-train_index, ]

colg_train_no_response <- X[train_index, ]
colg_test_no_response <- X[-train_index, ]
colg_train_yes_response <- Y[train_index]
colg_test_yes_response <- Y[-train_index]

####################################################################################
##
## LINEAR MODEL
####################################################################################

# Train data
mod_train <- lm(colg_train$Apps ~ ., data = colg_train)
pred_vals_test <- predict.lm(mod_train, colg_test[-2])
colg_test_mse <- mean((colg_test$Apps - round(pred_vals_test)) ^ 2)

# Checking the accuracy
mean((colg_test$Apps - pred_vals_test)/colg_test$Apps)*100

# Calculating the errors
colg_test_mse #495039.6

# Error Rate
error_rate_lm_test <- classError(pred_vals_test, colg_test$Apps)$errorRate
error_rate_lm_test
####################################################################################
##
## RIDGE
####################################################################################

ridge.mod = glmnet(X, Y, alpha=0)

set.seed(12345)
cv.out <- cv.glmnet(X[train_index, ], Y[train_index], alpha = 0)
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")
# Coefficient
ridge.pred


# Taking Test data for all features except "Apps"
ridge.pred1 <- predict(ridge.mod, s = bestlam, newx = X[train_index,], type = "response")

# Taking Train data for all features except "Apps"
ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X[-train_index,], type = "response")

# Calculating Train error
y_hat <- ridge.pred1
y_true <- Y[train_index]
train_error_ridge <- mean((y_hat - y_true)^2)  #test_error
train_error_ridge #1659382

# Calculating Test error
y_hat <- ridge.pred2
y_true <- Y[-train_index]
test_error_ridge <- mean((y_hat - y_true)^2)  #test_error
test_error_ridge #864661.7

# Error Rate
error_rate_lm_test <- classError(y_hat, y_true)$errorRate
error_rate_lm_test

####################################################################################
##
## LASSO
####################################################################################

lasso.mod <- glmnet(X[train_index,], Y[train_index], alpha = 1)
coefficients(lasso.mod)
plot(lasso.mod)

cv.out = cv.glmnet(X[train_index,], Y[train_index], alpha = 1)
bestlam = cv.out$lambda.min
# Best Lambda
bestlam

lasso.pred <- predict(lasso.mod, s = bestlam, type = "coefficients")
# Coefficients
lasso.pred

lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = X[-train_index,], type = "response")

y_hat_lasso <- lasso.pred2
y_true <- Y[-train_index]

test_error_lasso <- mean((y_hat_lasso-y_true)^2) 

test_error_lasso #906288.8

error_rate_lasso_test <- classError(y_hat_lasso, y_true)$errorRate
error_rate_lasso_test


####################################################################################
##
## PCR
####################################################################################

colg_pcr_mod = pcr(Apps ~. , data = colg_train, scale = TRUE, validation = "CV")
summary(colg_pcr_mod)
validationplot(colg_pcr_mod, val.type = "MSEP", main = "Principle Component Regression")

#Prediction
colg_test_pcr_pred = predict(colg_pcr_mod, colg_test, ncomp = 4)
colg_train_pcr_pred = predict(colg_pcr_mod, colg_data[train_index,], ncomp = 4)

#Errors
#PCR Errors
test_error_pcr = mean((colg_test$Apps - colg_test_pcr_pred)^2)
test_error_pcr #3049114
train_error_pcr = mean((colg_train$Apps - colg_train_pcr_pred)^2)
train_error_pcr #3519464

####################################################################################
##
## PLS
####################################################################################

colg_pls_mod = plsr(Apps ~. , data = colg_data[train_index,], scale = TRUE, 
                    validation = "CV")
summary(colg_pls_mod)
validationplot(colg_pls_mod, val.type = "MSEP", main = "Partial Least Squares")

#Prediction
colg_test_pls_pred = predict(colg_pls_mod, colg_test, ncomp = 6)
colg_train_pls_pred = predict(colg_pls_mod, colg_data[train_index,], ncomp = 6)

#Errors
#PLS Errors
test_error_pls = mean((colg_test$Apps - colg_test_pls_pred)^2)
test_error_pls #863900.4
train_error_pls = mean((colg_train$Apps - colg_train_pls_pred)^2)
train_error_pls #1233300

#PLS Model produced the least error
min(colg_test_mse, test_error_ridge, test_error_lasso, 
    test_error_pcr, test_error_pls)

#Error Rates

# Linear Model
error_rate_lm_test <- classError(pred_vals_test, colg_test$Apps)$errorRate
error_rate_lm_test
# Ridge
error_rate_ridge_test <- classError(y_hat, y_true)$errorRate
error_rate_ridge_test
# Lasso
error_rate_lasso_test <- classError(y_hat_lasso, y_true)$errorRate
error_rate_lasso_test
# PCR
error_rate_pcr_test <- classError(colg_test_pcr_pred, y_true)$errorRate
error_rate_pcr_test
# PLS
error_rate_pls_test <- classError(colg_test_pls_pred, y_true)$errorRate
error_rate_pls_test
