####################################################################################
##
## Problem 2
####################################################################################

library(data.table)
tic_data_train <- fread('http://kdd.ics.uci.edu/databases/tic/ticdata2000.txt')
tic_data_test <- fread('http://kdd.ics.uci.edu/databases/tic/ticeval2000.txt')
tic_data_test_target <- fread('http://kdd.ics.uci.edu/databases/tic/tictgts2000.txt')

####################################################################################
##
## OLS
####################################################################################

lm_mod <- lm(tic_data_train$V86 ~., data = tic_data_train)
train_mse <- mean(round(lm_mod$residuals)^2)
pred_vals <- predict.lm(lm_mod, tic_data_test)
test_mse <- mean((tic_data_test_target - round(pred_vals)) ^ 2)
train_mse*100 #5.96%
test_mse*100 #5.975%

plot(lm_mod)

# V4, V44, V47, V55, V57, V58, V59, V76, V78, V79, V80, V82

####################################################################################
##
## FORWARD SELECTION
####################################################################################

dim(tic_data_train)
subf.fwd <- regsubsets(tic_data_train$V86~., data = tic_data_train, nvmax = 86, 
                       method = "forward")
sum_fwd <- summary(subf.fwd)

par(mfrow = c(2,2))
plot(sum_fwd$cp, xlab = "Predictors", ylab = "Cp", type = "l")
plot(sum_fwd$bic, xlab = "Predictors", ylab = "BIC", type = "l")
plot(sum_fwd$rss, xlab = "Predictors", ylab = "RSS", type = "l")
plot(sum_fwd$adjr2, xlab = "Predictors", ylab = "Adjusted R^2", type = "l")

#They both agree model with 23 variables is the best.
which(sum_fwd$cp == min(sum_fwd$cp))
which(sum_fwd$bic == min(sum_fwd$bic))
which(sum_fwd$rss == min(sum_fwd$rss))
which(sum_fwd$adjr2 == max(sum_fwd$adjr2))

####################################################################################
##
## BACKWARD SELECTION
####################################################################################

dim(tic_data_train)
subf.bwd <- regsubsets(tic_data_train$V86~., data = tic_data_train, nvmax = 86, 
                       method = "backward")
summ_bwd <- summary(subf.bwd)
x11()
par(mfrow = c(2,2))
plot(summ_bwd$cp, xlab = "Predictors", ylab = "Cp", type = "l")
plot(summ_bwd$bic, xlab = "Predictors", ylab = "BIC", type = "l")
plot(summ_bwd$rss, xlab = "Predictors", ylab = "RSS", type = "l")
plot(summ_bwd$adjr2, xlab = "Predictors", ylab = "Adjusted R^2", type = "l")

#They both agree model with 29 variables is the best.
which(summ_bwd$cp == min(summ_bwd$cp))
which(summ_bwd$bic == min(summ_bwd$bic))
which(summ_bwd$rss == min(summ_bwd$rss))
which(summ_bwd$adjr2 == max(summ_bwd$adjr2))

####################################################################################
##
## COMPARING FORWARD BACKWARD SELECTION
####################################################################################

summary(subf.fwd)$outmat 
summary(subf.bwd)$outmat 

summary(subf.fwd)$outmat[23,]
summary(subf.bwd)$outmat[23,]
coef(subf.fwd, 23)
coef(subf.bwd, 23)

summary(subf.fwd)$outmat[29,]
summary(subf.bwd)$outmat[29,]
coef(subf.fwd, 29)
coef(subf.bwd, 29)

# Comparing the coefficients, # of predictors considered = 23

# Making a subset of 23 variables
# Getting the 23 columns

train_subs_fb <- subset(tic_data_train, select = c("V4", "V7", "V10", "V16", "V18", "V21", "V35", 
                                             "V36", "V41", "V42", "V43", "V44", "V46", "V47", "V57", "V58", 
                                             "V59", "V78", "V79", "V80", "V82", "V83", "V85", "V86"))
test_subs_fb <- subset(tic_data_test, select = c("V4", "V7", "V10", "V16", "V18", "V21", "V35", 
                                                   "V36", "V41", "V42", "V43", "V44", "V46", "V47", "V57", "V58", 
                                                   "V59", "V78", "V79", "V80", "V82", "V83", "V85"))

lm_mod_fwd <- lm(train_subs_fb$V86 ~., data = train_subs_fb)
train_fwd_mse <- mean(round(lm_mod_fwd$residuals)^2)
pred_vals_test <- predict.lm(lm_mod_fwd, test_subs_fb)
test_fwd_mse <- mean((tic_data_test_target - round(pred_vals_test)) ^ 2)
train_fwd_mse*100 #5.97%
test_fwd_mse*100 #5.975%


# Making a subset of 29 variables
# Getting the 29 columns

train_subs_bw <- subset(tic_data_train, select = c("V4", "V6", "V10", "V17", "V18", "V21", "V22", 
                                                   "V28", "V30", "V35", "V36", "V41", "V42", "V44", "V46", "V47", 
                                                   "V55", "V57", "V58", "V59", "V63", "V76", "V78", "V79", "V80", 
                                                   "V82", "V83", "V84", "V85", "V86"))
test_subs_bw <- subset(tic_data_test, select = c("V4", "V6", "V10", "V17", "V18", "V21", "V22", 
                                                 "V28", "V30", "V35", "V36", "V41", "V42", "V44", "V46", "V47", 
                                                 "V55", "V57", "V58", "V59", "V63", "V76", "V78", "V79", "V80", 
                                                 "V82", "V83", "V84", "V85"))

lm_mod_bwd <- lm(train_subs_bw$V86 ~., data = train_subs_bw)

train_bwd_mse <- mean(round(lm_mod_bwd$residuals)^2)
pred_vals_test <- predict.lm(lm_mod_bwd, test_subs_bw)
test_bwd_mse <- mean((tic_data_test_target - round(pred_vals_test)) ^ 2)
train_bwd_mse*100 #5.97%
test_bwd_mse*100 #5.975%



####################################################################################
##
## RIDGE
####################################################################################
tic_data_train[,-V86]
X <- as.matrix(tic_data_train[,1:85])
dim(X)
Y <- as.matrix(tic_data_train$V86)
dim(Y)
Z <- as.matrix(tic_data_test)
dim(Z)
cv.out <- cv.glmnet(X, Y, alpha = 0)

par(mfrow = c(1,1))
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.mod = glmnet(X, Y, alpha=0)

ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")

ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = Z, 
                       type = "response")

y_hat <- ridge.pred2

y_true <- tic_data_test_target

test_error <- mean((y_hat - y_true)^2)  #test_error
test_error*100 #5.369%

####################################################################################
##
## LASSO
####################################################################################

lasso.mod <- glmnet(X, Y, alpha = 1)
plot(lasso.mod)

cv.out = cv.glmnet(X, Y, alpha = 1)
bestlam = cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s = bestlam, type = "coefficients")

lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = Z, type = "response")

y_hat_lasso <- lasso.pred2
y_true <- tic_data_test_target

test_error_lasso <- mean((y_hat_lasso-y_true)^2) 

test_error_lasso*100 #5.37%
