#############################################################
# Question 1
#############################################################

rm(list = ls())
#install.packages("bootstrap")
library(ElemStatLearn)
library(bootstrap)
library(leaps)

#loading data

#data("prostate")
#names(prostate)
data_prostate = prostate[,-c(10)]

#preparing the training and test set
set.seed(12345)
indis = sample(1:nrow(data_prostate), 0.67*nrow(data_prostate))
y_tru_train = data_prostate$lpsa[indis]
y_tru_test = data_prostate$lpsa[-indis]
x_tru_train = data_prostate[indis,]
x_tru_test = data_prostate[-indis,]


########################
# Best Subset Selection
########################
fit <- regsubsets(lpsa~., data = x_tru_train, method = "exhaustive", nvmax = 8)
my_summary <- summary(fit)
names(my_summary)
my_summary$cp
my_summary$bic
which.min(my_summary$cp) # 3 variables is best according to min CP
which.min(my_summary$bic) #3 variables is best according to min BIC

select = summary(fit)$outmat
train_error <- c()
test_error <- c()
#For all 8 predictors, we make combinations to suit the best model
for (i in 1:8){
  temp = which(select[i,] == "*")
  temp = temp + 1
  
  temp_train = x_tru_train[, c(9,temp)]
  temp_test = x_tru_test[,c(9,temp)]
  
  temp_fit = lm(lpsa~., data = temp_train)
  
  pred_train = predict(temp_fit, newdata = temp_train)
  pred_test = predict(temp_fit, newdata = temp_test)
  
  test.error = (1/length(y_tru_test))*sum((pred_test - y_tru_test)^2)
  train.error = (1/length(y_tru_train))*sum((pred_train - y_tru_train)^2)
  
  train_error = c(train_error, train.error)
  test_error = c(test_error, test.error)
  
}

plot(test_error, ylab = "Test Error", xlab = "Predictors", main = "Best Subset Selection")

###########
# Bootstrap
###########

beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,y_hat){
  (Y-y_hat)^2
}

# Create X and Y
X = data_prostate[,]
Y = data_prostate[,9]

bootstrap_err = c()
for (i in 1:8){
  temp = which(select[i,] == "*")
  res = bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, 
                 theta.predict = beta.predict, err.meas = sq.error) 
  bootstrap_err = c(bootstrap_err, res[[3]])
  
}


plot(train_error, type = "o", lty = 2, col = "blue" , xlab = "Model size", 
     ylab = "Error", main = "Bootstrap with Best Subset Selection")
lines(test_error, type = "o", lty = 1, col = "red")
lines(bootstrap_err,type = "o", lty = 3, col = "green")
legend("topright", c("Train", "Test", "Bootstrap .632"), lty = c(2,1), 
       col = c("blue", "red", "green"), cex = 0.7)

###########
# 5-fold CV 
###########
set.seed (12345)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

#5 fold
k=5
folds=sample (1:k,nrow(data_prostate),replace =TRUE)

cv.err = matrix(NA,5,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = data_prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, data_prostate[folds == j, ], id = i)
    cv.err[j, i] = mean((data_prostate$lpsa[folds == j] - pred)^2)
  }
}
err5 = sqrt(apply(cv.err, 2, mean))
err5
which.min(err5) # 6 variable model


#############
# 10-fold CV
#############
set.seed (12345)
k=10

folds=sample (1:k,nrow(data_prostate),replace =TRUE)

cv.err = matrix(NA,10,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = data_prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, data_prostate[folds == j, ], id = i)
    cv.err[j, i] = mean((data_prostate$lpsa[folds == j] - pred)^2)
  }
}
err10 = sqrt(apply(cv.err, 2, mean))
err10
which.min(err10) # 3 variable model

