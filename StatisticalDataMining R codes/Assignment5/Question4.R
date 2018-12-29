#Question 4###########################

#install.packages("e1071")
library(ISLR)
library(e1071)
data(OJ)

set.seed(42)
idx=sample(seq(1:nrow(OJ)),0.7*nrow(OJ))
OJ.train=OJ[idx,]
OJ.test=OJ[-idx,]

#SVM model
costs=0.01:10

#testing for different cost parameter values
train_error_linear=c(cost=character(0),err=character(0))
test_error_linear=c(cost=character(0),err=character(0))

for (cost in costs){
  svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = cost)
  pred<-(predict(svm.linear,OJ.test[,-1]))
  
  te.error=(1/length(OJ.test[,'Purchase']))*
    sum((as.numeric(pred)-as.numeric(OJ.test[,'Purchase']))^2)
  
  tr.error=(1/length(OJ.train[,'Purchase']))*
    sum((as.numeric(predict(svm.linear,OJ.train[,-1]))
         -as.numeric(OJ.train[,'Purchase']))^2)
  
  
  train_error_linear=rbind(train_error_linear,cbind(cost,tr.error))
  test_error_linear=rbind(test_error_linear,cbind(cost,te.error))
}


### part b

#radial

train_error_radial=c(cost=character(0),err=character(0))
test_error_radial=c(cost=character(0),err=character(0))

for (cost in costs){
  svm.rad = svm(Purchase ~ ., kernel = "radial", data = OJ.train, cost = cost)
  pred<-(predict(svm.rad,OJ.test[,-1]))
  
  te.error=(1/length(OJ.test[,'Purchase']))*
    sum((as.numeric(pred)-as.numeric(OJ.test[,'Purchase']))^2)
  
  tr.error=(1/length(OJ.train[,'Purchase']))*
    sum((as.numeric(predict(svm.rad,OJ.train[,-1]))
         -as.numeric(OJ.train[,'Purchase']))^2)
  
  
  train_error_radial=rbind(train_error_radial,cbind(cost,tr.error))
  test_error_radial=rbind(test_error_radial,cbind(cost,te.error))
}


#quad

train_error_quad=c(cost=character(0),err=character(0))
test_error_quad=c(cost=character(0),err=character(0))

for (cost in costs){
  svm.quad = svm(Purchase ~ ., kernel = "poly", degree=2, data = OJ.train, cost = cost)
  pred<-(predict(svm.quad,OJ.test[,-1]))
  
  te.error=(1/length(OJ.test[,'Purchase']))*
    sum((as.numeric(pred)-as.numeric(OJ.test[,'Purchase']))^2)
  
  tr.error=(1/length(OJ.train[,'Purchase']))*
    sum((as.numeric(predict(svm.quad,OJ.train[,-1]))
         -as.numeric(OJ.train[,'Purchase']))^2)
  
  train_error_quad=rbind(train_error_quad,cbind(cost,tr.error))
  test_error_quad=rbind(test_error_quad,cbind(cost,te.error))
}

par(mfrow=c(1,2))
plot(test_error_linear, xlab="m values", ylab="Test error", type = 'l', main = "Test Errors Linear")
points(which.min(test_error_linear), test_error_linear[which.min(test_error_linear)], col = 'red', pch = 20)
min(test_error_linear[,2]) #0.16

plot(train_error_linear, xlab="m values", ylab="Train error", type = 'l', main = "Train Errors Linear")
points(which.min(train_error_linear), train_error_linear[which.min(train_error_linear)], col = 'red', pch = 20)
min(train_error_linear[,2]) #0.16

print(test_error_linear)
print(train_error_linear)

par(mfrow=c(1,2))
plot(test_error_radial, xlab="m values", ylab="Test error", type = 'l', main = "Test Errors Radial")
points(which.min(test_error_radial), test_error_radial[which.min(test_error_radial)], col = 'red', pch = 20)
min(test_error_radial[,2]) #0.14

plot(train_error_radial, xlab="m values", ylab="Train error", type = 'l', main = "Train Errors Radial")
points(which.min(train_error_radial), train_error_radial[which.min(train_error_radial)], col = 'red', pch = 20)
min(train_error_radial[,2]) #0.14

print(test_error_radial)
print(train_error_radial)

par(mfrow=c(1,2))
plot(test_error_quad, xlab="m values", ylab="Test error", type = 'l', main = "Test Errors Quad")
points(which.min(test_error_quad), test_error_quad[which.min(test_error_quad)], col = 'red', pch = 20)
min(test_error_quad[,2]) #0.14

plot(train_error_quad, xlab="m values", ylab="Train error", type = 'l', main = "Train Errors Quad")
points(which.min(train_error_quad), train_error_quad[which.min(train_error_quad)], col = 'red', pch = 20)
min(train_error_quad[,2]) #0.15

print(test_error_quad)
print(train_error_quad)

