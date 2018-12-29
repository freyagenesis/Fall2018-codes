#Question 1###########################

rm(list=ls());

install.packages("ElemStatLearn");
install.packages("class");
install.packages("glmnet");
install.packages("pls");
install.packages("leaps");
install.packages("randomForest")
install.packages("Metrics");

library(ElemStatLearn);
library(class);
library(glmnet);
library(pls);
library(leaps);
library(randomForest);
library(Metrics)

data("spam");
head(spam);

##As the data are not spread correctly we will shuffle the set completely
spam<-spam[sample(nrow(spam)),];

##Now dividing the dataset into Training and test Data:
set.seed(1);
vars=c("A.1","A.2","A.3","A.4","A.5","A.6","A.7","A.8","A.9","A.10","A.11","A.12","A.13","A.14","A.15","A.16","A.17","A.18","A.19","A.20","A.21","A.22","A.23","A.24","A.25","A.26","A.27","A.28","A.29","A.30","A.31","A.32","A.33","A.34","A.35","A.36","A.37","A.38","A.39","A.40","A.41","A.42","A.43","A.44","A.45","A.46","A.47","A.48","A.49","A.50","A.51","A.52","A.53","A.54","A.55","A.56","A.57","spam");
idx=sample(x=nrow(spam), size=0.55*nrow(spam))
train=spam[idx,vars]
test=spam[-idx,vars]

#Random Forest
spam.random=randomForest(train$spam~.,data=train,mtry=57,importance=TRUE)
spam.random

predicSpam = predict(spam.random,newdata=test)

summary(predicSpam)
summary(test$spam)

mValues<-1:57
OOBErr<-numeric(length = length(mValues))
testErr<-numeric(length = length(mValues))

#Now applying the Bagging concept for application of all the predictors at each split.
#Bagging
for (i in seq_along(mValues)) {
  spam_predvals_random=randomForest(train$spam~.,data=train,mtry=i,importance=TRUE)
  spam_predvals_random
  
  predict.randomForest = predict(spam_predvals_random,newdata=test)
  
  OOBErr[i]<-mean(spam_predvals_random$err.rate);
  testErr[i]<-rmse(summary(test$spam),summary(predict.randomForest));
  print(i)
}

##plot between the OOBErr,TestErr and the MValues
par(mfrow=c(1,2))
plot(mValues, OOBErr, ann = FALSE, type = "l", ylab= "OOBErr", xlab= "mValues")
plot(mValues, testErr, ann = FALSE, type = "l", ylab= "testErr", xlab = "mValues")

