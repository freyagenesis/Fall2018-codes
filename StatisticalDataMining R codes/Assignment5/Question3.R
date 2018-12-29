#Question 3##########################################################

library(neuralnet)
library(ElemStatLearn)
library(gam)
rm(list = ls())

data("spam");

index = sample(1:nrow(spam), nrow(spam)*0.80)
train = spam[index, ]
test = spam[-index, ]

n <- names(train)
f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))

nn.error.store=c()
for(i in 1:4){
  nn <- neuralnet(f,data=data.matrix(train),hidden=i,threshold = 0.1)
  nn_pred = round(compute(nn,test[,1:57])$net.result[,1])
  temp_test=test
  temp_test$spam=as.character(temp_test$spam)
  temp_test$spam[temp_test$spam=="email"] <- 1
  temp_test$spam[temp_test$spam=="spam"] <- 2
  err=mean(temp_test$spam != nn_pred)
  nn.error.store=c(nn.error.store,err)
}

which.min(nn.error.store)

formula = "spam~ A.1"
for (colname in colnames(spam)){
  if(colname != "spam" & colname != "A.1" )
  {
    formula = paste(formula, colname , sep = " + ")
  }
}

formula = as.formula(formula)

Spam = spam;

Spam$spam = as.numeric((Spam$spam))-1
Spam = as.data.frame(scale(Spam))
Spam$spam = spam$spam
Spam$spam = as.numeric((spam$spam))-1

set.seed(123)
train = sample(1:nrow(Spam), nrow(Spam)*0.80)
test = -train
trainData = Spam[train, ]
testData = Spam[test, ]

#0.067
train_demo_outlier = trainData
boxplot(train_demo_outlier[,4])$out
title("Boxplot with the outlier")

#introducing outlier (original value = -0.04)
train_demo_outlier[1,4] = 299

nn <- neuralnet(formula , data = train_demo_outlier, hidden = 1, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)
#0.087

#Bringing it closer to the actual value gradually
train_demo_outlier[1,4] = 150

nn <- neuralnet(formula , data = train_demo_outlier, hidden = 1, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)
#0.082

train_demo_outlier[1,4] = 49

nn <- neuralnet(formula , data = train_demo_outlier, hidden = 1, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)
#0.079

train_demo_outlier[1,4] = 3

nn <- neuralnet(formula , data = train_demo_outlier, hidden = 1, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)
#0.083

train_demo_outlier[1,4] = -0.06278457691 

nn <- neuralnet(formula , data = train_demo_outlier, hidden = 1, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)
#0.077

