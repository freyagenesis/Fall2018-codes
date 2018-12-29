#Question 2###########################

#install.packages("neuralnet")

library(neuralnet)
library(ElemStatLearn);
library(class);
library(glmnet);
library(pls);
library(leaps);
library(randomForest);
library(Metrics)

ls("package:neuralnet")

index = sample(1:nrow(spam), nrow(spam)*0.80)
spam_train = spam[index, ]
spam_test = spam[-index, ]

n <- names(spam_train)
f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))

set.seed(100)
# Testing and Cross validating (may take a while to compute)

nn.error.store=c()
for(i in 1:4){
  # Fit the net and calculate training error (point estimate)
  nn <- neuralnet(f,data=data.matrix(spam_train),hidden=i,threshold = 0.1)
  nn_predvals = round(compute(nn,spam_test[,1:57])$net.result[,1])
  spamTestData1=spam_test
  spamTestData1$spam=as.character(spamTestData1$spam)
  spamTestData1$spam[spamTestData1$spam=="email"] <- 1
  spamTestData1$spam[spamTestData1$spam=="spam"] <- 2
  err=mean(spamTestData1$spam != nn_predvals)
  nn.error.store=c(nn.error.store,err)
}

plot(nn.error.store, xlab="Neurons", ylab="test error", type = 'l')
points(which.min(nn.error.store), nn.error.store[which.min(nn.error.store)], col = 'red', pch = 20)

