#############################################################
# Question 3
#############################################################

rm(list = ls())
library(rpart)
library(gbm)
library(randomForest)
library(geneplotter)
library(ISLR)
library(leaps)

#load the data
#data from https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)
data_BC <- read.csv("/Users/freyadmello/Desktop/breast-cancer-wisconsin.csv", sep = ",", 
                    header = FALSE)
data_BC <- data.frame(data_BC[, -c((1))])

colnames(data_BC) <- c("ClumpThick","UniCellSize", "UniCellShape",
                       "MargAdh", "SingleEpi", "BareNuclei", "BlandChroma", "NormNuclei", 
                       "Mitosis", "ClassBM")

data_BC$ClassBM
#Set the seed and put aside the test set
set.seed(1234)
test_indis <- sample(1:nrow(data_BC), .15*nrow(data_BC))
test <- data_BC[test_indis, ]
train <- data_BC[-test_indis, ]

y_true <- (as.numeric(test$ClassBM) / 2) #0-No 1-Yes

train$ClassBM <- as.numeric(train$ClassBM)
test$ClassBM <- as.numeric(test$ClassBM)

#########################################
# Bagging
#########################################

bag.fit <- randomForest(ClassBM~., data = train, n.tree = 10000, mtry = 10)

varImpPlot(bag.fit, main = "Bagging Variable Importance", cex = 0.7)

importance(bag.fit)

y_hat <- predict(bag.fit, newdata = test, type = "response")

y_hat <- round(as.numeric(y_hat) / 2 )

misclassbag <- sum(abs(y_true-y_hat))/length(y_hat)

misclassbag # 0.03846154

#########################################
# Boosting
#########################################

boost.train <- train
boost.train$ClassBM <- as.numeric(train$ClassBM) / 2
boost.train$ClassBM <- boost.train$ClassBM - 1
boost.test <- test
boost.test$ClassBM <- as.numeric(test$ClassBM) / 2
boost.test$ClassBM <- boost.test$ClassBM - 1

boost.fit <- gbm(ClassBM~., data = boost.train, n.trees = 1000, 
                 shrinkage = .1, interaction.depth = 3, distribution = "adaboost")
boost.fit2 <- gbm(ClassBM~., data = boost.train, n.trees = 1000, 
                  shrinkage = .6, interaction.depth = 3, distribution = "adaboost")

# shrinkage = 0.1
y_hat <- predict(boost.fit, newdata = boost.test, n.trees = 1000, type = "response")

y_true <- (as.numeric(test$ClassBM) / 2) - 1

misclassboost1 <- sum(abs(y_true-y_hat))/length(y_hat)

misclassboost1 # 0.02980456

# shrinkage = 0.6
y_hat <- predict(boost.fit2, newdata = boost.test, n.trees = 1000, type = "response")

misclassboost2 <- sum(abs(y_true-y_hat))/length(y_hat)

misclassboost2 # 0.03057541

#with other shrinkage parameters
shrink <- c(0.1, 0.4, 0.6, 0.8)
max_iter <- 1000

store_error <- c()

for (i in 1:length(shrink))
{
  temp <- c()
  boost.fit <- gbm(ClassBM~., data = boost.train, n.trees = max_iter, shrinkage = 
                     shrink[i], interaction.depth =  3, distribution = "adaboost")
  for (j in 1:max_iter)
  {
    y_hat <- predict(boost.fit, newdata = boost.test, n.trees = j, type = "response")
    misclass_boost <- sum(abs(y_true-y_hat))/length(y_hat)
    temp <- c(temp, misclass_boost)
    
  }
  store_error <- cbind(store_error, temp) 
}

colnames(store_error) <- paste("Shrinkage", shrink, sep = ":")

plot(store_error[,1], main = "Error Profiles", ylab = "error", xlab = "boosting iterations",
     ylim = c(0.01, 0.05))

lines(store_error[,2], col = "red")
lines(store_error[,3], col = "pink")
lines(store_error[,4], col = "blue")

store_error[1000,] # around same values

######################################################################
# Random Forest
######################################################################

rf.fit <- randomForest(ClassBM~., data=train, n.tree = 10000)

varImpPlot(rf.fit, main = "Random forest Variable Importance")

importance(rf.fit)

y_hat <- predict(rf.fit, newdata = test, type = "response")

y_hat <- round(as.numeric(y_hat) / 2 ) - 1

misclassrf <- sum(abs(y_true-y_hat))/length(y_hat)

misclassrf #0.02884615

##############################################
#Logistic Regression
##############################################

train.error.store <- c()
test.error.store <- c()

y_true_test <- (as.numeric(test$ClassBM) / 2) - 1
test$ClassBM <- y_true_test
y_true_train <- (as.numeric(train$ClassBM) / 2) - 1
train$ClassBM <- y_true_train

glm.fit <- glm(ClassBM ~., data = train, family = "binomial")
summary(glm.fit)
names(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = train, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_hat_test <- round(glm.probs.test)

misclassLRtest <- sum(abs(y_true_test-y_hat_test))/length(y_hat_test)
misclassLRtest # 0.03846154
misclassLRtrain <- sum(abs(y_true_train-y_hat_train))/length(y_hat_train)
misclassLRtrain # 0.02016807


misclassbag # 0.03846154

misclassboost2 #  0.03057541

misclassrf # 0.02884615

misclassLRtest # 0.03846154

# misclassbag<misclassLRtest<misclassboost2<misclassrf
