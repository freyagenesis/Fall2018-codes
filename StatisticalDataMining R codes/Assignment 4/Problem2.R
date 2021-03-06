#############################################################
# Question 2
#############################################################

rm(list = ls())
library(rpart)
library(gbm)
library(randomForest)
library(geneplotter)
library(ISLR)

data_wine <- read.csv("/Users/freyadmello/Downloads/Wine.csv", sep = ",", header = FALSE)
data_wine <- data.frame(data_wine[, -c(2, 7, 8)])
colnames(data_wine) <- c("WineType", "MalicAcid","Ash", "AlcaAsh", "Mg", "Phenols", "Proa", 
                         "Colour", "Hue", "OD", "Proline")

#Set the seed and put aside the test set
set.seed(12345)
test_indis <- sample(1:nrow(data_wine), .30*nrow(data_wine))
test <- data_wine[test_indis, ]
train <- data_wine[-test_indis, ]

y_true_train <- as.factor(train$WineType) #1-Barolo 2-Grignolino 3-Barbera
y_true_test <- as.factor(test$WineType) #1-Barolo 2-Grignolino 3-Barbera

#####################################
#TREES
#####################################
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit <- rpart(WineType~., data = train, method = "class", control = model.control)

plot(fit, uniform=TRUE,margin=0.2,  main = "Train data before pruning")
text(fit, use.n = TRUE, cex = 0.8)

#prune the tree back
min_cp = which.min(fit$cptable[, 4])

plot(fit$cptable[, 4], ylab = "CP Values", main = "CP Table Values")

Wine_Model <- prune(fit, cp = fit$cptable[min_cp, 1])

plot(Wine_Model, uniform=TRUE,margin=0.2, main = "Train data after pruning")
text(Wine_Model, use.n = TRUE, all = TRUE,  cex = 0.6)

# Node count in test data
Wine_Model_Nodes <- Wine_Model
Wine_Model_Nodes$frame$yval = as.numeric(rownames(Wine_Model_Nodes$frame))
Test_Nodes <- predict(Wine_Model_Nodes, test, type="vector")
Test_NodesDF = data.frame(rowNum = c(1:length(Test_Nodes)),Test_Nodes)
Test_NodesDF_agg = data.frame(aggregate(rowNum~Test_Nodes,data = Test_NodesDF,FUN = length))


# Node count in train data
Train_Nodes <- predict(Wine_Model_Nodes, train, type="vector")
Train_NodesDF = data.frame(rowNum = c(1:length(Train_Nodes)),Train_Nodes)
Train_NodesDF_agg = data.frame(aggregate(rowNum~Train_Nodes,data = Train_NodesDF,FUN = length))

Wine_Pred_test <- predict(Wine_Model, newdata = test, type = 'class')
Wine_Pred_train <- predict(Wine_Model, newdata = train, type='class')

#Misclassification error

mismatch <- as.numeric(0)
for (i in 1:length(Wine_Pred_test))
{
  if(as.character(y_true_train[i]) != as.character(Wine_Pred_train[i]))
  {
    mismatch <- mismatch + 1
  }
}

misclasstrain <- mismatch/length(Wine_Pred_train)



mismatch <- as.numeric(0)
for (i in 1:length(Wine_Pred_test))
{
  if(as.character(y_true_test[i]) != as.character(Wine_Pred_test[i]))
  {
    mismatch <- mismatch + 1
  }
}

misclasstest <- mismatch/length(Wine_Pred_test)

misclasstrain # 0.048
misclasstest # 0.01886792

Test_NodesDF_agg
Train_NodesDF_agg
