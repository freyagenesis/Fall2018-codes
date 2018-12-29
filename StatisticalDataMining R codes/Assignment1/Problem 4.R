##################################################################
# Problem 4
##################################################################

# install some packages
#install.packages("DAAG")
#install.packages("lattice")
#install.packages("MASS")
#install.packages("ggplot2")


library("DAAG")
library("lattice")
library("MASS")
library("ggplot2")


# install.packages("ElemStatLearn")
library(ElemStatLearn)
Is("package:ElemStatLearn")

require(class)

?zip.test
?zip.train

### Load data
DATASET.train <- as.data.frame(zip.train)
DATASET.test <- as.data.frame(zip.test)
train_sub <- subset(DATASET.train, V1 > 1 & V1 < 4)
test_sub <- subset(DATASET.test, V1 > 1 & V1 < 4)

# Building the linear regression model
mod_train <- lm(V1 ~ ., data = train_sub)

# The values of the 257 coeffients that affect the train data
mod_train$coefficients

# Finding the predicted values of train data 
pred_vals_train<- predict(mod_train, train_sub)
# Considering only 2's and 3's by rounding off the values
pred_vals_train<- round(pred_vals_train)
# Mean square Error
train_error = sum((pred_vals_train - train_sub$V1) ** 2)/ nrow(train_sub)
train_error

# Finding the predicted values of test data 
pred_vals_test<- predict(mod_train, test_sub)
# Considering only 2's and 3's by rounding off the values
pred_vals_test<- round(pred_vals_test)
# Mean square Error
test_error = sum((pred_vals_test - test_sub$V1) ** 2)/ nrow(test_sub)
test_error

# for k=1
k1 = knn(train_sub, test_sub, train_sub$V1, k = 1)
# create empty lists to append error values
train_err <- c()
test_err <- c()
k_val_df <- c()

# Set the k values into a list
k_val <- c(1,3,5,7,9,11,13,15, 30)

# for various values of k = 1,3,5,7,9,11,13,15
for(k in 1:length(k_val)) {
  train_pred <- knn(train_sub, train_sub, train_sub$V1, k = k)
  test_pred <- knn(train_sub, test_sub, train_sub$V1, k = k)
  train_pred <- c(2, 3)[sapply(train_pred, as.numeric)]
  test_pred <- c(2, 3)[sapply(test_pred, as.numeric)]
  k_val_df <- c(k_val_df, k)
  # Calculating error
  train_err <- c(train_err, mean((as.numeric(train_pred) - train_sub$V1) ** 2))
  test_err <- c(test_err, mean((as.numeric(test_pred) - test_sub$V1) ** 2))
}

# KDF is a dataframe containing k values and the training data set errors
kdf <- as.data.frame(k_val_df)
kdf$train_err = train_err
kdf$test_err = test_err

# Plot to discover the trend in train and test data errors for different values of knn with respect to the 
# error value from Linear Regression model

ggplot() +
  geom_line(data = kdf, aes(x = k_val, y = train_err),color = "blue") +
  geom_line(data = kdf, aes(x = k_val, y = test_err), color = "red") + 
  ylab('Error')  +
geom_hline(yintercept = 0.005759539, linetype="dashed", 
           color = "black", size=0.5) +
  geom_text(aes(x = 28, y = 0.009, label = "Train error"),color = "blue") +
  geom_text(aes(x = 28, y = 0.032, label = "Test error"),color = "red") +
  geom_text(aes(x = 27.5, y = 0.005, label = "LM Model error"),color = "black") 
