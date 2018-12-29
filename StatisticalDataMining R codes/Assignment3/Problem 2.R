#########################################
# 
# Problem 2
#########################################

rm(list = ls())


diabetesData <- read.delim("DiabetesAndrews36_1.txt", sep = "", header= FALSE)
diabetesData$V1 = NULL
diabetesData$V2 = NULL
diabetesData$V3 = NULL
diabetesData$V4 = NULL
?colnames
colnames(diabetesData) = c('glucose.area', 'insulin.area', 'SSPG', 'relative.weight', 
                           'fasting.plasma.glucose','class.number')
diabetesData$class.number = as.factor(diabetesData$class.number)

attach(diabetesData)
diabetesData[which(class.number==1 ),-6]
cov(diabetesData[which(class.number==1 ),-6])
cov(diabetesData[which(class.number==2 ),-6])
cov(diabetesData[which(class.number==3 ),-6])

pairs(diabetesData[0:5], col = diabetesData$class.number, oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright", fill = unique(diabetesData$class.number), 
       legend = c( levels(diabetesData$class.number)))

# Datapoints of all 5 variables belonging to Class 1 and 2 have
# more correlation compared to Class 3 data points.


set.seed(12345)
train_index = sample(1:nrow(diabetesData), nrow(diabetesData)*.66)
diabetesData_train = diabetesData[train_index, ]
diabetesData_test = diabetesData[-train_index, ]

y_true_train <- as.numeric(diabetesData_train$class) #diabetes =0, normal = 1
y_true_train
y_true_test <- as.numeric(diabetesData_test$class) #diabetes =0, normal = 1
y_true_test

#(a)
###################################################
## Linear Discriminant Analysis (LDA)
###################################################

lda.fit <- lda(class.number~., data = diabetesData_train)
lda.pred.train <- predict(lda.fit, newdata = diabetesData_train)
y_hat_train <- as.numeric(lda.pred.train$class)
lda.pred.test <- predict(lda.fit, newdata = diabetesData_test)
y_hat_test <- as.numeric(lda.pred.test$class)

# Compute the error
lda_train_error <- 
  sum(abs(y_true_train - y_hat_train))/length(y_true_train) 
lda_test_error <- 
  sum(abs(y_true_test - y_hat_test))/length(y_true_test) 
lda_train_error #0.08421053
lda_test_error #0.14

###################################################
## Quadratic Discriminant Analysis (QDA)
###################################################

qda.fit <- qda(class.number~., data = diabetesData_train)
qda.pred.train <- predict(qda.fit, newdata = diabetesData_train)
y_hat_train <- as.numeric(qda.pred.train$class)
qda.pred.test <- predict(qda.fit, newdata = diabetesData_test)
y_hat_test <- as.numeric(qda.pred.test$class)

# Compute the error
qda_train_error <- 
  sum(abs(y_true_train - y_hat_train))/length(y_true_train) 
qda_test_error <- 
  sum(abs(y_true_test - y_hat_test))/length(y_true_test) 
qda_train_error #0.05263158
qda_test_error #0.06


#(c)
test_data <- data.frame(0.98, 122, 544, 186, 184, NA)
colnames(test_data) = c('glucose.area', 'insulin.area', 'SSPG', 
                        'relative.weight', 'fasting.plasma.glucose',
                        'class.number')
#LDA
predicted_lda <- predict(lda.fit, newdata = test_data)
clda_y_hat_train <- as.numeric(predicted_lda$class)
clda_y_hat_train #3
#QDA
predicted_qda <- predict(qda.fit, newdata = test_data)
cqda_y_hat_train <- as.numeric(predicted_lda$class)
cqda_y_hat_train #3

