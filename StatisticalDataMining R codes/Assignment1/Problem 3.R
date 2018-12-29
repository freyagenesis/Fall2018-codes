##################################################################
# Problem 3
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

?Boston

BostonHousDF <- as.data.frame(Boston)

# pairwise scatterplots of the predictors
pairs(BostonHousDF)

# (a)
# crim vs medv - As the price of the houses keep increasing, 
# the crime rate decreases

plot(BostonHousDF$crim, BostonHousDF$medv, xlab = "Crime Rate", ylab = "Median Value of houses")

# lstat vs medv - Majority of lower status of the population 
# live in houses worth lower than $20000

plot(BostonHousDF$lstat, BostonHousDF$medv, xlab = "Lower Status of the population", ylab = "Median Value of houses")

# nox vs dis - As the house to Boston employment centers 
# distance increases i.e. away from the industrial areas, 
# the nitrogen oxides concentration decreases

plot(BostonHousDF$nox, BostonHousDF$dis, xlab = "Nitrogen Oxide Concentraion", ylab = "Mean of distances to employment centres")

# age vs tax - A higher proportion of the owner-occupied units
# built prior to 1940 pay comparatively lesser full value 
# property tax
# The older the building, the lesser the tax paid

plot(BostonHousDF$age, BostonHousDF$tax, xlab = "Units built prior to 1940", ylab = "Property-tax rate per $10,000")

# rm vs medv - As the number of rooms per dwelling increases, 
# the value of the housing increases
# Most owners prefer dwellings with rooms ranging between 5 and 7

plot(BostonHousDF$rm, BostonHousDF$medv, xlab = "Number of rooms per dwelling", ylab = "Median Value of houses")

# (b)
#
BostonHousDF_cor <- as.data.frame(cor(BostonHousDF))


# (c)
#       crim
# crim	1
# zn	-0.20046922
# indus	0.40658341
# chas	-0.05589158
# nox	0.42097171
# rm	-0.2192467
# age	0.35273425
# dis	-0.37967009
# rad	0.62550515
# tax	0.58276431
# ptratio	0.28994558
# black	-0.38506394
# lstat	0.45562148
# medv	-0.38830461

# CRIME RATES
hist(BostonHousDF$crim, xlab = "Crime Rate", ylab = "Freq", data = BostonHousDF, main = "Crime Rates")
# According to the histogram, the crime rates 20 and above are at the higher range
nrow(subset(BostonHousDF, crim > 20))
# Percentage of higher crime rate for 18 suburbs when compared to the total
# 506 suburbs
nrow(subset(BostonHousDF, crim > 20))/nrow(BostonHousDF)*100

# TAX RATES
boxplot(BostonHousDF$tax, xlab = "Freq", ylab = "Tax Rates", data = BostonHousDF, main = "Property-tax rate per $10,000")
# Tax rates at the higher range are above the third quartile - 650
nrow(subset(BostonHousDF, tax > 650))
# Percentage of higher tax rate for 137 suburbs when compared to the total
# 506 suburbs
nrow(subset(BostonHousDF, tax > 650))/nrow(BostonHousDF)*100

# PUPIL-TEACHER RATIO
boxplot(BostonHousDF$ptratio, xlab = "Freq", ylab = "Pupil-Teacher Ratio", data = BostonHousDF, main = "Pupil-Teacher Ratio")
# Pupil-Teacher Ratio at the higher range are above the third quartile - 20
nrow(subset(BostonHousDF, ptratio > 20))
# Percentage of higher Pupil-Teacher Ratio for 201 suburbs when compared to the total 506 suburbs
nrow(subset(BostonHousDF, ptratio > 20))/nrow(BostonHousDF)*100


# (d)
hist(BostonHousDF$rm, xlab = "Number of rooms", ylab = "Freq", data = BostonHousDF, 
     main = "Rooms per dwelling Frequency")
# 64 dwellings have rooms > 7
nrow(subset(BostonHousDF, rm > 7))
# 12.6% of total number of dwellings have more than 7 rooms
nrow(subset(BostonHousDF, rm > 7))/nrow(BostonHousDF)*100
# 13 dwellings have rooms > 8
nrow(subset(BostonHousDF, rm > 8))
# 2.5% of total number of dwellings have more than 8 rooms
nrow(subset(BostonHousDF, rm > 8))/nrow(BostonHousDF)*100

