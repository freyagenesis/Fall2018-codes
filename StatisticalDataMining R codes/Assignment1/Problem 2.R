##################################################################
# Problem 2
##################################################################

# install some packages
#install.packages("DAAG")
#install.packages("lattice")
#install.packages("MASS")
#install.packages("plyr")

library("DAAG")
library("lattice")
library("MASS")
library("plyr")


## merging the two datasets

d1=read.table("/Users/freyadmello/Desktop/codes/student/student-mat.csv",sep=";",header=TRUE)
d2=read.table("/Users/freyadmello/Desktop/codes/student/student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob",
                    "Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

## Since the predictive model has to be built on first Period Grades
## The second and third period grades need not be considered
d3 = d3[,c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", 
           "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet", "guardian.x", 
           "traveltime.x", "studytime.x", "failures.x", "schoolsup.x", "famsup.x", 
           "paid.x", "activities.x", "higher.x", "romantic.x", "famrel.x", 
           "freetime.x", "goout.x", "Dalc.x", "Walc.x", "health.x", "absences.x", 
           "G1.x","G1.y")]

#Using lm() to build linear regression model with respect to frst period grades
math_mod <- lm(d3$G1.x ~ . -G1.y, data = d3)
por_mod <- lm(d3$G1.y ~ . -G1.x, data = d3)
var(d3)
# (a)
# lesser the PR(>|t|) more significant the variables are to Grades
# Variables are failures, school support, study time, family support, sex, go out, Fjob-services,
# higher education, F job other

# Adjusted R-squared:  0.287 
summary(math_mod)
# Adjusted R-squared:  0.2473
summary(por_mod)

# (b)
# Suggestions to improve first period grades
coef(math_mod)
plot(d3$failures.x, d3$G1.x,  xlab = "Failures", ylab = "Grades")
plot(d3$schoolsup.x, d3$G1.x, ylab = "Grades", xlab = "School support")
plot(d3$studytime.x, d3$G1.x, ylab = "Grades", xlab = "Study Time")
plot(d3$famsup.x, d3$G1.x, ylab = "Grades", xlab = "Family Support")
plot(d3$sex, d3$G1.x, ylab = "Grades", xlab = "Sex")
plot(d3$goout.x, d3$G1.x, ylab = "Grades", xlab = "Going out")
plot(d3$Fjob, d3$G1.x, ylab = "Grades", xlab = "Father Job")
plot(d3$higher.x, d3$G1.x, ylab = "Grades", xlab = "Higher Education")

coef(por_mod)
plot(d3$schoolsup.x, d3$G1.y, ylab = "Grades", xlab = "School support")
plot(d3$sex, d3$G1.y, ylab = "Grades", xlab = "Sex")
plot(d3$failures.x, d3$G1.y,  xlab = "Failures", ylab = "Grades")
plot(d3$school, d3$G1.y, ylab = "Grades", xlab = "School")
plot(d3$famsize, d3$G1.y, ylab = "Grades", xlab = "Family Size")
plot(d3$studytime.x, d3$G1.y, ylab = "Grades", xlab = "Study Time")


# (c)
# Interactions
# Adjusted R-squared:  0.2102 
inter_m <- lm(d3$G1.x ~ failures.x + schoolsup.x + famsup.x * studytime.x * goout.x, data = d3)
summary(inter_m)
# Adjusted R-squared:  0.2274 
inter_m <- lm(d3$G1.x ~ schoolsup.x * failures.x * famsup.x * studytime.x * goout.x, data = d3)
summary(inter_m)

# Adjusted R-squared:  0.2556
inter_p <- lm(d3$G1.y ~ failures.x + schoolsup.x + health.x * famsize * school * sex , data = d3)
summary(inter_p)
# Adjusted R-squared:  0.2541
inter_p <- lm(d3$G1.y ~ schoolsup.x * failures.x * health.x * famsize * school * sex, data = d3)
summary(inter_p)

