##################################################################
# Problem 1
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

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

## Since the predictive model has to be built on first Period Grades
## The second and third period grades need not be considered
## Other columns such as guardian, traveltime are common across both subjects - Math and Portuguese 
## And hence, the ".y"corresponding variables are ignored

d3 = d3[,c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", 
            "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet", "guardian.x", 
            "traveltime.x", "studytime.x", "failures.x", "schoolsup.x", "famsup.x", 
            "paid.x", "activities.x", "higher.x", "romantic.x", "famrel.x", 
            "freetime.x", "goout.x", "Dalc.x", "Walc.x", "health.x", "absences.x", 
            "G1.x","G1.y")]
print(nrow(d3))
attach(d3)
# Histogram of student count in both schools
par(mfrow = c(1,1))
counts <- table(d3$school)
barplot(counts, xlab="Schools GP and MS", main = "Student count in both schools")

# Strip Plot on the student count of both schools
stripplot(school ~ age, ylab = "Schools", data = d3, main = "Strip Plot on the student count of both schools")

# Grades distribution - subject wise
par(mfrow = c(1,2))
dens <- density(d3$G1.x)
xlim1 <- range(dens$x)
ylim1 <- range(dens$y)
hist(d3$G1.x, breaks = 0 + (0:20)*1, probability = T, xlab = "Grades", xlim = xlim1, ylim = ylim1, main = "Math")
lines(dens)
dens <- density(d3$G1.y)
xlim4 <- range(dens$x)
ylim4 <- range(dens$y)
hist(d3$G1.y, breaks = 0 + (0:20)*1, probability = T, xlab = "Grades", xlim = xlim4, ylim = ylim4, main = "Portuguese")
lines(dens)

# How the age affect the grades - ScatterPlot
# No effect on the grade
par(mfrow = c(1,2))
df_age <- aggregate(G1.x ~ age, d3, mean)
plot(df_age$age, df_age$G1.x, main="Grades vs Age(Math)", xlab="Age", ylab="1st Grades in Math")
df_age <- aggregate(G1.y ~ age, d3, mean)
plot(df_age$age, df_age$G1.y, main="Grades vs Age(Por)", xlab="Age", ylab="1st Grades in Portuguese")

# Removing outlier i.e. the student of ag 22
d3<-d3[!(d3$age>21),]

# How the absences affect the grades - ScatterPlot
aggdf = as.data.frame(df)
par(mfrow = c(1,2))
df <- aggregate(G1.x ~ absences.x, d3, mean)
plot( df$absences.x,df$G1.x, main="Grades vs Absences(Math)", xlab="Absences", ylab="1st Grades in Math")
df <- aggregate(G1.y ~ absences.x, d3, mean)
plot(df$absences.x, df$G1.y, main="Grades vs Absences(Por)", xlab="Absences", ylab="1st Grades in Portuguese")

# Travel vs Study vs Free time
par(mfrow = c(1,3))
travel_dens_m <- density(d3$traveltime.x)
study_dens_m <- density(d3$studytime.x)
free_dens_m <- density(d3$freetime.x)
plot(travel_dens_m, main = "Travel time density")
polygon(travel_dens_m, col="red", border="black")
plot(study_dens_m, main = "Study time density")
polygon(study_dens_m, col="blue", border="black")
plot(free_dens_m, main = "Free time density")
polygon(free_dens_m, col="yellow", border="black")

# Quantity of family relationships vs grades
# Ones with good to excellent levels (4 and 5) have better average grading than the rest.

famrel_avg_m = aggregate(G1.x ~ famrel.x,d3, mean)
famrel_avg_p = aggregate(G1.y ~ famrel.x,d3, mean)

par(mfrow = c(1,2))
barplot(famrel_avg_m$G1.x, names.arg = famrel_avg_m$famrel.x, xlab = "Family Relation", ylab = "Grade average", main="Math") 
barplot(famrel_avg_p$G1.y, names.arg = famrel_avg_p$famrel.x, xlab = "Family Relation", ylab = "Grade average", main = "Portuguese") 


# Quantity of Study time vs grades
# Students across Math and Portuguese subjects, after investing simillar study time perform 
# better in Portuguese
stud_avg_m = aggregate(G1.x ~ studytime.x,d3, mean)
stud_avg_p = aggregate(G1.y ~ studytime.x,d3, mean)

par(mfrow = c(1,2))
barplot(stud_avg_m$G1.x, names.arg = stud_avg_m$studytime.x, xlab = "Study Time", ylab = "Grade average", main="Math") 
barplot(stud_avg_p$G1.y, names.arg = stud_avg_p$studytime.x, xlab = "Study Time", ylab = "Grade average", main = "Portuguese") 

# Quantity of Going out time vs grades
# Students across Math and Portuguese subjects, after investing simillar travel time perform 
# better in Portuguese

goout_avg_m = aggregate(G1.x ~ goout.x,d3, mean)
goout_avg_p = aggregate(G1.y ~ goout.x,d3, mean)

par(mfrow = c(1,2))
barplot(goout_avg_m$G1.x, names.arg = goout_avg_m$goout.x, xlab = "Going out Time", ylab = "Grade average", main="Math") 
barplot(goout_avg_p$G1.y, names.arg = goout_avg_p$goout.x, xlab = "Going Time", ylab = "Grade average", main = "Portuguese") 


