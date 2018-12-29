###Q 5 ##################
#####################################

library(uskewFactors)
#Plotting the diaganosis plots
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

data("banknote")

bank_gen=subset(banknote,banknote$Y==0)
bank_fake=subset(banknote,banknote$Y==1)

genuine_PCA=prcomp(bank_gen[,c(2:7)],center = TRUE)
fake_PCA=prcomp(bank_fake[,c(2:7)],center = TRUE)
combined_PCA=prcomp(banknote[,c(2:7)],center = TRUE)

summary(genuine_PCA) #PC till the 4th one accounts for 92% variance
summary(fake_PCA) #PC till the 3rd one accounts for 90%
summary(combined_PCA) #PC till the 3rd one accounts for 93%

#selecting PCs using variance dip
plot(genuine_PCA,type = "l", main = "Genuine PCA")
plot(fake_PCA, type = "l", main = "Fake PCA")
plot(combined_PCA, type = "l", main = "Combined PCA")

#Analyzing loadings
print(genuine_PCA)
print(fake_PCA)
print(combined_PCA)
