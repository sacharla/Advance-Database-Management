#Srinidhi Acharla

set.seed(6476)

HousePrices

plot(HousePrices)


hist(HousePrices$Price)
hist(HousePrices$SqFt)           #normally distributed
hist(HousePrices$Bedrooms)
hist(HousePrices$Bathrooms)
hist(HousePrices$Offers)
hist(HousePrices$D_Brick)
hist(HousePrices$D_West)
hist(HousePrices$D_East)     
hist(HousePrices$PriceK)          #normally distributed



#as per above I could see that the SqFt and the PriceK are normally distributed. I was able to reach this conclusion by plotting the histogram of all the variables in the table.
cor(t(HousePrices[, c('Price', 'SqFt', 'Offers', 'PriceK')]))  #Variables appear to be perfectly correlated

a<-cor(t(HousePrices[, c('Price', 'SqFt', 'Offers', 'PriceK')]))


install.packages("corrplot")
library(corrplot)



corrplot(a , method = 'color')


plot(a)

#3. 1000 SAMPLES OF SIZE 30

replicate(1000, mean(sample(HousePrices$SqFt, 30 , replace = TRUE)))

Normally_Distributed <- replicate(1000, mean(sample(HousePrices$SqFt, 30, replace = TRUE)))
hist(Normally_Distributed , col = "blue")
k <- hist(Normally_Distributed , col = "blue")
k


#As per the graphical representation and the values of the histogram, it can be concluded that the thousand means are normally distributed for the size of 30

#REPEAT PROCEDURE FOR CALCULATING THE STANDARD DEVIATION OF THE SAMPLING DISTRIBUTION

sd(Normally_Distributed)
sd(HousePrices$SqFt)
hist(HousePrices$SqFt)

#Based on the results above it can be concluded that the standard deviation for the variable SqFt is different for the 1000 samples and to that of the population of the same variable SqFt

