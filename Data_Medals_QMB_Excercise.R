
data<-read.csv("C:/Users/Srinidhi/Downloads/medals.csv",header=T, na.strings="")
View(data)


hist(GDP, title = "Histogram of GDP", breaks = 15)
BoardaPoints <- data$BordaPoints

log(data$BordaPoints)

data$dv <- log(BordaPoints +1) #transmission
attach(data)


hist(data$dv)
hist(data$dv[data$dv>0])

data$flag <- ifelse(data$BordaPoints>0, 1 , 0)
data1<-subset(data, BoardaPoints>=1)
detach(data)
attach(data1)
data1$dv <- log(data1$BoardaPoints)
mod1 <-lm(dv~ Income+ GDP+ PopnSize , data= data1)
summary(mod1)
mod2 <- lm(dv~ GDP + PopnSize, data = data1)
summary(mod2)
View(data1)
mod3 <- lm(dv ~ data1$GDP)
mod4 <- lm(dv~data1$PopnSize)
mod5 <- lm(dv~ data1$Income + data1$PopnSize)
mod6 <- lm(dv~data1$Income)
library(stargazer)
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, modQ type="html", out= "tl.html")


mod_pop <- lm(dv~data1$PopnSize + data1$PopnSQ, data = data1)
summary(mod_pop)


modQ <- lm(dv~ data1$Income + data1$GDP+ data1$PopnSize+ data1$Income.SQ + data1$GDPSQ+ data1$PopnSQ, data = data1)
summary(modQ)


mod_int1 <- lm(dv~GDP + Income + GDP*Income, data=data1)
summary(mod_int1)

#INTERPRETATION

#Build model for atleast one medal

#Is GDP still tge strongest predictor?
detach(data1)
attach(data)
out1 <- glm(flag ~ GDP*Income*PopnSize, data=data, binomial(link = "logit"))
summary(out1)


#same data saturday
#build a model for medals (maybe not boardapoint)
#build a logistic regression (DV at least 5 medals)
#incestigate interactions in linear models
#interpret the coefficients of logistic regression.
#Present a nice report.

modQ <- lm(dv~ data1$Income + data1$GDP+ data1$PopnSize+ data1$GoldMedals, data1$TotalMedals*data1$PopnSize,
             data1$Income.SQ + data1$GDPSQ+ data1$PopnSQ, data = data1)
summary(modQ)



#Inference: As per the project I was able to understand how to create a deoendant variable using the data and 