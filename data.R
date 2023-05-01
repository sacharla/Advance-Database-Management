rm(list=ls())


#SRINIDHI ACHARLA


library(rio)
library(dplyr)
data <- read.csv('C:/Users/Srinidhi/Downloads/medals.csv') 
#Performing sorting on the dataset based on GDP column.
sort(data$GDP, decreasing = TRUE, na.last=TRUE)

#Creating G20 variable using GDP column.
data$G20 <- ifelse(df.medals$GDP == head(df.medals$GDP,n=20),1,0)

attach(data)

summary(GoldMedals)
sum(GoldMedals==0)




View(data)


data$logGM <- log(GoldMedals)
#creating subset
data1 <- subset(data, GoldMedals>0)

View(data1)

detach(data)
attach(data1)

#Investigate models to estimate the number of gold medals won
m1 <- lm(GM ~ G20,data=data1)
summary(m1)

#As per the above results the coefficient intercept says that for 

m2 <- lm(GM ~ GDP+Income+PopnSize,data = data1)
summary(m2)
m3 <- lm(GM ~ Income,data = data1)
summary(m3)
m4 <- lm(GM ~ PopnSize,data = data1)
summary(m4)
m5 <- lm(GM ~ GDP,data=data1)
summary(m5)
m6 <- lm(GM ~ Income+PopnSize,data = data1)
summary(m6)
m7 <- lm(GM ~ GDP+PopnSize,data = data1)
summary(m7)
m8 <- lm(GM ~ GDP+Income,data=data1)
summary(m8)
library(stargazer)
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,type = "html", out = "GM_Linear_Regression_Output.htm")


##Performing linear regression on the dataset:
mod1 <- lm(G20 ~ GDP ,data=data1)
summary(mod1)
mod2 <- lm(G20 ~ PopnSize ,data=data1)
summary(mod2)
mod3 <- lm(G20 ~ Income ,data=data1)
summary(mod3)
mod4 <- lm(G20 ~ GDP + PopnSize  ,data=data1)
summary(mod4)
mod5 <- lm(G20 ~ GDP + Income ,data=data1)
summary(mod5)
mod6 <- lm(G20 ~ Income + PopnSize ,data=data1)
summary(mod6)


library(stargazer)
stargazer(mod1,mod2,mod3,mod4,mod5,mod6,type = "html", out = "Linear_Regression_Output.htm")
#Including interaction between indepemdemt variables:
mod7 <- lm(G20 ~ GDP*Income+PopnSize,data=data1)
summary(mod7)
mod8 <- lm(G20 ~ GDP*Income*PopnSize,data = data1)
summary(mod8)
mod9 <- lm(G20 ~ GDP*PopnSize+Income,data=data1)
summary(mod9)
stargazer(mod7,mod8,mod9,type = "html", out = "Linear_Regression_Interactive_Variables_Output.htm")


#model to predict the probability of being at the top10 by total medal count using a logistic regression model.
sort(data$TotalMedals,decreasing = TRUE,na.last = TRUE)
#Creating the Top10 based on TotalMedals variable:
data$Top10 <- ifelse(data$TotalMedals == head(data$TotalMedals,n=10),1,0) 
#Performing Logistic regression:
glmout <- glm(Top10 ~ G20+GDP+PopnSize+Income,data=data,family=binomial(link="logit"))
summary(glmout)
glmout1 <- glm(Top10 ~ GDP+PopnSize+Income,data=data,family=binomial(link="logit"))
summary(glmout1)
glmout2 <- glm(Top10 ~ GDP+PopnSize,data=data,family=binomial(link="logit"))
summary(glmout2)
glmout3 <- glm(Top10 ~ PopnSize+Income,data=data,family=binomial(link="logit"))
summary(glmout3)
glmout4 <- glm(Top10 ~ GDP,data=data,family=binomial(link="logit"))
summary(glmout4)
glmout5 <- glm(Top10 ~ PopnSize,data=data,family=binomial(link="logit"))
summary(glmout5)
glmout6 <- glm(Top10 ~ Income,data=data,family=binomial(link="logit"))
summary(glmout6)
glmout7 <- glm(Top10 ~ G20,data=data,family=binomial(link="logit"))
summary(glmout7)
stargazer(glmout1,glmout2,glmout3,glmout4,glmout5,glmout6,glmout7,type = "html", out = "Logistic_Regression_Output.htm")
##Creating the confusion matrix:
actual<- data$Top10
prediction<-round(predict(glmout,type="response"))
table(prediction,actual)
