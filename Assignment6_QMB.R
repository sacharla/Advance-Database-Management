#Using the medals.csv create a dummy variable for G20 by checking whether the nation is at the top20 for the total GDP in 2012.



data<-read.csv("C:/Users/Srinidhi/Downloads/medals.csv",header=T, na.strings="")
View(data)
data$G20 <- ifelse(data$GDP),1,0)
head(data)
View(data)
unique(data$G20)
data$G20
sum(data$G20)

