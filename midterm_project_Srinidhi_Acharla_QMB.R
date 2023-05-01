rm(list=ls())

library(rio)

abd<-read.csv("C:/Users/Srinidhi/Desktop/Abandoned.csv",header=T, na.strings="")
View(abd)
rs<-read.csv("C:/Users/Srinidhi/Desktop/Reservation.csv",header=T, na.strings="")
View(rs)


#Matching the data keys to return the logical vector

match_email=abd$Email[complete.cases(abd$Email)] %in% rs$Email[complete.cases(rs$Email)] 
match_incoming=abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)] 
match_contact=abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)] 
match_incoming_contact= abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)] 
match_contact_incoming= abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)] 




#Creating flags for the match cases

#Email

abd$match_email <- 0
abd$match_email[complete.cases(abd$Email)] <- 1* match_email
sum(match_email)


#Incoming

abd$match_incoming <-0
abd$match_incoming[complete.cases(abd$Incoming_Phone)] <- 1* match_incoming
sum(match_incoming)  

#Contact

abd$match_contact <-0
abd$match_contact[complete.cases(abd$Contact_Phone)] <- 1* match_contact
sum(match_contact)  

#Incoming Contact

abd$match_incoming_contact <-0
abd$match_incoming_contact[complete.cases(abd$Incoming_Phone)] <- 1* match_incoming_contact
sum(match_incoming_contact)  

#Contact Incoming

abd$match_contact_incoming <-0
abd$match_contact_incoming[complete.cases(abd$Contact_Phone)] <- 1* match_contact_incoming
sum(match_contact_incoming)



#outcome variable

abd$Outcome <- 0
abd$Outcome <- 1*(abd$match_email | abd$match_incoming | abd$match_contact | abd$match_incoming_contact | abd$match_contact_incoming)
abd$treat <- 1*(abd$Test_Control=='test')


table(abd$treat,abd$Outcome)

#state wise reporting of variables

state1 <- data.frame(subset(abd,abd$Address == "FL"))
table(state1$Outcome, state1$treat)

state2 <- data.frame(subset(abd,abd$Address == "KS"))
table(state2$Outcome, state2$treat)

state3 <- data.frame(subset(abd,abd$Address == "AZ"))
table(state3$Outcome, state3$treat)

state4 <- data.frame(subset(abd,abd$Address == "UT"))
table(state4$Outcome, state4$treat)

state5 <- data.frame(subset(abd,abd$Address == "NY"))
table(state5$Outcome, state5$treat)

#using subset function to create a data frame

new_df<-subset(abd,abd$Outcome==1)

new_df$D_Email<- ifelse(new_df$Email != 'NA',1,0)
new_df["D_Email"][is.na(new_df["D_Email"])] <- 0

new_df$D_State<- ifelse(new_df$Address != 'NA',1,0)
new_df["D_State"][is.na(new_df["D_State"])] <- 0

new_file<- data.frame(
  Customer_ID=new_df$Caller_ID,
  Test_Variable =new_df$Test_Control,
  Outcome = new_df$Outcome,
  D_Email = new_df$D_Email,
  D_State = new_df$D_State
)

new_file

#writing and exporting file

library(writexl)

write_xlsx(new_file,"C:/Users/Srinidhi/DownloadsMatched.xlsx")

getwd() 

#linear regression

linear_model<-lm(abd$Outcome~abd$Test_Control, data = abd)
summary(linear_model)

#anova test

anova_out=aov(abd$Outcome ~ abd$Test_Control, data = abd)
summary(anova_out)

#multiple linear regression with D_State and D_Email:


out <- lm(new_file$Outcome~new_file$Test_Variable*new_file$D_State+ new_file$Test_Variable*new_file$D_Email)
summary(out)


#multiple linear regression

multiple_lm_out <- lm(new_file$Outcome~new_file$Test_Variable*new_file$D_State)
summary(multiple_lm_out)



install.packages("stargazer")
library(stargazer)

stargazer(out,multiple_lm_out,type = "html",out="midterm.htm")
