rm(list=(ls(all=TRUE)))
#~~~~~~~~~~~~~~~
# import all the necessary libraries
#~~~~~~~~~~~~~~~~~
library(car)
library(psych)#describe
library(caTools)#splitting
library(hydroGOF) #rmse
#library(MASS)#stepAIC

#~~~~~~~~~~~~~~~
#Import data in R
#~~~~~~~~~~~~~~~~~
# LGD<-read.csv("C:/Users/Admin/Downloads/R_Module_Day_5.2_Data_Case_Study_Loss_Given_Default.csv")
LGD<-read.csv("D:/DS Training Videos/ML algo script/1.Linear Regression/Bank dataset/Loss_Given_Default.csv")
attach(LGD)
str(LGD)
#~~~~~~~~~~~~~~~~~
#Exploratory data analysis
# perform univariate,bivariate analysis
#remove Account number because its just a reference and its unique
#~~~~~~~~~~~~~~~~~
LGD[,1]<-NULL

#Plots
logLoss<-log(LGD$Losses.in.Thousands)
hist(logLoss)
# LGD$Losses.in.Thousands<-NULL
LGD$logLoss<-logLoss


#Splitting data to train and test
split<-sample.split(LGD$Losses.in.Thousands,0.75)
Train1<-subset(LGD,split==T)
Test1<-subset(LGD,split==F)

write.csv(Train1,"D:/dat6/Train1.csv")
write.csv(Test1, "D:/dat6/Test1.csv")
#?write.csv
#Building model
#Age and years of experience are correlated lets taken either of them in the model to avoid multicollinearity
Model<-lm(Losses.in.Thousands~.-Years.of.Experience-Number.of.Vehicles-logLoss,data = Train1)
summary(Model)