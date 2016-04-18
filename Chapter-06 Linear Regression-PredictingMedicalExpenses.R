#get and set working directory
getwd()
setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-06 Linear Regression-PredictingMedical Expenses")

#READ DATA
insurance<-read.csv("data/insurance.csv")

names(insurance)

table(insurance$smoker)

cor.test(insurance$age,insurance$charges)

cor(insurance[c("age","bmi","children","charges")])

pairs(insurance[c("age","bmi","children","charges")])

library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

#fitting a linear model
insurance.model<-lm(charges~.,data = insurance)

insurance.model

summary(insurance.model)

insurance$sqage<-insurance$age^2

insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

insurance.model2<-lm(charges~.,data = insurance)
summary(insurance.model2)

insurance.model3<-lm(charges~age+sex+bmi+children+bmi30*smoker+region+sqage,data = insurance)
summary(insurance.model3)
