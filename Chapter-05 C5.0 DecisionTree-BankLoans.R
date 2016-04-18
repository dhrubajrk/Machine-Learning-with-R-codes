####################################
#Author: Dhruba Jyoti Roy Karmakar
####################################

getwd()
setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-05 C5.0 Decision Tree- Bank Loans")

credit.data<-read.csv("data/credit.csv")
str(credit.data)
head(credit.data)

table(credit.data$checking_balance)
prop.table(table(credit.data$checking_balance))
table(credit.data$savings_balance)
table(credit.data$default)
credit.data$default<-factor(credit.data$default,levels = c(1,2),labels = c("No","Yes"))
table(credit.data$default)

#create training and test set
library(caret)
train<-createDataPartition(credit.data$default,p=0.9,list = FALSE)
train.data<-credit.data[train,]
test.data<-credit.data[-train,]
train.label<-credit.data$default[train]
test.label<-credit.data$default[-train]

library("C50")
credit.model<-C5.0(train.data[-21],train.label)
prediction<-predict(credit.model,test.data[-21])
credit.model
summary(credit.model)

library(gmodels)
CrossTable(prediction,test.label,prop.chisq=FALSE,prop.r = F,prop.c = F,dnn = c("predicted","actual"))

#improve by boosting
credit.model.boost10<-C5.0(train.data[-21],train.label,trials = 10)
credit.model.boost10
prediction.boost10<-predict(credit.model.boost10,test.data[-21])
CrossTable(prediction.boost10,test.label,prop.chisq=FALSE,prop.r = F,prop.c = F,dnn = c("predicted","actual"))

#improve by cost matrix
matrix_dimensions<-list(c("No","Yes"),c("No","Yes"))
names(matrix_dimensions)<-c("Predicted","Actual")
matrix_dimensions
error_cost<-matrix(c(0,1,4,0),nrow=2,dimnames = matrix_dimensions)
error_cost
credit.model.cost<-C5.0(train.data[-21],train.label,costs = error_cost)
prediction.cost<-predict(credit.model.cost,test.data[-21])
CrossTable(prediction.cost,test.label,dnn = c("Predicted","Actual"),prop.r = F,prop.c = F,prop.chisq = F)





