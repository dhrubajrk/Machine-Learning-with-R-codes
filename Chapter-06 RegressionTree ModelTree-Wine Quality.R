#set working directory
setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-06 Regression Tree -White Wine")

#read data
wines<-read.csv("data/whitewines.csv")

table(wines$quality)

str(wines)
library(caret)
train<-createDataPartition(wines$quality,p=0.75,list = F)
train.data<-wines[train,]
test.data<-wines[-train,]

#modelling
library(rpart)
wines.model<-rpart(quality~.,data = train.data)
library(rattle)
fancyRpartPlot(wines.model)
wines.model

#prediction
prediction<-predict(wines.model,test.data)
summary(prediction)
summary(test.data$quality)

#testing  prediction by the corelation
cor.test(prediction,test.data$quality)

#mean absolute error function
MAE<-function(x,y){
  return(mean(abs(x-y)))
}

MAE(prediction,test.data$quality)
MAE(mean(test.data$quality),test.data$quality)

#building a model tree M5P algorithm
library(RWeka)
mt<-M5P(quality~.,data = train.data)
summary(mt)
mt.prediction<-predict(mt,test.data)

cor.test(mt.prediction,test.data$quality)
MAE(mt.prediction,test.data$quality)
MAE(mean(test.data$quality),test.data$quality)

mt