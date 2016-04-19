######################################################
# Author:Dhruba Jyoti Roy Karmakar
#####################################################

#load iris data
data(iris)

#structure of iris data
str(iris)

#table of species
prop.table(table(iris$Species))

#normalisation function
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#normalize the data
iris_n<-as.data.frame(lapply(iris[c(1:4)],normalize))

#separate the data into train and test set
library(caret)
train<-createDataPartition(iris$Species,p=.8,list=F)
iris.traindata<-iris_n[train,]
iris.testdata<-iris_n[-train,]

#create labels for train and test data
train.labels<-iris$Species[train]
test.labels<-iris$Species[-train]

#apply knn algorithm 
library(class)
sqrt(120)
prediction<-knn(iris.traindata,iris.testdata,train.labels,k=13)

#use crossTable to see results
library(gmodels)
CrossTable(test.labels,prediction,chisq = F)










