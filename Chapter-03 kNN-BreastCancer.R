#Author: Dhruba Jyoti Roy Karmakar
#get,set and verify working directory
getwd()
setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-03 kNN-Breast Cancer")
getwd()

#read the data set
bc.data<-read.csv("data/wisc_bc_data.csv",stringsAsFactors = F)

#names of attributes
names(bc.data)

#structure of data
str(bc.data)


length(which(bc.data$diagnosis=="M"))
length(which(bc.data$diagnosis=="B"))

#table of patients in dataset with malignant and benign cancer
table(bc.data$diagnosis)

?as.factor

#reassign labels M&B as Malignant and Benign
bc.data$diagnosis<-factor(bc.data$diagnosis,levels = c("M","B"),labels = c("Malignant","Beneign"))

#proportion of patients in dataset with malignant and benign cancer
prop.table(table(bc.data$diagnosis))

#proportion of patients in dataset with malignant and benign cancer upto 1 decimal places
round(prop.table(table(bc.data$diagnosis))*100,digits = 1)
summary(bc.data[c("radius_mean","area_mean","smoothness_mean")])

#min-max normalize function
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#normalize the numeric values leaving the diagnosis column
bc.data_n<-as.data.frame(lapply(bc.data[2:31], normalize))

#check the values
summary(bc.data_n)

#import library caret- Classification and Regression Technique
library(caret)

#partition the data into training and test and also create labels(the corresponding diagnosis values)
train<-createDataPartition(bc.data$diagnosis,p=0.8,list = F)
bc.traindata<-bc.data_n[train,]
bc.testdata<-bc.data_n[-train,]
train.labels<-bc.data$diagnosis[train]
table(train.labels)
test.labels<-bc.data$diagnosis[-train]
table(test.labels)
prop.table(table(train.labels))
prop.table(table(test.labels))


#import libraries class for knn and gmodels for CrossTable
library(class)
library(gmodels)

prediction<-knn(bc.traindata,bc.testdata,train.labels,k=13)
length(which(prediction==test.labels))
CrossTable(test.labels,prediction,chisq = F)

#try scale() for z score standardization

bc.data_z<-as.data.frame(scale(bc.data[-1]))
train<-createDataPartition(bc.data$diagnosis,p=0.8,list=FALSE)
bc.traindata<-bc.data_z[train,]
bc.testdata<-bc.data_z[-train,]
train.labels<-bc.data$diagnosis[train]
test.labels<-bc.data$diagnosis[-train]
prediction_z<-knn(bc.traindata,bc.testdata,train.labels,k=13)
length(which(prediction_z==test.labels))
CrossTable(test.labels,prediction_z,chisq = F)

