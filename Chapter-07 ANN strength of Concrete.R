setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-07 ANN-Strength of Concrete/")

concrete<-read.csv("data/concrete.csv")

str(concrete)
summary(concrete)

#normalize the data
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
concrete_n<-as.data.frame(lapply(concrete[1:9], normalize))
summary(concrete_n)

#create training and test data
library(caret)
train<-createDataPartition(concrete$strength,p=0.75,list=F)
train.data<-concrete_n[train,]
test.data<-concrete_n[-train,]

#modelling the training data
library(neuralnet)
concrete.model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
                          data = train.data)
plot(concrete.model)

#test the model
cm.result<-compute(concrete.model,test.data[1:8])
cm.result
concrete.result<-cm.result$net.result
cor.test(concrete.result,test.data[,9])

#improve by increasing number of hidden nodes
concrete.model2<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
                           data = train.data,
                           hidden = 5)

cm2.result<-compute(concrete.model2,test.data[1:8])
concrete.result2<-cm2.result$net.result

cor.test(concrete.result2,test.data[,9])
