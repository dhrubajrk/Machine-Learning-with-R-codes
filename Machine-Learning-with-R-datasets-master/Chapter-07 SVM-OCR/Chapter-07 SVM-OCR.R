setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-07 SVM-OCR/")

letter.data<-read.csv("data/letterdata.csv")

library(caret)
train<-createDataPartition(letter.data$letter,p=.8,list=F)
train.data<-letter.data[train,]
test.data<-letter.data[-train,]

library(kernlab)
?ksvm

#linear kernel
letter.model<-ksvm(letter~.,data=train.data,kernel="vanilladot")
letter.model
result<-predict(letter.model,test.data[2:17])
result
table(result,test.data$letter)
agreement<-result==test.data$letter
prop.table(table(agreement))

#gaussian pbf model
letter.model.gaussian<-ksvm(letter~.,data=train.data,kernel="rbfdot")
result.gaussian<-predict(letter.model.gaussian,test.data[2:17])
agreement.gaussian<-result.gaussian==test.data$letter
prop.table(table(agreement.gaussian))




