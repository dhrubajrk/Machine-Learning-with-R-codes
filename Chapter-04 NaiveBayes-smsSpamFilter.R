#Author: Dhruba Jyoti Roy Karmakar
#get,set and verify working directory
getwd()
setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-04 Naive Bayes Classifier-SMS Spam/")
getwd()

#read the data
sms.data<-read.csv("data/sms_spam.csv",stringsAsFactors = F,header = T)

#structure of data
str(sms.data)

#factor the type of sms
sms.data$type<-factor(sms.data$type)

#see the number of spam and ham and proportions
table(sms.data$type)
prop.table(table(sms.data$type))

#import tm package
library(tm)

#create text corpus
sms_corpus<-VCorpus(VectorSource(sms.data$text))
str(sms_corpus)
print(sms_corpus)
inspect(sms_corpus[1:5])

#to see the text stored
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:5],as.character)

library(tm)
?tm_map
#clean the data
sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())

replacePunctuation<-function(x)
{
  gsub("[[:punct:]]+"," ",x)
}
sms_corpus_clean<-tm_map(sms_corpus_clean,replacePunctuation)

sms_corpus_clean

as.character(sms_corpus_clean[[1]])

as.character(sms_corpus[[1]])

library(SnowballC)

sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)
sms_corpus_clean <- tm_map(sms_corpus_clean, PlainTextDocument)

sms_dtm<-DocumentTermMatrix(sms_corpus_clean)

str(sms_dtm)

sms_dtm2<-DocumentTermMatrix(sms_corpus,control = list(c(
  removeNumbers=TRUE,
  removePunctuation=TRUE,
  stripWhitespace=TRUE,
  stopwords=function(x){ removeWords(x,stopwords())},
  stripWhitespace=TRUE,
  stemming=TRUE)))

dim(sms_dtm)
dim(sms_dtm2)


#create train and test data and labels
library(caret)
train<-createDataPartition(sms.data$type,p=0.75,list=FALSE)
train.data<-sms_dtm[train,]
test.data<-sms_dtm[-train,]
train.label<-sms.data$type[train]
test.label<-sms.data$type[-train]

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50)
findFreqTerms(train.data,5)
frequentWords<-findFreqTerms(train.data,5)
train.data_freq<-train.data[,frequentWords]
dim(train.data_freq)
test.data_freq<-test.data[,frequentWords]
dim(test.data_freq)

#convert into categorical variables
convertCount<-function(x){
  x<-ifelse(x>0,"Yes","No")
}
sms_train<-apply(train.data_freq,MARGIN = 2,convertCount)
sms_test<- apply(test.data_freq,MARGIN = 2,convertCount)

install.packages("e1071")
library(e1071)
nbModel<-naiveBayes(sms_train,train.label)
prediction<-predict(nbModel,sms_test)

library(gmodels)
CrossTable(prediction,test.label,prop.chisq = F,dnn = c('Prediction','Actual'))

#improvement using laplacian estimator
modified_nbModel<-naiveBayes(sms_train,train.label,laplace = 1)
modified_prediction<-predict(modified_nbModel,sms_test)
CrossTable(modified_prediction,test.label,prop.chisq = F,dnn = c('Prediction','Actual'))









