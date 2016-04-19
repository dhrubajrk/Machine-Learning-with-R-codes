#get and set working directory
getwd()
setwd("G:/R Study/Machine-Learning-with-R-datasets-master/Chapter-06 Simple Linear Regression-Challenger shuttle")

#read data
challenger.data<-read.csv("data/challenger.csv",stringsAsFactors = F)

cor.test(challenger.data$distress_ct,challenger.data$temperature)

b<-cov(challenger.data$temperature,challenger.data$distress_ct)/var(challenger.data$temperature)
a<-mean(challenger.data$distress_ct)-b*mean(challenger.data$temperature)

cov(challenger.data$temperature,challenger.data$distress_ct)/
   (sd(challenger.data$distress_ct)*sd(challenger.data$temperature))

cor.test(challenger.data$temperature,challenger.data$pressure)

#REGRESSION FUNTION
regression<-function(x,y)
{
  x<-as.matrix(x)
  x<-cbind(Intercept=1,x)
  b<-solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b)<-"estimates"
  print(b)
}

#checking with just temperature
regression(challenger.data[3],challenger.data[,2])

#cleaning data
challenger.data$o_ring_ct<-NULL

#regression for the whole data
regression(challenger.data[2:4],challenger.data[,1])

challenger.data[,c(1,3:5)]



