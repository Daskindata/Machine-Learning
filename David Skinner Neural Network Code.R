install.packages('neuralnet')
library(tidyverse)
library(neuralnet)
library(GGally)
install.packages('dplyr')
library(dplyr)

dataA41 <- read.csv("H:JCU/MA5832/Week 6/Capstone/non-compliance-in-personal-insolvencies.csv", header=TRUE, sep=",", dec=".", na.strings=c("?"))
str(dataA41)
#head(dataA4)
colSums(is.na(dataA4))

#dataA4[,] <-sapply(dataA4[,], as.numeric)
#EAdmissions[,] <-sapply(EAdmissions[,], as.numeric)
#dataA4[,2] <- sapply(dataA4[,2], as.factor)
#str(dataA4)
library(dplyr)
dataA4 <- select(dataA4, -c(1,3,5:7,10,13:23))
str(dataA4)
dataA4[,1:2] <-sapply(dataA4[,1:2], as.numeric)
dataA4[,4:6] <-sapply(dataA4[,4:6], as.numeric)
str(dataA4)
dataA4[,3] <-sapply(dataA4[,3], as.integer)
str(dataA4)
#scaleddata<-scale(dataA4[,1:2,4:6])
normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x))) 
} 
maxmindf <- as.data.frame(lapply(dataA4, normalise))

colnames(maxmindf)[1] <- "insolvencyyear"
colnames(maxmindf)[2] <- "sacodedebtor"
colnames(maxmindf)[3] <- "genderdebtor"
colnames(maxmindf)[4] <- "familystatus"
colnames(maxmindf)[5] <- "occupationname"
colnames(maxmindf)[6] <- "insolvencycause"
#colnames(maxmindf)[7] <- "businessrelated"

# Data Partition set the seed for reproduce and split in 60/40 ratio
set.seed(222)
#ind <- sample(2, nrow(dataA4), replace = TRUE, prob = c(0.6 0.4))
training <- maxmindf[1:142600,]
testing <- maxmindf[142601:356500,]

#Neural Network Construct several neural networks using different numbers of hidden layers.
install.packages('neuralnet')
library(neuralnet)

nn <- neuralnet(genderdebtor ~  insolvencyyear + sacodedebtor + familystatus + occupationname +
                  insolvencycause ,data = training, hidden = 0, linear.output=FALSE, threshold=0.1)

nn$result.matrix
plot(nn)

nn1 <- neuralnet(genderdebtor ~  insolvencyyear + sacodedebtor +  
   familystatus + occupationname +insolvencycause ,data = training, stepmax = 100000, hidden = 1, linear.output=FALSE, threshold=0.1)
nn1$result.matrix
plot(nn1)

nn2 <- neuralnet(genderdebtor ~ insolvencyyear + sacodedebtor +  
 familystatus + occupationname +insolvencycause ,data = training, stepmax = 100000, hidden = , linear.output=FALSE, threshold=0.1)
nn2$result.matrix
plot(nn2)

nn3 <- neuralnet(genderdebtor ~  insolvencyyear + sacodedebtor +  
                   familystatus + occupationname +insolvencycause , data = training, stepmax = 100000, hidden = c(3,2), linear.output=FALSE, threshold=0.5)
nn3$result.matrix
plot(nn3)


nn4 <- neuralnet(genderdebtor ~  insolvencyyear + sacodedebtor +  
                   familystatus + occupationname +insolvencycause , data = training, stepmax = 100000, hidden = c(3,2), act.fct = 'tanh', linear.output=FALSE, threshold=0.5)
nn4$result.matrix
plot(nn4)

sigmoid = function(x) {
  1 / (1 + exp(-x))
}
set.seed(123)
nn5 <- neuralnet(genderdebtor ~  insolvencyyear + sacodedebtor +  
                   familystatus + occupationname +insolvencycause , data = training, stepmax = 100000, hidden = 1, act.fct = 'sigmoid', err.fct="ce",linear.output=FALSE, threshold=0.5)
nn5$result.matrix
plot(nn5)

#Test the resulting output Predict the network by applying your model with hidden layers  on the test data.
test_temp <- subset(testing, select = c("insolvencyyear" , "sacodedebtor" ,  
                  "familystatus" , "occupationname" ,"insolvencycause"))
nn.results <- compute(nn, test_temp)
results <- data.frame(actual = testing$genderdebtor, prediction = nn.results$net.result)
#Test accuracy using a confusion matrix nn.
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,pred1)

#Test accuracy using a confusion matrix nn1.
nn1.results <- compute(nn1, test_temp)
results1 <- data.frame(actual = testing$genderdebtor, prediction = nn1.results$net.result)
roundedresults1<-sapply(results1,round,digits=0)
roundedresultsdf1=data.frame(roundedresults1)
attach(roundedresultsdf1)
table(actual,prediction)


#Test accuracy using a confusion matrix nn2.
results2 <- data.frame(actual = testing$genderdebtor, prediction = nn2.results$net.result)
roundedresults2<-sapply(results2,round,digits=0)
roundedresultsdf2=data.frame(roundedresults2)
attach(roundedresultsdf2)
table(actual,prediction)

str(dataA4)
#Repeat Steps 4 and 5 using the c(7,3) hidden layer neural net and compare the accuracies.

htemp_test7 <- subset(testing , select = c("insolvencyyear" , "sacodedebtor" , 
                  "familystatus" , "occupationname" ,"insolvencycause"), hidden=c(7,3))
nn7.hresults <- compute(nn, htemp_test7)
hresults7 <- data.frame(actual = testing$genderdebtor, prediction = nn7.hresults$net.result)
hroundedresults7 <-sapply(hresults7,round,digits=0)
roundedresultsdf7=data.frame(hroundedresults7)
attach(roundedresultsdf7)
table(actual,prediction)

#############################################################################

