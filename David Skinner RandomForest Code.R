#Load packages and libraries
library(MASS)
library(e1071)
library(randomForest)
library(ISLR)
library(tree)
install.packages('rpart')
library(rpart)
install.packages("caret")
library(caret)
install.packages('gbm')
library(gbm)
install.packages('dplyr')
library(dplyr)

dataA4 <- read.csv("H:/JCU/MA5832/Week 6/Capstone/non-compliance-in-personal-insolvencies.csv", header=TRUE, sep=",", dec=".", na.strings=c("?"))
str(dataA4)
#head(dataA4)

#dataA4[,] <-sapply(dataA4[,], as.numeric)
#EAdmissions[,] <-sapply(EAdmissions[,], as.numeric)
#dataA4[,2] <- sapply(dataA4[,2], as.factor)
#str(dataA4)

library(dplyr)
dataA4 <- select(dataA4, -c(1,3,5:7,10,13:23))
#str(dataA4)
dataA4[,1:2] <-sapply(dataA4[,1:2], as.numeric)
#dataA4[,3] <- sapply(dataA4[,3], as.factor)
str(dataA4)

colnames(dataA4)[1] <- "solvencyyear"
colnames(dataA4)[2] <- "sacodedebtor"
colnames(dataA4)[3] <- "genderdebtor"
colnames(dataA4)[4] <- "familystatus"
colnames(dataA4)[5] <- "occupationname"
colnames(dataA4)[6] <- "insolvencycause"

library(caret)
library(ggplot2)
set.seed(1)
inTrain <- createDataPartition(y = dataA4$genderdebtor, p = .6,list = FALSE)
training <- dataA4[ inTrain, ]
testing  <- dataA4[-inTrain, ]

##Random Forest 
library(MASS)
library(randomForest)
library(caret)
set.seed(1234)# can be any number 

data4.RFtree<-randomForest(genderdebtor~., data = dataA4, importance=TRUE, na.action = na.pass)
data4.RFtree
summary(data4.RFtree)
print(data4.RFtree)
res<- (dataA4$genderdebtor - data4.RFtree$predicted)
plot(dataA4$genderdebtor, res)
data4.RFtree$importance
confusionMatrix(table(data4.RFtree, data4.RFtree$genderdebtor))

varImpPlot(data4.RFtree)

# Increasing mtry & ntree values

Rftree <-randomForest(genderdebtor~., data = training, importance=TRUE, na.action = na.pass)
Rftree

Rftree2 <-randomForest(genderdebtor~., data=training, mtry=3, ntree = 700, importance=TRUE, na.action = na.pass)
Rftree2

Rftree3 <-randomForest(genderdebtor~., data=testing, mtry=5, ntree = 900,importance=TRUE, na.action = na.pass)
Rftree3

install.packages('devtools')
library(devtools)
install_bitbucket("mkuhn/parallelRandomForest", ref="parallelRandomForest")
Rftree1 <-randomForest(genderdebtor~., data = testing, importance=TRUE, na.action = na.pass)
Rftree1

help(parallelRandomForest)

preddata4 <- predict(data4.RFtree, dataA4, type = "class")
# Checking classification accuracy of full model
mean(preddata4 == dataA4$genderdebtor)
table(preddata4, dataA4$genderdebtor)
confusionMatrix(table(preddata4, dataA4$genderdebtor))
classification_error <- 1- sum(preddata4 == dataA4$genderdebtor)/length(preddata4)
classification_error

# Predicting on train set
predTrain <- predict(Rftree, training, type = "class")
# Checking classification accuracy
table(predTrain, training$genderdebtor) 
confusionMatrix(table(predTrain, training$genderdebtor))
classification_error <- 1- sum(predTrain == training$genderdebtor)/length(predTrain)
classification_error


# Predicting on test set
predValid <- predict(Rftree1, testing, type = "class")
# Checking classification accuracy
mean(predValid == testing$genderdebtor)                    
table(predValid,testing$genderdebtor)
confusionMatrix(table(predValid, testing$genderdebtor))
classification_error <- 1- sum(predValid == testing$genderdebtor)/length(predValid)
classification_error
varImpPlot(Rftree1)

###############################

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  LS.tree3 <- randomForest(LOS_cat~ ., data = LSEA.Train, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(LS.tree3, LSEA.Test, type = "class")
  a[i-2] = mean(predValid == LSEA.Test$LOS_cat)
}
a

plot(3:8,a)

bag.Rtree<-randomForest(LOS_cat~., data=LSEA.Train, mtry=3, ntree = 25, importance=TRUE, na.action = na.pass)
#setting mtry = number of predictors to get bagging results
print(bag.Rtree)
res<-EAdmissions$LOS_cat-bag.Rtree$predicted
plot(EAdmissions$LOS_cat, res)
bag.Rtree$importance

varImpPlot(bag.Rtree)

par(mfrow=c(2,1), cex=0.7)
barplot(sort(bag.Rtree$importance[,1], decreasing = TRUE))
barplot(sort(bag.Rtree$importance[,2], decreasing = TRUE))

#Boosting
library (gbm)
set.seed (1)
boost.EAdmissions =gbm(LOS_cat~.,data=LSEA.Train ,distribution = "gaussian"
                       ,n.trees =500 , interaction.depth =4)
boost.EAdmissions
summary(boost.EAdmissions)

yhat.boost = predict(boost.EAdmissions ,data = LSEA.Train,n.trees =5000)
mean(yhat.boost == LSEA.Test$LOS_cat)


library (randomForest)
library (MASS)
library(tree)
set.seed (1)
bag.EAdmissions = randomForest(LOS_cat~.,data = LSEA.Test ,
                               mtry=3, importance =TRUE, na.rm = TRUE)
bag.EAdmissions
yhat.bag = predict(bag.EAdmissions , newdata = LSEA.Test, type = "response",  norm.votes=TRUE, predict.all=FALSE, 
                   proximity=FALSE, nodes=FALSE)
#plot(yhat.bag, LSEA.Test)
#abline(0,1)
str(LSEA.Test)
is.double(bag.EAdmissions)
#LSEA.Test$LOS_cat <- as.numeric(as.character(LSEA.Test[,6]))
yhat.bag <- as.numeric(yhat.bag)
class(yhat.bag)
class(LSEA.Test)
w <- square
mean(yhat.bag == LSEA.Test$LOS_cat)^2


bag.EAdmissions = randomForest(LOS_cat~.,data=LS.EAdmissions ,
                               mtry=3, ntree = 25, importance =TRUE, na.rm = TRUE)

set.seed (1)
rf.EAdmissions = randomForest(LOS_cat~.,data = LSEA.Train,
                              mtry=3, importance =TRUE)

yhat.rf = predict (rf.EAdmissions , newdata = LSEA.Test)
mean(yhat.rf - LSEA.Test$LOS_cat)^2
