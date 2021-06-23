library(naivebayes)
library(dplyr)
#Mushrooms <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", header=FALSE, sep=",", dec=".", na.strings=c("?"))
dataA4 <- read.csv("H:/JCU/MA5832 Rstudio/Week 6/Capstone/non-compliance-in-personal-insolvencies.csv",na.strings = c("?"),  header= FALSE, sep=",", dec=".")
dataA4 = dataA4[-1,]
str(dataA4)
is.na(dataA4)

#dataA4 <- select(dataA4, -c(18:23))
#head(dataA4)
set.seed(0)

for (i in colnames(dataA4[, sapply(dataA4, is.factor)])){
  for (level in unique(dataA4[, i])){
    dataA4[paste(i, level, sep = "_")] = 
      as.integer(ifelse(dataA4[, i] == level, 1, -1))
  }
}
no_observations <- dim(dataA4)[1]                   # total observations (8124)
no_predictors <- dim(dataA4)[2] - 13                 # total predictors (16)
# total variables (17) -
# dependent var is the 13th column
set.seed(0)
Feature_Set <- c()                                     # Initialise Feature Subset (empty)
Test_Error <- c()                                      # Initialise Error Subset
for(Size_Feature_Set in 1:no_predictors){              # Continue for each predictor starting from 1
  best_accuracy <- -Inf
  for(feature in 2:(no_predictors+1)){                 # Skip 1st variable (class labels)
    if (!(feature %in% Feature_Set)){
      Test_Feature_Set <- c(Feature_Set, feature)
      accuracy <- 0
      for(i in 1:10){
        test_index <- sample(no_observations, size=as.integer(no_observations*0.3), replace=FALSE)
        # 20% test
        training_index <- -test_index                  # 80% training
        candidate_variables_index <- c(13, Test_Feature_Set) # "1" is the class variable (V1),
        # this vector selects specific data columns
        NaiveBayesModel <- naive_bayes(V13 ~. ,         # . takes the available data columns   
                                       data = dataA4[training_index, candidate_variables_index])
        Pred_class <- predict(NaiveBayesModel,
                              newdata = dataA4[test_index, candidate_variables_index])
        tab <- table(Pred_class, dataA4[test_index,"V13"])
        accuracy <- accuracy + sum(diag(tab))/sum(tab)
      }
      
       accuracy <- accuracy/10  # get the best feature that contributes to improve accuracy
      if (accuracy > best_accuracy) { 
        best_accuracy <- accuracy
        best_new_feature <- feature
      }
    }  
  }
  Feature_Set <- c(Feature_Set, best_new_feature)  # list of best features in each iteration
  print(Feature_Set)
  Test_Error <- c(Test_Error, 1-best_accuracy)     # calculate the error rate in each iteration
}
plot(1:22,Test_Error) # finally plot the error rates

#is.na(dataA4)
sum(is.na(dataA4))
sum(is.NULL(dataA4))
na.omit(dataA4)
warnings()
