# This code is exactly the same as illustrated in the class, just change the min bucket from 25 to 5 and 100

#Initialization
rm(list = ls())

#Set file path properly
# path <- getwd()
# path <- paste(path,"/files", sep = "")
# setwd(path)

#Load the dataset
stevens <- read.csv("stevens.csv")

#Basic data check
str(stevens)
summary(stevens)

#Randomly split the data to training vs. test
#Install and/or load library caTools
# install.packages("caTools")
library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)

train <- subset(stevens, spl == T)
test <- subset(stevens, spl == F)

str(train)
str(test)

#Install and/or load rpart packages
# install.packages("rpart")
library(rpart)
#Install and/or load rpart.plot packages
# install.packages("rpart.plot")
library(rpart.plot)

#create a tree
stevens_tree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
summary(stevens_tree)
#plot the tree
prp(stevens_tree)

#check how well the model works on the test data
predict_cart <- predict(stevens_tree, newdata = test, type = "class")
#create a confusion matrix and see the accuracy
conf_table <-table(test$Reverse, predict_cart)
accuracy <- (conf_table[1,1] + conf_table[2,2])/(sum(conf_table))

#Plot the ROC curve
# install.packages("ROCR")
library(ROCR)
#generate tree to ROC without type = class arument
predict_roc <- predict(stevens_tree, newdata = test)
summary(predict_roc)
#use the second col to generate preidction
pred <- prediction(predict_roc[,2], test$Reverse)
#create the performance check
pref <- performance(pred, "tpr","fpr")
#calculate AUC value
stevens_auc <- as.numeric(performance(pred,"auc")@y.values)
