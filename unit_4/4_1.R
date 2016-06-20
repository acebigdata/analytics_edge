#Initialization
rm(list = ls())

#Set file path properly
path <- getwd()
path <- paste(path,"/files", sep = "")
setwd(path)

#Load the dataset
stevens <- read.csv("stevens.csv")

#Basic data check
str(stevens)
summary(stevens)

#Randomly split the data to stevens_training vs. stevens_test
#Install and/or load library caTools
install.packages("caTools")
library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)

stevens_train <- subset(stevens, spl == T)
stevens_test <- subset(stevens, spl == F)

str(stevens_train)
str(stevens_test)

#Install and/or load rpart packages
install.packages("rpart")
library(rpart)
#Install and/or load rpart.plot packages
install.packages("rpart.plot")
library(rpart.plot)

#create a tree
stevens_tree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevens_train, method = "class", minbucket = 25)
summary(stevens_tree)
#plot the tree
prp(stevens_tree)

#check how well the model works on the stevens_test data
predict_cart <- predict(stevens_tree, newdata = stevens_test, type = "class")
#create a confusion matrix and see the accuracy
conf_table <-table(stevens_test$Reverse, predict_cart)
accuracy <- (conf_table[1,1] + conf_table[2,2])/(sum(conf_table))

#Plot the ROC curve
install.packages("ROCR")
library(ROCR)
#generate tree to ROC without type = class arument
predict_roc <- predict(stevens_tree, newdata = stevens_test)
summary(predict_roc)
#use the second col to generate preidction
pred <- prediction(predict_roc[,2], stevens_test$Reverse)
#create the performance check
pref <- performance(pred, "tpr","fpr")
#calculate AUC value
stevens_auc <- as.numeric(performance(pred,"auc")@y.values)

#Random forest
install.packages("randomForest")
library(randomForest)
#convert Reverse variable in both stevens_training and stevens_test sets to factor as randomTree can only run regression type
stevens_train$Reverse <- as.factor(stevens_train$Reverse)
stevens_test$Reverse <- as.factor(stevens_test$Reverse)
stevens_forest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevens_train, nodesize = 25, ntree = 200)
#make predictions based on new forest model
predict_forest <- predict(stevens_forest, newdata = stevens_test)
conf_table_forest <- table(stevens_test$Reverse, predict_forest)
accuracy_forest <- (conf_table_forest[1,1] + conf_table_forest[2,2])/(sum(conf_table_forest))

###Cross Validation
#install and new package caret
install.packages("caret")
library(caret)
#install and new package e1071
install.packages("e1071")
library(e1071)

#define number of folds
num_folds <- trainControl(method = "cv", number = 10)
#define CP Grid
cp_grid <- expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevens_train,method = "rpart", trControl = num_folds, tuneGrid = cp_grid)
#optimal cp value is 0.18
stevens_tree_cv <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevens_train, method = "class", cp = 0.18)
#predict based on above created model
predict_cv <- predict(stevens_tree_cv, newdata = stevens_test ,type = "class")
confusion_cv <- table(stevens_test$Reverse, predict_cv)
accuracy_cv <- (confusion_cv[1,1]+confusion_cv[2,2])/sum(confusion_cv)
