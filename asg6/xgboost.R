library(openxlsx)
library(caret)
require(xgboost) # For the XGBoost model
library(InformationValue) # For the optimal threshold
library(ISLR)
library(caret)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# d <- read.xlsx("../data/creditcard-excel.xlsx") # Insert your data path here.

dtrain = read.xlsx('../data/train.xlsx')
dvalidation = read.xlsx('../data/validation.xlsx')

xtrain = dtrain[,1:ncol(dtrain)-1]
ytrain = dtrain[,ncol(dtrain)]

xvalidation = dvalidation[,1:ncol(dvalidation)-1]
yvalidation = dvalidation[,ncol(dvalidation)]

# Converting data to DMatric for XGBoost
dmtrain <- xgb.DMatrix(data = as.matrix(xtrain), label = dtrain$Class)
dmvalidation <- xgb.DMatrix(data = as.matrix(xvalidation), label = dvalidation$Class)

#### Basic XGBoost model ####
bst1 <- xgboost(data = dmtrain,
                      max.depth = 2, 
                      eta = 1, 
                      nthread = 100,
                      nrounds = 2, 
                      objective = "binary:logistic",
                      verbose = 2)

pred <- predict(bst1, dmvalidation)

# Finding the optimal threshold
thresholds <- seq(0, 1, 0.01)
F1s <- matrix(data = NA, nrow = length(thresholds), ncol = 1)
F1s <- cbind(thresholds, F1s)
recalls <- cbind(thresholds, F1s)
counter <-  0
for (threshold in thresholds){
  counter <- counter + 1
  recalls[counter, 2] <- sensitivity(yvalidation, pred, threshold = threshold) #Recall
  precision <- precision(yvalidation, pred, threshold = threshold)  #Precision
  F1s[counter, 2] <- (2 * precision * recalls[counter,2]) / (precision + recalls[counter,2])
}

optimal = F1s[which(F1s[,2] == max(F1s[,2], na.rm = T))][1]
print(paste0("The optimal threshold is as per F1 ", optimal))

# optimal <- optimalCutoff(yvalidation, pred, optimiseFor = "Ones")

confusionMatrix(yvalidation, pred, threshold = optimal)

recall <- sensitivity(yvalidation, pred, threshold = optimal) #Recall
precision <- precision(yvalidation, pred, threshold = optimal)  #Precision
F1 <- (2 * precision * recall) / (precision + recall)

cat(paste0("Precision: ", precision,"\n", 
           "Recall: ", recall,"\n",
           "F1: ", F1))

pred_b <- pred
for (row in 1:length(pred_b)){
  if (pred_b[row] >= optimal){pred_b[row] = 1}
  if (pred_b[row] < optimal){pred_b[row] = 0}
}

pred_b <- as.factor(pred_b)
yvalidation <- as.factor(yvalidation)
caret::confusionMatrix(data = pred_b, reference = yvalidation, mode = "prec_recall", positive = "1")


#### A more complex model ####
bst2 <-  xgboost(data = dmtrain,
                 max.depth = 3, 
                 eta = 1, 
                 nthread = 2,
                 nrounds = 2, 
                 objective = "binary:logistic",
                 verbose = 2)

pred <- predict(bst2, dmvalidation)

# Finding the optimal threshold
thresholds <- seq(0, 1, 0.01)
F1s <- matrix(data = NA, nrow = length(thresholds), ncol = 1)
F1s <- cbind(thresholds, F1s)
recalls <- cbind(thresholds, F1s)
counter <- 0
for (threshold in thresholds){
  counter <- counter + 1
  recalls[counter, 2] <- sensitivity(yvalidation, pred, threshold = threshold) #Recall
  precision <- precision(yvalidation, pred, threshold = threshold)  #Precision
  F1s[counter, 2] <- (2 * precision * recalls[counter,2]) / (precision + recalls[counter,2])
}

optimal = F1s[which(F1s[,2] == max(F1s[,2], na.rm = T))][1]
print(paste0("The optimal threshold is as per F1 ", optimal))

# optimal <- optimalCutoff(yvalidation, pred, optimiseFor = "Ones")

cm = confusionMatrix(yvalidation, pred, threshold = optimal)
cm

recall <- sensitivity(yvalidation, pred, threshold = optimal) #Recall
precision <- precision(yvalidation, pred, threshold = optimal)  #Precision
F1 <- (2 * precision * recall) / (precision + recall)

cat(paste0("Precision: ", precision,"\n", 
           "Recall: ", recall,"\n",
           "F1: ", F1))

pred_b <- pred
for (row in 1:length(pred_b)){
  if (pred_b[row] >= optimal){pred_b[row] = 1}
  if (pred_b[row] < optimal){pred_b[row] = 0}
}

pred_b <- as.factor(pred_b)
yvalidation <- as.factor(yvalidation)
caret::confusionMatrix(data = pred_b, reference = yvalidation, mode = "prec_recall", positive = "1")
      