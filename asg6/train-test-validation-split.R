library(openxlsx)
library(caret)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# d <- read.xlsx("../data/creditcard-excel.xlsx") # Insert your data path here.

set.seed(180)
# Below function obtained from: https://stackoverflow.com/questions/63424421/r-how-to-split-data-into-training-and-testing-set-while-preserving-proportions
inTrain <- createDataPartition(
  y = d$Class,
  ## the outcome data are needed
  p = .7,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

train = d[inTrain,]
test_val = d[-inTrain,]

sum(train$Class)
sum(test_val$Class)
print(sum(test_val$Class)/sum(train$Class)) # Verifying response proportions in test/validate and train 

# Checking that the distribution of the Test/Validate and Train sets are similar
for (i in 1:ncol(d)){
  plot(density(train[,i], na.rm = T), main = paste0("Distribution of variable ", i) ,
       xlab = paste0("Variable ", i), 
       col = "red", lwd = 2)
  
  lines(density(test[,i], na.rm = T), col = "blue", lwd = 2)
  
  legend("topleft", col = c("red", "blue"),
         lty = 1, bty = "n", lwd = 2,
         c("Train", "Test"),
         y.intersp = 1,
         cex = 1)
}


#Creating the validation set
inTest <- createDataPartition(
  y = test_val$Class,
  ## the outcome data are needed
  p = .5,
  ## The percentage of data in the
  ## training set
  list = FALSE
)


test = test_val[inTest,]
validation = test_val[-inTest,]

sum(test$Class)
sum(validation$Class)
print(sum(test$Class)/sum(validation$Class)) # Verifying response proportions in test/validate and train 

# Checking that the distribution of the Test and Validation sets are similar
for (i in 1:ncol(d)){
  plot(density(test[,i], na.rm = T), main = paste0("Distribution of variable ", i) ,
       xlab = paste0("Variable ", i), 
       col = "red", lwd = 2)
  
  lines(density(validation[,i], na.rm = T), col = "blue", lwd = 2)
  
  legend("topleft", col = c("red", "blue"),
         lty = 1, bty = "n", lwd = 2,
         c("Test", "Validation"),
         y.intersp = 1,
         cex = 1)
}

nrow(train)/nrow(d) -> train_prop
nrow(test)/nrow(d) -> test_prop
nrow(validation)/nrow(d) -> validation_prop

write.xlsx(train, "../data/train.xlsx") #Specify path where you want your data here
write.xlsx(test, "../data/test.xlsx") #Specify path where you want your data here
write.xlsx(validation, "../data/validation.xlsx") #Specify path where you want your data here


