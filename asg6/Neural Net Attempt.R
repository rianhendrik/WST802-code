rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
options(max.print = .Machine$integer.max)
gc() #free up memrory and report the memory usage.

install.packages("tensorflow")
install.packages("keras") 
library(openxlsx)
library(tidyselect)
library(caret)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.xlsx('../data/creditcard-excel.xlsx')

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


# nrow(train)/nrow(d) -> train_prop
# nrow(test)/nrow(d) -> test_prop
# nrow(validation)/nrow(d) -> validation_prop
# 
chc1=sum(test$Class)
chc2=sum(train$Class)
chc3=sum(d$Class)
chc4=sum(test_val$Class)

test=rbind(test,validation)

# chc1=sum(test$Class)
# chc2=sum(train$Class)
# chc3=sum(d$Class)
# chc4=sum(test_val$Class)

#SETTING UP TRAINING DATA
kk=ncol(train)
train[, 1:kk] <- sapply(train[, 1:kk], as.numeric)
train=as.matrix(train)
dimnames(train)=NULL
train[,c(1:kk-1)] <- normalize(train[,c(1:kk-1)])

tr_dat=train[,2:(kk-1)]
kin=ncol(tr_dat)
traintarget=train[,kk]

#SETTING UP TESTING DATA
kk=ncol(test)
test[, 1:kk] <- sapply(test[, 1:kk], as.numeric)
test=as.matrix(test)
dimnames(test)=NULL
test[,c(1:kk-1)] <- normalize(test[,c(1:kk-1)])

test_dat=test[,2:(kk-1)]
testtarget=test[,kk]
#head(testtarget)

library(keras)
library(tensorflow)
install_tensorflow()

model <- keras_model_sequential()
model %>% layer_dense(units =84, activation ="relu", input_shape = c(kin)) %>%
  layer_dense(units =56, activation = 'relu') %>%
  layer_dense(units =28, activation = 'relu') %>%
  layer_dense(units =1, activation ="sigmoid")
summary(model)

model %>% compile(loss ="binary_crossentropy",
                  optimizer ="adam",
                  metrics ="accuracy")
class(traintarget)
fit <- model %>%
  fit(tr_dat,traintarget,
      epochs =50,
      batch_size = 128,
      validation_split =0.2)
plot(fit)



# Evaluate 
model %>%  evaluate(test_dat, testtarget)  

# Prediction 
pred=model %>% predict(test_dat) %>% `>`(0.5) %>% k_cast("int32")

# CONFUSION MATRIX
predv=as.vector(pred)
table(Predicted=predv, Actual=testtarget)
print(fit)
cf1=table(Predicted=predv, Actual=testtarget)
predf=as.factor(predv)
ref=as.factor(testtarget)
cf2 <- confusionMatrix(data=predf, reference = ref, positive = "1", mode = 'prec_recall')

#Display results 
cf1
cf2

