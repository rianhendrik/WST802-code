# Simulating dummy data -> Betta fish survival data in a 30 litre tank with a heater
# Temperature is measures in degrees celcius

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##### QUESTION 1: Generating a dataset ################################################################################
n <- 3000 # Number of observations

# Predictor 1 (sig, negative)
min_food_grams <- 10 #Min of predictor
max_food_grams <- 50 #Max of predictor

# Predictor 2 (sig, positive)
min_water_temp = 5
max_water_temp = 28

# Predictor 3 (insig, ~0)
min_room_temp = 5
max_room_temp = 35

# Predictor 4 (sig, negative)
min_companions = 0
max_companions = 10

# Predictor 5 (insig, ~0)
neighbours_min = 0
neighbours_max = 10

#Predictor 6 (sig, positive)
# dangerous_objects = boolean

set.seed(1)
food_grams <- round(runif(n, min_food_grams, max_food_grams)) # Predictor 1
water_temp <- round(runif(n, min_water_temp, max_water_temp)) # Predictor 2
room_temp <- round(runif(n, min_room_temp, max_room_temp)) # Predictor 3
companions <- round(runif(n, min_companions, max_companions)) # Predictor 4
neighbours <- round(runif(n, neighbours_min, neighbours_max)) # Predictor 5
dangerous_objects <- round(runif(n))

beta0 <- 2
beta1 <- -0.2
beta2 <- 0.3
beta3 <- 0.003
beta4 <- -0.4
beta5 <- -0.001
beta6 <- 0.8

xb <- beta0 + beta1*food_grams + beta2*water_temp + beta3*room_temp +
              beta4*companions + beta5*neighbours + beta6*dangerous_objects # Linear combination of our predictor
p <- 1/(1 + exp(-xb)) # Theoretical logistic probabilities
summary(p)
survival <- rbinom(n = n, size = 1, prob = p) # Generating response variable (brkd = breakdown)


# The final generated dataset
betta_fish <- data.frame(cbind(survival, food_grams, water_temp, room_temp, 
                               companions, neighbours, dangerous_objects))


##### QUESTION 2 ##############################################################################

# Manually fitting a logistic regression model to the data

fit <- glm(survival ~ ., data = betta_fish, family = binomial) # Fitting the model
summary(fit)

##### QUESTIONS 3, 4 AND 5 ####################################################################
call_execute = function(data, dep_variable, prune = F){

  xvars <- setdiff(c(names(data)), dep_variable)
  sums <- c(rep(' + ', length(xvars) - 1), '') # adding a white space at end
  x_string = c()
  for (i in 1:length(xvars)){
    x_string = paste0(x_string, xvars[i], sums[i])
  }
  
  pred_string = paste0(dep_variable, ' ~ ', x_string)
  func <- as.formula(pred_string)
  
  fit1 <- glm(func, data = data, family = binomial)
  summary(fit1)$coeff[-1,4] < 0.05
  
  toselect.x <- summary(fit1)$coeff[-1,4] < 0.05 # credit to kith
  relevant.x <- names(toselect.x)[toselect.x == TRUE] 
  irrelevant.x <- names(toselect.x)[toselect.x == FALSE]
  
  if (length(irrelevant.x) > 0){
    print("There are insignificant variables in this model. Pruning is recommended.")
  }
  
  if (prune == T){
    sums <- c(rep(' + ', length(relevant.x) - 1), '') # adding a white space at end
    x_string = c()
    for (i in 1:length(relevant.x)){
      x_string = paste0(x_string, relevant.x[i], sums[i])
    }
    pred_string = paste0(dep_variable, ' ~ ', x_string)
    func_sig <- as.formula(pred_string)
    
    fit2 <- glm(func_sig, data = data, family = binomial)
    print("Insignificant variables removed are:")
    for (var in irrelevant.x){
      print(var)
    }
    
    #Check to see if there are now new insignificant variables.
    toselect.x <- summary(fit2)$coeff[-1,4] < 0.05 # credit to kith
    relevant.x <- names(toselect.x)[toselect.x == TRUE] 
    irrelevant.x <- names(toselect.x)[toselect.x == FALSE]
    
    if (length(irrelevant.x) < 1){
      print("All variables remaining are significant")
    }else{sums <- c(rep(' + ', length(relevant.x) - 1), '') # adding a white space at end
    x_string = c()
    for (i in 1:length(relevant.x)){
      x_string = paste0(x_string, relevant.x[i], sums[i])
    }
    pred_string = paste0(dep_variable, ' ~ ', x_string)
    func_sig <- as.formula(pred_string)
    
    fit2 <- glm(func_sig, data = data, family = binomial)
    print("Insignificant variables removed are:")
    for (var in irrelevant.x){
      print(var)
    }}
  }
  
  if(prune == T){
    return(fit2)
  }else{return(fit1)}

}


logit_result <- call_execute(data = betta_fish, dep_variable = 'survival', prune = F)
summary(logit_result)

pruned_logit_result <- call_execute(data = betta_fish, dep_variable = 'survival', prune = T)
summary(pruned_logit_result)

