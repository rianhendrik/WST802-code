# Data generations was achieved with the help of
# https://data.library.virginia.edu/simulating-a-logistic-regression-model/

############################## Question 1 #####################################
library(dplyr)
library(matrixStats)
#Generating values for our two predictors

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

n <- 300 # Number of observations
min_mileage <- 5 #Min of predictor
max_mileage <- 150 #Max of predictor

set.seed(1)
mileage <- round(runif(n, min_mileage, max_mileage)) # Our predictor
mileage <- sort(mileage, decreasing = F)

beta0 <- -12
beta1 <- 0.2
xb <- beta0 + beta1*mileage # Linear combination of our predictor

p <- 1/(1 + exp(-xb)) # Theoretical logistic probabilities
summary(p)

brkd <- rbinom(n = n, size = 1, prob = p) # Generating response variable (brkd = breakdown)

# Theoretical logit
mileage_grid <- as.numeric(unlist(data.frame(mileage=seq(min(mileage), max(mileage),len=n))))
xb_theo <- beta0 + beta1*mileage_grid
theo_probs <- 1/(1 + exp(-xb_theo))


############################## Question 2 #####################################
theoplot_data <- data.frame(cbind(theo_probs, mileage_grid)) 


png(paste0("logit-1-",n,".png"), width = 2400, height = 1600, units = 'px', res = 200)

plot(brkd ~ mileage, data = theoplot_data, col="black", pch = 19, 
     main = 'Logistic regression applied to vehicle breakdown data',
     ylab = 'Likelihood of breakdown',
     xlab = 'Vehicle mileage')
lines(theo_probs ~ mileage_grid, theoplot_data, col="blue", lwd=2)

# Fitting a logit to the generated data
cars <- data.frame(cbind(brkd, mileage))
fit <- glm(brkd ~ mileage, data=cars, family=binomial) # Fitting the model
beta0_hat <- unname(fit$coefficients[1])
beta1_hat <- unname(fit$coefficients[2])

xb_emp <- beta0_hat + beta1_hat*mileage_grid
emp_probs <- 1/(1 + exp(-xb_emp))

empplot_data <- data.frame(cbind(emp_probs, mileage_grid)) 

lines(emp_probs ~ mileage_grid, empplot_data, col="red", lwd=2, lty = 2)

legend(3, 0.95, legend=c("Theoretical logistic curve", "Fitted (empirical) logistic curve"),
       col=c("blue", "red"), lty=1:2, cex=1, bty = "n", y.intersp = 2)
dev.off()

########################### Question 3 ########################################
d = data.frame(cbind(brkd, intercept = matrix(1, n, 1), mileage))
colnames(d)[2] <- "intercept"

d$Fold <- sample(factor(rep(1:3, length.out=nrow(d)), 
                                 labels=paste0("Fold", 1:3)))

fold1 <- d[which(d$Fold == 'Fold1'),]
fold2 <- d[which(d$Fold == 'Fold2'),]
fold3 <- d[which(d$Fold == 'Fold3'),]


########################## Question 4 #########################################
# Secure logistic regression (Test using code from 
# https://stats.stackexchange.com/questions/544853/fake-distributed-computation-secure-summation-on-irls-for-binary-logistic-regr)
# This code was, however, adjusted

# age <- round(runif(10, 18, 80))
# df <- age
# xb <- -9 + 0.2*age
# p <- 1/(1 + exp(-xb))
# y <- rbinom(n = 10, size = 1, prob = p)
# 
# lst = split(df, (0:nrow(df) %/% 100))
# df_1 <- lst$`0`
# df_2 <- lst$`1`
# df_3 <- lst$`2`

#########IRLS##########
bho <- matrix(c(0,0),nrow = 2, ncol = 1)
#DF 1
x_1 <- data.matrix(fold1[,2:3])

#DF 2
x_2 <- data.matrix(fold2[,2:3])

#DF 3
x_3 <- data.matrix(fold3[,2:3])

for (i in 1:30) {
  
  #DF 1
  p_fit_1 <- 1/(1 + exp(-(x_1%*%bho)))
  wp_1 <- as.vector(p_fit_1*(1-p_fit_1))
  w_1 <- diag(wp_1)
  
  hes_1 <- t(x_1)%*%w_1%*%x_1
  grad_1 <- (t(x_1)%*%(fold1$brkd-p_fit_1))
  
  #DF 2
  p_fit_2 <- 1/(1 + exp(-(x_2%*%bho)))
  wp_2 <- as.vector(p_fit_2*(1-p_fit_2))
  w_2 <- diag(wp_2)
  
  hes_2 <- t(x_2)%*%w_2%*%x_2
  grad_2 <- (t(x_2)%*%(fold2$brkd-p_fit_2))
  
  #DF 3
  p_fit_3 <- 1/(1 + exp(-(x_3%*%bho)))
  wp_3 <- as.vector(p_fit_3*(1-p_fit_3))
  w_3 <- diag(wp_3)
  
  hes_3 <- t(x_3)%*%w_3%*%x_3
  grad_3 <- (t(x_3)%*%(fold3$brkd-p_fit_3))
  
  #Put it together now
  hes <- hes_1+hes_2+hes_3
  grad <- grad_1+grad_2+grad_3
  
  bhn <- bho + solve(hes)%*%grad
  
  change = max(abs(bhn-bho))
  if (change<1E-3) break
  bho <- bhn
  
}
x <- data.matrix(d[,2:3])
xbn <- x%*%bhn
pn <- 1/(1 + exp(-xbn))

xb_dist <- bhn[1] + bhn[2]*mileage_grid
dist_probs <- 1/(1 + exp(-xb_dist))

distlogit_data <- data.frame(cbind(dist_probs, mileage_grid)) 

# Seems as if the distributed approach gives the same answer as the approach 
# using all the data from question

##Plot
png(paste0("logit-2-",n,".png"), width = 2400, height = 1600, units = 'px', res = 200)
plot(brkd ~ mileage, data = theoplot_data, col="black", pch = 19, 
     main = 'Logistic regression applied to vehicle breakdown data',
     ylab = 'Likelihood of breakdown',
     xlab = 'Vehicle mileage')
lines(theo_probs ~ mileage_grid, theoplot_data, col="blue", lwd=2)

# Fitting a logit to the generated data
cars <- data.frame(cbind(brkd, mileage))
fit <- glm(brkd ~ mileage, data=cars, family=binomial) # Fitting the model
beta0_hat <- unname(fit$coefficients[1])
beta1_hat <- unname(fit$coefficients[2])

xb_emp <- beta0_hat + beta1_hat*mileage_grid
emp_probs <- 1/(1 + exp(-xb_emp))

empplot_data <- data.frame(cbind(emp_probs, mileage_grid)) 

lines(emp_probs ~ mileage_grid, empplot_data, col="red", lwd=2, lty = 2)

lines(dist_probs ~ mileage_grid, distlogit_data, col="green", lwd=2, lty = 3)

legend(3, 0.95, legend=c("Theoretical logistic curve", 
                         "Fitted (empirical) logistic curve",
                         "Distributed Computing (empirical) logistic curve"),
       col=c("blue", "red", "green"), lty=1:3, cex=1, bty = "n", y.intersp = 2)

dev.off()
#######################


########################## Question 5 #########################################
# This question may be answered by increasing the value of n at the top of this
# script.
