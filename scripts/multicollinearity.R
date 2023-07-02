#EDS241: Demonstrate problems with the OLS estimator when the regressors are multicollinear

library(estimatr)
library(stargazer)
library(ggplot2)
library(MASS)

#set.seed(145777)
#bigN <- 10000

set.seed(124578)
bigN <- 10000

# Generate X1 and X2, correlated regressors (here rho_12=0.5)
sigma<-rbind(c(1,0.5), c(0.5,1))
mu<-c(5,10) 
# generate the multivariate normal distribution
X <- data.frame(mvrnorm(n=bigN, mu=mu, Sigma=sigma))
X1 <- X[,1]
X2 <- X[,2]
cor(X1,X2)

# Generate u
u <- rnorm(bigN, sd = 2)

# Population regression function
Y <- 5 + 1.5*X1 + 7*X2 + u

RegData=data.frame(X1, X2, Y)
model1 <- lm(formula = Y ~ X1 + X2, data = RegData)
se_model1 <- starprep(model1, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 




# Generate X1 and X2, correlated regressors (here rho_12=0.9)
sigma<-rbind(c(1,0.9), c(0.9,1))
mu<-c(5,10) 
# generate the multivariate normal distribution
X <- data.frame(mvrnorm(n=bigN, mu=mu, Sigma=sigma))
X1 <- X[,1]
X2 <- X[,2]
cor(X1,X2)

# Population regression function
Y <- 5 + 1.5*X1 + 7*X2 + u

RegData=data.frame(X1, X2, Y)
model2 <- lm(formula = Y ~ X1 + X2, data = RegData)
se_model2 <- starprep(model2, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 




# Generate X1 and X2, correlated regressors (here rho_12=0.99999)
sigma<-rbind(c(1,0.99999), c(0.99999,1))
mu<-c(5,10) 
# generate the multivariate normal distribution
X <- data.frame(mvrnorm(n=bigN, mu=mu, Sigma=sigma))
X1 <- X[,1]
X2 <- X[,2]
cor(X1,X2)

# Population regression function
Y <- 5 + 1.5*X1 + 7*X2 + u

RegData=data.frame(X1, X2, Y)
model3 <- lm(formula = Y ~ X1 + X2, data = RegData)
se_model3 <- starprep(model3, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 



# make table with estimated coefficients from all 3 regressions
stargazer(model1, model2, model3, se = c(se_model1, se_model2, se_model3), type="text")


