#EDS241: RCT demonstration


library(MASS)
library(ggplot2)
library(vtable)
library(stargazer)
library(estimatr)

set.seed(7107)

bigN=20000

W <- runif(bigN,0,5)
X=as.integer(W+1)
X1 <- as.numeric(X==1)
X2 <- as.numeric(X==2)
X3 <- as.numeric(X==3)
X4 <- as.numeric(X==4)
X5 <- as.numeric(X==5)



# GENERATE MEAN COMPONENT OF POTENTIAL OUTCOMES
MU0=(1/2)*X1 + (2/2)*X2 + (3/2)*X3 + (4/2)*X4 + (5/2)*X5
mean(MU0)
MU1=1*X1 + 2*X2 + 3*X3 + 4*X4 + 5*X5
mean(MU1)


# GENERATE ERROR COMPONENT OF POTENTIAL OUTCOMES
Sigma <- matrix(c(1,0.75,0.75,1),2,2)
Sigma
e <- (mvrnorm(n=bigN, rep(0, 2), Sigma))
e0 <- e[,c(1)]
mean(e0)
e1 <- e[,c(2)]  
mean(e1)



# GENERATE POTENTIAL OUTCOMES
Y0 <- MU0 + e0
mean(Y0)
Y1 <- MU1 + e1
mean(Y1)

ATE <- mean(Y1)-mean(Y0)
print(ATE)

PO_DF <- data.frame(Y0,Y1,X)


  


# RANDOMLY ASSIGN A TREATMENT INDICATOR
D <- as.numeric((runif(bigN,0,1)) > 0.5)
mean(D)



# USE SUTVA TO MAP POTENTIAL OUTCOMES INTO OBSERVED OUTCOMES
Y = D*Y1 + (1-D)*Y0


# COLLECT ALL RELEVANT VARIABLES IN A DATAFRAME
RCT_DATA <- data.frame(Y, D, Y0, Y1, X, X1, X2, X3, X4, X5)



# CHECK THAT D IS INDEPENDENT OF X, Y0, Y1 (RECALL Y0,Y1 NOT OBSERVED IN REALITY)
# "TEST" OF COVARIATE BALANCE
sumtable(RCT_DATA, vars=c('Y0','Y1', 'Y', 'X1', 'X2', 'X3', 'X4', 'X5'), group='D', group.test=TRUE)

mA <- lm(formula = X ~ D, data=RCT_DATA)
mB <- lm(formula = Y0 ~ D, data=RCT_DATA)
mC <- lm(formula = Y1 ~ D, data=RCT_DATA)
se_models = starprep(mA, mB, mC, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(mA, mB, mC, se = se_models, type="text")

# ESTIMATE ATE USING SIMPLE OLS REGRESSION OF Y on D
ate1 <- lm(formula = Y ~ D, data=RCT_DATA)
ate2 <- lm(formula = Y ~ D + X, data=RCT_DATA)
se_models = starprep(ate1, ate2, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(ate1, ate2, se = se_models, type="text")

