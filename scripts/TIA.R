#EDS241: Simulation of data and demonstration of methods based on "treatment ignorability"
#Lecture 7 & 8

library(MASS)
library(vtable)
library(stargazer)
library(estimatr)
library(dplyr)
library(tidyr)

set.seed(7307)

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

# ASSIGN A TREATMENT IGNORABLE CONDITIONAL ON X
v <- rnorm(bigN, 0, 2)
D <- as.numeric((-2*X1+ -1*X2 + 0.5*X3 + 1*X4 + 2*X5 + v)>0)
mean(D)


# USE SUTVA TO MAP POTENTIAL OUTCOMES INTO OBSERVED OUTCOMES
Y = D*Y1 + (1-D)*Y0


# COLLECT ALL RELEVANT VARIABLES IN A DATAFRAME
TIA_DATA <- data.frame(D, Y0, Y1, X, X1, X2, X3, X4, X5)


# SHOW THAT D IS NOT INDEPENDENT OF X, Y0, Y1 (RECALL Y0,Y1 NOT OBSERVED IN REALITY)
sumtable(TIA_DATA, vars=c('Y0','Y1', 'X'), group='D', group.test=TRUE)


# SIMPLE OLS ESTIMATOR BIASED  FOR ATE
ols <- lm(formula = Y ~ D, data=TIA_DATA)
se_models = starprep(ols, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(ols, se = se_models, type="text")

# MULTIVARIATE MATCHING ESTIMATOR (LECTURE 7)

# Approach 1 with reshaping data ###########
#MANIPULATE DATA

TIA_table1 <- TIA_DATA %>%
  mutate(Y = ifelse(D==1, Y1,Y0))%>% #Create observed Y variable
  group_by(X,D)%>% 
  summarise(n_obs = n(),
            Y_mean= mean(Y, na.rm = T))%>% #Calculate number of observations and Y mean by X by treatment cells
  gather(variables, values, n_obs:Y_mean)%>% #Reshape data
  mutate(variables = paste0(variables,"_",D, sep=""))%>% #Combine the treatment and variables for reshaping
  pivot_wider(id_cols = X, names_from = variables,values_from = values)%>% #Reshape data by treatment and X cell
  ungroup()%>%  #Ungroup from X values
  mutate(Y_diff = Y_mean_1 - Y_mean_0, #calculate Y_diff
         w_ATE = (n_obs_0+n_obs_1)/(sum(n_obs_0)+sum(n_obs_1)),
         w_ATT = n_obs_1/sum(n_obs_1))%>% #calculate weights
  mutate_if(is.numeric, round, 2) #Round data

stargazer(TIA_table1, type= "text", summary = FALSE, digits = 2)

#MULTIVARIATE MATCHING ESTIMATES OF ATE AND ATT
ATE=sum((TIA_table1$w_ATE)*(TIA_table1$Y_diff))
ATE
ATT=sum((TIA_table1$w_ATT)*(TIA_table1$Y_diff))
ATT

# Approach 2 with grouping data ###########
#MANIPULATE DATA

TIA_table2 <- TIA_DATA %>%
  mutate(Y = ifelse(D==1, Y1,Y0))%>% #Create observed Y variable
  group_by(X,D)%>% 
  summarise(n_obs = n(),
            Y_mean= mean(Y, na.rm = T))%>% #Calculate number of observations and Y mean by X by treatment cells
  ungroup()%>%
  mutate(total_obs = sum(n_obs))%>% #Calculate total number of observations
  group_by(D)%>%
  mutate(total_obs_d = sum(n_obs))%>% #Calculate total number of observations by treatment cells
  group_by(X)%>%
  mutate(Y_diff = lead(Y_mean)-Y_mean,
         W_ATE = sum(n_obs)/total_obs,
         W_ATT = lead(n_obs)/lead(total_obs_d))%>% #Calculate difference in outcome and ATE and ATT weights by X
  ungroup()%>%
  mutate(ATE=sum(W_ATE*Y_diff, na.rm= T),
         ATT=sum(W_ATT*Y_diff, na.rm= T))%>% #Calculate ATE and ATT 
  mutate_if(is.numeric, round, 2) #Round data

TIA_table2

#MULTIVARIATE MATCHING ESTIMATES OF ATE AND ATT
ATE=unique(TIA_table2$ATE)
ATE
ATT=unique(TIA_table2$ATT)
ATT

# MULTIVARIATE MATCHING AS REGRESSION ESTIMATOR
reg_ate <- lm(formula = Y ~ D + X2 + X3 + X4 + X5, data=TIA_DATA)
se_models = starprep(reg_ate, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(reg_ate, se = se_models, type="text")



