#EDS241: Demonstrate that OLS is consistent (unbiased) and demonstrate omitted variable bias

library(estimatr)
library(stargazer)
library(ggplot2)

################################################################################
# 1. Demonstrate OLS estimate is consistent under LSA 1-3
################################################################################

set.seed(124578)
bigN <- 10000

# Generate X1 and u
X1 <- runif(bigN, min = 0, max = 10)
u <- rnorm(bigN, mean = 0, sd = 4)

# Bivariate population regression function
Y <- 5 + 1.5*X1 + u
population_data <- data.frame(X1, Y)

# OLS estimation, full sample
model1 <- lm(formula = Y ~ X1, data = population_data)
se_model1 <- starprep(model1, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 
stargazer(model1, se = se_model1, type="text")

# OLS estimation, with sample size increasing from n=1 to 10,000
betahat_output <- matrix(ncol = 2, nrow = bigN)

for (n in 1:bigN) {
  sample <- population_data[1:n,]
  betahat_output[n,] <- lm(Y ~ X1, data = sample)$coefficients
} 

n <- seq(1,bigN)
forgraph <- data.frame(n , "beta0hat" = betahat_output[,1], "beta1hat" = betahat_output[,2])

# Graphing the results
ggplot(forgraph , aes(x=n, y=beta1hat)) + geom_line(size=0.5, color="blue") +
  geom_hline(yintercept=1.5, size=2, color="red") +
  labs(x="n", y = "Beta1hat") + theme_bw() 



###############################################################################
# 2. Demonstrating omitted variables bias
###############################################################################

X2 = X1 +rnorm(bigN , mean=0 , sd=2.2)

# Multiple population regression function
Y <- 5 + 1.5*X1 + 10*X2 + u
population_data <- data.frame(X1, Y)

# OLS estimation, full sample, but ignoring X2
model2 <- lm(formula = Y ~ X1, data = population_data)
se_model2 <- starprep(model2, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 
stargazer(model1, model2, se = c(se_model1, se_model2), type="text")

# Compute correlation between X1 and X2, and standard deviations
# Compute "probability limit" of Beta1_hat
cor(X1,X2)
sd(X1)
sd(X2)
1.5 + 10*cor(X1,X2)*sd(X2)/sd(X1)

# OLS estimation with omitted variable bias (OVB), with sample size increasing from n=1 to 10,000
betahat_output <- matrix(ncol = 2, nrow = bigN)

for (n in 1:bigN) {
  sample <- population_data[1:n,]
  betahat_output[n,] <- lm(Y ~ X1, data = sample)$coefficients
} 

n <- seq(1,bigN)
forgraph <- data.frame(n , "beta0hat" = betahat_output[,1], "beta1hat" = betahat_output[,2])

ggplot(forgraph , aes(x=n, y=beta1hat)) + geom_line(size=0.5, color="blue") +
  geom_hline(yintercept=1.5, size=2, color="red") +
  labs(x="n", y = "Beta1hat") + theme_bw()
