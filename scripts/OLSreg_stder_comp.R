#EDS241: OLS regressions and comparison of standard errors

library(estimatr)
library(stargazer)
library(ggplot2)

# PATH TO DIRECTORY WHERE DATA FILE IS LOCATED. MAKE SURE TO ADJUST TO YOUR COMPUTER
setwd("C:\\Users\\Olivier Deschenes\\Dropbox\\Econ Desktop\\Teaching\\EDS 241\\Data")


# IMPORT CSV DATA
HPRICE2 <- read.csv("HPRICE2.csv")


# SUMMARY STATISTICS
stargazer(HPRICE2, type="text", digits=1)


# BIVARIATE REGRESSION WITH ROBUST STD ERRORS
model1 <- lm(formula = price ~ nox, data = HPRICE2)
se_model1 <- starprep(model1, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 
stargazer(model1, se = se_model1, type="text")


# SCATTERPLOT OF Y AND X, AND ESTIMATED REGRESSION LINE
ggplot(HPRICE2, aes(x=nox, y=price)) + geom_point(size=2, color="blue") +    
  labs(x="NOx (pp100m)", y = "Median housing price") + 
  theme_bw() + geom_abline(intercept = 39232, slope = -3060, size=1.5, color="gray37")


# MULTIPLE REGRESSION WITH ROBUST STD ERRORS
model2 <- lm(formula = price ~ nox + rooms, data = HPRICE2)
se_model2 <- starprep(model2, stat = c("std.error"), se_type = "HC1", alpha = 0.05)
stargazer(model1, model2, se = c(se_model1, se_model2), type="text")


# LECTURE 3:
# COMPARISON OF HOMOSKEDASTIC AND HETEROSKEDASTIC STANDARD ERRORS
model1 <- lm(formula = price ~ rooms, data = HPRICE2)
# HOMOSKEDASTIC ERRORS
se_ho_model1 = list(summary(model1)$coefficients[,2])
# HETEROSKEDASTIC ERRORS
se_he_model1 <- starprep(model1, stat = c("std.error"), se_type = "HC1", alpha = 0.05)
#TABLE OF RESULTS
stargazer(model1, model1, se = c(se_ho_model1, se_he_model1), type="text")  
