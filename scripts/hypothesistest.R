#EDS241: Various hypothesis tests and confidence intervals

library(estimatr)
library(stargazer)
library(car)


# IMPORT CSV DATA
HPRICE2 <- read.csv("HPRICE2.csv")

# SUMMARY STATISTICS
stargazer(HPRICE2, type="text", digits=1)

# 1. MULTIPLE REGRESSION WITH HOMOSKEDASTIC T-TESTS
model1 <- lm(formula = price ~ nox + rooms + stratio, data = HPRICE2)
summary(model1)


# 2. MULTIPLE REGRESSION WITH HETEROSKEDASTICITY-ROBUST T-TESTS (note "lm_robust")
model1 <- lm_robust(formula = price ~ nox + rooms + stratio, data = HPRICE2)
summary(model1)


# 3. MULTIPLE REGRESSION WITH HETEROSKEDASTICITY-ROBUST T-TESTS
# THIS IS THE COMMAND I RECOMMEND
model1 <- lm(formula = price ~ nox + rooms + stratio, data = HPRICE2)

# TEST THAT BETA_NOX=0
linearHypothesis(model1,c("nox=0"), white.adjust = "hc1")

# TEST THAT BETA_NOX=-1000
linearHypothesis(model1,c("nox=-1000"), white.adjust = "hc1")

# TEST THAT BETA_NOX=0 & BETA_STRATIO=0
linearHypothesis(model1,c("nox=0", "stratio=0"), white.adjust = "hc1")

# TEST THAT 4*BETA_NOX=-BETA_ROOMS (just an example, this is a silly hypothesus)
linearHypothesis(model1,c("4*nox=-rooms"), white.adjust = "hc1")

# 4. HETEROSKEDASTICITY-ROBUST CONFIDENCE INTERVALS
model1 <- lm_robust(formula = price ~ nox + rooms + stratio, data = HPRICE2)
summary(model1)