library(estimatr)
library(stargazer)



# IMPORT CSV DATA
HPRICE2 <- read.csv("HPRICE2.csv")


# PREDICTING HOUSING PRICE WHEN NOX =7 AND DERIVING STD ERROR + 95% CI
# HOMOSKEDASTIC STD ERRORS
model1 <- lm(formula = price ~ nox , data = HPRICE2)
PredPrice=data.frame(nox=c(7))
predict(model1, newdata=PredPrice, se.fit=TRUE, interval='confidence')


# PREDICTING HOUSING PRICE WHEN NOX =7 AND DERIVING STD ERROR + 95% CI
# HETEROSKEDASTIC STD ERRORS
# NOTE: USES LM_ROBUST
model2 <- lm_robust(formula = price ~ nox , data = HPRICE2, se_type = "HC1", alpha = 0.05)
predict(model2, newdata=list(nox=7), se.fit=TRUE, interval='confidence')




# PREDICTING HOUSING PRICE WHEN NOX=5 & ROOMS=6 AND DERIVING STD ERROR + 95% CI
# HETEROSKEDASTIC STD ERRORS
# NOTE: USES LM_ROBUST
PredPrice=data.frame(nox=c(5), rooms=c(6))
predict(model2, newdata=PredPrice, se.fit=TRUE, interval='confidence')