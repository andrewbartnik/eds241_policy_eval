
library(ggplot2)
library(stargazer)
library(estimatr)

### Directory

setwd("C:\\Users\\Olivier Deschenes\\Dropbox\\Econ Desktop\\Teaching\\EDS 241\\2023\\Data")


# IMPORT CSV DATA
CGL <- read.csv("cgl_2008_data_extract.csv")


# SUMMARY STATISTICS
stargazer(CGL, type="text", digits=2)


# EXAMINE BALANCE IN COVARIATES
# COVARIATE MEAN DIFFERENCES by DAPever
m1 <- lm(formula = LME ~ DAPever, data=CGL)
m2 <- lm(formula = genus ~ DAPever, data=CGL)
m3 <- lm(formula = species ~ DAPever, data=CGL)
se_models = starprep(m1, m2, m3, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(m1, m2, m3, se = se_models, type="text")

# BOXPLOTS TO EXAMINE BALANCE IN COVARIATES
ggplot(CGL, aes(x=as.factor(DAPever), y=LME)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")

ggplot(CGL, aes(x=as.factor(DAPever), y=genus)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")

ggplot(CGL, aes(x=as.factor(DAPever), y=species)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")


# BASIC OLS by DAPever -- THEN ADD INDICATORS FOR OTHER COVARIATES 
# NOTE DO NOT INCLUDE SPECIES IN MODELS TO KEEP RUNNING TIME FAST
mA <- lm(formula = collapse ~ DAPever, data=CGL)
mB <- lm(formula = collapse ~ DAPever + as.factor(LME), data=CGL)
mC <- lm(formula = collapse ~ DAPever + as.factor(LME) + as.factor(genus), data=CGL)
se_models = starprep(mA, mB, mC, stat = c("std.error"), se_type = "HC1", alpha = 0.05)
stargazer(mA, mB, mC, se = se_models, type="text", omit = "(LME)|(genus)|(species)")


