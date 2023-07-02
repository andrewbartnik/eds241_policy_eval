
library(stargazer)
library(estimatr)
library(ggplot2)
library(plm)
library(lmtest)
library(dplyr)


# SET WORKING DIRECTORY


# IMPORT CSV DATA 
RPS <- read.csv("RPS_data.csv")

# SUMMARY STATISTICS
stargazer(RPS, type="text", digits=2)

# LIST SOME VARIABLES FOR CALIFORNIA
RPS%>%
  filter(state_name == "California")%>%
  select(state_name, year, rps_D, rps_ever_adopter, rps_implementation_year)%>%
  View

# CREATE "TREATED" times "POST" INDICATOR  (SAME AS RPS_D)

NEW_RPS_D = matrix(0, nrow(RPS), 1)
NEW_RPS_D = as.numeric((RPS$rps_ever_adopter==1) & (RPS$year>=RPS$rps_implementation_year))

cor(NEW_RPS_D, RPS$rps_D)
mean(RPS$rps_D)
mean(NEW_RPS_D)

# DD REGRESSION, Y = Wind+Solar installed capacity (MW), using lm package
DD_cap1 <- lm(formula = cap_WS_mw ~ rps_D + as.factor(state_name) + as.factor(year), data=RPS)
se_DD_cap1 <- starprep(DD_cap1, stat = c("std.error"), se_type = "HC2", alpha = 0.05) 

DD_cap2 <- lm(formula = cap_WS_mw ~ rps_D + as.factor(state_name) + as.factor(year), data=RPS)
se_DD_cap2 <- starprep(DD_cap2, stat = c("std.error"), se_type = "CR2", clusters=RPS$state_name, alpha = 0.05) 

se_models <- list(se_DD_cap1[[1]], se_DD_cap2[[1]])
stargazer(DD_cap1, DD_cap2, se = se_models, keep=c("rps_D"), type="text")


# DD REGRESSION, Y = Wind+Solar installed capacity (MW), using plm package
DD_cap3 <- plm(cap_WS_mw ~ rps_D, 
               index = c("state_name", "year"), model = "within", effect = "twoways", data = RPS)

# Calculate standard errors (note slightly different procedure with plm package)
se_DD_cap3 <- coeftest(DD_cap3, vcov = vcovHC(DD_cap3, type = "HC2"))[, "Std. Error"]
# Reformat standard errors for stargazer()
se_DD_cap3 <- list(se_DD_cap3)
# Output results with stargazer
stargazer(DD_cap3, keep=c("rps_D"), se = se_DD_cap3, type="text")



