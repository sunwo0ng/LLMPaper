# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Asian Disease Replication Analysis and Tables

# Functions ---------------------------------------------------------------

# Uncomment to install
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages("estimatr")

library(dplyr)
library(stargazer)
library(estimatr)

source("LucidValidationHelperFunctions.R")

# Load stacked dataset ----------------------------------------------------

load("AsianDiseaseStacked.rdata")

# OLS Regression Models -------------------------------------------------------

fit_AD_lucid <- lm(pfp ~ mortality_frame, data=filter(ad_stacked, survey == "lucid"))
fit_AD_mturk <- lm(pfp ~ mortality_frame, data=filter(ad_stacked, survey == "mturk"))
fit_AD_original <- lm(pfp ~ mortality_frame, data=filter(ad_stacked, survey == "original"))

# OLS Tables --------------------------------------------------------------

# sink("AsianDiseaseReplications.tex")
stargazer(fit_AD_lucid, fit_AD_mturk, fit_AD_original,
          se = starprep(fit_AD_lucid, fit_AD_mturk, fit_AD_original),
          p = starprep(fit_AD_lucid, fit_AD_mturk, fit_AD_original, stat = "p.value"),
          style = "apsr",
          column.labels = c("Lucid", "MTurk", "Original"),
          dep.var.labels = c("Preference for the Probabilistic Outcome"),
          covariate.labels = c("Mortality Frame", "Intercept"),
          title = "Asian Disease Replications",
          label = "tab: KamSimas",
          column.sep.width = "0pt",
          omit.stat = c("f", "adj.rsq", "ser"),
          notes= c("Robust standard errors are in parentheses.")
          )
# sink()

