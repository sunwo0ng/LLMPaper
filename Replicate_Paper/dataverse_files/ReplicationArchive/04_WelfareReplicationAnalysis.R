# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.
# Welfare Replications

# Functions ---------------------------------------------------------------

# Uncomment to install packages
# install.packages("dplyr")
# install.packages("stargazer")
# install.packges("estimatr")

library(dplyr)
library(stargazer)
library(estimatr)
source(file = "LucidValidationHelperFunctions.R")

# Load Stacked Dataset ----------------------------------------------------

load("WelfareReplicationStacked.rdata")

fit_lucid <- lm(welfare_recode ~ welfare, data = filter(welfare_stacked, survey == "lucid"))
fit_mturk <- lm(welfare_recode ~ welfare, data = filter(welfare_stacked, survey == "mturk"))
fit_GSS84 <- lm(welfare_recode ~ welfare, data = filter(welfare_stacked, survey == "GSS84"))
fit_GSS14 <- lm(welfare_recode ~ welfare, data = filter(welfare_stacked, survey == "GSS14"))

# sink("welfare_replications.tex")
stargazer(fit_lucid, fit_mturk, fit_GSS84, fit_GSS14,
          se = starprep(fit_lucid, fit_mturk, fit_GSS84, fit_GSS14),
          p = starprep(fit_lucid, fit_mturk, fit_GSS84, fit_GSS14, stat = "p.value"),
          style = "apsr",
          column.labels = c("Lucid", "MTurk", "GSS 1984", "GSS 2014"),
          dep.var.labels = c("Support for Welfare/Assistance"),
          covariate.labels = c("Treatment: 'assistance'", "Treatment: 'caring'", "Constant (Control)"),
          title = "Welfare Replications",
          label = "tab: welfare",
          omit.stat = c("f", "adj.rsq", "ser"))
# sink()
