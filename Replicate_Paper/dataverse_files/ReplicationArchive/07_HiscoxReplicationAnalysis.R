# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Hiscox Replication Analysis 

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

load("HiscoxReplicationStacked.rdata")

# Regression models -------------------------------------------------------

fit_hiscox_original <- lm(Y_Hiscox ~ Z_Hiscox_expert + Z_Hiscox_valence, data = filter(hiscox_stacked, survey=="hiscox"))
fit_hiscox_lucid <- lm(Y_Hiscox ~ Z_Hiscox_expert + Z_Hiscox_valence, data = filter(hiscox_stacked, survey=="lucid"))
fit_hiscox_mturk <- lm(Y_Hiscox ~ Z_Hiscox_expert + Z_Hiscox_valence, data = filter(hiscox_stacked, survey=="mturk"))
fit_hiscox_tess <- lm(Y_Hiscox ~ Z_Hiscox_expert + Z_Hiscox_valence, data = filter(hiscox_stacked, survey=="tess"))

# Regression table -------------------------------------------------------------------

# sink("hiscoxReplications.tex")
stargazer(fit_hiscox_lucid, fit_hiscox_mturk, fit_hiscox_tess, fit_hiscox_original,
          se = starprep(fit_hiscox_lucid, fit_hiscox_mturk, fit_hiscox_tess, fit_hiscox_original),
          p = starprep(fit_hiscox_lucid, fit_hiscox_mturk, fit_hiscox_tess, fit_hiscox_original, stat = 'p.value'),
          style = "apsr",
          column.labels = c("Lucid", "MTurk", "TESS", "Original"),
          dep.var.labels = c("Support for Free Trade"),
          covariate.labels = c("Expert", "Positive Frame", "Negative Frame", "Both Frames", "Constant (Control)"),
          title = "Hiscox Replications",
          label = "tab: hiscox",
          omit.stat = c("f", "adj.rsq", "ser"),
          notes= c("Robust standard errors are in parentheses.")
          )
# sink()

