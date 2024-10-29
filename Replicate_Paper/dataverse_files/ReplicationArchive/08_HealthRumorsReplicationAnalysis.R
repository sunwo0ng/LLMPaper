# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Health Care Rumors Analysis and Graphs

# Functions ---------------------------------------------------------------

# Uncomment to install
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages("estimatr")
# install.packages("ggplot2")
# install.packages("coefplot")

library(dplyr)
library(stargazer)
library(estimatr)
library(ggplot2)
library(coefplot)
source("LucidValidationHelperFunctions.R")

# Load stacked dataset ----------------------------------------------------

load("HealthRumorsReplicationStacked.rdata")

# Regression models -------------------------------------------------------

fit_deathpanel_rumor_lucid <-
  lm(Y_deathpanel ~ Z_health_rumor,
     data = filter(rumor_stacked, survey == "lucid"))
fit_deathpanel_rumor_ssi <-
  lm(Y_deathpanel ~ Z_health_rumor,
     data = filter(rumor_stacked, survey == "ssi"))


# Regression table -------------------------------------------------------------------

# sink("HealthRumorsReplications.tex")
stargazer(fit_deathpanel_rumor_lucid,  fit_deathpanel_rumor_ssi,
          se = starprep(fit_deathpanel_rumor_lucid, fit_deathpanel_rumor_ssi),
          p = starprep(fit_deathpanel_rumor_lucid, fit_deathpanel_rumor_ssi, stat = "p.value"),
          style = "apsr",
          column.labels = c("Lucid", "Original"),
          dep.var.labels = "Death Panel Rumor Belief (-1 to 1)",
          covariate.labels = c("Rumor Only", "Rumor + Nonpartisan Correction", "Rumor + Republican Correction", "Rumor + Democratic Correction"),
          title = "Berinsky (2016) Replication",
          label = "tab: rumors",
          omit.stat = c("f", "adj.rsq", "ser")
          )
# sink()
