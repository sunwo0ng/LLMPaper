# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Kam and Simas Replication Analysis and Tables

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

load("KamSimasReplicationStacked.rdata")

# OLS Regression Models ---------------------------------------------------

fit_kam_kam <- lm(pfp1 ~ mortalityfirst + ramean, data = filter(kam_stacked, survey == "kam"))
fit_kam_kam_ctrl <- lm(pfp1 ~ mortalityfirst + ramean + female_imp + age01_imp + education01_7_imp + income01_imp + ideology01_imp, data = filter(kam_stacked, survey == "kam"))
fit_kam_kam_X <- lm(pfp1 ~ mortalityfirst + ramean + mortalityfirst * ramean, data = filter(kam_stacked, survey == "kam"))

fit_kam_lucid <- lm(pfp1 ~ mortalityfirst + ramean, data = filter(kam_stacked, survey == "lucid"))
fit_kam_lucid_ctrl <- lm(pfp1 ~ mortalityfirst + ramean + female_imp + age01_imp + education01_7_imp + income01_imp + ideology01_imp + missing_covs, 
                         data = filter(kam_stacked, survey == "lucid"))
fit_kam_lucid_X <- lm(pfp1 ~ mortalityfirst + ramean + mortalityfirst * ramean, data = filter(kam_stacked, survey == "lucid"))

fit_kam_mturk <- lm(pfp1 ~ mortalityfirst + ramean, data = filter(kam_stacked, survey == "mturk_risk"))
fit_kam_mturk_ctrl <- lm(pfp1 ~ mortalityfirst + ramean + female_imp + age01_imp + education01_7_imp + income01_imp + ideology01_imp + missing_covs, 
                         data = filter(kam_stacked, survey == "mturk_risk"))
fit_kam_mturk_X <- lm(pfp1 ~ mortalityfirst + ramean + mortalityfirst * ramean, data = filter(kam_stacked, survey == "mturk_risk"))

# OLS Table --------------------------------------------------------------


# sink("KamSimasReplications.tex")
stargazer(fit_kam_lucid, fit_kam_lucid_ctrl, fit_kam_lucid_X, fit_kam_mturk, fit_kam_mturk_ctrl, fit_kam_mturk_X, fit_kam_kam, fit_kam_kam_ctrl, fit_kam_kam_X,
          se = starprep(fit_kam_lucid, fit_kam_lucid_ctrl, fit_kam_lucid_X, fit_kam_mturk, fit_kam_mturk_ctrl, fit_kam_mturk_X, fit_kam_kam, fit_kam_kam_ctrl, fit_kam_kam_X),
          p = starprep(fit_kam_lucid, fit_kam_lucid_ctrl, fit_kam_lucid_X, fit_kam_mturk, fit_kam_mturk_ctrl, fit_kam_mturk_X, fit_kam_kam, fit_kam_kam_ctrl, fit_kam_kam_X, stat = "p.value"),
          style = "apsr",
          omit = c("female|age|education|income|ideology|missing_covs"),
          omit.labels = "Covariates",
          column.labels = c("Lucid", "MTurk", "Original"),
          column.separate = c(3, 3, 3),
          dep.var.labels = c("Preference for the Probabilistic Outcome"),
          covariate.labels = c("Mortality Frame", "Risk Acceptance", "RA X MF", "Intercept"),
          title = "Kam and Simas (2010) Replication",
          label = "tab: KamSimas",
          column.sep.width = "0pt",
          float = FALSE,
          omit.stat = c("f", "adj.rsq", "ser"),
          notes = c("Robust standard errors are in parentheses.")
)
# sink()

# GLM models (Appendix 1.3.4)--------------------------------------------------------------

glm_fit_kam_kam <- glm(pfp1 ~ mortalityfirst + ramean, 
                       family = binomial(link = "probit"), 
                       data = filter(kam_stacked, survey == "kam"))

glm_fit_kam_kam_ctrl <- glm(pfp1 ~ mortalityfirst + ramean + female_imp + age01_imp + education01_7_imp + income01_imp + ideology01_imp, 
                            family = binomial(link = "probit"), 
                            data = filter(kam_stacked, survey == "kam"))

glm_fit_kam_kam_X <- glm(pfp1 ~ mortalityfirst + ramean + mortalityfirst * ramean, 
                         family = binomial(link = "probit"), 
                         data = filter(kam_stacked, survey == "kam"))

glm_fit_kam_lucid <- glm(pfp1 ~ mortalityfirst + ramean, 
                         family = binomial(link = "probit"), 
                         data = filter(kam_stacked, survey == "lucid"))

glm_fit_kam_lucid_ctrl <- glm(pfp1 ~ mortalityfirst + ramean + female_imp + age01_imp + education01_7_imp + income01_imp + ideology01_imp, 
                              family = binomial(link = "probit"), 
                              data = filter(kam_stacked, survey == "lucid"))

glm_fit_kam_lucid_X <- glm(pfp1 ~ mortalityfirst + ramean + mortalityfirst * ramean, 
                           family = binomial(link = "probit"), 
                           data = filter(kam_stacked, survey == "lucid"))

glm_fit_kam_mturk <- glm(pfp1 ~ mortalityfirst + ramean, 
                         family = binomial(link = "probit"), 
                         data = filter(kam_stacked, survey == "mturk_risk"))

glm_fit_kam_mturk_ctrl <- glm(pfp1 ~ mortalityfirst + ramean + female_imp + age01_imp + education01_7_imp + income01_imp + ideology01_imp,
                              family = binomial(link = "probit"),
                              data = filter(kam_stacked, survey == "mturk_risk"))

glm_fit_kam_mturk_X <- glm(pfp1 ~ mortalityfirst + ramean + mortalityfirst * ramean,
                            family = binomial(link = "probit"),
                            data = filter(kam_stacked, survey == "mturk_risk"))


# GLM Table ---------------------------------------------------------------


# sink("KamSimasReplicationsGlm.tex")
stargazer(glm_fit_kam_lucid, glm_fit_kam_lucid_ctrl, glm_fit_kam_lucid_X, glm_fit_kam_mturk, glm_fit_kam_mturk_ctrl, glm_fit_kam_mturk_X, glm_fit_kam_kam, glm_fit_kam_kam_ctrl, glm_fit_kam_kam_X,
          style = "apsr",
          omit = c("female|age|education|income|ideology|missing_covs"),
          omit.labels = "Covariates",
          column.labels = c("Lucid", "MTurk", "Original"),
          column.separate = c(3, 3, 3),
          dep.var.labels = c("Preference for the Probabilistic Outcome"),
          covariate.labels = c("Mortality Frame", "Risk Acceptance", "RA X MF", "Intercept"),
          title = "Kam and Simas (2010) Replication",
          label = "tab: KamSimas",
          column.sep.width = "0pt",
          float = FALSE,
          omit.stat = c("f", "adj.rsq", "ser")
)
# sink()
