# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Survey Behavior Table

# Functions ---------------------------------------------------------------

# Uncomment to install
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("scales")
# install.packages("xtable")
# install.packages("purrr")
# install.packages("matrixStats")

library(dplyr)
library(reshape2)
library(scales)
library(xtable)
library(purrr)
library(matrixStats)
source("LucidValidationHelperFunctions.R")

# Load Stacked Dataset ----------------------------------------------------

load("SurveyBehavior.rdata")

# Means and SEs -----------------------------------------------------------

means <- 
  lucid_survey %>% 
  group_by(survey) %>%
  summarize_all(funs(weighted.mean(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "mean")

ses <- 
  lucid_survey %>%
  group_by(survey) %>%
  summarize_all(funs(weighted.se(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "se")

means_and_ses <- left_join(means, ses) %>%
  filter(variable != "weights") 

dont_multiply <- c("numsurvey", "numsurvey_trim", "surveycompamount", "surveycompamount_trim")

means_and_ses <- within(means_and_ses,{
  entry <- rep(NA, nrow(means_and_ses))
  entry[variable %in% dont_multiply] <- 
    paste0(format_num(mean[variable %in% dont_multiply], digits = 2), " ", 
           add_parens(se[variable %in% dont_multiply], digits = 2))
  entry[!variable %in% dont_multiply] <- 
    paste0(format_num(100*mean[!variable %in%dont_multiply], digits = 2), " ", 
           add_parens(100*se[!variable %in%dont_multiply], digits = 2))
})

Ns <- 
lucid_survey %>%
  group_by(survey) %>%
  summarize(n = n(),
            n = comma_format()(n)) %>%
  melt(id.var = "survey", value.name = "entry")

df <- bind_rows(means_and_ses %>% select(survey, variable, entry), 
                Ns)

survey_table <- df %>% dcast(variable ~ survey, value.var = "entry") %>%
  bind_rows(data.frame(variable = c("Survey Location", "Survey Compensation Type")))

survey_table <- within(survey_table,{
  
  variable <- factor(variable, levels = c(
    "numsurvey", "numsurvey_trim",
    "Survey Location", "surveylocation_home", "surveylocation_work", "surveylocation_public", "surveylocation_other",
    "Survey Compensation Type", "surveycomptype_dollars", "surveycomptype_points", "surveycomptype_bitcoin", "surveycomptype_currency","surveycomptype_nocomp",
    "surveycompamount", "surveycompamount_trim",
    "n"))
  })
    
survey_table <- 
  survey_table[,c("variable", "lucid")] %>%
  arrange(variable)

# Uncomment to print
# survey_table %>%
#   xtable() %>%
#   print(file = "drafts/survey_table_for_edit.tex")

