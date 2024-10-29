# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Political Table

# Functions ---------------------------------------------------------------

# uncomment to install
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("scales")
# install.packages("xtable")
library(dplyr)
library(reshape2)
library(scales)
library(xtable)
source("LucidValidationHelperFunctions.R")

# Load Stacked Dataset ----------------------------------------------------

load("PoliticsTableStacked.rdata")

# Means and SEs -----------------------------------------------------------

means <- 
  politics_stacked %>% 
  group_by(survey) %>%
  summarize_all(funs(weighted.mean(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "mean")

ses <- 
  politics_stacked %>%
  group_by(survey) %>%
  summarize_all(funs(weighted.se(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "se")

means_and_ses <- 
  left_join(means, ses) %>%
  filter(variable != "weights") 

dont_multiply <- c("party7", "ideology", "interest")

means_and_ses <- within(means_and_ses,{
  entry <- rep(NA, nrow(means_and_ses))
  entry[variable %in% dont_multiply] <- 
    paste0(format_num(mean[variable %in% dont_multiply], digits = 2), " ", 
           add_parens(se[variable %in% dont_multiply], digits = 2))
  entry[!variable %in%dont_multiply] <- 
    paste0(format_num(100*mean[!variable %in%dont_multiply], digits = 2), " ", 
           add_parens(100*se[!variable %in%dont_multiply], digits = 2))
})

Ns <- 
  politics_stacked %>%
  group_by(survey) %>%
  summarize(n = n(),
            n = comma_format()(n)) %>%
  melt(id.var = "survey", value.name = "entry")

df <- 
  bind_rows(means_and_ses %>% select(survey, variable, entry), Ns)


politics_table <- df %>% dcast(variable ~ survey, value.var = "entry") %>%
  bind_rows(data.frame(variable = c("Opinions", "Behavior", "Traits")))


politics_table <- within(politics_table,{
  
  anes <- gsub(pattern = " \\(NA\\)", replacement = "", anes)
  anes <- gsub(pattern = "NA", replacement = "", anes)
  anes2012 <- gsub(pattern = " \\(NA\\)", replacement = "", anes2012)
  anes_panel <- gsub(pattern = " \\(NA\\)", replacement = "", anes_panel)
  cps <- gsub(pattern = " \\(NA\\)", replacement = "", cps)
  lucid <- gsub(pattern = " \\(NA\\)", replacement = "", lucid)
  mturk <- gsub(pattern = " \\(NA\\)", replacement = "", mturk)
  cps[cps == "NaN (NaN)"] <- ""
  anes[anes == "NaN (NaN)"] <- ""
  anes2012[anes2012 == "NaN (NaN)"] <- ""
  
  variable <- factor(variable, levels = c("Behavior","register","vote", 
                                          "Traits","party7", "ideology", "interest", "knowl_score",
                                          "Opinions", "seniors", "healthcare", "immigration", "gaymarriage", "taxrich", "taxpoor", "n"))
})

politics_table <- 
  politics_table[,c("variable", "lucid", "mturk", "anes_panel", "cps", "anes", "anes2012")] %>%
  arrange(variable)

# Uncomment to print
# politics_table %>%
#   xtable() %>%
#   print(file = "politicsTable.tex")

