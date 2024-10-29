# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Demographics Table


# Packages ----------------------------------------------------------------

# uncomment to install
# install.packages("reshape2")
# install.packages("scales")
# install.packages("xtable")
# install.packages("purrr")
# install.packages("dplyr")
# install.packages("matrixStats")
library(reshape2)
library(scales)
library(xtable)
library(purrr)
library(dplyr)
library(matrixStats)
source("LucidValidationHelperFunctions.R")

# Load Stacked Dataset ----------------------------------------------------

load("DemosTableStacked.rdata")

# Means and SEs -----------------------------------------------------------

means <- 
  demos_stacked %>% 
  filter(survey != "cps_income") %>%
  select(-contains("income")) %>%
  group_by(survey) %>%
  summarize_all(funs(weighted.mean(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "mean")

ses <- 
  demos_stacked %>%
  filter(survey != "cps_income") %>%
  select(-contains("income")) %>%
  group_by(survey) %>%
  summarize_all(funs(weighted.se(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "se")


income_mean_df <- 
  demos_stacked %>%
  group_by(survey) %>%
  summarize(income_mean = dollar_format()(weighted.mean(income,w = weights, na.rm = TRUE)),
            income_median = dollar_format()(matrixStats::weightedMedian(income,w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "mean")

income_ses_df <- 
  demos_stacked %>%
  group_by(survey) %>%
  summarize(income_mean = dollar_format()(weighted.se(income,w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "se")


income_df <- 
  left_join(income_mean_df, income_ses_df) %>%
  filter(survey != "cps")

income_df <- within(income_df, {
  survey[survey == "cps_income"] <- "cps"
  entry <- paste0(mean, " (", se, ")")
})


means_and_ses <- left_join(means, ses) %>%
  filter(variable != "weights") 
dont_multiply <- c("age", "education", "income_mean", "income_median")

means_and_ses <- within(means_and_ses,{
  entry <- rep(NA, nrow(means_and_ses))
  entry[variable %in% c("age", "education")] <- 
    paste0(format_num(mean[variable %in% c("age", "education")], digits = 2), " ", 
           add_parens(se[variable %in% c("age", "education")], digits = 2))
  entry[!variable %in%dont_multiply] <- 
    paste0(format_num(100*mean[!variable %in%dont_multiply], digits = 2), " ", 
           add_parens(100*se[!variable %in%dont_multiply], digits = 2))
})

Ns <- 
demos_stacked %>%
  group_by(survey) %>%
  summarize(n = n(),
            n = comma_format()(n)) %>%
  melt(id.var = "survey", value.name = "entry") %>%
  filter(survey != "cps_income")

#150,799


df <- 
  bind_rows(means_and_ses %>% select(survey, variable, entry), 
                income_df %>% select(survey, variable, entry),
                Ns)


demo_table <- df %>% dcast(variable ~ survey, value.var = "entry") %>%
  bind_rows(data.frame(variable = c("Race", "Region")))


demo_table <- within(demo_table,{
  
  anes <- gsub(pattern = " \\(NA\\)", replacement = "", anes)
  anes2012 <- gsub(pattern = " \\(NA\\)", replacement = "", anes2012)
  anes_panel <- gsub(pattern = " \\(NA\\)", replacement = "", anes_panel)
  cps <- gsub(pattern = " \\(NA\\)", replacement = "", cps)
  lucid <- gsub(pattern = " \\(NA\\)", replacement = "", lucid)
  mturk <- gsub(pattern = " \\(NA\\)", replacement = "", mturk)
  mturk[mturk == "NaN (NaN)"] <- ""
  
  variable <- factor(variable, levels = c("female","age","education","income_mean","income_median",
                                          "Race", "race_white","race_black","race_hispanic","race_asian","race_native",
                                          "Region", "region_northeast","region_midwest","region_south","region_west", "n"))
})

demo_table <- 
  demo_table[,c("variable", "lucid", "mturk", "anes_panel", "cps", "anes", "anes2012")] %>%
  arrange(variable)

# Uncomment to save
# demo_table %>%
#   xtable() %>%
#   print(file = "demosTable.tex")

