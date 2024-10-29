# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Psychological Table

# Functions ---------------------------------------------------------------

# Uncomment to uninstall
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

load("PsychTableStacked.rdata")
# Means and SEs -----------------------------------------------------------

means <- 
  psych_stacked %>% 
  group_by(survey) %>%
  summarize_all(funs(weighted.mean(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "mean")

ses <- 
  psych_stacked %>%
  group_by(survey) %>%
  summarize_all(funs(weighted.se(x = ., w = weights, na.rm = TRUE))) %>%
  melt(id.vars = "survey", value.name = "se")

means <- within(means,{
  mean[variable=="NFC" & survey=="mturk"] <- mean[variable=="NFC" & survey=="mturk_risk"]
  mean[variable=="NTE" & survey=="mturk"] <- mean[variable=="NTE" & survey=="mturk_risk"]
  mean[variable=="ramean" & survey=="mturk"] <- mean[variable=="ramean" & survey=="mturk_risk"]
})
ses <- within(ses,{
  se[variable=="NFC" & survey=="mturk"] <- se[variable=="NFC" & survey=="mturk_risk"]
  se[variable=="NTE" & survey=="mturk"] <- se[variable=="NTE" & survey=="mturk_risk"]
  se[variable=="ramean" & survey=="mturk"] <- se[variable=="ramean" & survey=="mturk_risk"]
})

means_and_ses <- left_join(means, ses) %>%
  filter(variable != "weights", survey!="mturk_risk") 


means_and_ses <- within(means_and_ses,{
  entry <- rep(NA, nrow(means_and_ses))
  entry <- 
    paste0(format_num(mean, digits = 2), " ", 
           add_parens(se, digits = 2))
})

Ns <- 
  psych_stacked %>%
  filter(survey!="mturk_risk")%>%
  group_by(survey) %>%
  summarize(n = n(),
            n = comma_format()(n)) %>%
  melt(id.var = "survey", value.name = "entry")

df <- bind_rows(means_and_ses %>% select(survey, variable, entry), 
                Ns)

psych_table <- df %>% dcast(variable ~ survey, value.var = "entry") %>%
  bind_rows(data.frame(variable = "Big 5 Personality Index"))


psych_table <- within(psych_table,{
  
  anes[anes == "NA (NA)"] <- ""
  anes_panel[anes_panel == "NA (NA)"] <- ""
  anes2012[anes2012 == "NA (NA)"] <- ""
  anes2012[anes2012 == "NaN (NaN)"] <- ""
  cces[cces == "NA (NA)"] <- ""
  cces[cces == "NaN (NaN)"] <- ""
  ccap[ccap == "NA (NA)"] <- ""
  ccap[ccap == "NaN (NaN)"] <- ""
  kam[kam == "NaN (NaN)"] <- ""
  variable <- factor(variable, levels = c("Big 5 Personality Index","agreeableness","conscientiousness", 
                                          "emotionalstability","extraversion", "opennesstoexperiences", "ramean", "NFC",
                                          "NTE", "n"))
})

psych_table <- 
  psych_table[,c("variable", "lucid", "mturk", "cces", "ccap", "anes_panel", "anes", "anes2012","kam" )] %>%
  arrange(variable)

# Uncomment to print
# psych_table %>%
#   xtable() %>%
#   print(file = "psychTable.tex")

