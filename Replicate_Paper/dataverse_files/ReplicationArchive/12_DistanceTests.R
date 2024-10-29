# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

rm(list = ls())

# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("rsample")
# install.packages("xtable")

library(tidyverse)
library(reshape2)
library(rsample)
library(xtable)

standardized_stacked_st <- read_rds("standardized_stacked_st.rds")

my_fun <- . %>%
  group_by(survey) %>%
  summarise_all(funs(weighted.mean(
    x = ., w = weights, na.rm = TRUE
  ))) %>%
  melt(value.name = "mean",
       variable.name = "variable",
       id.var = "survey") %>%
  dcast(variable ~ survey, value.var = "mean") %>%
  transmute(variable,
            lucid_is_closer = abs(mturk - anes2012) - abs(lucid - anes2012))


est <- standardized_stacked_st %>% my_fun()

boots <- bootstraps(standardized_stacked_st, 100)
ses <-
  boots %>%
  mutate(model = map(splits, ~ (analysis(.x) %>% my_fun))) %>%
  select(id, model) %>%
  unnest %>%
  group_by(variable) %>%
  summarize(se = sd(lucid_is_closer))


varnames_demos <-
  c(
    "Female",
    "Education",
    "Age",
    "Mean income",
    "White",
    "Black",
    "Hispanic",
    "Asian",
    "Native American",
    "Northeast",
    "Midwest",
    "South",
    "West",
    "Voter registration",
    "Voter turnout",
    "Party ID",
    "Ideology",
    "Political Interest",
    "Extraverted",
    "Agreeable",
    "Conscientious",
    "Stable",
    "Open"
  )

dat <-
  left_join(est, ses) %>%
  filter(variable != "weights") %>%
  mutate(varnames = varnames_demos,
         p = pnorm(q = 2 * (1 - abs(lucid_is_closer / se)), lower.tail = TRUE)) %>%
  select(varnames, lucid_is_closer, se, p) %>%
  filter(!is.na(se))

# Uncomment to print
# dat %>%
#   xtable(digits = 3) %>%
#   print.xtable(
#     include.colnames = FALSE,
#     include.rownames = FALSE,
#     only.contents = TRUE,
#     hline.after = c(),
#     file = "abs_demo_distance.tex"
#   )

dat %>%
  summarize(sum(lucid_is_closer > 0),
            sum(lucid_is_closer > 0 & p < 0.05),
            sum(lucid_is_closer < 0 & p < 0.05),
            n = n())
