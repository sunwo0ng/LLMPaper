# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Heterogeneous Treatment Effect Plots

# Functions ---------------------------------------------------------------

# Uncomment to install
# install.packages("ggplot2")
# install.packages("coefplot")
# install.packages("dplyr")

library(ggplot2)
library(coefplot)
library(dplyr)

# Datasets ----------------------------------------------------------------

load("standardizedExperimentsHeterogeneousEffects.rdata")

standardizedExperiments1 <- filter(standardizedExperiments, facet=="Welfare"| facet=="Asian Disease" | facet=="Kam and Simas")
standardizedExperimentsHiscox <- filter(standardizedExperiments, facet=="Expert"| facet=="Positive frame"|facet=="Negative frame" | facet=="Both frames")
standardizedExperimentsBerinsky <- filter(standardizedExperiments, facet=="Rumor only"| facet=="Rumor + Nonpartisan correction"|facet=="Rumor + Republican correction" | facet=="Rumor + Democratic correction")

# Figures -----------------------------------------------------------------

experiments_plot <- 
  ggplot(standardizedExperiments1, aes(x = coef, y = condition, group = Survey, color = Survey, shape= Survey)) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_shape_manual(values=c(16, 15)) +
  scale_color_manual(values=c('#0099CC', '#CC3333')) +
  geom_errorbarh(aes(xmin = li, xmax = ui), position = position_dodgev(height = .5), height = 0) +
  facet_grid(facet_f~., scales = "free", space = "free") +
  theme_bw() +
  theme(axis.title = element_blank(), 
        legend.position="bottom",
        legend.title = element_blank(),
        strip.background = element_blank()) +
  ggtitle("Conditional Average Treatment Effects For Welfare, Asian Disease, and Kam and Simas Replications")

experiments_plotHiscox <- 
  ggplot(standardizedExperimentsHiscox, aes(x = coef, y = condition, group = Survey, color = Survey, shape= Survey)) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_shape_manual(values=c(16, 15)) +
  scale_color_manual(values=c('#0099CC', '#CC3333')) +
  geom_errorbarh(aes(xmin = li, xmax = ui), position = position_dodgev(height = .5), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_shape_manual(values=c(15, 16, 17)) +
  scale_color_manual(values=c('#CC3333', '#0099CC', '#00CC66')) +
  facet_grid(facet_f~., scales = "free", space = "free") +
  theme_bw() +
  theme(axis.title = element_blank(), 
        legend.position="bottom",
        legend.title = element_blank(),
        strip.background = element_blank()) +
  ggtitle("Conditional Average Treatment Effects For Hiscox Free Trade Experiment")

experiments_plotBerinsky <- 
  ggplot(standardizedExperimentsBerinsky, aes(x = coef, y = condition, group = Survey, color = Survey, shape= Survey)) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_shape_manual(values=c(16, 15)) +
  scale_color_manual(values=c('#0099CC', '#CC3333')) +
  geom_errorbarh(aes(xmin = li, xmax = ui), position = position_dodgev(height = .5), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(facet_f~., scales = "free", space = "free") +
  theme_bw() +
  theme(axis.title = element_blank(), 
        legend.position="bottom",
        legend.title = element_blank(),
        strip.background = element_blank()) +
  ggtitle("Conditional Average Treatment Effects For Berinsky Health Care Rumors Experiment")

# Uncomment to save
# ggsave("experiments_standardized_CATE1.pdf", experiments_plot, height = 12.5, width = 8.5)
# ggsave("experiments_standardized_CATEHiscox.pdf", experiments_plotHiscox, height = 12.5, width = 8.5)
# ggsave("experiments_standardized_CATEBerinsky.pdf", experiments_plotBerinsky, height = 12.5, width = 8.5)



