# Replication archive for: 
# Coppock, Alexander and Oliver McClellan. 
# "Validating the Demographic, Political, Psychological, and Experimental Results 
# Obtained from a New Source of Online Survey Respondents."  
# Research & Politics, forthcoming.

# Standardized Means Plots

# Functions ---------------------------------------------------------------

# Uncomment to install
# install.packages("ggplot2")
# install.packages("coefplot")
library(ggplot2)
library(coefplot)

# Load stacked datasets ----------------------------------------------------

load("StandardizedDemos.rdata")
load("StandardizedExperiments.rdata")

experiments_standardized$facet_f <-
  factor(
    experiments_standardized$facet,
    levels = c('Welfare', 'Asian Disease', 'Kam and Simas', 'Hiscox', 'Berinsky')
  )

demos_plot <- 
  ggplot(demos_standardized, aes(x = mean, y = Variable, group = Survey, color = Survey, shape = Survey)) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_errorbarh(aes(xmin = li, xmax = ui),position = position_dodgev(height = .5), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_shape_manual(values=c(15, 16, 17)) +
  scale_color_manual(values=c('#CC3333', '#0099CC', '#00CC66')) +
  theme_bw() +
  facet_grid(facet~., scales = "free_y", space = "free_y") +
  theme(axis.title = element_blank(), 
        legend.position="bottom",
        legend.title = element_blank(),
        strip.background = element_blank()) +
  coord_cartesian(xlim = c(-1, 1)) +
  ggtitle("Standardized Means for Demographic Variables")

# Experiments Plot

experiments_plot <- 
  ggplot(experiments_standardized, aes(x = coef, y = Variable, group = Survey, color = Survey, shape = Survey)) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_errorbarh(aes(xmin = li, xmax = ui), position = position_dodgev(height = .5), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_shape_manual(values=c(17, 16, 15)) +
  scale_color_manual(values=c('#00CC66', '#0099CC', '#CC3333')) +
  facet_grid(facet_f~., scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(axis.title = element_blank(), 
        legend.position="bottom",
        legend.title = element_blank(),
        strip.background = element_blank()) +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  ggtitle("Standardized Treatment Effects For Experimental Replications")

# Uncomment to save
# ggsave("StandardizedDemos.pdf", demos_plot, height = 10, width = 8)
# ggsave("StandardizedExperiments.pdf", experiments_plot, height = 12.5, width = 8.5)
