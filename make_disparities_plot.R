library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(lubridate)
library(gridExtra)
library(AUC)
library(tidyr)

# This script makes the plot illustrating the disparities in fairness metrics produced
# by applying equal thresholds. 

theme_set(
  theme_bw(base_size         = 15) +
    theme(panel.grid.major     = element_blank()) + 
    theme(panel.grid.minor     = element_blank()) + 
    theme(legend.background    = element_rect(fill = 'transparent'))
)


make_metrics_plot = function(df, threshold, dataset, old = FALSE){
  # Given a data frame where each row is one person with columns "p", "outcome", "Race"
  # computes standard metrics of fairness + plots disparities. Checked. 
  
  stats = df %>% 
    mutate(above_threshold = p > threshold) %>%
    group_by(Race) %>% 
    summarise(FPR = mean(above_threshold[outcome == 0]),
              `Detention rate,\n5+ priors` = mean(above_threshold[priors_count >= 5]),
              `Detention rate` = mean(above_threshold)) %>%
    gather(key = 'metric', value = 'val', FPR:`Detention rate`)
  
  stats$metric = factor(stats$metric, levels = c('Detention rate,\n5+ priors', 'FPR', 'Detention rate'))
  if(dataset == 'synthetic'){levels(stats$metric) = c('', 'FPR', 'Detention rate')}
  
  p = ggplot(stats) + 
    geom_point(aes(x = val, y = metric, group = Race, color = Race)) + 
    scale_color_manual(values = c('red', 'blue')) +
    xlab("") +
    ylab("") + 
    scale_x_continuous(breaks = c(0, .25, .5, .75, 1), limits = c(0, 1), expand = c(0, 0), labels = scales::percent) + 
    theme(legend.position="none", plot.margin=unit(c(1,0.7,0,0), "cm"))
  
  return(p)
}

make_risk_profile_plot = function(d, title_string, include_legend, threshold){
  # plot a histogram of risk scores. Checked. 
  breaks = c(0, .25, .5, .75, 1)
  p = ggplot(d) + 
    stat_density(aes(x=p, colour=Race), geom="line",position="identity", adjust = 1.5, show.legend = include_legend) + 
    geom_vline(xintercept = threshold, linetype = 'dashed') + 
    scale_y_continuous(expand = c(0, 0), breaks = c(), lim = c(0, 9)) +
    scale_x_continuous(labels = scales::percent, breaks = breaks, lim = c(0, 1),expand = c(0, 0)) +
    ylab("") + 
    xlab('Probability of reoffending') + 
    scale_color_manual(values = c('red', 'blue')) + 
    ggtitle(title_string)  + 
    theme(plot.margin=unit(c(0.5,0.7,.2,0), "cm")) + 
    theme(legend.position=c(.7, .7), legend.title=element_blank(), legend.key = element_blank())
  
  if(include_legend){
    p = p + theme(legend.position=c(.7, .7), legend.title=element_blank())
  }else{
    p = p + theme(legend.position="none")
  }
  return(p)
}

make_main_figure = function(propublica_d, synthetic_d){
  # this makes the entire figure. 
  titles = c('Broward County', 'Simulated')
  threshold = round(as.numeric(quantile(propublica_d$p, .7)), 6) # this threshold detains 30% of defendants. 
  
  propublica_hist = make_risk_profile_plot(propublica_d, titles[1], include_legend = TRUE, threshold)
  synthetic_hist = make_risk_profile_plot(synthetic_d, titles[2], include_legend = FALSE, threshold)
  propublica_metrics = make_metrics_plot(propublica_d, threshold, 'propublica')
  synthetic_metrics = make_metrics_plot(synthetic_d, threshold, 'synthetic')
  
  pdf("figures/main_figure.pdf", width = 8, height = 5)
  grid.arrange(propublica_hist, synthetic_hist, propublica_metrics, synthetic_metrics, ncol = 2, nrow = 2)
  dev.off()
}

# Load in dataframes and make plot. 
setwd('~/cost-of-fairness/')

# Propublica data
propublica_d = read_csv('improved_scores.csv') %>%
  rename(outcome = two_year_recid, p = score, Race = race) %>%
  mutate(Race = paste(Race, 'defendants')) %>%
  select(Race, p, outcome, priors_count)


# Simulated data. Draw from two betas with different lambdas. 
phi = .15
lambda1 = 10
lambda2 = 50
n_synthetic = 50000
p = c(rbeta(n = n_synthetic, shape1 = phi * lambda1, shape2 = (1 - phi) * lambda1),
      rbeta(n = n_synthetic, shape1 = phi * lambda2, shape2 = (1 - phi) * lambda2))
outcome = runif(n = n_synthetic * 2) < p
Race = c(rep('Black', n_synthetic), rep('White', n_synthetic))
synthetic_d = data.frame(p, Race, outcome, priors_count = 0)

make_main_figure(propublica_d, synthetic_d)

