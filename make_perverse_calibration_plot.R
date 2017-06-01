library(ggplot2)
library(dplyr)
library(gridExtra)

# This script produces the plot which illustrates that calibration is an insufficient criterion to 
# guarantee fair risk scores. Checked. 

setwd("~/cost-of-fairness/")

theme_set(
  theme_bw(base_size         = 15) +
    theme(panel.grid.major     = element_blank()) + 
    theme(panel.grid.minor     = element_blank()) + 
    theme(legend.background    = element_rect(fill = 'transparent'))
)

test_calibration = function(df, col){
  # Given a dataframe with risk scores in column "col" and outcome in column "outcome", 
  # returns a dataframe showing whether the risk scores are approximately calibrated in each bin. 
  df$p_hat = plyr::round_any(df[,col], accuracy = .03)
  calibration_df = df %>% 
    group_by(p_hat) %>%
    summarise(true_p = mean(outcome), n = n()) %>%
    filter(n > 5000)
  return(calibration_df)
}

# Simulate data by drawing from a beta distribution. 
lambda = 10
phi = .2
n = 1e6
risk_scores = rbeta(n = n, shape1 = lambda * phi, shape2 = lambda * (1 - phi))
noisy_risk_scores = risk_scores + rnorm(n = n, mean = 0, sd = .5)
outcome =  runif(n = n) < risk_scores
df = data.frame(risk_scores, noisy_risk_scores, outcome)

# recalibrate noisy risk scores
model = glm(outcome ~ noisy_risk_scores, data = df, family = binomial)
df$fitted_noisy_risk_scores = predict(model, df, type = 'response')
density_plot = ggplot(df) + 
  geom_density(aes(x = risk_scores), color = 'black') + 
  geom_density(aes(x = fitted_noisy_risk_scores), color = 'red') + 
  geom_vline(xintercept = .3, linetype = 'dashed') + # draw line for threshold. 
  xlab("Risk score") + ylab("") + 
  scale_y_continuous(expand = c(0, 0), lim = c(0, 30), breaks = NULL) + 
  scale_x_continuous(labels = scales::percent, expand = c(0, 0), lim = c(0, 1.05))

original_calibration = test_calibration(df, 'risk_scores') %>% mutate(noisy = 'not.noisy')
noisy_calibration = test_calibration(df, 'fitted_noisy_risk_scores') %>% mutate(noisy = 'noisy')
df = rbind(original_calibration, noisy_calibration)
calibration_plot = ggplot(df) + 
  geom_point(aes(x = p_hat, y = true_p, size = n, group = noisy, color = noisy), shape = 1) + 
  xlab("Risk score") + 
  ylab("True probability") + 
  scale_x_continuous(labels = scales::percent, limits = c(0, .5)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, .5)) + 
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  theme(legend.position="none") + 
  scale_color_manual(values = c('red', 'black')) + 
  scale_size_area(max_size = 15)
print(calibration_plot)

pdf("figures/calibration_counterexample.pdf", width = 10, height = 5)
grid.arrange(density_plot, calibration_plot, ncol = 2, nrow = 1)
dev.off()

  
