library(readr)
library(dplyr)

# This makes the plot illustrating that the original risk deciles (for all offenders)
# in the ProPublica dataset are approximately calibrated. 

setwd('~/cost-of-fairness/')
df <- read_csv('compas-scores-two-years.csv')

#read in  dataset w/ ProPublica's filtering. 
df = df %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A') %>% 
  mutate(race = plyr::mapvalues(race, from = c("African-American", "Caucasian"), 
                          to = c("Black defendants", "White defendants")))

#Make calibration plot. 
recidivism_by_decile = df %>% 
  group_by(race, decile_score) %>% 
  summarise(recidivism_prob = mean(two_year_recid), 
            n = n(), 
            recidivism_err = 1.96 * sqrt(recidivism_prob * (1 - recidivism_prob) / n)) %>%
  filter(race %in% c('Black defendants', 'White defendants')) %>% 
  ungroup()

by_decile_plot = ggplot(recidivism_by_decile) + 
  geom_line(aes(x = decile_score, y = recidivism_prob, group = race, color = race), size = 1.2) + 
  geom_ribbon(aes(x = decile_score, ymax = recidivism_prob + recidivism_err, ymin = recidivism_prob - recidivism_err, group = race), alpha = .3, fill = 'grey70', colour=NA)+ 
  xlab("Risk score") + 
  ylab("Likelihood of recidivism") + 
  scale_x_continuous(breaks = 1:10, limits = c(1, 10), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1), labels=percent) + 
  theme(legend.text=element_text(size=15), 
        legend.key = element_blank(), 
        legend.title = element_blank(), 
        legend.position=c(.2, .8)) + 
  scale_color_manual(values = c('red', 'blue'))
ggsave(filename = 'figures/recidivism_plot.pdf', width = 8, height = 5)