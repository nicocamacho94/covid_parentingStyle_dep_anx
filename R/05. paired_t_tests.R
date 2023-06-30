# title: Repeated Measures ANOVA - including outliers - Karina's thesis project
# author: Nicolas L. Camacho
# date created: 02/04/2021
# date updated: 06/30/2023

# set packages
library(tidyverse)
library(see)
library(ggpaired)
library(ggpubr)
library(rstatix)

# setwd("X:/Gaffrey/Lab/NTREC/Nicolas/parenting_and_emotreg/analysis_data/final")
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
withCOV_data <- read_csv("de_ident_t1withCOV_data_voi_nona.csv") 

# split data into variables
## PAS
pas_withCOV <- withCOV_data %>% dplyr::select(., subid, contains("pasTot"))
## PFC
pfc_withCOV <- withCOV_data %>% dplyr::select(., subid, contains("pfcTot"))

# make long versions of variable-specific df's
## PAS 
pas_long <- pas_withCOV %>% rename(., pas_base = pasTot_T1, pas_covid = pasTot_cov) %>% 
  gather(., key = "measure", value = "score", pas_base, pas_covid) %>% 
  arrange(., subid)
## PFC
pfc_long <- pfc_withCOV %>% rename(., pfc_base = pfcTot_T1, pfc_covid = pfcTot_cov)  %>% 
  gather(., key = "measure", value = "score", pfc_base, pfc_covid) %>% 
  arrange(., subid)

# conduct paired samples t-test
## PAS
pas_t_test <- t_test(score ~ measure, data = pas_long, paired = T) %>% add_significance()
## PFC
pfc_t_test <- t_test(score ~ measure, data = pfc_long, paired = T) %>% add_significance()

# calculate effect size (Cohen's d) for the paired samples t-tests
## PAS
pas_d <- cohens_d(score ~ measure, data = pas_long, paired = T)
## PFC
pfc_d <- cohens_d(score ~ measure, data = pfc_long, paired = T)

# plot the data for each variable
## PAS
pas_t_test <- 
  ggplot(data = pas_long, aes(x = measure, y = score, fill = measure)) +
  geom_violinhalf(alpha=0.4, position = position_dodge(width = .75),
                  color="black", flip = 1, show.legend = F) +
  geom_line(aes(group = subid), color = "black", lwd = 0.05, alpha = 0.2) +
  geom_boxplot(outlier.shape = NA, color="black", width = 0.1, alpha = 0.7,
               show.legend = F) +
  geom_jitter(shape = 21, size = 1, color="black", alpha = 1, width = 0.04,
              show.legend = F) + 
  theme_light() + 
  labs(y = 'PAS Mean Scores', x = 'Timepoint') +
  scale_x_discrete(labels=c("pas_base" = "Baseline", "pas_covid" = "COVID Follow-Up")) +
  theme(text = element_text(family = "sans serif"),
        axis.ticks.y = element_blank()) + ylim(-0.01, 2) # +
  # annotate("text", x = 1.5, y = 1.885,
  #          label = "Paired Samples t-test\nt(105) = 6.11, p < .001",
  #          colour = "black", family = "serif") + coord_fixed(ratio = 0.75)
ggsave(pas_t_test, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/pas_t_test.png")

## PFC
pfc_t_test <- 
  ggplot(data = pfc_long, aes(x = measure, y = score, fill = measure)) +
  geom_violinhalf(alpha=0.4, position = position_dodge(width = .75),
                  color="black", flip = 1, show.legend = F) +
  geom_line(aes(group = subid), color = "black", lwd = 0.05, alpha = 0.2) +
  geom_boxplot(outlier.shape = NA, color="black", width = 0.1, alpha = 0.7,
               show.legend = F) +
  geom_jitter(shape = 21, size = 1, color="black", alpha = 1, width = 0.04,
              show.legend = F) + 
  theme_light() + 
  labs(y = 'PFC-S Mean Scores', x = 'Timepoint') +
  scale_x_discrete(labels=c("pfc_base" = "Baseline", "pfc_covid" = "COVID Follow-Up")) +
  theme(text = element_text(family = "sans serif"),
        axis.ticks.y = element_blank()) + ylim(-0.01, 2) # +
  # annotate("text", x = 1.5, y = 1.885,
  #          label = "Paired Samples t-test\nt(105) = 4.62, p < .001",
  #          colour = "black", family = "serif") + coord_fixed(ratio = 0.75)
ggsave(pfc_t_test, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/pfc_t_test.png")

# Put the t-test plots together and save them
both_plots <- ggarrange(pas_t_test, pfc_t_test, labels = c("a", "b"), 
                        ncol = 2, nrow = 1)
ggsave(both_plots, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/both_t_tests.png")

