# title: Diagnostics Script - data for Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/2/2021
# date updated: 8/11/2022

# set packages
library(tidyverse)
library(VIM)
library(Hmisc)
library(ggcorrplot)
library(apaTables)
library(rstatix)
library(ggpubr)
library(MVN)
library(psych)
library(car)
library(ggfortify)

# set working directory
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
withCOV_nona <- read_csv("t1withCOV_data_voi_nona.csv")

# show missing data
png("C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/missing_data.png")
aggr(dplyr::select(withCOV_nona, childSex_T1, childAge_T1, childAge_cov, months_T1_cov, i2n_T1, 
                   lec_everBad_mean_T1, pasTot_T1, pasTot_cov, pfcTot_T1, pfcTot_cov, 
                   epii_negAllExp_cov, atvSelf_T1, negAffect_T1), 
     combined = T, numbers = T, prop = F, only.miss = F, cex.axis = 0.6,
     labels = c("Sex T1", "Age T1", "Age T2", "T1-COV", "i2n T1", "LEC T1", "PAS T1",
                "PAS COV", "PFC T1", "PFC COV", "EPII All",
                "ATV T1", "NegAff T1"))
dev.off()

# make data long to make distribution plots
withCOV_long <- gather(withCOV_nona, variable, value, childAge_T1, childAge_cov, months_T1_cov, 
                       i2n_T1, lec_everBad_mean_T1, pasTot_T1, pasTot_cov, pfcTot_T1, pfcTot_cov, 
                       epii_negAllExp_cov, atvSelf_T1, negAffect_T1)

## Sex
withCOV_nona %>% 
  ggplot(., aes(x = as.factor(childSex_T1))) +
  geom_bar(fill = c("blue", "pink"), alpha = 0.8) +
  ggtitle(" Sex breakdown T1, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/child_sex_T1.png")

## T1 - COV timeframe
withCOV_long %>% 
  filter(variable == "months_T1_cov") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE)  +
  facet_wrap(~variable, ncol = 2) +
  ggtitle("Months b/w T1 and COVID Timepoints, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/months_T1_cov.png")

## Age
withCOV_long %>% 
  filter(variable == "childAge_T1" | variable == "childAge_cov") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE)  +
  facet_wrap(~variable, ncol = 2) +
  ggtitle("Age, N = 106\nIn years")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/child_age.png")

## Income-to-needs
withCOV_long %>% 
  filter(variable == "i2n_T1") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  ggtitle("Income-to-needs ratio T1, N = 104")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/i2n_T1.png")

## LEC Ever bad score
withCOV_long %>% 
  filter(variable == "lec_everBad_mean_T1") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  ggtitle("LEC Ever Bad Score T1, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/lec_everBad_T1.png")

## PAS
withCOV_long %>% 
  filter(variable == "pasTot_cov" | variable == "pasTot_T1") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE)  +
  facet_wrap(~variable, ncol = 2) +
  ggtitle("PAS, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/pas.png")

## PFC
withCOV_long %>% 
  filter(variable == "pfcTot_cov" | variable == "pfcTot_T1") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE)  +
  facet_wrap(~variable, ncol = 2) +
  ggtitle("PFC, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/pfc.png")

## Styles
withCOV_long %>% 
  filter(variable == "atvSelf_T1") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  ggtitle("Authoritative Style, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/atv_T1.png")

## EPII
withCOV_long %>% 
  filter(variable == "epii_negAllExp_cov") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  ggtitle("EPII, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/epii_cov.png")

## PANAS (parent) - Negative Affect
withCOV_long %>% 
  filter(variable == "negAffect_T1") %>%
  ggplot(., aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  ggtitle("PANAS (parent) - Negative Affect, N = 106")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/p_panas_negaffect_T1.png")

# correlation matrix
correl_df <- dplyr::select(withCOV_nona, childSex_T1, childAge_T1, childAge_cov, months_T1_cov,                    i2n_T1, lec_everBad_mean_T1, pasTot_T1, pasTot_cov, 
                           pfcTot_T1, pfcTot_cov, epii_negAllExp_cov, atvSelf_T1, negAffect_T1)
correl <- rcorr(as.matrix(correl_df))
colnames(correl$r) <- c("Sex T1", "Age T1", "Age T2", "T1-COV", "i2n T1", "LEC T1", "PAS T1",
                        "PAS COV", "PFC T1", "PFC COV", "EPII All",
                        "ATV T1", "NegAff T1")
rownames(correl$r) <- c("Sex T1", "Age T1", "Age T2", "T1-COV", "i2n T1", "LEC T1", "PAS T1",
                        "PAS COV", "PFC T1", "PFC COV", "EPII All",
                        "ATV T1", "NegAff T1")
colnames(correl$P) <- c("Sex T1", "Age T1", "Age T2", "T1-COV", "i2n T1", "LEC T1", "PAS T1",
                        "PAS COV", "PFC T1", "PFC COV", "EPII All",
                        "ATV T1", "NegAff T1")
rownames(correl$P) <- c("Sex T1", "Age T1", "Age T2", "T1-COV", "i2n T1", "LEC T1", "PAS T1",
                        "PAS COV", "PFC T1", "PFC COV", "EPII All",
                        "ATV T1", "NegAff T1")

ggcorrplot(correl$r, type = "lower", method = "square", 
           p.mat = correl$P, 
           sig.level = 0.05, insig = "pch", lab = TRUE, lab_size = 3, tl.cex = 10) +
  scale_fill_gradient2(low = "#41598F", high = "#C8102E", mid = "#FFFFFF", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  ggtitle("Pearson Correlation of Main Vars (p < .05)") +
  coord_fixed()
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/corr_plot_1.png")

ggcorrplot(correl$r, type = "lower", method = "square", 
           p.mat = correl$P, 
           sig.level = 0.05, insig = "blank", lab = TRUE, lab_size = 3, tl.cex = 10) +
  scale_fill_gradient2(low = "#41598F", high = "#C8102E", mid = "#FFFFFF", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  ggtitle("Pearson Correlation of Main Vars (p < .05)") +
  coord_fixed()
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/corr_plot_2.png")

apa.cor.table(correl_df, table.number = 1, show.sig.stars = T,
              filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/corr_plot_apa.doc")

# check for sex influences on other variables
sex_long <- dplyr::select(withCOV_nona, childSex_T1, childAge_T1, childAge_cov, months_T1_cov, i2n_T1, 
                          lec_everBad_mean_T1, pasTot_T1, pasTot_cov, pfcTot_T1, pfcTot_cov, 
                          epii_negAllExp_cov, atvSelf_T1, negAffect_T1) %>% 
  pivot_longer(-childSex_T1, names_to = "vars", values_to = "value")
sex_long$childSex_T1 <- as.factor(sex_long$childSex_T1)

sex_stat.test <- sex_long %>% 
  group_by(vars) %>% 
  t_test(value ~ childSex_T1) %>% 
  adjust_pvalue(method = "fdr") %>% 
  add_significance() %>% 
  add_xy_position(x = "childSex_T1")

ggplot(data = na.omit(sex_long), aes(x = childSex_T1, y = value)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~vars) +
  stat_pvalue_manual(sex_stat.test, label = "p.adj.signif") +
  ggtitle("Sex differences across main variables (FDR corrected)")
ggsave(last_plot(), 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/sex_diff.png")

# assess multivariate normality
mvn(na.omit(dplyr::select(withCOV_nona, childSex_T1, childAge_T1, childAge_cov, months_T1_cov,
                          i2n_T1, lec_everBad_mean_T1, pasTot_T1, pasTot_cov, 
                          pfcTot_T1, pfcTot_cov, epii_negAllExp_cov, atvSelf_T1, negAffect_T1)),
    mvnTest = "mardia", alpha = 0.5, multivariatePlot = "qq")

# get general descriptives of the main variables
descriptives <- as.data.frame(psych::describe(dplyr::select(withCOV_nona, childSex_T1, childAge_T1, childAge_cov, i2n_T1, 
                       months_T1_cov, lec_everBad_mean_T1, pasTot_T1, pasTot_cov, pfcTot_T1, pfcTot_cov, 
                       epii_negAllExp_cov, atvSelf_T1, negAffect_T1))) # most are fine, LEC not good
descriptives <- rownames_to_column(descriptives, "var_names")
write_excel_csv(descriptives,
                file = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/descriptives.csv")

# create models; assess VIFs and diagnostic plots
withCOV_nona$childSex_T1 <- as.factor(withCOV_nona$childSex_T1)
## anx / atv
anxAtv_lm <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + 
                    epii_negAllExp_cov + atvSelf_T1 + negAffect_T1, data = withCOV_nona)
vif(anxAtv_lm)
autoplot(anxAtv_lm)
## dep / atv
depAtv_lm <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + 
                  epii_negAllExp_cov + atvSelf_T1 + negAffect_T1, data = withCOV_nona)
vif(depAtv_lm)
autoplot(depAtv_lm)

