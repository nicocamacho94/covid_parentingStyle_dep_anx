# title: Plot results - with outliers - Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/10/2021
# date updated: 3/18/2022

# set packages
library(tidyverse)
library(ggeffects)
library(ggplot2)

# setwd("X:/Gaffrey/Lab/NTREC/Nicolas/parenting_and_emotreg/analysis_data/final")
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
withCOV_nona <- read_csv("de_ident_t1withCOV_data_voi_nona.csv")
nona_mcd_anx <- read_csv("noOut_nona_mcd_anx_final.csv")
nona_mcd_dep <- read_csv("noOut_nona_mcd_dep_final.csv")

# create variables that specify whether participant is outlier in other analyses
withCOV_nona <- withCOV_nona %>% 
  mutate(outlier_anx = if_else(subid %in% nona_mcd_anx$subid, 0, 1),
         outlier_dep = if_else(subid %in% nona_mcd_dep$subid, 0, 1))

# specify final models
anx_atv_step3 <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + epii_negAllExp_cov + 
                      atvSelf_T1, data = withCOV_nona)
dep_atv_step3 <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + epii_negAllExp_cov + 
                      atvSelf_T1, data = withCOV_nona)

# gather data to create partial plot of anx x epii
## specify models
anx_cov_epii <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + atvSelf_T1, data = withCOV_nona)
epii_anx <- lm(epii_negAllExp_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + atvSelf_T1, data = withCOV_nona)
## gather residuals
anx_cov_epii_resid <- anx_cov_epii$residuals
epii_anx_resid <- epii_anx$residuals
anx_epii <- as.data.frame(cbind(anx_cov_epii_resid, epii_anx_resid))
## create partial plot
anx_epii_plot <- ggplot(anx_epii, aes(x = epii_anx_resid, y = anx_cov_epii_resid)) +
  geom_point(aes(alpha = 0.8, colour = withCOV_nona$outlier_anx)) +
  geom_smooth(method = lm, se = T, level = 0.95, colour = "black") + theme_light() +
  labs(title = "Association between Child Anxiety and COVID Stressors",
       subtitle = "Outliers included (in blue; N = 106)",
       x = "EPII T2 (residuals)", y = "PAS T2 (residuals)") +
  theme(axis.title.x = element_text(family = "serif"),
        axis.title.y = element_text(family = "serif"),
        plot.title = element_text(face = "italic", family = "serif"),
        plot.subtitle = element_text(face = "italic", family = "serif"),
        legend.position = "none")

# gather data to create partial plot of anx x atv
## specify models
anx_cov_atv <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + epii_negAllExp_cov, data = withCOV_nona)
atv_anx <- lm(atvSelf_T1 ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + epii_negAllExp_cov, data = withCOV_nona)
## gather residuals
anx_cov_atv_resid <- anx_cov_atv$residuals
atv_anx_resid <- atv_anx$residuals
anx_atv <- as.data.frame(cbind(anx_cov_atv_resid, atv_anx_resid))
## create partial plot
anx_atv_plot <- ggplot(anx_atv, aes(x = atv_anx_resid, y = anx_cov_atv_resid)) +
  geom_point(aes(alpha = 0.8, colour = withCOV_nona$outlier_anx)) +
  geom_smooth(method = lm, se = T, level = 0.95, colour = "black") + theme_light() +
  labs(title = "Association between Child Anxiety and Authoritative Style",
       subtitle = "Outliers included (in blue; N = 106)",
       x = "ATV T1 (residuals)", y = "PAS T2 (residuals)") +
  theme(axis.title.x = element_text(family = "serif"),
        axis.title.y = element_text(family = "serif"),
        plot.title = element_text(face = "italic", family = "serif"),
        plot.subtitle = element_text(face = "italic", family = "serif"),
        legend.position = "none")

# arrange plots together and save them
anx_plots <- ggarrange(anx_epii_plot, anx_atv_plot, labels = c("A", "B"),
                       ncol = 2, nrow = 1)
ggsave(anx_plots, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/nona_out_anx_partialPlots.png")


# gather data to create partial plot of dep x epii
## specify models
dep_cov_epii <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + atvSelf_T1, data = withCOV_nona)
epii_dep <- lm(epii_negAllExp_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + atvSelf_T1, data = withCOV_nona)
## gather residuals
dep_cov_epii_resid <- dep_cov_epii$residuals
epii_dep_resid <- epii_dep$residuals
dep_epii <- as.data.frame(cbind(dep_cov_epii_resid, epii_dep_resid))
## create partial plot
dep_epii_plot <- ggplot(dep_epii, aes(x = epii_dep_resid, y = dep_cov_epii_resid)) +
  geom_point(aes(alpha = 0.8, colour = withCOV_nona$outlier_dep)) +
  geom_smooth(method = lm, se = T, level = 0.95, colour = "black") + theme_light() +
  labs(title = "Association between Child Depression and COVID Stressors", 
       subtitle = "Outliers included (in blue; N = 106)",
       x = "EPII T2 (residuals)", y = "PFC T2 (residuals)") +
  theme(axis.title.x = element_text(family = "serif"),
        axis.title.y = element_text(family = "serif"),
        plot.title = element_text(face = "italic", family = "serif"),
        plot.subtitle = element_text(face = "italic", family = "serif"),
        legend.position = "none")

# gather data to create partial plot of dep x atv
## specify models
dep_cov_atv <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + epii_negAllExp_cov, data = withCOV_nona)
atv_dep <- lm(atvSelf_T1 ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + epii_negAllExp_cov, data = withCOV_nona)
## gather residuals
dep_cov_atv_resid <- dep_cov_atv$residuals
atv_dep_resid <- atv_dep$residuals
dep_atv <- as.data.frame(cbind(dep_cov_atv_resid, atv_dep_resid))
## create partial plot
dep_atv_plot <- ggplot(dep_atv, aes(x = atv_dep_resid, y = dep_cov_atv_resid)) +
  geom_point(aes(alpha = 0.8, colour = withCOV_nona$outlier_dep)) +
  geom_smooth(method = lm, se = T, level = 0.95, colour = "black") + theme_light() +
  labs(title = "Association between Child Depression and Authoritative Style", 
       subtitle = "Outliers included (in blue; N = 106)",
       x = "ATV T1 (residuals)", y = "PFC T2 (residuals)") +
  theme(axis.title.x = element_text(family = "serif"),
        axis.title.y = element_text(family = "serif"),
        plot.title = element_text(face = "italic", family = "serif"),
        plot.subtitle = element_text(face = "italic", family = "serif"),
        legend.position = "none")

# arrange plots together and save them
dep_plots <- ggarrange(dep_epii_plot, dep_atv_plot, labels = c("A", "B"),
                       ncol = 2, nrow = 1)
ggsave(dep_plots, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/nona_out_dep_partialPlots.png")
