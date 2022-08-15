# title: Plot main results - no outliers - Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/10/2021
# date updated: 7/20/2022

# set packages
library(tidyverse)
library(ggpubr)
library(ggplot2)

# setwd("X:/Gaffrey/Lab/NTREC/Nicolas/parenting_and_emotreg/analysis_data/final")
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
nona_mcd_anx <- read_csv("noOut_nona_mcd_anx_final.csv")
nona_mcd_dep <- read_csv("noOut_nona_mcd_dep_final.csv") 

# specify final models
anx_atv_step3 <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + epii_negAllExp_cov + 
                      atvSelf_T1, data = nona_mcd_anx)
dep_atv_step4 <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + epii_negAllExp_cov + 
                      atvSelf_T1, data = nona_mcd_dep)

# gather data to create partial plot of anx x epii
## specify models
anx_cov_epii <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + atvSelf_T1, data = nona_mcd_anx)
epii_anx <- lm(epii_negAllExp_cov ~ 1 + pasTot_T1 + pfcTot_cov + atvSelf_T1, data = nona_mcd_anx)
## gather residuals
anx_cov_epii_resid <- anx_cov_epii$residuals
epii_anx_resid <- epii_anx$residuals
anx_epii <- as.data.frame(cbind(anx_cov_epii_resid, epii_anx_resid))
## create partial plot
anx_epii_plot <- ggplot(anx_epii, aes(x = epii_anx_resid, y = anx_cov_epii_resid)) +
  geom_point(aes(alpha = 0.8)) +
  geom_smooth(method = lm, se = F, colour = "black", size = 0.8) + theme_light() +
  labs(x = "EPII (residuals)", y = "PAS COV (residuals)") +
  theme(axis.title.x = element_text(family = "sans serif"),
        axis.title.y = element_text(family = "sans serif"),
        legend.position = "none")
## add 95% confidence bands
pred_epii_anx <- as.data.frame(predict(lm(anx_cov_epii_resid ~ epii_anx_resid, data = anx_epii), interval = "confidence"))
anx_epii_plot <- anx_epii_plot + 
  geom_smooth(data = pred_epii_anx, aes(y = lwr), color = "black", linetype = "dashed", size = 0.5) +
  geom_smooth(data = pred_epii_anx, aes(y = upr), color = "black", linetype = "dashed", size = 0.5)

# gather data to create partial plot of anx x atv
## specify models
anx_cov_atv <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + epii_negAllExp_cov, data = nona_mcd_anx)
atv_anx <- lm(atvSelf_T1 ~ 1 + pasTot_T1 + pfcTot_cov + epii_negAllExp_cov, data = nona_mcd_anx)
## gather residuals
anx_cov_atv_resid <- anx_cov_atv$residuals
atv_anx_resid <- atv_anx$residuals
anx_atv <- as.data.frame(cbind(anx_cov_atv_resid, atv_anx_resid))
## create partial plot
anx_atv_plot <- ggplot(anx_atv, aes(x = atv_anx_resid, y = anx_cov_atv_resid)) +
  geom_point(aes(alpha = 0.8)) +
  geom_smooth(method = lm, se = F, colour = "black", size = 0.8) + theme_light() +
  labs(x = "ATV Baseline (residuals)", y = "PAS COV (residuals)") +
  theme(axis.title.x = element_text(family = "sans serif"),
        axis.title.y = element_text(family = "sans serif"),
        legend.position = "none")
## add 95% confidence bands
pred_atv_anx <- as.data.frame(predict(lm(anx_cov_atv_resid ~ atv_anx_resid, data = anx_atv), interval = "confidence"))
anx_atv_plot <- anx_atv_plot +
  geom_smooth(data = pred_atv_anx, aes(y = lwr), color = "black", linetype = "dashed", size = 0.5) +
  geom_smooth(data = pred_atv_anx, aes(y = upr), color = "black", linetype = "dashed", size = 0.5)
  
# arrange both anx plots together and save
anx_plots <- ggarrange(anx_epii_plot, anx_atv_plot, labels = c("a", "b"),
                       ncol = 2, nrow = 1)
ggsave(anx_plots, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/nona_noOut_anx_partialPlots.png")


# gather data to create partial plot of dep x epii
## specify models
dep_cov_epii <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + atvSelf_T1, data = nona_mcd_dep)
epii_dep <- lm(epii_negAllExp_cov ~ 1 + pfcTot_T1 + pasTot_cov + atvSelf_T1, data = nona_mcd_dep)
## gather residuals
dep_cov_epii_resid <- dep_cov_epii$residuals
epii_dep_resid <- epii_dep$residuals
dep_epii <- as.data.frame(cbind(dep_cov_epii_resid, epii_dep_resid))
## create partial plot
dep_epii_plot <- ggplot(dep_epii, aes(x = epii_dep_resid, y = dep_cov_epii_resid)) +
  geom_point(aes(alpha = 0.8)) +
  geom_smooth(method = lm, se = F, colour = "black", size = 0.8) + theme_light() +
  labs(x = "EPII (residuals)", y = "PFC-S COV (residuals)") +
  theme(axis.title.x = element_text(family = "sans serif"),
        axis.title.y = element_text(family = "sans serif"),
        legend.position = "none")
## add 95% confidence bands
pred_epii_dep <- as.data.frame(predict(lm(dep_cov_epii_resid ~ epii_dep_resid, data = dep_epii), interval = "confidence"))
dep_epii_plot <- dep_epii_plot +
  geom_smooth(data = pred_epii_dep, aes(y = lwr), color = "black", linetype = "dashed", size = 0.5) +
  geom_smooth(data = pred_epii_dep, aes(y = upr), color = "black", linetype = "dashed", size = 0.5)

# gather data to create partial plot of dep x atv
## specify models
dep_cov_atv <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + epii_negAllExp_cov, data = nona_mcd_dep)
atv_dep <- lm(atvSelf_T1 ~ 1 + pfcTot_T1 + pasTot_cov + epii_negAllExp_cov, data = nona_mcd_dep)
## gather residuals
dep_cov_atv_resid <- dep_cov_atv$residuals
atv_dep_resid <- atv_dep$residuals
dep_atv <- as.data.frame(cbind(dep_cov_atv_resid, atv_dep_resid))
## create partial plot
dep_atv_plot <- ggplot(dep_atv, aes(x = atv_dep_resid, y = dep_cov_atv_resid)) +
  geom_point(aes(alpha = 0.8)) +
  geom_smooth(method = lm, se = F, colour = "black", size = 0.8) + theme_light() +
  labs(x = "ATV Baseline (residuals)", y = "PFC-S COV (residuals)") +
  theme(axis.title.x = element_text(family = "sans serif"),
        axis.title.y = element_text(family = "sans serif"),
        legend.position = "none")
## add 95% confidence bands
pred_atv_dep <- as.data.frame(predict(lm(dep_cov_atv_resid ~ atv_dep_resid, data = dep_atv), interval = "confidence"))
dep_atv_plot <- dep_atv_plot +
  geom_smooth(data = pred_atv_dep, aes(y = lwr), color = "black", linetype = "dashed", size = 0.5) +
  geom_smooth(data = pred_atv_dep, aes(y = upr), color = "black", linetype = "dashed", size = 0.5)


# arrange both anx plots together and save
dep_plots <- ggarrange(dep_epii_plot, dep_atv_plot, labels = c("a", "b"),
                       ncol = 2, nrow = 1)
ggsave(dep_plots, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/nona_noOut_dep_partialPlots.png")
