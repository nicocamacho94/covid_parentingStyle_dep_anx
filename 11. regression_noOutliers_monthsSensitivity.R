# title: Simple regression analyses - without outliers - Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/7/2021
# date updated: 3/18/2022

# set packages
library(tidyverse)
library(lm.beta)
library(miceadds)

# setwd("X:/Gaffrey/Lab/NTREC/Nicolas/parenting_and_emotreg/analysis_data/final")
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
nona_mcd_anx <- read_csv("noOut_nona_mcd_months_anx_final.csv")
nona_mcd_dep <- read_csv("noOut_nona_mcd_months_dep_final.csv") 

##### conduct hierarchical regressions #####
## anxiety / authoritative model
### null model
null_anx_authoritative <- lm(pasTot_cov ~ 1, data = nona_mcd_anx)
summary(null_anx_authoritative)
### step 1
anx_atv_step1 <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + months_T1_cov, data = nona_mcd_anx)
anova(null_anx_authoritative, anx_atv_step1)
summary(lm.beta(anx_atv_step1))
### step 2
anx_atv_step2 <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov +  + negAffect_T1 + months_T1_cov + epii_negAllExp_cov, 
                    data = nona_mcd_anx)
anova(anx_atv_step1, anx_atv_step2)
summary(lm.beta(anx_atv_step2))
### step 3
anx_atv_step3 <- lm(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + months_T1_cov + epii_negAllExp_cov + 
                      atvSelf_T1, data = nona_mcd_anx)
anova(anx_atv_step2, anx_atv_step3)
summary(lm.beta(anx_atv_step3))

## depression / authoritative model
### null model
null_dep_authoritative <- lm(pfcTot_cov ~ 1, data = nona_mcd_dep)
summary(null_dep_authoritative)
### step 1
dep_atv_step1 <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + months_T1_cov, data = nona_mcd_dep)
anova(null_dep_authoritative, dep_atv_step1)
summary(lm.beta(dep_atv_step1))
### step 2
dep_atv_step2 <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + months_T1_cov + epii_negAllExp_cov, 
                    data = nona_mcd_dep)
anova(dep_atv_step1, dep_atv_step2)
summary(lm.beta(dep_atv_step2))
### step 3
dep_atv_step3 <- lm(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + months_T1_cov + epii_negAllExp_cov + 
                      atvSelf_T1, data = nona_mcd_dep)
anova(dep_atv_step2, dep_atv_step3)
summary(lm.beta(dep_atv_step3))

# conduct analyses accounting for family clusters, correcting standard errors
## anxiety / authoritative models
### Step 1
anx_atv_step1_robust <- lm.cluster(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + months_T1_cov, 
                                   cluster = "fam_code", data = nona_mcd_anx)
summary(anx_atv_step1_robust)
### Step 2
anx_atv_step2_robust <- lm.cluster(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 +  months_T1_cov + 
                                     epii_negAllExp_cov, cluster = "fam_code",
                                   data = nona_mcd_anx)
summary(anx_atv_step2_robust)
### Step 3
anx_atv_step3_robust <- lm.cluster(pasTot_cov ~ 1 + pasTot_T1 + pfcTot_cov + negAffect_T1 + months_T1_cov + 
                                     epii_negAllExp_cov + atvSelf_T1, cluster = "fam_code",
                                   data = nona_mcd_anx)
summary(anx_atv_step3_robust)

## depression / authoritative model
### Step 1
dep_atv_step1_robust <- lm.cluster(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + months_T1_cov, 
                                   cluster = "fam_code", data = nona_mcd_dep)
summary(dep_atv_step1_robust)
### Step 2
dep_atv_step2_robust <- lm.cluster(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + months_T1_cov + 
                                     epii_negAllExp_cov, cluster = "fam_code",
                                   data = nona_mcd_dep)
summary(dep_atv_step2_robust)
### Step 3
dep_atv_step3_robust <- lm.cluster(pfcTot_cov ~ 1 + pfcTot_T1 + pasTot_cov + negAffect_T1 + months_T1_cov + 
                                     epii_negAllExp_cov + atvSelf_T1, cluster = "fam_code",
                                   data = nona_mcd_dep)
summary(dep_atv_step3_robust)
