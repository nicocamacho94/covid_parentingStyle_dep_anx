# title: Detect outliers - Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/10/2021
# date updated: 8/11/2022

# set packages
library(tidyverse)
library(MASS)

# setwd("X:/Gaffrey/Lab/NTREC/Nicolas/parenting_and_emotreg/analysis_data/final")
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
withCOV_nona_full <- read_csv("t1withCOV_data_voi_nona.csv")

# with variables that are going into models, omit NA's, without i2n or LEC
withCOV_nona <- dplyr::select(withCOV_nona_full, subid, pasTot_T1, pfcTot_T1,
                              atvSelf_T1, epii_negAllExp_cov, negAffect_T1,
                              pasTot_cov, pfcTot_cov, fam_code)
# with variables that are going into models, omit NA's, with i2n
withCOV_i2n_nona <- na.omit(dplyr::select(withCOV_nona_full, subid, pasTot_T1, pfcTot_T1, i2n_T1,
                                          atvSelf_T1, epii_negAllExp_cov, negAffect_T1,
                                          pasTot_cov, pfcTot_cov, fam_code))
# with variables that are going into models, omit NA's, with LEC
withCOV_lec_nona <- na.omit(dplyr::select(withCOV_nona_full, subid, pasTot_T1, pfcTot_T1,
                                          lec_everBad_mean_T1, atvSelf_T1, epii_negAllExp_cov,
                                          negAffect_T1, pasTot_cov, pfcTot_cov, fam_code))
# with variables that are going into models, omit NA's, with T1-COV
withCOV_months_nona <- na.omit(dplyr::select(withCOV_nona_full, subid, pasTot_T1, pfcTot_T1,
                                          months_T1_cov, atvSelf_T1, epii_negAllExp_cov,
                                          negAffect_T1, pasTot_cov, pfcTot_cov, fam_code))

# find and remove outliers using MCD for models without i2n or LEC
## anxiety
anx_atv_df <- dplyr::select(withCOV_nona, subid, pasTot_T1, atvSelf_T1, negAffect_T1, 
                            epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
alpha <- .001
cutoff_anx <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(anx_atv_df, -subid))))
output75_anx <- cov.mcd(dplyr::select(anx_atv_df, -subid), 
                        quantile.used = nrow(dplyr::select(anx_atv_df, -subid))*.75)
mcd75_anx <- mahalanobis(dplyr::select(anx_atv_df, -subid), 
                         output75_anx$center, output75_anx$cov)
outliers_MCD75_anx <- which(mcd75_anx > cutoff_anx)
nona_mcd_anx <- anx_atv_df[-outliers_MCD75_anx, ]
anx_atv_df[outliers_MCD75_anx, ] # 6 outliers
nona_mcd_anx <- left_join(nona_mcd_anx, withCOV_nona)
## depression
dep_atv_df <- dplyr::select(withCOV_nona, subid, pfcTot_T1, atvSelf_T1, negAffect_T1,
                            epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
alpha <- .001
cutoff_dep <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(dep_atv_df, -subid))))
output75_dep <- cov.mcd(dplyr::select(dep_atv_df, -subid), 
                        quantile.used = nrow(dplyr::select(dep_atv_df, -subid))*.75)
mcd75_dep <- mahalanobis(dplyr::select(dep_atv_df, -subid), output75_dep$center, 
                         output75_dep$cov)
outliers_MCD75_dep <- which(mcd75_dep > cutoff_dep)
nona_mcd_dep <- dep_atv_df[-outliers_MCD75_dep, ]
dep_atv_df[outliers_MCD75_dep, ] # 6 outliers
nona_mcd_dep <- left_join(nona_mcd_dep, withCOV_nona)

# find and remove outliers using MCD for models with i2n
## anxiety
anx_atv_i2n_df <- dplyr::select(withCOV_i2n_nona, subid, pasTot_T1, atvSelf_T1, negAffect_T1,
                                i2n_T1, epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
cutoff_i2n_anx <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(anx_atv_i2n_df, -subid))))
output75_i2n_anx <- cov.mcd(dplyr::select(anx_atv_i2n_df, -subid), 
                            quantile.used = nrow(dplyr::select(anx_atv_i2n_df, -subid))*.75)
mcd75_i2n_anx <- mahalanobis(dplyr::select(anx_atv_i2n_df, -subid), 
                             output75_i2n_anx$center, output75_i2n_anx$cov)
outliers_MCD75_i2n_anx <- which(mcd75_i2n_anx > cutoff_i2n_anx)
nona_mcd_i2n_anx <- anx_atv_i2n_df[-outliers_MCD75_i2n_anx, ]
anx_atv_i2n_df[outliers_MCD75_i2n_anx, ] # 10 outliers
nona_mcd_i2n_anx <- left_join(nona_mcd_i2n_anx, withCOV_nona)
## depression
dep_atv_i2n_df <- dplyr::select(withCOV_i2n_nona, subid, pfcTot_T1, i2n_T1, negAffect_T1,
                                atvSelf_T1, epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
cutoff_i2n_dep <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(dep_atv_i2n_df, -subid))))
output75_i2n_dep <- cov.mcd(dplyr::select(dep_atv_i2n_df, -subid), 
                            quantile.used = nrow(dplyr::select(dep_atv_i2n_df, -subid))*.75)
mcd75_i2n_dep <- mahalanobis(dplyr::select(dep_atv_i2n_df, -subid), output75_i2n_dep$center, 
                             output75_i2n_dep$cov)
outliers_MCD75_i2n_dep <- which(mcd75_i2n_dep > cutoff_i2n_dep)
nona_mcd_i2n_dep <- dep_atv_i2n_df[-outliers_MCD75_i2n_dep, ]
dep_atv_i2n_df[outliers_MCD75_i2n_dep, ] # 12 outliers
nona_mcd_i2n_dep <- left_join(nona_mcd_i2n_dep, withCOV_nona)

# find and remove outliers using MCD for models with LEC
## anxiety
anx_atv_lec_df <- dplyr::select(withCOV_lec_nona, subid, pasTot_T1, atvSelf_T1, negAffect_T1,
                                lec_everBad_mean_T1, epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
cutoff_lec_anx <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(anx_atv_lec_df, -subid))))
output75_lec_anx <- cov.mcd(dplyr::select(anx_atv_lec_df, -subid), 
                            quantile.used = nrow(dplyr::select(anx_atv_lec_df, -subid))*.75)
mcd75_lec_anx <- mahalanobis(dplyr::select(anx_atv_lec_df, -subid), 
                             output75_lec_anx$center, output75_lec_anx$cov)
outliers_MCD75_lec_anx <- which(mcd75_lec_anx > cutoff_lec_anx)
nona_mcd_lec_anx <- anx_atv_lec_df[-outliers_MCD75_lec_anx, ]
anx_atv_lec_df[outliers_MCD75_lec_anx, ] # 7 outliers
nona_mcd_lec_anx <- left_join(nona_mcd_lec_anx, withCOV_nona)
## depression
dep_atv_lec_df <- dplyr::select(withCOV_lec_nona, subid, pfcTot_T1, lec_everBad_mean_T1, 
                                negAffect_T1, atvSelf_T1, epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
cutoff_lec_dep <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(dep_atv_lec_df, -subid))))
output75_lec_dep <- cov.mcd(dplyr::select(dep_atv_lec_df, -subid), 
                            quantile.used = nrow(dplyr::select(dep_atv_lec_df, -subid))*.75)
mcd75_lec_dep <- mahalanobis(dplyr::select(dep_atv_lec_df, -subid), output75_lec_dep$center, 
                             output75_lec_dep$cov)
outliers_MCD75_lec_dep <- which(mcd75_lec_dep > cutoff_lec_dep)
nona_mcd_lec_dep <- dep_atv_lec_df[-outliers_MCD75_lec_dep, ]
dep_atv_lec_df[outliers_MCD75_lec_dep, ] # 7 outliers
nona_mcd_lec_dep <- left_join(nona_mcd_lec_dep, withCOV_nona)

# find and remove outliers using MCD for models with T1-COV
## anxiety
anx_atv_months_df <- dplyr::select(withCOV_months_nona, subid, pasTot_T1, atvSelf_T1, negAffect_T1,
                                months_T1_cov, epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
cutoff_months_anx <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(anx_atv_months_df, -subid))))
output75_months_anx <- cov.mcd(dplyr::select(anx_atv_months_df, -subid), 
                            quantile.used = nrow(dplyr::select(anx_atv_months_df, -subid))*.75)
mcd75_months_anx <- mahalanobis(dplyr::select(anx_atv_months_df, -subid), 
                             output75_months_anx$center, output75_months_anx$cov)
outliers_MCD75_months_anx <- which(mcd75_months_anx > cutoff_months_anx)
nona_mcd_months_anx <- anx_atv_months_df[-outliers_MCD75_months_anx, ]
anx_atv_months_df[outliers_MCD75_months_anx, ] # 2 outliers
nona_mcd_months_anx <- left_join(nona_mcd_months_anx, withCOV_nona)
## depression
dep_atv_months_df <- dplyr::select(withCOV_months_nona, subid, pfcTot_T1, months_T1_cov, 
                                negAffect_T1, atvSelf_T1, epii_negAllExp_cov, pasTot_cov, pfcTot_cov)
cutoff_months_dep <- (qchisq(p = 1 - alpha, df = ncol(dplyr::select(dep_atv_months_df, -subid))))
output75_months_dep <- cov.mcd(dplyr::select(dep_atv_months_df, -subid), 
                            quantile.used = nrow(dplyr::select(dep_atv_months_df, -subid))*.75)
mcd75_months_dep <- mahalanobis(dplyr::select(dep_atv_months_df, -subid), output75_months_dep$center, 
                             output75_months_dep$cov)
outliers_MCD75_months_dep <- which(mcd75_months_dep > cutoff_months_dep)
nona_mcd_months_dep <- dep_atv_months_df[-outliers_MCD75_months_dep, ]
dep_atv_months_df[outliers_MCD75_months_dep, ] # 4 outliers
nona_mcd_months_dep <- left_join(nona_mcd_months_dep, withCOV_nona)

# save data
write.csv(nona_mcd_anx,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_anx_final.csv",
          row.names = F)

write.csv(nona_mcd_dep,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_dep_final.csv",
          row.names = F)

write.csv(nona_mcd_i2n_anx,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_i2n_anx_final.csv",
          row.names = F)

write.csv(nona_mcd_i2n_dep,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_i2n_dep_final.csv",
          row.names = F)

write.csv(nona_mcd_lec_anx,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_lec_anx_final.csv",
          row.names = F)

write.csv(nona_mcd_lec_dep,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_lec_dep_final.csv",
          row.names = F)

write.csv(nona_mcd_months_anx,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_months_anx_final.csv",
          row.names = F)

write.csv(nona_mcd_months_dep,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/noOut_nona_mcd_months_dep_final.csv",
          row.names = F)

