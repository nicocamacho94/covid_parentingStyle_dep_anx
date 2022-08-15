# title: Internal consistency - Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/2/2021
# date updated: 3/4/2022

# set packages
library(tidyverse)
library(VIM)
library(psych)

# set working directory
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
t1_ic_data <- read_csv("t1_ic_data_final.csv")
cov_ic_data <- read_csv("cov_ic_data_final.csv")

withCOV_allData_nona <- read_csv("t1withCOV_allData_nona.csv")

# leave only subid column as reference from withCOV_nona
withCOV_nona_subid <- dplyr::select(withCOV_nona, subid)

# truncate ic datasets to only include participants with full datasets
t1_ic_nona <- left_join(withCOV_nona_subid, t1_ic_data)
cov_ic_nona <- left_join(withCOV_nona_subid, cov_ic_data)

##### T1 DATA #####
# assess missingness
aggr(dplyr::select(withCOV_allData_nona, contains("pas"), contains("pfc"), contains("convert"),
            contains("ppqs")) %>% dplyr::select(., contains("T1")), 
     combined = T, numbers = T, prop = F, only.miss = F, cex.axis = 0.6)

# split t1 data into subsets based on measure & omit NA's
t1_pas <- dplyr::select(withCOV_allData_nona, contains("pas") & contains("T1")) %>% 
  dplyr::select(., -pasTot_T1) %>% na.omit(.)
t1_pfc <- dplyr::select(withCOV_allData_nona, contains("pfc") & contains("T1")) %>% 
  dplyr::select(., -pfcTot_T1) %>% na.omit(.)
t1_lec <- dplyr::select(withCOV_allData_nona, contains("lec") & contains("convert") & 
                          contains("T1")) %>% mutate_all(~replace(., is.na(.), 0))
t1_ppqs <- dplyr::select(withCOV_allData_nona, contains("ppqs") & contains("T1")) %>% na.omit(.)
t1_negAff <- dplyr::select(withCOV_allData_nona, contains("panas") & contains("T1")) %>% na.omit(.)

# conduct internal consistency analysis
alpha(t1_pas, cumulative = F, check.keys = F) # alpha = 0.79, 95% CI[0.73, 0.85] -- using FALSE to avoid reverse-coding pas3
alpha(t1_pfc, cumulative = F, check.keys = T) # alpha = 0.83, 95% CI[0.78, 0.87]
alpha(t1_lec, cumulative = F, check.keys = T) # alpha = 0.71, 95% CI[0.64, 0.78] -- SOME GET REVERSE CODED... maybe don't need alpha here?
alpha(t1_ppqs, cumulative = F, check.keys = T) # alpha = 0.86, 95% CI[0.82, 0.9]
alpha(t1_negAff, cumulative = F, check.keys = T) # alpha = 0.85, 95% CI[0.81, 0.89]

##### COVID DATA #####
# assess missingness
aggr(dplyr::select(withCOV_allData_nona, contains("pas"), contains("pfc"), 
                   contains("effect")) %>% dplyr::select(., contains("cov")), 
     combined = T, numbers = T, prop = F, only.miss = F, cex.axis = 0.6)

# split t1 data into subsets based on measure & omit NA's
cov_pas <- dplyr::select(withCOV_allData_nona, contains("pas") & contains("cov")) %>% 
  dplyr::select(., -pasTot_cov, -missing_pas_cov) %>% na.omit(.)
cov_pfc <- dplyr::select(withCOV_allData_nona, contains("pfc") & contains("cov")) %>% 
  dplyr::select(., -pfcTot_cov, -missing_pfc_cov) %>% na.omit(.)
cov_epii <- select(withCOV_allData_nona, contains("effect")) %>% 
  mutate_all(~replace(., is.na(.), 0))

# conduct internal consistency analysis
alpha(cov_pas, cumulative = F, check.keys = F) # alpha = 0.85, 95% CI[0.8, 0.89] -- using FALSE to avoid reverse-coding pas24
alpha(cov_pfc, cumulative = F, check.keys = T) # alpha = 0.88, 95% CI[0.85, 0.91]
alpha(cov_epii, cumulative = F, check.keys = F) # alpha = 0.92, 95% CI[0.9, 0.94] -- using FALSE to avoid reverse-coding a few items
