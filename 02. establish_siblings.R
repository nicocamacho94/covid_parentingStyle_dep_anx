# title: Establish siblings script - data for Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/2/2021
# date updated: 8/11/2022

# set packages
library(tidyverse)

# set working directory
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
all_data <- read_csv("all_data_temp.csv")

# grab info on all siblings
haveSibs <- subset(all_data, sibling_T1 == 1) # 39 participants
sibsID <- subset(all_data, !is.na(siblingid_T1)) # 35 participants

# check discrepancies in sibling identification
setdiff(haveSibs$subid, sibsID$subid) # 7789, 7796, 7804, 7811
setdiff(sibsID$subid, haveSibs$subid) # 0 discrepancies
setdiff(sibsID$subid, haveSibs$sibling_T1) # many
setdiff(haveSibs$subid, haveSibs$sibling_T1) # many

# fix sibling data for certain participants
all_data$siblingid_T1[all_data$subid == 7714] <- 7713
all_data$sibling_T1[all_data$subid == 7714] <- 1
all_data$siblingid_T1[all_data$subid == 7718] <- 7719
all_data$sibling_T1[all_data$subid == 7718] <- 1
all_data$siblingid_T1[all_data$subid == 7725] <- 7726
all_data$sibling_T1[all_data$subid == 7725] <- 1
all_data$siblingid_T1[all_data$subid == 7553] <- 7790
all_data$sibling_T1[all_data$subid == 7553] <- 1
all_data$siblingid_T1[all_data$subid == 7791] <- 7792
all_data$sibling_T1[all_data$subid == 7791] <- 1
all_data$siblingid_T1[all_data$subid == 7607] <- 7798
all_data$sibling_T1[all_data$subid == 7607] <- 1
all_data$siblingid_T1[all_data$subid == 7807] <- 7808
all_data$sibling_T1[all_data$subid == 7807] <- 1
all_data$siblingid_T1[all_data$subid == 7741] <- NA # take 7742 off bc did not complete NTREC
all_data$sibling_T1[all_data$subid == 7741] <- 0
all_data$siblingid_T1[which(all_data$subid == 7718)] <- 7719
all_data$sibling_T1[which(all_data$subid == 7718)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7725)] <- 7726
all_data$sibling_T1[which(all_data$subid == 7725)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7505)] <- 7756 # and 7756; family of 3
all_data$sibling_T1[which(all_data$subid == 7505)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7811)] <- 7756 # and 7505; family of 3
all_data$sibling_T1[which(all_data$subid == 7811)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7657)] <- 7658
all_data$sibling_T1[which(all_data$subid == 7657)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7803)] <- 7804
all_data$sibling_T1[which(all_data$subid == 7803)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7804)] <- 7803
all_data$sibling_T1[which(all_data$subid == 7804)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7796)] <- 7568
all_data$sibling_T1[which(all_data$subid == 7796)] <- 1
all_data$siblingid_T1[which(all_data$subid == 7568)] <- 7796
all_data$sibling_T1[which(all_data$subid == 7568)] <- 1
all_data$sibling_T1[which(all_data$subid == 7789)] <- 0 # no sibling in study; parent specified only her and child at home

# re-assess info on all siblings
haveSibs <- subset(all_data, sibling_T1 == 1) # 47 participants
sibsID <- subset(all_data, !is.na(siblingid_T1)) # 47 participants
# check which participants are in haveSibs but not sibsID
setdiff(haveSibs$subid, sibsID$subid) # 0 discrepancies
# check which participants are in sibsID but not haveSibs
setdiff(sibsID$subid, haveSibs$subid) # 0 discrepancies
# for the participants with siblings,
# find which participants are listed under SUBID but not sibling ID
setdiff(sibsID$subid, sibsID$siblingid_T1) 
### 7811 comes up - that's b/c of the 3 kid family; 7811 does not have COVID data, so chose to leave out
# and then find which participants are under sibling ID but not SUBID
setdiff(sibsID$siblingid_T1, sibsID$subid) # none

setdiff(sibsID$subid, haveSibs$sibling_T1) # all of these have been checked
setdiff(haveSibs$subid, haveSibs$sibling_T1) # all of these have been checked

# filter to only keep data for participants with COVID data
withCOV_data <- filter(all_data, !is.na(date_cov))

# create sibling df's for data with COVID
haveSibs_cov <- subset(withCOV_data, sibling_T1 == 1) # 30 participants
sibsID_cov <- subset(withCOV_data, !is.na(siblingid_T1)) # 30 participants

# assess the sibling data for COVID sample
setdiff(haveSibs_cov$subid, sibsID_cov$subid) # 0 discrepancies
# check which participants are in sibsID_cov but not haveSibs_cov
setdiff(sibsID_cov$subid, haveSibs_cov$subid) # 0 discrepancies
# for the participants with siblings,
# find which participants are listed under SUBID but not sibling ID
setdiff(sibsID_cov$subid, sibsID_cov$siblingid_T1) # 7553, 7568, 7607, 7657
# and then find which participants are under sibling ID but not SUBID
setdiff(sibsID_cov$siblingid_T1, sibsID_cov$subid) # 7790, 7796, 7798, 7658

setdiff(sibsID_cov$subid, haveSibs_cov$sibling_T1) # all of these have been checked
setdiff(haveSibs_cov$subid, haveSibs_cov$sibling_T1) # all of these have been checked

# fix the subid data for the COVID sample (some participants not in this sample)
withCOV_data$siblingid_T1[which(withCOV_data$subid == 7553)] <- NA # 7790
withCOV_data$sibling_T1[which(withCOV_data$subid == 7553)] <- 0
withCOV_data$siblingid_T1[which(withCOV_data$subid == 7568)] <- NA # 7796
withCOV_data$sibling_T1[which(withCOV_data$subid == 7568)] <- 0
withCOV_data$siblingid_T1[which(withCOV_data$subid == 7607)] <- NA # 7798
withCOV_data$sibling_T1[which(withCOV_data$subid == 7607)] <- 0
withCOV_data$siblingid_T1[which(withCOV_data$subid == 7657)] <- NA # 7798
withCOV_data$sibling_T1[which(withCOV_data$subid == 7657)] <- 0

# create new sibling df's for data with COVID
haveSibs_cov <- subset(withCOV_data, sibling_T1 == 1) # 26 participants
sibsID_cov <- subset(withCOV_data, !is.na(siblingid_T1)) # 26 participants

# create family codes
sibsID_cov$fam_code <- NA
n = 1
for(i in 1:nrow(sibsID_cov)){
  if(is.na(sibsID_cov$fam_code[i]) & is.na(sibsID_cov$fam_code[which(sibsID_cov$subid == sibsID_cov$siblingid_T1[i])])){
    sibsID_cov$fam_code[i] = n
    sibsID_cov$fam_code[which(sibsID_cov$subid == sibsID_cov$siblingid_T1[i])] = n
    n = n+1
  }
  else{
    sibsID_cov$fam_code[i] = sibsID_cov$fam_code[i]
  }
}

# join data from 2 and 3 person families and join back to slim_duke
withCOV_data <- left_join(withCOV_data, sibsID_cov, by = colnames(withCOV_data))

# give unique fam codes for all singletons
c = n + 1
for(i in 1:nrow(withCOV_data)){
  if(is.na(withCOV_data$fam_code[i])){
    withCOV_data$fam_code[i] = c
    c = c+1
  }
  else{
    withCOV_data$fam_code[i] = withCOV_data$fam_code[i]
  }
}


##### CREATE FINAL VERSIONS OF DATA #####
t1withCOV_data_voi <- dplyr::select(withCOV_data, subid, contains("child"), date_T1,
                             date_cov, pand_covFU, months_T1_cov, contains("sibling"), fam_code, contains("bio"), 
                             reporter_cov, i2n_T1, atvSelf_T1, lec_everBad_mean_T1, 
                             negAffect_T1, contains("pasToT"), contains("pfcTot"),
                             epii_negAllExp_cov) # data for main vars of interest for participants with data at both timepoints

t1withCOV_data_voi_nona <- na.omit(dplyr::select(t1withCOV_data_voi, atvSelf_T1, lec_everBad_mean_T1, 
                                          negAffect_T1, contains("pasTot"), contains("pfcTot"),
                                          epii_negAllExp_cov)) %>% 
  right_join(t1withCOV_data_voi, .) # data without NA's for main vars of interest for participants with data at both timepoints

t1withCOV_allData_nona <- right_join(withCOV_data, t1withCOV_data_voi_nona) # all data for participants with data without NA's for main vars of interest at both timepoints

# save data
write.csv(all_data,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/all_data_withSibs.csv",
          row.names = F)
write.csv(t1withCOV_data_voi,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/t1withCOV_data_voi.csv",
          row.names = F)
write.csv(t1withCOV_data_voi_nona,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/t1withCOV_data_voi_nona.csv",
          row.names = F)
write.csv(t1withCOV_allData_nona,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/t1withCOV_allData_nona.csv",
          row.names = F)
