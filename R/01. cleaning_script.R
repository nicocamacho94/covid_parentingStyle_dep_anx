# title: Cleaning Script, 5th pass - data for Karina's thesis project
# author: Nicolas L. Camacho
# date created: 10/07/2021
# date updated: 8/11/2022

# set packages
library(tidyverse)
library(lubridate)
library(VIM)
library(psych)

# set working directory
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/raw_data")

# download data
t1_raw_data <- read_csv("raw_t1_ntrec_data_2022_02_11.csv")
cov_raw_data <- read_csv("raw_covid_ntrec_data_2021_09_15.csv")
consent_data <- read_csv("raw_covid_ntrec_consent_data_2021_09_30.csv")
enrolled <- read_csv("ntrec_duke_enrolled.csv")

# ensure subid column is of class character
t1_raw_data$subid <- as.character(t1_raw_data$subid)
cov_raw_data$subid <- as.character(cov_raw_data$subid)
consent_data$subid <- as.character(consent_data$subid)
enrolled$subid <- as.character(enrolled$subid)

# separate out individuals who completed visit 1 and those who did not
v1_complete <- subset(enrolled, completed_visit_1 == 1) %>% 
  dplyr::select(., subid)
v1_incomplete <- subset(enrolled, completed_visit_1 == 0)

# keep only participants who completed visit 1 from the t1 raw data
t1_complete <- left_join(v1_complete, t1_raw_data, by = "subid")

# create df for participants who were removed from t1 raw data in join
t1_incomplete <- anti_join(t1_raw_data, t1_complete, by = "subid")

# add timepoint specifiers to columns of each dataframe
colnames(t1_complete) <- paste(colnames(t1_complete),"T1",sep="_")
colnames(cov_raw_data) <- paste(colnames(cov_raw_data),"cov",sep="_")

# rename subid columns to match
t1_complete <- t1_complete %>% rename(subid = subid_T1)
cov_raw_data <- cov_raw_data %>% rename(subid = subid_cov)

## remove test participants
cov_raw_data_notest <- subset(cov_raw_data, subid != "TEST" & subid!= "TEST2")
consent_data_notest <- subset(consent_data, subid != "TEST" & subid != "TEST2")
## remove 2 participants who don't have t1 data that get included in the t1 and cov join
## SUBID's: 7646 and 7702 -- INELIGIBLE
cov_raw_data_final <- subset(cov_raw_data_notest, subid != 7646 & subid != 7702)
consent_data_final <- subset(consent_data_notest, subid != 7646 & subid != 7702)

# retain T1 variables that are needed to create scores for each construct of interest
t1_item_voi_complete <- t1_complete %>% 
  dplyr::select(., subid, contains("pas"), contains("pfc"), contains("lec"), contains("ppanas"),
         contains("ppqs")) %>% 
  dplyr::select(-contains("pas_"), -contains("total"), -contains("notes"),
         -contains("lec_"), -contains("conversion"), -contains("v2"), -contains("timestamp"),
         -contains("complete"), -contains("_3_T1"))
## pas
t1_item_pas <- t1_item_voi_complete %>% dplyr::select(., subid, contains("pas")) %>% 
  dplyr::select(., -pas29_T1, -pas30_T1, -pas31_T1, -pas32_T1, -pas33_T1, -pas34_T1)
## pfc
t1_item_pfc <- t1_item_voi_complete %>% dplyr::select(., subid, contains("pfc"))
## lec
t1_item_lec <- t1_item_voi_complete %>% dplyr::select(., subid, contains("lec"))
## parenting style -- authoritative
t1_item_atv <- t1_item_voi_complete %>% 
  dplyr::select(., subid, ppqs3_T1, ppqs33_T1, ppqs5_T1, ppqs12_T1, ppqs35_T1, ppqs9_T1,
         ppqs27_T1, ppqs21_T1, ppqs1_T1, ppqs46_T1, ppqs39_T1, ppqs58_T1, ppqs25_T1,
         ppqs62_T1, ppqs29_T1, ppqs53_T1, ppqs42_T1, ppqs16_T1, ppqs55_T1, ppqs22_T1,
         ppqs31_T1, ppqs48_T1, ppqs60_T1, ppqs14_T1, ppqs18_T1, ppqs7_T1, ppqs51_T1)
### take extra step here to put values in same range as other questionnaires (0-4)
t1_item_atv <- t1_item_atv %>% 
  mutate_at(colnames(dplyr::select(., contains("ppqs"))), 
            list(~recode(., `1`= 0, `2`= 1, `3` = 2, `4` = 3, `5` = 4)))
## parent panas - negative affect
t1_item_panas <- t1_item_voi_complete %>% 
  dplyr::select(., subid, ppanas2_T1, ppanas4_T1, ppanas6_T1, ppanas7_T1, ppanas8_T1,
         ppanas11_T1, ppanas13_T1, ppanas15_T1, ppanas18_T1, ppanas20_T1)
### take extra step here to put values in same range as other questionnaires (0-4)
t1_item_panas <- t1_item_panas %>% 
  mutate_at(colnames(dplyr::select(., contains("ppanas"))), 
            list(~recode(., `1`= 0, `2`= 1, `3` = 2, `4` = 3, `5` = 4)))

# create mean scores for each measure
## will not create scores for participants with too little data
percentmissing = function (x){sum(is.na(x))/length(x)*100} ## missing function
## mean scores will be calculated using the number of items with data in the denominator
### pas
t1_item_pas <- t1_item_pas %>% mutate(., missing_pas = apply(dplyr::select(., -subid), 1, percentmissing))
table(t1_item_pas$missing_pas) # 3 participants missing 75%+ data
t1_withSubscale_pas <- t1_item_pas %>% 
  mutate(., pasTot_T1 = case_when(missing_pas < 75 ~ rowMeans(dplyr::select(., -subid, -missing_pas), na.rm = T)))
### pfc
t1_item_pfc <- t1_item_pfc %>% mutate(., missing_pfc = apply(dplyr::select(., -subid), 1, percentmissing))
table(t1_item_pfc$missing_pfc) # 3 participants missing 75%+ data
t1_withSubscale_pfc <- t1_item_pfc %>% 
  mutate(., pfcTot_T1 = case_when(missing_pfc < 75 ~ rowMeans(dplyr::select(., -subid, -missing_pfc), na.rm = T)))
### parenting style -- authoritative
t1_item_atv <- t1_item_atv %>% mutate(., missing_atv = apply(dplyr::select(., -subid), 1, percentmissing))
table(t1_item_atv$missing_atv) # 4 participants missing 75%+ data
t1_withSubscale_atv <- t1_item_atv %>% 
  mutate(., atvSelf_T1 = case_when(missing_atv < 75 ~ rowMeans(dplyr::select(., -subid, -missing_atv), na.rm = T)))
### panas - negative affect
t1_item_panas <- t1_item_panas %>% mutate(., missing_panas = apply(dplyr::select(., -subid), 1, percentmissing))
table(t1_item_panas$missing_panas) # 2 participants missing 75%+ data
t1_withSubscale_panas <- t1_item_panas %>% 
  mutate(., negAffect_T1 = case_when(missing_panas < 75 ~ rowMeans(dplyr::select(., -subid, -missing_panas), na.rm = T)))
### LEC
summary(aggr(dplyr::select(t1_item_lec, contains("_2_")), combined = F, numbers = T, prop = T))
#### never happened for all participants: lec10, lec14, lec27, lec38, lec43, lec44
t1_item_lec_hap <- dplyr::select(t1_item_lec, -contains("lec10"), -contains("lec14"),
                                 -contains("lec27"), -contains("lec38"), -contains("lec43"),
                                 -contains("lec44"))
t1_item_lec_countGood <- colSums(dplyr::select(t1_item_lec_hap, contains("_2_")), na.rm = T)


t1_item_lec_forCountNA <- t1_item_lec_hap %>% mutate_at(colnames(dplyr::select(., contains("_2_"))),
                                                        list(~replace(., !is.na(.), 0))) %>%
  mutate_at(colnames(dplyr::select(., contains("_2_"))), list(~replace(., is.na(.), 1)))

t1_item_lec_countNA <- colSums(dplyr::select(t1_item_lec_forCountNA, contains("_2_")), na.rm = F)

t1_item_lec_countBad <- 171-(t1_item_lec_countGood + t1_item_lec_countNA)
#### number of participants for whom "bad" was endorsed per column
#### can get rid of lec16, lec20, lec21, lec23, lec30, lec35, lec37, lec42
t1_item_lec_bad <- dplyr::select(t1_item_lec_hap, -contains("lec16"), -contains("lec20"),
                                 -contains("lec21"), -contains("lec23"), -contains("lec30"),
                                 -contains("lec35"), -contains("lec37"), -contains("lec42"))

t1_item_lec_forMean <- dplyr::select(t1_item_lec_bad, subid, contains("_2_")) %>% 
  mutate_at(colnames(dplyr::select(., contains("_2_"))),
            list(~recode(., `1` = 0, `0` = 1))) %>% 
  mutate(lec_sumBadEndorsed = rowSums(dplyr::select(., -subid), na.rm = T)) %>% 
  dplyr::select(., subid, lec_sumBadEndorsed)

t1_withSubscale_lec <- full_join(t1_item_lec_forMean, dplyr::select(t1_complete, subid, lec_everbad_score_T1), by = "subid") %>% 
  mutate(lec_everBad_mean_T1 = case_when(lec_sumBadEndorsed != 0 ~ lec_everbad_score_T1 / lec_sumBadEndorsed,
                                         lec_sumBadEndorsed == 0 & lec_everbad_score_T1 == 0 ~ 0))

# COVID variables
## remove variables that are not needed
cov_slim_data <- cov_raw_data_final %>% 
  dplyr::select(-contains("redcap"), -contains("sibling"), -contains("notes"),
         -contains("complete"), -contains("timestamp"), -contains("panas"),
         -contains("pas_"), -pfctotal_cov)

## pas
cov_item_pas <- cov_slim_data %>% dplyr::select(., subid, contains("pas")) %>% 
  dplyr::select(., -pas29_cov, -pas30_cov, -pas31_cov, -pas32_cov, -pas33_cov, -pas34_cov)
## pfc
cov_item_pfc <- cov_slim_data %>% dplyr::select(., subid, contains("pfc"))
## epii
### take extra step here to clean up the variables used to calculate scores
### and put values in similar range as other questionnaires (0-3)
cov_item_epii <- cov_slim_data %>% dplyr::select(., -contains("pfc"), -contains("pas")) %>% 
  mutate_at(vars(contains("effect")), funs(as.numeric(.))) 

colMeans(is.na(dplyr::select(cov_item_epii, contains("effect"))))
#### delete following columns with all NA's:
#### effect21, effect25, effect38, effect66, effect71, effect72
cov_item_epii_noAllNA <- cov_item_epii %>% dplyr::select(., -effect21_cov, -effect25_cov,
                                          -effect38_cov, -effect66_cov, -effect71_cov,
                                          -effect72_cov, -effect74_cov, -effect75_cov, 
                                          -effect76_cov, -effect77_cov, -effect78_cov, 
                                          -effect79_cov, -effect80_cov, -effect81_cov, 
                                          -effect82_cov, -effect83_cov, -effect84_cov, 
                                          -effect85_cov, -effect86_cov, -effect87_cov, 
                                          -effect88_cov, -effect89_cov, -effect90_cov, 
                                          -effect91_cov, -effect92_cov)

cov_item_epii_recoded <- cov_item_epii_noAllNA %>% 
  mutate_at(colnames(dplyr::select(., contains("effect"))),
            list(~recode(., `1`= 0, `2`= 1, `3` = 2, `4` = 3)))

# create mean scores for each measure
## will not create scores for participants with too little data
## mean scores will be calculated using the number of items with data in the denominator
### pas
cov_item_pas <- cov_item_pas %>% mutate(., missing_pas_cov = apply(dplyr::select(., -subid), 1, percentmissing))
table(cov_item_pas$missing_pas_cov) # 17 participants missing 75%+ data
cov_withSubscale_pas <- cov_item_pas %>% 
  mutate(., pasTot_cov = case_when(missing_pas_cov < 75 ~ rowMeans(dplyr::select(., -subid, -missing_pas_cov), na.rm = T)))
### pfc
cov_item_pfc <- cov_item_pfc %>% mutate(., missing_pfc_cov = apply(dplyr::select(., -subid), 1, percentmissing))
table(cov_item_pfc$missing_pfc_cov) # 16 participants missing 100% data
cov_withSubscale_pfc <- cov_item_pfc %>% 
  mutate(., pfcTot_cov = case_when(missing_pfc_cov < 75 ~ rowMeans(dplyr::select(., -subid, -missing_pfc_cov), na.rm = T)))
### epii -- negative events
### subscales
cov_withSubscale_epii <- cov_item_epii_recoded %>% 
  mutate(
    epii_work_mean = rowMeans(dplyr::select(., effect1_cov, effect2_cov, effect3_cov, ### work / employment
                                     effect4_cov, effect5_cov, effect6_cov, effect7_cov, 
                                     effect8_cov, effect9_cov, effect10_cov, effect11_cov), na.rm = T),
    epii_edTrain_mean = rowMeans(dplyr::select(., effect12_cov, effect13_cov), na.rm = T), ### ed / train
    epii_homelife_mean = rowMeans(dplyr::select(., effect14_cov, effect15_cov, effect16_cov, effect17_cov, ### home life
                                        effect18_cov, effect19_cov, effect20_cov, effect22_cov, 
                                        effect23_cov, effect24_cov, effect26_cov), na.rm = T),
    epii_socialAct_mean = rowMeans(dplyr::select(., effect27_cov, effect28_cov, effect29_cov, ### social activities
                                          effect30_cov, effect31_cov, effect32_cov, 
                                          effect33_cov, effect34_cov, effect35_cov, effect36_cov), na.rm = T),
    epii_econ_mean = rowMeans(dplyr::select(., effect37_cov, effect39_cov, effect40_cov, effect41_cov), na.rm = T), ### economic
    epii_emotHealth_mean = rowMeans(dplyr::select(., effect42_cov, effect43_cov, effect44_cov, effect45_cov, ### emotional health and well-being
                                           effect46_cov, effect47_cov, effect48_cov, effect49_cov), na.rm = T),
    epii_physHealth_mean = rowMeans(dplyr::select(., effect50_cov, effect51_cov, effect52_cov, effect53_cov, ### physical health
                                          effect54_cov, effect55_cov, effect56_cov, effect57_cov), na.rm = T),
    epii_quarantine_mean = rowMeans(dplyr::select(., effect58_cov, effect59_cov, effect60_cov, effect61_cov, ### quarantine
                                           effect62_cov, effect63_cov, effect64_cov, effect65_cov), na.rm = T),
    epii_infect_mean = rowMeans(dplyr::select(., effect67_cov, effect68_cov, effect69_cov, ### infection
                                       effect70_cov, effect73_cov), na.rm = T),
    epii_negAllExp_cov = rowMeans(dplyr::select(., contains("effect")), na.rm = T)) %>% 
  mutate(epii_negAllExp_cov = if_else(is.nan(epii_negAllExp_cov), 0, epii_negAllExp_cov))


# combine the data for analyses per timepoint
t1_allData <- 
  full_join(t1_withSubscale_atv, dplyr::select(t1_item_lec, subid, 
                                        contains("convert")), by = "subid") %>% 
  full_join(., t1_withSubscale_lec, by = "subid") %>% 
  full_join(., t1_withSubscale_panas, by = "subid") %>% 
  full_join(., t1_withSubscale_pas, by = "subid") %>% 
  full_join(., t1_withSubscale_pfc, by = "subid") 

t1_meanSubscales <- t1_allData %>% 
  dplyr::select(., subid, atvSelf_T1, lec_everBad_mean_T1, negAffect_T1, pfcTot_T1,
         pasTot_T1)

cov_allData <- full_join(cov_withSubscale_pfc, cov_withSubscale_pas, by = "subid") %>% 
  full_join(., cov_withSubscale_epii, by = "subid")

# grab demographics data + add to data for analyses per timepoint
## T1
t1_demos <- dplyr::select(t1_complete, subid, p_gender_T1, dateofparentinterview_T1,
                   childsage_T1, sibling_T1, siblingid_T1, sdemo3___3_T1, 
                   sdemo3___4_T1, contains("crace"), cethnic_T1, incometoneed_T1) %>% 
  rename(childSex_T1 = p_gender_T1, date_T1 = dateofparentinterview_T1, 
         childAge_T1 = childsage_T1,
         bioMom_T1 = sdemo3___3_T1, bioDad_T1 = sdemo3___4_T1, childEthnic_T1 = cethnic_T1,
         childAian_T1 = crace___1_T1, childAsian_T1 = crace___2_T1,
         childBlack_T1 = crace___3_T1, childNhopi_T1 = crace___4_T1,
         childWhite_T1 = crace___5_T1, childOthRace_T1 = crace___6_T1,
         childRaceSpec_T1 = craceother_T1, i2n_T1 = incometoneed_T1)

t1_allData_withDemos <- full_join(t1_demos, t1_allData, by = "subid")

## COVID consent data
### rename variables
consent_data_final <- consent_data_final %>% rename(date_cov = consentdate_fu1online,
                                        reporter_cov = relation_fu1online)

cov_allData_consent <- full_join(consent_data_final, cov_allData, by = "subid")
### change the format of the covid data collection date
cov_allData_consent$date_cov <- as.Date(cov_allData_consent$date_cov, format = "%m/%d/%Y")

# isolate participants who only completed the consent but provided no data
covConsent_only <- subset(cov_allData_consent, subid == 7502 | subid == 7509 |
                            subid == 7520 | subid == 7561 | subid == 7573 |
                            subid == 7621 | subid == 7658 | subid == 7672 | 
                            subid == 7684 | subid == 7694 | subid == 7723 | 
                            subid == 7727 | subid == 7728 | subid == 7752)

cov_withActualData <- subset(cov_allData_consent, subid != 7502 & subid != 7509 &
                                  subid != 7520 & subid != 7561 & subid != 7573 &
                                  subid != 7621 & subid != 7658 & subid != 7672 & 
                                  subid != 7684 & subid != 7694 & subid != 7723 & 
                                  subid != 7727 & subid != 7728 & subid != 7752)

## combined T1 + COVID
t1andCOV_allData <- full_join(t1_allData_withDemos, cov_withActualData, by = "subid")

## calculate age / timing for COVID data
t1andCOV_allData <- t1andCOV_allData %>% 
  mutate(months_T1_cov = difftime(date_cov, date_T1, units = "days")/30) %>% 
  mutate(childAge_cov = childAge_T1 + months_T1_cov) %>% 
  mutate(pand_covFU = difftime(date_cov, "2020-03-11", units = "days")/30) %>% 
  mutate(months_T1_cov = as.numeric(months_T1_cov),
         childAge_cov = as.numeric(childAge_cov),
         pand_covFU = as.numeric(pand_covFU)) %>% 
  mutate(childAge_cov = childAge_cov / 12,
         childAge_T1 = childAge_T1 / 12)

t1andCOV_allData # all data, all participants, both timepoints


# save datasets
write.csv(t1andCOV_allData,
          "C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final/all_data_temp.csv",
          row.names = F)
