# title: Demographics - Karina's thesis project
# author: Nicolas L. Camacho
# date created: 11/11/2021
# date updated: 3/18/2022

# set packages
library(tidyverse)
library(VIM)
library(lubridate)

# setwd("X:/Gaffrey/Lab/NTREC/Nicolas/parenting_and_emotreg/analysis_data/final")
setwd("C:/Users/forev/Documents/Nicolas/karina_thesis/analysis_data/final")

# pull data
withCOV_nona <- read_csv("de_ident_t1withCOV_data_voi_nona.csv")

# assess missingness
png("C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/missing_demos_data.png")
aggr(dplyr::select(withCOV_nona, childSex_T1, childAge_T1, childAge_cov, i2n_T1, 
                   childEthnic_T1, bioMom_T1, bioDad_T1, childAian_T1, childAsian_T1, 
                   childBlack_T1, childNhopi_T1, childWhite_T1, childOthRace_T1, 
                   childRaceSpec_T1), combined = T, numbers = T, prop = F, 
     only.miss = F, cex.axis = 0.6, labels = c("Sex T1", "Age T1", "Age T2", 
                                               "i2n T1", "Ethnic", "BioMom", 
                                               "BioDad", "AIAN", "Asian", "Black", 
                                               "NHOPI", "White", "OthRace", "OthRaceSpec"))
dev.off()

# adjust race data
## gather subid's and race specification for those with specified race data
## assess whether race columns match with specified data
specRace <- withCOV_nona %>% 
  dplyr::select(., subid, childAian_T1, childAsian_T1, childBlack_T1, childNhopi_T1,
                childWhite_T1, childOthRace_T1, childRaceSpec_T1, childEthnic_T1) %>% 
  na.omit(dplyr::select(., childOthRace_T1, childRaceSpec_T1))
# 7505 - not specified in separate columns; 7521 - specified in separate columns, 1 race
# 7540 - only specified ethnicity; 7543 - only specified ethnicity
# 7634 - only specified ethnicity, stated Mexican & not hispanic
# 7636 - specified ethnicity, need to update race - Black
# 7666 - only specified ethnicity; 7688 - mixed - not specified in separate columns - White & Black
# 7696 - mixed - not specified in separate columns - White & Black
# 7732 - mixed - not specified in separate columns - White & Black
# 7741 - mixed - unspecified; 7750 - only specified ethnicity
# 7756 - mixed - not specified in separate columns - White & Black

## gather subid's and race specs for those with multiple races endorsed (excl. "other race")
multRaceEndorsed <- withCOV_nona %>% 
  mutate(numRace = childAian_T1 + childAsian_T1 + childBlack_T1 + childNhopi_T1 + childWhite_T1) %>%
subset(., numRace > 1) %>% 
  dplyr::select(., subid, childAian_T1, childAsian_T1, childBlack_T1, childNhopi_T1,
                childWhite_T1, childOthRace_T1, childRaceSpec_T1, childEthnic_T1,
                numRace)

## assess what the race specfiications were for those with multiple selections
multRace <- withCOV_nona %>% 
  dplyr::select(., subid, childAian_T1, childAsian_T1, childBlack_T1, childNhopi_T1,
                childWhite_T1, childOthRace_T1, childRaceSpec_T1, childEthnic_T1) %>% 
  filter(., multRaceEndorsed$subid %in% .$subid)
# 7539 - White & Asian; 7544 - White & NHOPI; 7545 - White & Black
# 7579 - White & Asian; 7633 - White & Asian; 7692 - White & Asian

## make necessary edits to data
### update race to match specification for 7636
withCOV_nona$childBlack_T1[which(withCOV_nona$subid == 7636)] <- 1
### clear the specifications for those with multiple selections
### in order to specify their mixed races later and not include them in the
### percentages of participants with None or 1 specified race
race_cols <- c("childAian_T1", "childAsian_T1", "childBlack_T1", "childNhopi_T1",
               "childWhite_T1", "childOthRace_T1")
withCOV_nona <- withCOV_nona %>% 
  mutate_at(race_cols,
            ~ if_else(subid == 7539 | subid == 7544 | subid == 7545 | subid == 7579 |
                        subid == 7633 | subid == 7692, 0, .))

## create new columns to account for those of mixed race
### mixed - unspecified
withCOV_nona <- withCOV_nona %>% 
  mutate(childMixed_unspec_T1 = 
           if_else(subid == 7741, 1, 0),
         childMixed_WB_T1 = 
           if_else(subid == 7688 | subid == 7696 | subid == 7732 | subid == 7756 |
                     subid == 7545, 1, 0), ### White & Black
         childMixed_WAS_T1 =
           if_else(subid == 7539 | subid == 7579 | subid == 7633 | subid == 7692, 1, 0), ### White & Asian
         childMixed_WN_T1 =
           if_else(subid == 7544, 1, 0)) %>% ### White & NHOPI
  ### create a mixed race specifier columns
  mutate(child_mixedRace = if_else(childMixed_WB_T1 == 1, 1,
                                   if_else(childMixed_WAS_T1 == 1, 1,
                                           if_else(childMixed_WN_T1 == 1, 1, 0))))

# adjust reporter data from T1
withCOV_nona <- withCOV_nona %>% 
  mutate(reporter_T1 = if_else(bioMom_T1 == 1, "Mother",
                               ifelse(bioDad_T1 == 1, "Father", NA)))
# adjust reporter data from COVID follow-up
## gather subid's and reporter spec
specCovReporter <- withCOV_nona %>% dplyr::select(., subid, reporter_cov) %>% 
  subset(., reporter_cov != "Mother" & reporter_cov != "Father")


withCOV_nona <- withCOV_nona %>% 
  mutate(reporter_cov = case_when(reporter_cov == "mom" ~ "Mother",
                                  reporter_cov == "Mom" ~ "Mother",
                                  reporter_cov == "mother" ~ "Mother",
                                  reporter_cov == "father" ~ "Father",
                                  reporter_cov == "Mother" ~ "Mother",
                                  reporter_cov == "Father" ~ "Father",
                                  reporter_cov == "Parent" ~ "Unspecified"))

# adjust child sex data (males = 1, females = 2 --> 0)
withCOV_nona <- withCOV_nona %>% 
  mutate(childSex_T1 = case_when(childSex_T1 == 2 ~ 0,
                                 childSex_T1 == 1 ~ 1))

# adjust child ethnicity data (hispanic = 1, not hispanic = 2 --> 0)
withCOV_nona <- withCOV_nona %>% 
  mutate(childEthnic_T1 = case_when(childEthnic_T1 == 2 ~ 0,
                                 childEthnic_T1 == 1 ~ 1))

# summarize demographics
demos_data <- withCOV_nona %>% 
  summarise(
    # child sex
    n_males = sum(childSex_T1),
    perc_males = (sum(childSex_T1)/length(childSex_T1)*100),
    n_female = length(childSex_T1) - sum(childSex_T1),
    perc_females = ((length(childSex_T1) - sum(childSex_T1))/length(childSex_T1)*100),
    # child ethnicity
    n_hispanic = sum(childEthnic_T1),
    perc_hispanic = (sum(childEthnic_T1)/length(childEthnic_T1)*100),
    n_notHispanic = length(childEthnic_T1) - sum(childEthnic_T1),
    perc_notHispanic = ((length(childEthnic_T1) - sum(childEthnic_T1))/length(childEthnic_T1)*100),
    # child race -- single race specified
    n_aian = sum(childAian_T1),
    perc_aian = (sum(childAian_T1) / length(childAian_T1))*100,
    n_asian = sum(childAsian_T1),
    perc_asian = (sum(childAsian_T1) / length(childAsian_T1))*100,
    n_black = sum(childBlack_T1),
    perc_black = (sum(childBlack_T1) / length(childBlack_T1))*100,
    n_nhopi = sum(childNhopi_T1),
    perc_nhopi = (sum(childNhopi_T1) / length(childNhopi_T1))*100,
    n_white = sum(childWhite_T1),
    perc_white = (sum(childWhite_T1) / length(childWhite_T1))*100,
    # child race -- mixed race specified
    n_mixed = sum(child_mixedRace),
    perc_mixed = (sum(child_mixedRace) / length(child_mixedRace))*100,
    n_white_black = sum(childMixed_WB_T1),
    perc_white_black = (sum(childMixed_WB_T1) / length(childMixed_WB_T1))*100,
    n_white_asian = sum(childMixed_WAS_T1),
    perc_white_asian = (sum(childMixed_WAS_T1) / length(childMixed_WAS_T1))*100,
    n_white_nhopi = sum(childMixed_WN_T1),
    perc_white_nhopi = (sum(childMixed_WN_T1) / length(childMixed_WN_T1))*100,
    # child race -- unspecified
    n_unspec_race = length(which(withCOV_nona$childAian_T1 == 0 & 
                                   withCOV_nona$childAsian_T1 == 0 & 
                                   withCOV_nona$childBlack_T1 == 0 & 
                                   withCOV_nona$childNhopi_T1 == 0 & 
                                   withCOV_nona$childWhite_T1 == 0 & 
                                   withCOV_nona$child_mixedRace == 0)),
    perc_unspec_race = (length(which(withCOV_nona$childAian_T1 == 0 & 
                                      withCOV_nona$childAsian_T1 == 0 & 
                                      withCOV_nona$childBlack_T1 == 0 & 
                                      withCOV_nona$childNhopi_T1 == 0 & 
                                      withCOV_nona$childWhite_T1 == 0 & 
                                      withCOV_nona$child_mixedRace == 0)) / length(subid)) * 100,
    # siblings
    n_sibling_pairs = sum(as.numeric(duplicated(fam_code))),
    perc_sibling_pairs = (sum(as.numeric(duplicated(fam_code))) / length(fam_code))*100,
    # parent reporters -- T1
    n_father_report_T1 = length(subset(withCOV_nona, reporter_T1 == "Father")$reporter_T1),
    perc_father_report_T1 = (length(subset(withCOV_nona, reporter_T1 == "Father")$reporter_T1) / length(reporter_T1))*100,
    n_mother_report_T1 = length(subset(withCOV_nona, reporter_T1 == "Mother")$reporter_T1),
    perc_mother_report_T1 = (length(subset(withCOV_nona, reporter_T1 == "Mother")$reporter_T1) / length(reporter_T1))*100, 
    # parent reporters -- COVID follow-up
    n_father_report_cov = length(subset(withCOV_nona, reporter_cov == "Father")$reporter_cov),
    perc_father_report_cov = (length(subset(withCOV_nona, reporter_cov == "Father")$reporter_cov) / length(reporter_cov))*100,
    n_mother_report_cov = length(subset(withCOV_nona, reporter_cov == "Mother")$reporter_cov),
    perc_mother_report_cov = (length(subset(withCOV_nona, reporter_cov == "Mother")$reporter_cov) / length(reporter_cov))*100,
    n_unspec_report_cov = length(subset(withCOV_nona, reporter_cov == "Unspecified")$reporter_cov),
    perc_unspec_report_cov = (length(subset(withCOV_nona, reporter_cov == "Unspecified")$reporter_cov) / length(reporter_cov))*100,
  )
write_excel_csv(demos_data,
                file = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/demos_data.csv")

# two-wave data collection plot
## ages at both waves
twoWave_age_long <-
  dplyr::select(withCOV_nona, subid, childAge_T1, months_T1_cov) %>% 
  dplyr::mutate(childAge_cov = childAge_T1 + (months_T1_cov/12)) %>% 
  dplyr::select(., subid, childAge_T1, childAge_cov) %>% 
  pivot_longer(cols = -subid, names_to = "vars", values_to = "value") %>% 
  mutate(wave = as.factor(case_when(vars == "childAge_T1" ~ "Baseline",
                                    vars == "childAge_cov" ~ "COVID Follow-Up")))
twoWave_age_plot <- 
  twoWave_age_long %>% 
  dplyr::group_by(subid) %>%
  dplyr::mutate(min_age = min(value)) %>%
  ggplot(aes(x = value, y = forcats::fct_reorder(as.factor(subid), min_age))) +
  geom_line(aes(group = subid)) + 
  geom_point(aes(color = wave)) +
  theme_bw() + theme(axis.text.y = element_blank(), panel.grid.major = element_blank()) +
  labs(y = 'Participants', x = 'Age at visit (years)') +
  guides(color = guide_legend(title = 'Study Timepoint')) +
  theme(text = element_text(face = 'bold', family = "serif"),
        axis.ticks.y=element_blank())
ggsave(twoWave_age_plot, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/twoWave_age_plot.png")

## dates at both waves
twoWave_date_long <-
  dplyr::select(withCOV_nona, subid, date_T1, months_T1_cov) %>% 
  dplyr::mutate(date_cov = date_T1 %m+% months(as.integer(months_T1_cov)),
                cov_lockdown = as.Date("2020-03-13", format = "%Y-%m-%d")) %>% 
  dplyr::select(., subid, date_T1, date_cov, cov_lockdown) %>% 
  pivot_longer(cols = -subid, names_to = "vars", values_to = "value") %>% 
  mutate(wave = as.factor(case_when(vars == "date_T1" ~ "Baseline",
                                    vars == "cov_lockdown" ~ "COVID Emergency NC", # https://www.nc.gov/covid-19/covid-19-orders-directives#executive-orders--2020-executive-orders-issued-by-gov--roy-cooper
                                    vars == "date_cov" ~ "COVID Follow-Up")))
twoWave_date_plot <-
  twoWave_date_long %>%
  dplyr::group_by(subid) %>%
  dplyr::mutate(min_date = min(value)) %>%
  ggplot(aes(x = value, y = forcats::fct_reorder(as.factor(subid), min_date))) +
  geom_line(aes(group = subid)) + 
  geom_point(aes(color = wave)) +
  annotate("rect", xmin = as.Date("2020-03-01", format = "%Y-%m-%d"), 
           xmax = as.Date("2020-03-23", format = "%Y-%m-%d"), ymin = 35, ymax = 65,
           alpha = 0.8, colour = "white", fill = "white") +
  annotate("text", x = as.Date("2020-03-10", format = "%Y-%m-%d"), y = 50, 
           label = "March 27, 2020", colour = "black", angle = 90) +
  theme_bw() + theme(axis.text.y = element_blank(), panel.grid.major = element_blank()) +
  labs(y = 'Participants', x = 'Date at visit') +
  guides(color = guide_legend(title = 'Study Timepoint')) +
  theme(text = element_text(face = 'bold', family = "serif"),
        axis.ticks.y=element_blank())
ggsave(twoWave_date_plot, 
       filename = "C:/Users/forev/Documents/Nicolas/karina_thesis/visuals/final/twoWave_date_plot.png")


