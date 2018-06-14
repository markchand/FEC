################################
# Data Validation Exercise     #
# 09/13/2017                   #
# by @markchand                #
# See RMarkdown for details    #
################################


setwd("~/Google Drive/PhD/Money in Politics/FEC")
library(tidyverse)
library(dplyr)
library(WriteXLS)
Candidates <- read_csv("CandidateSummaryAction_nodollars.csv")

House <- Candidates %>% filter(can_off=="H")
HouseGOPandDem <- House %>% filter(can_par_aff=="REP" | can_par_aff=="DEM")
party <- c("DEM","REP")
a <- as.data.frame(party)
names(a)[1] <- "can_par_aff"
b <- as.data.frame(rbind(nrow(HouseDem  <- House %>% filter(can_par_aff=="DEM")), nrow(HouseGOP <- House %>% filter(can_par_aff=="REP"))))
names(b)[1] <- "No_of_Cands"
b <- cbind(a,b)
HouseGOPandDem %>% 
  group_by(can_par_aff) %>% 
  summarise(total_raised=sum(tot_con, na.rm=TRUE)) -> c
HouseGOPandDem %>% 
  group_by(can_par_aff) %>% 
  summarise(total_spent=sum(tot_dis, na.rm=TRUE)) -> d
HouseGOPandDem %>% 
  group_by(can_par_aff) %>% 
  summarise(cash_on_hand=sum(cas_on_han_clo_of_per, na.rm=TRUE)) -> e
HouseGOPandDem %>% 
  group_by(can_par_aff) %>% 
  summarise(total_from_PACs=sum(oth_com_con, na.rm=TRUE)) -> f
HouseGOPandDem %>% 
  group_by(can_par_aff) %>% 
  summarise(total_from_Indivs=sum(ind_con, na.rm=TRUE)) -> g
h <- cbind(a,b,c,d,e,f,g)

library(WriteXLS)
WriteXLS(h, ExcelFileName = "2016 House Candidates.xls")

Ind_Expenditures <- read_csv("independent-expenditure-nodollars.csv")
Ind_Expenditures %>% 
  group_by(spe_nam) %>% 
  summarise(total_spent = sum(exp_amo, na.rm=TRUE),maybe_raised = sum(agg_amo, na.rm=TRUE)) -> Amounts 

Ind_Expenditures %>% 
  group_by(spe_nam) %>% 
  select(spe_nam, can_par_aff, sup_opp) -> View

Amounts %>% top_n(5, total_spent) -> TopAmounts
WriteXLS(TopAmounts, ExcelFileName = "2016 Super PACs.xls")