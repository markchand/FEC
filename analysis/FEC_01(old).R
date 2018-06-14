#### Getting started ####
# All data available here: 
# https://www.dropbox.com/sh/dtvzgyp4blf3mb3/AAC8OJNTjH3xisLro3sMRHDya?dl=0

# Original data available here: 
  # http://classic.fec.gov/finance/disclosure/ftpdet.shtml#a2017_2018
# This analysis requires downloading: 
  # cm18.zip
  # cn18.zip
  # ccl18.zip
  # indiv18.zip
  # They column names are two links away, on the metadata pages linked there. I've included that below, so that you don't have to. 
# I converted each of these .txt files to .csv using Text to Column in Excel; be careful here, as there seem to be more observations in those files than in the direct .txt files I am now trying to learn how to read in. 
setwd("~/Dropbox/PhD_data/FEC")
library(data.table)
library(tidyverse)
library(WriteXLS)
library(readxl)
library(readr)
library(stringr)
AlexID <- c("H8TX07124", "C00639187")
?read_csv
#### Importing and cleaning and joining ####
US1 <- read_csv("~/Dropbox/PhD_data/FEC/itcont_2018_20170531_20171103.csv", col_names=F)
ind_names <- c("CMTE_ID","AMNDT_IND","RPT_TP","TRANSACTION_PGI","IMAGE_NUM","TRANSACTION_TP","ENTITY_TP","NAME","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION","TRANSACTION_DT","TRANSACTION_AMT","OTHER_ID","TRAN_ID","FILE_NUM","MEMO_CD","MEMO_TEXT","SUB_ID")
colnames(US1) = ind_names
US2 <- read_csv("~/Dropbox/PhD_data/FEC/itcont_2018_20120521_20170530.csv", col_names=F)
colnames(US2) = ind_names
US3 <- read_csv("~/Dropbox/PhD_data/FEC/itcont_2018_invalid_dates.csv", col_names=F)
colnames(US3) = ind_names
US <- rbind(US1, US2, US3)
US$donations <- as.numeric(US$donations)

candidates <- read_csv("~/Dropbox/PhD_data/FEC/cn.csv", col_names=F)
can_col_names <- c("CAND_ID","CAND_NAME","CAND_PTY_AFFILIATION","CAND_ELECTION_YR","CAND_OFFICE_ST","CAND_OFFICE","CAND_OFFICE_DISTRICT","CAND_ICI","CAND_STATUS","CAND_PCC","CAND_ST1","CAND_ST2","CAND_CITY","CAND_ST","CAND_ZIP")
colnames(candidates) = can_col_names

committees <- read_csv("~/Dropbox/PhD_data/FEC/cm.csv", col_names=F)
com_col_names <- c("CMTE_ID","CMTE_NM","TRES_NM","CMTE_ST1","CMTE_ST2","CMTE_CITY","CMTE_ST","CMTE_ZIP","CMTE_DSGN","CMTE_TP","CMTE_PTY_AFFILIATION","CMTE_FILING_FREQ","ORG_TP","CONNECTED_ORG_NM","CAND_ID")
colnames(committees) = com_col_names

links <- read_csv("~/Dropbox/PhD_data/FEC/ccl.csv")
links_col_names <- c("CAND_ID","CAND_ELECTION_YR","FEC_ELECTION_YR","CMTE_ID","CMTE_TP","CMTE_DSGN","LINKAGE_ID")
colnames(links) = links_col_names

committees_w_links <- left_join(committees, links)
US_and_candidates <- left_join(US, candidates, by = c("CMTE_ID" = "CAND_PCC"))
US_all <- left_join(US_and_candidates, committees_w_links, by = "CMTE_ID")

#### Old (ignore) ####
# TX1 <- read_excel("~/Dropbox/PhD_data/FEC/TX_itcont_2018_20170531_20171103.xlsx")
# TX2 <- read_excel("~/Dropbox/PhD_data/FEC/TX_itcont_2018_20120521_20170530.xlsx")
# TX3 <- read_excel("~/Dropbox/PhD_data/FEC/TX_itcont_2018_invalid_dates.xlsx")
# TX <- rbind(TX1, TX2, TX3)
# TX_and_candidates <- left_join(TX, candidates, by = c("CMTE_ID" = "CAND_PCC"))
# TX_all <- left_join(TX_and_candidates, committees_w_links, by = "CMTE_ID")
# WriteXLS(TX_and_candidates, ExcelFileName = "TX Donors 2017, maybe.xls")
# WriteXLS(US_and_candidates, ExcelFileName = "US Donors 2017, maybe.xls")

#### Searching for donors and links #####

# Filter for House Candidates in Texas
US_all %>% 
  filter(CAND_OFFICE=="H" & CAND_OFFICE_ST=="TX") %>%   
  group_by(CAND_NAME) %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT))

# Filter for Committees in Texas 
US_all %>% 
  filter(CMTE_ST=="TX") %>% 
  group_by(CMTE_NM) %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT)) 

# Filter for House Candidates in District 7 
US_all %>% 
  filter(CAND_OFFICE=="H" & CAND_OFFICE_ST=="TX" & CAND_OFFICE_DISTRICT==7) %>% 
  group_by(CAND_NAME) %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT))

# Filter for a particular donor 
US_all %>% 
  group_by(CMTE_NM) %>% 
  filter(NAME=="MAYER, JOHN") %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT))

# Filter for a particular donor, but check other information
US_all %>% 
  filter(NAME=="MAYER, JOHN") %>% 
  select(NAME, CITY, STATE, CMTE_NM, TRANSACTION_AMT) 

# Make a list of Alex's donors 
AlexTDonors <- US_all %>% 
  group_by(NAME) %>% 
  filter(CMTE_ID=="C00639187") %>% 
  distinct(NAME)
AlexTDonors <- as.vector(AlexTDonors$NAME)

# Other campaigns that Alex's donors have donated to
US_all %>% 
  filter(NAME=="TAPPER, FANNIE") %>% 
  group_by(CMTE_NM) %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT))

# Donors to other campaigns that have NOT donated to Alex 
US_all %>% 
  subset(!NAME %in% AlexTDonors) %>% 
  group_by(NAME) %>% 
  filter(CMTE_NM=="KOPSER FOR CONGRESS") %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT))
# Build a function here? Find a way to do it faster? 

# All of Ossoff's Texas donors 
OssoffTX <- US_all %>% 
  filter(CMTE_NM=="JON OSSOFF FOR CONGRESS" & STATE=="TX") %>% 
  # subset(!NAME %in% AlexTDonors) %>% 
  group_by(NAME) %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT)) %>% 
  select("CITY") 
  # That was way faster! 
WriteXLS(OssoffTX, ExcelFileName = "Ossoff Donors based in TX.xls")

# Beto's donors 
US_all %>% 
  filter(CMTE_NM=="BETO FOR TEXAS") %>% 
  group_by(NAME) %>% 
  summarise(TOTAL = sum (TRANSACTION_AMT)) 
# %>% filter(TOTAL>5400)

#### Building my igraph lists ####
TX_edges <- US_all %>% 
  filter(CMTE_ST=="TX" | STATE=="TX") %>% 
  select(NAME, CMTE_NM, TRANSACTION_AMT)
edge_names <- c("from", "to", "weight")
colnames(TX_edges) = edge_names

TX_donors <- US_all %>% 
  filter(STATE=="TX") %>% 
  distinct(NAME) %>% 
  mutate(NAME, type = "Donor")
colnames(TX_donors) = c("name", "type")

TX_committees <- US_all %>% 
  filter(CMTE_ST=="TX") %>% 
  distinct(CMTE_NM) %>% 
  mutate(CMTE_NM, type = "Committee")
colnames(TX_committees) = c("name", "type")

TX_nodes <- rbind(TX_donors, TX_committees)
nrow(TX_nodes); length(unique(TX_nodes$name))
nrow(TX_edges); nrow(unique(TX_edges[,c("from", "to")]))
# I don't understand this part, because beer, but it prevented one type of error. 
TX_edges <- aggregate(TX_edges[,3], TX_edges[,-3], sum)
TX_edges <- TX_edges[order(TX_edges$from, TX_edges$to),]

TX_web <- graph_from_data_frame(d=TX_edges, vertices=TX_nodes, directed=T)
# Error in graph_from_data_frame(d = TX_edges, vertices = TX_nodes, directed = T) : 
#  Duplicate vertex names
# In addition: Warning message:
#  In graph_from_data_frame(d = TX_edges, vertices = TX_nodes, directed = T) :
#  In `vertices[,1]' `NA' elements were replaced with string "NA"
  
#### Building lists for Kumu ####

# Edges 
# All donations to Democratic District 7 Candidates from anywhere
# Edges All donations to Beto for Congress or Ossoff for Congress from Texas 
# Nodes: Commitees and Candidates for District 7; their donors tagged; Ossoff donors tagged; Beto donors tagged

# Filter for House Candidates in District 7 
TX_7_donations <- US_all %>% 
  filter((CAND_OFFICE=="H" & CAND_OFFICE_ST=="TX" & CAND_OFFICE_DISTRICT==7 & CMTE_PTY_AFFILIATION!="REP"))
Beto_donations <- US_all %>% 
  filter(CMTE_NM=="BETO FOR TEXAS")
Ossoff_TX_donations <- US_all %>% 
  filter(CMTE_NM=="JON OSSOFF FOR CONGRESS" & STATE=="TX")
TX_7_edges <- rbind(TX_7_donations, Beto_donations, Ossoff_TX_donations)
TX_7_edges <- rename(TX_7_edges, "from" = "NAME")
TX_7_edges <- rename(TX_7_edges, "to" = "CMTE_NM")
WriteXLS(TX_7_edges, ExcelFileName = "Kumu test.xls")
write.xlsx(TX_7_edges, "Kumu test.xlsx")

#### Next: ####
# Figure out how to create a dataframe with all of the people to whom Alex T's donors have donated.
# Figure out why there are duplicates here. 
# subset(NAME %in% AlexTDonors) %>% 
# Get all this into igraph somehow 
# Figure out how to set this up so that someone else could replciate it without using Excel. 
  # test <- read.table(file="~/Dropbox/PhD_data/FEC/itcont_2018_20170531_20171103.txt", sep="|", header=F, fill=T)