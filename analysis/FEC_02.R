#############################################
# Network-ifying FEC campaign data (take 2) #
# Script by Mark Hand                       #
# Oct 2017                                  #
#############################################

# ---- Getting started ----
# This script generates a network map of donations to candidates 
# for US federal office in 2017-2018, according to the US government's 
# Federal Election Commission. Federal law requires that campaigns 
# report any donation over $200. A note of caution to the reader: This 
# does not include donations to or expenditures from Super PACs, which 
# have to report political donations but (I think) not non-campaign 
# expenditures , or to 5014c4 organiations, which do not have to 
# (publicly) political donations. It also doesn't include money spent 
# on "issue ads," which don't (legally) qualify as political speech. 

# Anywho, you can get all this data directly from the FEC  
# http://classic.fec.gov/finance/disclosure/ftpdet.shtml#a2017_2018
# or from my Dropbox
# https://www.dropbox.com/sh/dtvzgyp4blf3mb3/AAC8OJNTjH3xisLro3sMRHDya?dl=0
# I converted each of these .txt files to .csv using Text to Column in 
# Excel; be careful here, as there seem to be more observations in 
# those files than in the direct .txt files I am trying to learn how 
# to read in.

# ---- Working directory and libraries ----
# setwd("~/Dropbox/PhD_data/FEC")
library(data.table)
library(tidyverse)
library(WriteXLS)
library(readxl)
library(readr)
library(stringr)
library(igraph)
library(statnet)
AlexID <- c("H8TX07124", "C00639187")
# Tutorial http://www.kateto.net/wp-content/uploads/2015/06/Polnet%202015%20Network%20Viz%20Tutorial%20-%20Ognyanova.pdf

# ---- Importing and Joining datasets ----
# ---- ---- Donations (links) ---- 
US1 <- read_csv("~/Dropbox/PhD_data/FEC/itcont_2018_20170531_20171103.csv", col_names=F)
ind_names <- c("CMTE_ID","AMNDT_IND","RPT_TP","TRANSACTION_PGI","IMAGE_NUM","TRANSACTION_TP","ENTITY_TP","NAME","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION","TRANSACTION_DT","TRANSACTION_AMT","OTHER_ID","TRAN_ID","FILE_NUM","MEMO_CD","MEMO_TEXT","SUB_ID")
colnames(US1) = ind_names
US2 <- read_csv("~/Dropbox/PhD_data/FEC/itcont_2018_20120521_20170530.csv", col_names=F)
colnames(US2) = ind_names
US3 <- read_csv("~/Dropbox/PhD_data/FEC/itcont_2018_invalid_dates.csv", col_names=F)
colnames(US3) = ind_names
donations <- rbind(US1, US2, US3)
head(donations)
donations$IMAGE_NUM <- as.numeric(donations$IMAGE_NUM)
donations$ZIP_CODE <- as.numeric(donations$ZIP_CODE)
donations$TRANSACTION_DT <- as.numeric(donations$TRANSACTION_DT)
donations$TRANSACTION_AMT <- as.numeric(donations$TRANSACTION_AMT)
donations$FILE_NUM <- as.numeric(donations$FILE_NUM)
donations$SUB_ID <- as.numeric(donations$SUB_ID)
donations %>% 
  rename("from" = "NAME") -> donations
str(donations)
write.csv(donations, file = "donations.csv")
# Lots of warning messages here about NAs introduced by coercion
# Also, surely there's a more elegant way of going about converting these variables? 

comtocom <- read_csv("itoth.csv", col_names=F)
itothnames <- c("CMTE_ID","AMNDT_IND","RPT_TP","TRANSACTION_PGI","IMAGE_NUM","TRANSACTION_TP","ENTITY_TP","NAME","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION","TRANSACTION_DT","TRANSACTION_AMT","OTHER_ID","TRAN_ID","FILE_NUM","MEMO_CD","MEMO_TEXT","SUB_ID")
colnames(comtocom) = itothnames
head(comtocom)
comtocom %>% 
  rename("from" = "OTHER_ID") -> comtocom
str(comtocom)

comtocan <- read_csv("itpas2.csv", col_names=F)
itpas2names <- c("CMTE_ID","AMNDT_IND","RPT_TP","TRANSACTION_PGI","IMAGE_NUM","TRANSACTION_TP","ENTITY_TP","NAME","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION","TRANSACTION_DT","TRANSACTION_AMT","OTHER_ID","CAND_ID","TRAN_ID","FILE_NUM","MEMO_CD","MEMO_TEXT","SUB_ID")
colnames(comtocan) = itpas2names
head(comtocan)
comtocan %>% 
  rename("from" = "OTHER_ID") -> comtocan
str(comtocan)

links <- bind_rows(donations, comtocom, comtocan)
links %>% rename("to" = "CMTE_ID") -> links
write.csv(links, file = "links.csv")

# ---- ---- Committees and Donors (nodes) ----
candidates <- read_csv("cn.csv", col_names=F)
can_col_names <- c("CAND_ID","CAND_NAME","CAND_PTY_AFFILIATION","CAND_ELECTION_YR","CAND_OFFICE_ST","CAND_OFFICE","CAND_OFFICE_DISTRICT","CAND_ICI","CAND_STATUS","CAND_PCC","CAND_ST1","CAND_ST2","CAND_CITY","CAND_ST","CAND_ZIP")
colnames(candidates) = can_col_names
head(candidates)

committees <- read_csv("cm.csv", col_names=F)
com_col_names <- c("CMTE_ID","CMTE_NM","TRES_NM","CMTE_ST1","CMTE_ST2","CMTE_CITY","CMTE_ST","CMTE_ZIP","CMTE_DSGN","CMTE_TP","CMTE_PTY_AFFILIATION","CMTE_FILING_FREQ","ORG_TP","CONNECTED_ORG_NM","CAND_ID")
colnames(committees) = com_col_names
head(committees)

recipients <- full_join(candidates, committees)

donors <- donations %>% 
  select("from","CITY","STATE","ZIP_CODE","EMPLOYER","OCCUPATION") %>% 
  distinct()

nodes <- bind_rows(recipients, donors)
nodes <- mutate(nodes, id = rownames(nodes))
nodes <- nodes %>% select(id, everything())
str(nodes)
head(nodes)
tail(nodes)
write.csv(nodes, file = "nodes.csv")

# ---- Next steps ---- 
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
# Use igraph functions to build network, somehow, and then 
# Construct a shortest-path shiny app, whatever that is? 

head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))
# Per Katya: "Notice that there are more links than unique from-to combinations. That means we have cases in the data where there are multiple links between the same two nodes. We will collapse all links of the same type between the same two nodes by summing their weights, using aggregate() by “from”, “to”, & “type"
# But this didn't work 

links %>% group_by(to, from) -> testlinks
links %>% summarize(to, from, testweights = sum(TRANSACTION_AMT))
links %>% select (to, from)
