library(dplyr)
library(ggplot2)
setwd("~/R Working Directory/Election_Data")

rm(list=ls())

#Uploads a data frame of all of the candidates
cand <- read.table("cn.txt",header = F, sep = "|",comment.char = "",stringsAsFactors = F,quote = "")
names(cand) <- read.csv("cn_header_file.csv",header = F, stringsAsFactor = F)

#Uploads a data frame of all the individual contributions to committees
indContr <- read.table("itcont.txt", sep= "|", comment.char = "", stringsAsFactors = F, quote = "")
names(indContr) <- read.csv("indiv_header_file.csv", header = F, stringsAsFactor = F)

#Uploads the contributions from committees to candidates
comContr <- read.table("itpas2.txt", sep= "|", comment.char = "", stringsAsFactors = F, quote = "")
names(comContr) <- read.csv("pas2_header_file.csv", header = F, stringsAsFactors = F)

#Uploads the list of committees
committee <- read.table("cm.txt", sep = "|", comment.char = "", stringsAsFactors = F, quote = "")
names(committee) <- read.csv("cm_header_file.csv",header = F,stringsAsFactors = F)

#Uploads candidate/committee linkage file
linkage <- read.table("ccl.txt", sep = "|", comment.char = "", stringsAsFactors = F, quote = "")
names(linkage) <- read.csv("ccl_header_file", header = F, stringsAsFactors = F)
#names(linkage) <- c("CAND_ID", "CAND_ELECTION_YR", "FEC_ELECTION_YR", "CMTE_ID", "CMTE_TP", "CMTE_DSGN", "LINKAGE_ID")

#Uploads change of funds between committees
cm2cm <- read.table("itoth.txt", sep = "|", comment.char = "", stringsAsFactors = F, quote = "")
names(cm2cm) <- read.csv("oth_header_file.csv", header = F, comment.char = "", stringsAsFactors = F, quote = "")

candMoney <- cand %>%
    inner_join(comContr)

candMoney[, c(2, 3, 5)] <- lapply(candMoney[, c(2,3,5)],function(x) as.factor(x))
candMoney$ZIP5 <- substr(candMoney$CAND_ZIP,1,5)

candAg <- candMoney %>%
    group_by(CAND_PTY_AFFILIATION) %>%
    summarise(n = n(), Amount = sum(TRANSACTION_AMT))

indCand <- indContr %>%
    inner_join(comContr, c("CMTE_ID" = "CMTE_ID"),)

