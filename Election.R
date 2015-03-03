library(dplyr)
library(ggplot2)
library(reshape2)
setwd("~/R Working Directory/Election_Data")

rm(list=ls())

#Uploads a data frame of all of the candidates
cand <- read.table("cn.txt",header = F, sep = "|",comment.char = "",stringsAsFactors = F,quote = "")
names(cand) <- read.csv("cn_header_file.csv",header = F, stringsAsFactor = F)

#Uploads a data frame of all the individual contributions to committees
indColClasses <- c(rep("character", 4), "numeric", rep("character",5), "numeric", rep("character", 3), "integer", rep("character",6))
indContr <- read.table("itcont.txt", sep= "|", comment.char = "", stringsAsFactors = F, quote = "", colClasses = indColClasses)
names(indContr) <- read.csv("indiv_header_file.csv", header = F, stringsAsFactor = F)
indContr$TRANSACTION_DT <- as.Date(indContr$TRANSACTION_DT,format = "%m%d%Y")

#Uploads the contributions from committees to candidates
comContr <- read.table("itpas2.txt", sep= "|", comment.char = "", stringsAsFactors = F, quote = "")
names(comContr) <- read.csv("pas2_header_file.csv", header = F, stringsAsFactors = F)

#Uploads the list of committees
committee <- read.table("cm.txt", sep = "|", comment.char = "", stringsAsFactors = F, quote = "")
names(committee) <- read.csv("cm_header_file.csv",header = F,stringsAsFactors = F)

#Uploads candidate/committee linkage file
linkage <- read.table("ccl.txt", sep = "|", comment.char = "", stringsAsFactors = F, quote = "")
names(linkage) <- read.csv("ccl_header_file.csv", header = F, stringsAsFactors = F)

#Uploads change of funds between committees
cm2cm <- read.table("itoth.txt", sep = "|", comment.char = "", stringsAsFactors = F, quote = "")
names(cm2cm) <- read.csv("oth_header_file.csv", header = F, comment.char = "", stringsAsFactors = F, quote = "")

#Select relevent rows from the committee table
smallCM <- select(committee, CMTE_ID, CMTE_NM, CMTE_PTY_AFFILIATION)

#Gain a view of which committees are giving to which other committees, along with
#associated political parties
cmView <- smallCM %>%
    inner_join(cm2cm, by = c("CMTE_ID" = "CMTE_ID")) %>%
    inner_join(smallCM, by = c("OTHER_ID" = "CMTE_ID"))

#Reshape the data to show how much each committee has contributed to each party
cmSpending <- dcast(cmView,formula = CMTE_NM.x ~ CMTE_PTY_AFFILIATION.y,
                value.var = "TRANSACTION_AMT", fun.aggregate = sum)


indMoney <- smallCM %>%
    inner_join(indContr, by = c("CMTE_ID" = "CMTE_ID")) %>%
    filter(TRANSACTION_DT > as.Date("2014-12-31"))

grpMoney <- indMoney %>%
    group_by(CMTE_PTY_AFFILIATION) %>%
    arrange(TRANSACTION_DT)


grpMoney <- grpMoney %>%
    mutate(Funds = cumsum(TRANSACTION_AMT))

repdem <- grpMoney[grpMoney$CMTE_PTY_AFFILIATION == "REP" | grpMoney$CMTE_PTY_AFFILIATION == "DEM",]

ggplot(repdem, aes(x = TRANSACTION_DT, y = Funds)) +
    geom_line(aes(colour = CMTE_PTY_AFFILIATION))
    

