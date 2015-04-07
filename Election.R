library(dplyr)
library(ggplot2)
library(reshape2)
setwd("~/R Working Directory/Election_Data")
rm(list=ls())

#Create a vector with the links leading to CSV files that hold the column names
nameLinks <- c("http://www.fec.gov/finance/disclosure/metadata/cn_header_file.csv", 
              "http://www.fec.gov/finance/disclosure/metadata/indiv_header_file.csv", 
              "http://www.fec.gov/finance/disclosure/metadata/pas2_header_file.csv", 
              "http://www.fec.gov/finance/disclosure/metadata/cm_header_file.csv", 
              "http://www.fec.gov/finance/disclosure/metadata/ccl_header_file.csv",
              "http://www.fec.gov/finance/disclosure/metadata/oth_header_file.csv")

#Create a vector with the links leading to the 6 data frames
datLinks <- c("ftp://ftp.fec.gov/FEC/2016/cn16.zip",
              "ftp://ftp.fec.gov/FEC/2016/indiv16.zip",
              "ftp://ftp.fec.gov/FEC/2016/pas216.zip",
              "ftp://ftp.fec.gov/FEC/2016/cm16.zip",
              "ftp://ftp.fec.gov/FEC/2016/ccl16.zip",
              "ftp://ftp.fec.gov/FEC/2016/oth16.zip")

#Create a vector with the file names of each of the tables
fileName <- c('cn.txt',
              'itcont.txt',
              'itpas2.txt',
              'cm.txt',
              'ccl.txt',
              'itoth.txt')

#Create vectors describing the class of each column in each table
candClasses <- rep('character', 15)
indColClasses <- c(rep("character", 14), "numeric", rep("character",6))
comContrClasses <- c(rep('character', 14), 'numeric', rep('character', 7))
comClasses <- rep('character', 15)
linkageClasses <- rep('character', 7)
cm2cmClasses <- c(rep('character', 14), 'numeric', rep('character', 6))
#Place these vectors in a list in preparation for a loop
iterClasses <- list(candClasses, indColClasses, comContrClasses, comClasses, linkageClasses, cm2cmClasses)
#Create a list of 6 blank data frames in preparation for a loop
frames <- list(rep(data.frame(),6))

#Loop goes from 1 to 6.  
#Creates two temp files to place the 'names' data and the actual data
#Unzips and reads the actual data, and uses the iterClasses vector to properly class the variables
#Names the variables via the tempNames file, which reads the CSV
#Discard the temp files
for (i in 1:length(nameLinks)){
    tempNames <- tempfile()
    download.file(nameLinks[i], tempNames, method = "curl")
    temp <- tempfile()
    download.file(datLinks[i], temp, method = "curl")
    frames[[i]] <- read.table(unz(temp,fileName[i]),header = F, sep = "|",comment.char = "",stringsAsFactors = F,
                       colClasses = unlist(iterClasses[i]), quote = "")
    
    names(frames[[i]]) <- read.csv(tempNames,header = F, stringsAsFactor = F)
    unlink(temp)
    unlink(tempNames)
    remove(temp,tempNames)
}

#Place the items in the frames list into specific variables
#Delete the list items as they are used to save memory
candidates <- as.data.frame(frames[[1]])
frames[[1]] <- NULL
indiv2cm <- as.data.frame(frames[[1]])
frames[[1]] <- NULL
cm2cand <- as.data.frame(frames[[1]])
frames[[1]] <- NULL
committee <- as.data.frame(frames[[1]])
frames[[1]] <- NULL
linkage <- as.data.frame(frames[[1]])
frames[[1]] <- NULL
cm2cm <- as.data.frame(frames[[1]])

#Remove variables not being used
remove(frames, candClasses, cm2cmClasses, comClasses, comContrClasses, datLinks, fileName, 
       i, indColClasses, iterClasses, linkageClasses, nameLinks)


#Create a committee data frame with less variables
cmSmall <- select(committee, CMTE_ID, CMTE_NM, CMTE_PTY_AFFILIATION,ORG_TP)

#Change to date type where applicable
indiv2cm$TRANSACTION_DT <- as.Date(indiv2cm$TRANSACTION_DT,format = "%m%d%Y")
cm2cand$TRANSACTION_DT <- as.Date(cm2cand$TRANSACTION_DT, format = '%m%d%Y')
cm2cm$TRANSACTION_DT <- as.Date(cm2cm$TRANSACTION_DT, format = '%m%d%Y')

#Gain a view of which committees are giving to which other committees, along with
#associated political parties
# corps <- cmSmall %>%
#     inner_join(cm2cm, by = c("CMTE_ID" = "CMTE_ID")) %>%
#     inner_join(cmSmall, by = c("OTHER_ID" = "CMTE_ID")) %>%
#     filter(!ORG_TP.x %in% c('L', 'M', 'T', 'V') & !CMTE_PTY_AFFILIATION.x %in% c('REP', 'DEM'))
# 
# trimCorps <- select(corps, CMTE_NM.x,NAME, TRANSACTION_DT, TRANSACTION_AMT, CMTE_NM.y, CMTE_PTY_AFFILIATION.y)
# 
# #Reshape the data to show how much each committee has contributed to each party
# cmSpending <- dcast(corps,formula = CMTE_NM.x ~ CMTE_PTY_AFFILIATION.y,
#                 value.var = "TRANSACTION_AMT", fun.aggregate = sum)

days <- seq(as.Date('2015-01-01'), as.Date(Sys.Date()), 'day')

#Join names to committee IDs, take transactions in 2015, group by committee party
#sort by date, and add accumulator
    

cmGrpMoney <- arrange(expand.grid(CMTE_ID = cmSmall$CMTE_ID, OnDate = days), CMTE_ID)

cmGrpMoney <- cmGrpMoney %>%
    left_join(cmSmall, by = ('CMTE_ID' = 'CMTE_ID'))

#This data frame shows how much a committee has spent on other committees
indivAssets <- indiv2cm %>%
    group_by(CMTE_ID, TRANSACTION_DT) %>%
    arrange(TRANSACTION_DT) %>%
    summarise(IndSum = sum(TRANSACTION_AMT))

cm2cmLiabilities <- cm2cm %>%
    group_by(CMTE_ID, TRANSACTION_DT) %>%
    arrange(TRANSACTION_DT) %>%
    summarise(Liabilities = sum(TRANSACTION_AMT)) %>%
    select(CMTE_ID,TRANSACTION_DT, Liabilities)

#Shows how much a commmittee has received from other committees
cm2cmAssets <- cm2cm %>%
    group_by(OTHER_ID, TRANSACTION_DT) %>%
    arrange(TRANSACTION_DT) %>%
    summarise(Assets = sum(TRANSACTION_AMT)) %>%
    select(OTHER_ID,TRANSACTION_DT, Assets)

#Shows how much a committee has spent on candidates
cm2candLiabilities <- cm2cand %>%
    group_by(CMTE_ID, TRANSACTION_DT) %>%
    arrange(TRANSACTION_DT) %>%
    summarise(ToCand= sum(TRANSACTION_AMT)) %>%
    select(CMTE_ID,TRANSACTION_DT, ToCand)

#Shows how much a candidate committee has received from other committees (should be 0)
cm2candAssets <- cm2cand %>%
    group_by(OTHER_ID, TRANSACTION_DT) %>%
    arrange(TRANSACTION_DT) %>%
    summarise(CandReceived = sum(TRANSACTION_AMT)) %>%
    select(OTHER_ID,TRANSACTION_DT, CandReceived)

#Join the Asset and Liability data frames to the master group
cmGrpMoney <- cmGrpMoney %>%
    left_join(indivAssets, by = c('CMTE_ID' = 'CMTE_ID', 'OnDate' = 'TRANSACTION_DT'))

cmGrpMoney <- cmGrpMoney %>%
    left_join(cm2cmAssets, by = c('CMTE_ID' = 'OTHER_ID', 'OnDate' = 'TRANSACTION_DT'))

cmGrpMoney <- cmGrpMoney %>%
    left_join(cm2cmLiabilities, by = c('CMTE_ID' = 'CMTE_ID', 'OnDate' = 'TRANSACTION_DT'))

cmGrpMoney <- cmGrpMoney %>%
    left_join(cm2candLiabilities, by = c('CMTE_ID' = 'CMTE_ID', 'OnDate' = 'TRANSACTION_DT'))

cmGrpMoney <- cmGrpMoney %>%
    left_join(cm2candAssets, by = c('CMTE_ID' = 'OTHER_ID', 'OnDate' = 'TRANSACTION_DT'))

#Replace NAs induced by the join to 0
cmGrpMoney[is.na(cmGrpMoney)] <- 0

#Grouped by Committee ID, show the cumuluative sums of: funds given by individuals,
#funds received by a candidate committee (should be 0), funds spent on committees,
#and funds spent on candidates
cmGrpMoney <- cmGrpMoney %>%
    group_by(CMTE_ID) %>%
    mutate(FromCommittees = cumsum(Assets), ReceivedCandidates = cumsum(CandReceived),
           Funds = cumsum(IndSum), SpentCommittees = cumsum(Liabilities), SpentCandidates = cumsum(ToCand), 
           TotalRaised = FromCommittees+Funds+ReceivedCandidates, TotalSpent = SpentCommittees+SpentCandidates) %>%
    select(CMTE_ID, CMTE_NM, OnDate, Funds, FromCommittees, TotalRaised, SpentCommittees, SpentCandidates, TotalSpent)



check <- cmGrpMoney %>%
    group_by(CMTE_ID) %>%
    summarise(raised = sum(TotalRaised), spent = sum(TotalSpent)) %>%
    filter(raised == 0 & spent == 0) %>%
    select(CMTE_ID)

check <- check$CMTE_ID

cmGrpMoney <- cmGrpMoney[(!cmGrpMoney$CMTE_ID %in% check),]

cmGrpMoney$CMTE_NM <- as.factor(cmGrpMoney$CMTE_NM)
write.table(cmGrpMoney, 'cmGrpMoney.txt', col.names = F, row.names = F, sep = '|', quote = F)
write.table(levels(cmGrpMoney$CMTE_NM), 'orgs.txt', col.names = F, row.names = F, sep = '|', quote = F)
