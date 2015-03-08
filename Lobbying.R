#Prepare the environment for Lobbying data
library(XML)
library(dplyr)
setwd("~/R Working Directory/Election_Data")
rm(list=ls())

#Place all items in this directory into a vector
files <- dir('2015_1')

#Create blank data frames to append to later
filing <- data.frame()
registrant <- data.frame()

#For each file, we will read and parse the XML data. Then we will read in 
#basic filing information, and registrant information.
for (file in files){
    
    data <- xmlParse(paste('2015_1/',file,sep = ""), useInternalNodes = T)
    
    #Filing Key data frame
    filingID <- xpathSApply(data, "/PublicFilings/Filing", function(x) xmlGetAttr(x,'ID'))
    year <- xpathSApply(data, "/PublicFilings/Filing", function(x) xmlGetAttr(x,'Year'))
    received <- xpathSApply(data, "/PublicFilings/Filing", function(x) xmlGetAttr(x,'Received'))
    amount <- as.numeric(xpathSApply(data, "/PublicFilings/Filing", function(x) xmlGetAttr(x,'Amount')))
    
    reg <- do.call(rbind, xpathApply(data, "/PublicFilings/Filing", function(node) {
        
        id <- xmlGetAttr(node, "ID")
        
        xp <- "./Registrant"
        registrant <- xpathSApply(node, xp, xmlGetAttr, 'RegistrantID')
        registrant <- xpathSApply(node, xp, xmlGetAttr, 'RegistrantID')
        data.frame(ID=id, RegistrantID=registrant, stringsAsFactors = FALSE)}))
    temp<- cbind(reg,data.frame(Year = year, Amount = amount, stringsAsFactors = F))
    filing <- rbind(filing, temp)
    
    
    #Registrant Key data frame
    regNum <- xpathSApply(data, "/PublicFilings/Filing/Registrant", function(x) xmlGetAttr(x,'RegistrantID'))
    regName <-xpathSApply(data, "/PublicFilings/Filing/Registrant", function(x) xmlGetAttr(x,'RegistrantName'))
    regAd <-xpathSApply(data, "/PublicFilings/Filing/Registrant", function(x) xmlGetAttr(x,'Address'))
    rtemp <- data.frame(RegistrantID = regNum, Name = regName, Address = regAd, stringsAsFactors = F)
    registrant <- unique(rbind(registrant, rtemp))
}

#Delete everything except data frames from memory
rm(list=(ls()[ls()!=c("filing", "registrant")])) 

by_registrant <- filing %>%
    group_by(RegistrantID) %>%
    summarise(Amount = sum(Amount,na.rm = T)) %>%
    left_join(registrant) %>%
    arrange(desc(Amount))
