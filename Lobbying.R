#XML data gained from the Lobbying Disclosure Act database

#Prepare the environment for Lobbying data
library(XML)
library(dplyr)
setwd("~/R Working Directory/Election_Data")
rm(list=ls())

#Download the file directly from the government, then unzip into a directory
#temp <- tempfile()
#download.file('http://soprweb.senate.gov/downloads/2015_1.zip', temp, method = 'curl')
#unz(temp, '2015_1')
#unlink(temp)
#rm(temp)

#Place all items in this directory into a vector
files <- dir('2015_1')[1:2]

#Create blank data frames to append to later
filing <- data.frame()
registrant <- data.frame()
client <- data.frame()

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
        data.frame(ID=id, RegistrantID=registrant, stringsAsFactors = FALSE)}))
    
    clientID <- xpathSApply(data, '/PublicFilings/Filing/Client', function(x) xmlGetAttr(x, 'ClientID'))
    
    temp <- cbind(reg,data.frame(Year = year, Amount = amount, ClientID = clientID, stringsAsFactors = F))
    filing <- rbind(filing, temp)
    
    #Client Key data frame
    clientID <- xpathSApply(data, '/PublicFilings/Filing/Client', function(x) xmlGetAttr(x, 'ClientID'))
    clientName <- xpathSApply(data, '/PublicFilings/Filing/Client', function(x) xmlGetAttr(x, 'ClientName'))
    clientState <- xpathSApply(data, '/PublicFilings/Filing/Client', function(x) xmlGetAttr(x, 'ClientState'))
    clientCountry <- xpathSApply(data, '/PublicFilings/Filing/Client', function(x) xmlGetAttr(x, 'ClientCountry'))
    clientDesc <- xpathSApply(data, '/PublicFilings/Filing/Client', function(x) xmlGetAttr(x, 'GeneralDescription'))
    
    ctemp <- data.frame(clientID = clientID, Name = clientName, State = clientState,
                        Country = clientCountry, Description = clientDesc,
                        stringsAsFactors = F)
    client <- rbind(client, ctemp)
    cdups <- duplicated(client[,c('clientID', 'Name')])
    client <- client[!cdups,]
    
    #Registrant Key data frame
    regNum <- xpathSApply(data, "/PublicFilings/Filing/Registrant", function(x) xmlGetAttr(x,'RegistrantID'))
    regName <-xpathSApply(data, "/PublicFilings/Filing/Registrant", function(x) xmlGetAttr(x,'RegistrantName'))
    regAd <-xpathSApply(data, "/PublicFilings/Filing/Registrant", function(x) xmlGetAttr(x,'Address'))
    regDesc <-xpathSApply(data, "/PublicFilings/Filing/Registrant", function(x) xmlGetAttr(x,'GeneralDescription'))
    rtemp <- data.frame(RegistrantID = regNum, Name = regName, Address = regAd,
                        Description = regDesc, stringsAsFactors = F)
    registrant <- rbind(registrant, rtemp)
    dups <- duplicated(registrant[,c('RegistrantID', 'Name')])
    registrant <- registrant[!dups,]
    

}

#Delete everything except data frames from memory
remove(reg, ctemp, rtemp, temp, amount, data, file, files, filingID, 
               received, regAd, regName, regNum, year, clientCountry, clientDesc,
       clientID, clientName, clientState, dups, cdups, regDesc)

by_registrant <- filing %>%
    group_by(RegistrantID) %>%
    summarise(Amount = sum(Amount,na.rm = T), Number = n()) %>%
    left_join(registrant) %>%
    arrange(desc(Amount))

#Issues Key data frame

