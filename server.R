library(shiny)
library(rCharts)
library(data.table)

cmGrpMoney <- fread("cmGrpMoney.txt", sep = '|')
setnames(cmGrpMoney, c('CMTE_ID', 'CMTE_NM', 'OnDate', 'Funds', 'FromCommittees', 'TotalRaised', 'SpentCommittees', 'SpentCandidates', 'TotalSpent'))
orgs <- fread('orgs.txt', sep = '\n', header = F)$V1

shinyServer(function(input, output, session){
    
    updateSelectizeInput(session, 'committee', choices = orgs, server = T, options = list(placeholder = 'Start typing!'))
    
    output$raised <- renderChart({
        dat <- cmGrpMoney[cmGrpMoney$CMTE_NM == input$committee,]
        p1 <- mPlot(x = 'OnDate', y = list('TotalRaised', 'TotalSpent'), data = dat, type = 'Line', 
                    labels = c('Total Raised', 'Total Spent'), 
                    pointSize = 0, lineSize = 1)

        p1$set(dom = 'raised', lineColors = c('green', 'red'))
        return(p1)})


})