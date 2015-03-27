library(shiny)
library(ggplot2)
classes <- c('factor', 'factor', 'Date', rep('numeric', 6))
cmGrpMoney <- read.table("cmGrpMoney.txt", sep = '|', quote = "", comment.char = "",colClasses = classes)
names(cmGrpMoney) <- c('CMTE_ID', 'CMTE_NM', 'OnDate', 'Funds', 'FromCommittees', 'TotalRaised', 'SpentCommittees', 'SpentCandidates', 'TotalSpent')

shinyServer(function(input, output){
    output$table <- renderDataTable(cmGrpMoney)
    
    output$plot <- renderPlot({
        dat <- cmGrpMoney[cmGrpMoney$CMTE_NM == input$committee,]
        
        ggplot(aes(x = OnDate, y = TotalRaised), data = dat) + geom_line()
    })
})