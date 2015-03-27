library(shiny)
library(ggplot2)
source('Election.R')

shinyServer(function(input, output){
    output$table <- renderDataTable(cmGrpMoney)
    
    output$plot <- renderPlot({
        dat <- cmGrpMoney[cmGrpMoney$CMTE_NM == input$committee,]
        
        ggplot(aes(x = OnDate, y = TotalRaised), data = dat) + geom_line()
    })
})