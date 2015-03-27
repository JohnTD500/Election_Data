library(shiny)
classes <- c('factor', 'factor', 'Date', rep('numeric', 6))
cmGrpMoney <- read.table("cmGrpMoney.txt", sep = '|', quote = "", comment.char = "",colClasses = classes)
names(cmGrpMoney) <- c('CMTE_ID', 'CMTE_NM', 'OnDate', 'Funds', 'FromCommittees', 'TotalRaised', 'SpentCommittees', 'SpentCandidates', 'TotalSpent')
shinyUI(fluidPage(
    titlePanel('Election Data'),
    sidebarPanel(
        selectInput('committee', choice = levels(cmGrpMoney$CMTE_NM), label = 'Select a Committee')
    ),
    mainPanel('Plot', plotOutput('plot')

)))