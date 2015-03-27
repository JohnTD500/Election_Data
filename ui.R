library(shiny)
source('Election.R')
shinyUI(fluidPage(
    titlePanel('Election Data'),
    sidebarPanel(
        selectInput('committee', choice = levels(cmGrpMoney$CMTE_NM), label = 'Select a Committee')
    ),
    mainPanel('Plot', plotOutput('plot')

)))