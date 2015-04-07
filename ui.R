library(shiny)
require(rCharts)


shinyUI(fluidPage(
    titlePanel('PAC Fundraising vs. Spending'),
    sidebarPanel(
        selectizeInput('committee', choices = NULL, label = 'Search for a Committee')
    ),
    mainPanel(
        showOutput("raised", 'morris')
        )
    )

)