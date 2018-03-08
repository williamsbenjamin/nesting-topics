# 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Hand Nested App

library(shiny)
library(sunburstR)
library(shinydashboard)
library(tidyverse)
ui <- dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar( 
    sidebarMenu(
      menuItem("Sunburst Plot", tabName = "sunbrstPlot")
      
    )
  ),
  
  dashboardBody( tabBox(id = "sunbrstPlot", width = "100%", height = "1000px",
                        sunburstOutput("sunburstPlot", height = "750", width = "100%")
  )
  
  )
)         

server <- function(input, output) { 
  
  # Create Sunburst plot
  cols_124_test <- read.csv("colors_124.csv")
    output$sunburstPlot <- renderSunburst({ 
    first500try <- read.csv("500topicsfirsttry.csv",header=F,stringsAsFactors = FALSE)
    sunburst(first500try,colors=cols_124_test)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

