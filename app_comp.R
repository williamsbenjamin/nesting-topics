#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sunburstR)
library(shinydashboard)
library(readr)
library(nlme)

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
  cols_124_test <- read.csv("colors_124.csv")
  # Create Sunburst plot
  output$sunburstPlot <- renderSunburst({ 
    computernested <- read_csv("lev_sunburst_with_names.csv")
    sunburst(computernested[,-1],colors=cols_124_test)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

