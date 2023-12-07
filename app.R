library(shiny)
library(shinydashboard)
library(markdown)

# ------------------------------------------------ UI ------------------------------------------------ #
ui <- navbarPage('Temperature Comparison',
                 tabPanel('Introduction',
                          fluidRow(
                            column(6,
                                   h1('This is Introduction')),
                            column(3,
                                   h1('Here is an image'),
                                   tags$small(
                                     "Source: xxxxxx ",
                                     "bbbbbb ",
                                     "ccccc ",
                                     a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                       "User:Sfoskett"))))),
                 tabPanel('Data Explorer'),
                 tabPanel('Visualization')
  
)


# ------------------------------------------------ Server ------------------------------------------------ #
server <- function(input, output){
  
}


# ------------------------------------------------ Run App ------------------------------------------------ #
shinyApp(ui=ui, server=server)
