library(shiny)
library(shinydashboard)
library(leaflet)  # for plotting maps
library(markdown)  # for inserting markdown files

# ----------------------------------------------- Data ----------------------------------------------- #


# ------------------------------------------------ UI ------------------------------------------------ #
ui <- navbarPage('Temperature Comparison',
                 tabPanel('Introduction',
                          fluidRow(
                            column(5, includeMarkdown('intro.md')),
                            column(3,img(src = 'daymet.png', width='100%'),
                                   img(src = 'landsat.png', width='150%'),
                                   img(src='nlcd.png', width='150%')))
                          ),
                 tabPanel('Data Explorer',
                          sidebarPanel(
                            helpText('Preprocessing includes interpolation, reprojection, and polygon generation.'),
                            radioButtons('dataLevel', 'Data Level', c('Raw', 'Preprocessed'), selected='Raw'),
                            
                            conditionalPanel(condition = "input.dataLevel == 'Raw'",
                              selectInput('varRaw', 'Variable', c('Land Cover Type', 'Daymet', 'LST'), selected='Land Cover Type'),
                              conditionalPanel(condition= "input.varRaw != 'Land Cover Type'",
                                               numericInput('month', 'Month', min=1, max=11, value=7),
                                               helpText('Enter Month from 1 to 11'))
                            ),
                            conditionalPanel(condition = "input.dataLevel == 'Preprocessed'",
                              selectInput('varPro', 'Variable', c('Land Cover Type', 'Daymet (Raster)', 'Daymet (Polygon)', 'LST'), selected='Land Cover Type'),
                              conditionalPanel(condition="input.varPro != 'Land Cover Type'",
                                               numericInput('month', 'Month', min=1, max=11, value=7),
                                               helpText('Enter Month from 1 to 11')))
                            ),
                          mainPanel(plotOutput('plotout'))),  # Change it to leaflet later
                 tabPanel('Statistical Analysis',
                          sidebarPanel(
                            radioButtons('plot1', 'Result to Plot', c('LST vs. Daymet', 'LST-Daymet Offset by Land Cover Type', 'LST by Land Cover Type in one Daymet Polygon'), selected='LST vs. Daymet'),
                            conditionalPanel(condition="input.plot1=='LST by Land Cover Type in one Daymet Polygon'",
                                             helpText('See Daymet Polygon ID in Data Explorer'),
                                             selectInput('tileID', 'Tile ID',seq(1,900,1),selected=1),  # ---- change this to another wiget more suitable for numbers
                                             selectInput('tileMon', 'Month', seq(1,11,1), selected=1)
                            ),
                            conditionalPanel(condition="input.plot1=='LST vs. Daymet'",
                                             helpText('This is help text for LST vs. Daymet'),
                                             selectInput('vsMon', 'Month', seq(1,11,1), selected=1))
                            ),
                          mainPanel(plotOutput('plotres'))
                          )
  
)


# ------------------------------------------------ Server ------------------------------------------------ #
server <- function(input, output){
  
}


# ------------------------------------------------ Run App ------------------------------------------------ #
shinyApp(ui=ui, server=server)
