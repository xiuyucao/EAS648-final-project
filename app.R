library(shiny)
library(shinydashboard)
library(terra)
library(sf)
library(dplyr)
library(RColorBrewer)
library(leaflet)  # for plotting maps
library(markdown)  # for inserting markdown files

# ----------------------------------------------- Data ----------------------------------------------- #
# Raw
land_cover0 <- rast('data/NLCD.tiff')  # original
LST <- rast('data/LST.tif')  # LST stack
macro.df <- readRDS('data/macro.df.rds') %>%
  st_as_sf(coords=c("lon","lat"),crs=crs(LST[[1]]))

# Preprocessed
land_cover1 <- rast('data/NLCD1.tiff')  # reprojected and reclassified
macro30 <- rast('data/macro30.tiff')
macro.V <- vect('data/macro.V.shp')

# ----------------------------------- Plot Test
# --- plot macro raw
month <- 7  # get month

data2plot <- macro.sf[month]
names(data2plot)[1] <- 'macro'
pal <- colorNumeric(c('blue','green','red'),range(data2plot$macro))
leaflet() %>%  # plot macro raw
  addTiles() %>%
  addCircles(data=data2plot, 
             label = ~paste0(round(macro,1),' Degrees Celsius'),
             fillOpacity = 1, color=~pal(macro), radius = 100) %>%
  addLegend(pal=pal, values=data2plot$macro, opacity=0.5,title='Temperature (&deg;C)')

# --- plot macro processed raster
month <- 4

data2plot <- macro30[[month]]
pal <- colorNumeric(c(rev(brewer.pal(n = 5, name = "RdYlBu"))), values(data2plot),
                    na.color = "transparent")
leaflet() %>%
  addTiles() %>%
  addRasterImage(data2plot, colors=pal, opacity=1) %>%
  addLegend(pal=pal, values=values(data2plot), opacity=1, title='Temperature (&deg;C)')


# --- plot macro Voronoi polygon
month <- 7

macro.V <- st_as_sf(macro.V)
data2plot <- macro.V[month]
names(data2plot)[1] <- 'macro'
pal <- colorNumeric(c("red", "green", "blue"), range(data2plot$macro))
leaflet(data2plot) %>%
  addTiles() %>%
  addPolygons(color='#444444', weight=1, smoothFactor = 0,opacity=1.0, fillOpacity = .8,
              fillColor = ~pal(macro),
              label=~paste0(round(macro,1), ' Degrees Celsius')) %>%
  addLegend(pal=pal, values=data2plot$macro, opacity=1.0, title='Temperature (&deg;C)')



# --- plot Land Cover Type
data2plot <- land_cover0

colors <- c("#FFC0CB", "#CD2626", "#FF0000", "#00FF00", "#8B1A1A", "#87CEEB", "#0000FF", "#9BCD9B", 
            "#FFFF00", "#C1FFC1", "#BEBEBE", "#6CA6CD", "#698B69", "#006400", "#A52A2A", "#000000")
leaflet() %>%
  addTiles() %>%
  addRasterImage(data2plot, colors=colors, opacity=.8) %>%
  addLegend(colors=colors, labels=unique(values(data2plot)), opacity=1, title='Land Cover Code')

# --- plot Land Cover Type processed
data2plot <- land_cover1

colors <- c("#000000", "#6CA6CD", "#FF0000", "#BEBEBE", "#006400", "#9BCD9B", "#C1FFC1", "#FFFF00", "#87CEEB")
labels <- c('NA', 'Water', 'Developed', 'Barren', 'Forest', 'Lichens', 'Grasslands', 'Crops', 'Wetland')
leaflet() %>%
  addTiles() %>%
  addRasterImage(data2plot, colors=colors, opacity=.8) %>%
  addLegend(colors=colors, labels=labels, opacity=1, title='Land Cover Code')

# --- plot LST
month <- 6

data2plot <- LST[[month]]
pal <- colorNumeric(c(rev(brewer.pal(n = 5, name = "RdYlBu"))), values(data2plot),
                    na.color = "transparent")
leaflet() %>%
  addTiles() %>%
  addRasterImage(data2plot, colors=pal, opacity=1) %>%
  addLegend(pal=pal, values=values(data2plot), opacity=1, title='Temperature (&deg;C)')

 

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
                            helpText('Preprocessing includes reclassification, interpolation, reprojection, cropping, masking, and Voronoi polygon generation.'),
                            radioButtons('dataLevel', 'Data Level', c('Raw', 'Preprocessed'), selected='Raw'),
                            
                            conditionalPanel(condition = "input.dataLevel == 'Raw'",
                              selectInput('varRaw', 'Variable', c('Land Cover Type', 'Daymet', 'Land Surface Temperature (LST)'), selected='Land Cover Type'),
                              conditionalPanel(condition= "input.varRaw != 'Land Cover Type'",
                                               numericInput('month', 'Month', min=1, max=11, value=7),
                                               helpText('Enter Month from 1 to 11 (Data of December in NYC were absent in 2019).'))
                            ),
                            conditionalPanel(condition = "input.dataLevel == 'Preprocessed'",
                              selectInput('varPro', 'Variable', c('Land Cover Type', 'Daymet (Raster)', 'Daymet (Polygon)'), selected='Land Cover Type'),
                              conditionalPanel(condition="input.varPro != 'Land Cover Type'",
                                               numericInput('month', 'Month', min=1, max=11, value=7),
                                               helpText('Enter Month from 1 to 11 (Data of December in NYC were absent in 2019).')))
                            ),
                          mainPanel(leafletOutput('dataMap'))
                          ),
                 
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
  output$dataMap <- renderLeaflet({ 
    leaflet() %>% addTiles()
  })
  
  output$min_max <- renderText({ 
    paste("You have chosen a range that goes from",input$varRaw)
  })
  
}


# ------------------------------------------------ Run App ------------------------------------------------ #
shinyApp(ui=ui, server=server)
