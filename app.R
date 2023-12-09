# Created by Xiuyu Cao on Dec 8, 2023
library(shiny)
library(shinydashboard)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(leaflet)
library(markdown)

# ----------------------------------------------- Global ----------------------------------------------- #
# Raw Data
land_cover0 <- rast('data/NLCD.tiff')  # original
LST <- rast('data/LST.tif')  # LST stack
macro.sf <- readRDS('data/macro.df.rds') %>%
  st_as_sf(coords=c("lon","lat"),crs=crs(LST[[1]]))
# Preprocessed Data
land_cover1 <- rast('data/NLCD1.tiff')  # reprojected and reclassified
macro30 <- rast('data/macro30.tiff')
macro.V <- vect('data/macro.V.shp') %>%
  st_as_sf() %>%
  mutate(ID=seq(1,nrow(.),1))
# data2plot
macroLST <- readRDS('data/macroLST.rds')
offset <- readRDS('data/offset.rds')

# Other Variables
months=month.name[-length(month.name)]

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
                          sidebarLayout(
                            sidebarPanel(
                              helpText('Preprocessing includes reclassification, interpolation, reprojection, cropping, masking, and Voronoi polygon generation.',
                                       HTML('<br><br><span style="color: red; font-weight: bold;">Rendering raster data takes some time. Please be Patient.</span>')),
                              radioButtons('dataLevel', 'Data Level', c('Raw', 'Preprocessed'), selected='Raw'),
                              
                              conditionalPanel(condition = "input.dataLevel == 'Raw'",
                                               selectInput('varRaw', 'Variable', c('Daymet', 'Land Cover Type', 'Land Surface Temperature (LST)'), selected='Daymet'),
                                               conditionalPanel(condition= "input.varRaw != 'Land Cover Type'",
                                                                selectInput('month', 'Month', months, selected='January'),
                                                                helpText('Enter Month from 1 to 11 (Data of December in NYC were absent in 2019).'))
                              ),
                              conditionalPanel(condition = "input.dataLevel == 'Preprocessed'",
                                               selectInput('varPro', 'Variable', c('Daymet (Polygon)', 'Daymet (Raster)', 'Land Cover Type'), selected='Dayemet (Polygon)'),
                                               conditionalPanel(condition="input.varPro != 'Land Cover Type'",
                                                                selectInput('month1', 'Month', months, selected='January'),
                                                                helpText('Enter Month from 1 to 11 (Data of December in NYC were absent in 2019).')))
                            ),
                            mainPanel(leafletOutput('dataMap', width = "100%", height = "800px"))
                          )
                          ),
                 
                 tabPanel('Statistical Analysis',
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons('which2plot', 'Result to Plot', 
                                           c('LST vs. Daymet', 'LST-Daymet Offset', 'LST in each Daymet Polygon'), selected='LST-Daymet Offset'),
                              conditionalPanel(condition="input.which2plot=='LST in each Daymet Polygon'",
                                               helpText('See Daymet Polygon ID in Data Explorer'),
                                               numericInput('tileID', 'Polygon ID', min=1, max=900, value=856),
                                               selectInput('tileMon', 'Month', months, selected='January')
                              ),
                              conditionalPanel(condition="input.which2plot!='LST in each Daymet Polygon'",
                                               helpText('This is help text'),
                                               selectInput('vsMon', 'Month', months, selected='January')
                              )
                            ),
                            mainPanel(plotOutput('plotres', width = "100%", height = "800px"))
                          )
                 )
  
)


# ------------------------------------------------ Server ------------------------------------------------ #
server <- function(input, output, session){
  output$dataMap <- renderLeaflet({ 
    if(input$dataLevel=='Raw'){
      if(input$varRaw=='Land Cover Type'){
        data2plot <- land_cover0
        colors <- c("#FFC0CB", "#CD2626", "#FF0000", "#00FF00", "#8B1A1A", "#87CEEB", "#0000FF", "#9BCD9B", 
                    "#FFFF00", "#C1FFC1", "#BEBEBE", "#6CA6CD", "#698B69", "#006400", "#A52A2A", "#000000")
        leaflet() %>%
          addTiles() %>%
          addRasterImage(data2plot, colors=colors, opacity=.8) %>%
          addLegend(colors=colors, labels=unique(values(data2plot)), opacity=1, title='Land Cover Code')
      } else if(input$varRaw=='Daymet'){
        month <- match(input$month, month.name)  # get month
        data2plot <- macro.sf[month]
        names(data2plot)[1] <- 'macro'
        pal <- colorNumeric(c('blue','green','red'),range(data2plot$macro))
        leaflet() %>%  # plot macro raw
          addTiles() %>%
          addCircles(data=data2plot, 
                     label = ~paste0(round(macro,1),' Degrees Celsius'),
                     fillOpacity = 1, color=~pal(macro), radius = 100) %>%
          addLegend(pal=pal, values=data2plot$macro, opacity=0.5,title='Temperature (&deg;C)')
      } else{ # --- plot LST
        month <- match(input$month, month.name)
        data2plot <- LST[[month]]
        pal <- colorNumeric(c(rev(brewer.pal(n = 5, name = "RdYlBu"))), values(data2plot),
                            na.color = "transparent")
        leaflet() %>%
          addTiles() %>%
          addRasterImage(data2plot, colors=pal, opacity=1) %>%
          addLegend(pal=pal, values=values(data2plot), opacity=1, title='Temperature (&deg;C)')
      }
    }
    
    else{  # ---------- Pre-processed Data
      if(input$varPro=='Land Cover Type'){
        data2plot <- land_cover1
        colors <- c("#000000", "#6CA6CD", "#FF0000", "#BEBEBE", "#006400", "#9BCD9B", "#C1FFC1", "#FFFF00", "#87CEEB")
        labels <- c('NA', 'Water', 'Developed', 'Barren', 'Forest', 'Lichens', 'Grasslands', 'Crops', 'Wetland')
        leaflet() %>%
          addTiles() %>%
          addRasterImage(data2plot, colors=colors, opacity=.8) %>%
          addLegend(colors=colors, labels=labels, opacity=1, title='Land Cover Type')
      } else if(input$varPro=='Daymet (Polygon)'){
        month <- match(input$month1, month.name)
        data2plot <- macro.V[c(month, ncol(macro.V))]
        names(data2plot)[1] <- 'macro'
        pal <- colorNumeric(c("red", "green", "blue"), range(data2plot$macro))
        leaflet(data2plot) %>%
          addTiles() %>%
          addPolygons(color='#444444', weight=1, smoothFactor = 0,opacity=1.0, fillOpacity = .8,
                      fillColor = ~pal(macro),
                      label=~paste0('ID ',ID,': ', round(macro,1), ' Degrees Celsius')) %>%
          addLegend(pal=pal, values=data2plot$macro, opacity=1.0, title='Temperature (&deg;C)')
      } else{ # --- plot macro processed raster
        month <- match(input$month1, month.name)
        data2plot <- macro30[[month]]
        pal <- colorNumeric(c(rev(brewer.pal(n = 5, name = "RdYlBu"))), values(data2plot),
                            na.color = "transparent")
        leaflet() %>%
          addTiles() %>%
          addRasterImage(data2plot, colors=pal, opacity=1) %>%
          addLegend(pal=pal, values=values(data2plot), opacity=1, title='Temperature (&deg;C)')
      }
    }
  })

    
  output$plotres <- renderPlot({ 
    if(input$which2plot=='LST vs. Daymet'){
      month <- match(input$vsMon, month.name)

      data2plot <- macroLST[c(month, 11+month, ncol(macroLST))]
      names(data2plot) <- c('LST', 'Daymet', 'land_cover')
      data2plot <- melt(data2plot)
      
      legend_labels <- c('Water', 'Developed', 'Barren', 'Forest', 'Shrub', 'Grassland', 'Pasture', 'Wetlands')
      ggplot(data2plot, aes(x=variable, y=value, fill=land_cover)) +
        scale_fill_manual(values=c('blue','red','gray','darkgreen','brown','green','yellow','skyblue'),
                          labels=legend_labels) + 
        geom_boxplot() +
        guides(fill=guide_legend(title=NULL)) +  # remove legend title
        theme(strip.text = element_blank()) +  # remove subplot titles
        facet_wrap(~land_cover) +
        scale_x_discrete(labels = c('LST','Daymet')) + 
        labs(x='Land Cover Type', y='Temperature (Degrees Celsius)') +
        ggtitle(paste0('LST vs. Daymet in New York City, ', input$vsMon,', 2019'))
      
    
    } else if(input$which2plot=='LST-Daymet Offset'){
      month <- match(input$vsMon, month.name)
      
      data2plot <- offset[c(month,ncol(offset))]
      names(data2plot) <- c('offset','land_cover')
      data2plot <- melt(data2plot)
      
      legend_labels <- c('1: Water', '2: Developed', '3: Barren', '4: Forest', '5: Shrub', '7: Grassland', '8: Pasture', '9: Wetlands')
      ggplot(data2plot, aes(x=land_cover, y=value, fill=land_cover)) +
        geom_boxplot() +
        scale_fill_manual(values=c('blue','red','gray','darkgreen','brown','green','yellow','skyblue'),
                          labels=legend_labels) + 
        guides(fill=guide_legend(title=NULL)) +  # remove legend title
        labs(x='Land Cover Type', y='Temperature Offset (Degrees Celsius)') +
        ggtitle(paste0('LST-Daymet Offset by Land Cover Type in NYC, ', input$vsMon, ', 2019'))
      
      
    } else{  # LST in each polygon
      polyID <- input$tileID
      month <- match(input$tileMon, month.name)
      
      data2plot <- readRDS(paste0('data/by_poly',month,'.rds')) %>%
        filter(ID==polyID) %>%
        mutate(land_cover =case_when(
          land_cover.1 == 1 ~ 'Water',
          land_cover.1 == 2 ~ 'Developed',
          land_cover.1 == 3 ~ 'Barren',
          land_cover.1 == 4 ~ 'Forest',
          land_cover.1 == 5 ~ 'Shrub',
          land_cover.1 == 7 ~ 'Grassland',
          land_cover.1 == 8 ~ 'Pasture',
          land_cover.1 == 9 ~ 'Wetlands',
        ))
      
      ggplot(data2plot, aes(x=land_cover, y=micro,group=land_cover.1)) +
        geom_boxplot() +
        # scale_x_continuous(breaks = seq(min(data2plot$land_cover), max(data2plot$land_cover.1), 1)) +
        labs(x='Land Cover Type', y='LST (Degrees Celsius)') +
        ggtitle(paste0('LST in New York City Polygon ',input$tileID, ', ',input$tileMon,', 2019',
                       '\nPolygon Daymet Temperature: ', round(data2plot$macro.mean[1],1), ' degrees Celsius'))
    }
  })
  
}


shinyApp(ui=ui, server=server)  # Run app

