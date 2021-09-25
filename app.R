#
# This is a Shiny web application. 
#
#

# Load required libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(MASS)
library(ggtern)

# Reading the data
fatal <- read.csv("FinalRuralRoadCrashesInfo.csv")
tod <- fatal$Light_Condition_Desc %>% unique() %>% sort()
atm <- fatal$Atmosph_Cond_Desc %>% unique() %>% sort()
lga <- fatal$LGA %>% unique() %>% sort()

# Constants
# Epsilon
eps <- 0.00001
# Number of shades on the heatmap
nlevels <- 10 
# Adjust bandwidth for kernel density estimation
gaussianRadius <- 0.4
# Setting contourSmoothness
cs <- 400
# Setting opacity of the contours on the map
fillOpacity <- 0.2
# Setting contour line width
contourLineWidth <- 1

# Define UI for application that draws a hotspot map
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = 'Accident Hotspots'),
    dashboardSidebar(
        # Filters go here
        h3('FILTERS:'),
        # Drop down for weather
        selectInput("weather", 
                    "Weather Condition:", 
                    atm),
        # Drop down for lga
        selectInput("lga", 
                    "LGA Travelling To:", 
                    lga),
        # Drop down for time
        selectInput("time", 
                    "Time of Day:", 
                    tod),
        # Instructions
        p('Click on apply filters to reset the data on the map and the table.'),
        
        # Button to apply selected filters
        actionButton("apply","Apply Filters")
    ),
    dashboardBody(
        htmlOutput('Note'),
        fluidRow(
            box(
                width = 12,
                leafletOutput("hotspotMap")
            )
        ),
        fluidRow(
            box(
                width = 12,
                dataTableOutput("table")
            )
        )
    )
)


server <- function(input, output, session) {
    
    # SOURCE:
    # https://github.com/WishartLab/heatmapper/blob/master/geocoordinate/server.R
    get_density <- function(accdf){
        
        # Get lat and long from the data
        df <- accdf[c('Long','Lat')]
        df <- df %>% dplyr::count(Long, Lat)
        x <- df[[1]]
        y <- df[[2]]
        
        # To get bandwidth for density via normal reference distribution
        # REFERENCE https://www.rdocumentation.org/packages/MASS/versions/7.3-54/topics/bandwidth.nrd
        bandwidth <- c(bandwidth.nrd(x), bandwidth.nrd(y))*gaussianRadius
        
        # Adding epsilon to avoid 0 values
        if(bandwidth[[1]] == 0){
            bandwidth[[1]] <- eps
        }
        if(bandwidth[[2]] == 0){
            bandwidth[[2]] <- eps
        }
        
        xmin <- min(x) - bandwidth[[1]]*nlevels
        xmax <- max(x) + bandwidth[[1]]*nlevels
        
        ymin <- min(y) - bandwidth[[2]]*nlevels
        ymax <- max(y) + bandwidth[[2]]*nlevels
        
        # Getting weighted density for accident occurance at different locations
        if(is.null(df$n) || var(df$n) == 0){
            # REFERENCE: https://www.rdocumentation.org/packages/KernSmooth/versions/2.23-20/topics/bkde2D
            dens <- kde2d(x, y, h = bandwidth, n = cs, lims = c(xmin,xmax, ymin,ymax))
        }
        else{
            dens <- kde2d.weighted(x, y, df$n, h = bandwidth, n = cs, lims = c(xmin,xmax, ymin,ymax))
        }
        
        return(contourLines(x = dens$x, y = dens$y, z = dens$z, nlevels = nlevels))
        
    }
    
    # SOURCE:
    # https://github.com/WishartLab/heatmapper/blob/master/geocoordinate/server.R
    get_colours <- function(level_list){
        n <- length(level_list)
        palette <- substr(rev(rainbow(n, end = 5/6)), 0, 7)
        names(palette) <- level_list
        return(palette)
    }
    
    # SOURCE:
    # https://github.com/WishartLab/heatmapper/blob/master/geocoordinate/server.R
    get_contour_shapes <- function(m, df){
        cl <- get_density(df)
        max_cl <- length(cl)
        
        cl_levels <- as.character(unique(unlist(lapply(cl, function(x){x$level}))))
        
        colours <- get_colours(cl_levels)
        
        fill_op <- fillOpacity
        contours <- contourLineWidth
        
        for(i in 1:max_cl){	
            m <- addPolygons(m, cl[[i]]$x,cl[[i]]$y, 
                             fillColor = colours[[as.character(cl[[i]]$level)]], 
                             fillOpacity = fill_op, weight = contours)
        }
        
        return(m)
    }
    
    # Function to add markers to map
    get_point_shapes <- function(m, df){
        m <- m %>%
            addCircleMarkers(~Long, ~Lat,
                             label = ~as.character(Accident_No),
                             radius = 5,
                             stroke = FALSE,
                             layerId = ~Accident_No)
    }
    
    # Create hotspot contours and add markers to map
    get_shapes <- function(m, df){
        m <- get_contour_shapes(m, df)
        m <- get_point_shapes(m, df)
        m
    }
    
    
    # https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
    observeEvent(input$apply, {
        df <- fatal %>%
            filter(Atmosph_Cond_Desc == input$weather) %>%
            filter(LGA == input$lga)  %>%
            filter(Light_Condition_Desc == input$time)
        
        output$table <- renderDataTable({df}, options = list(scrollX = TRUE))
        
        # Reference: https://www.youtube.com/watch?v=G5BDubIyQZY
        m <- leafletProxy("hotspotMap", session, df)
        m %>% clearShapes() %>% clearMarkers()
        
        if(nrow(df) != 0){
            get_shapes(m, df)
        }
        
        # REFERENCE: https://stackoverflow.com/questions/54272640/is-it-possible-to-clear-the-displayed-output-in-shinyapp-using-actionbutton
        removeUI(
            selector = "#Note"
        )
    })
    
    
    observeEvent(input$hotspotMap_marker_click, {
        map_data <- fatal
        
        accId <- input$hotspotMap_marker_click$id
        
        if(!is.null(accId)){
            map_data <- map_data %>% filter(Accident_No == accId)
        }
        output$table <- renderDataTable({map_data}, options = list(scrollX = TRUE))
    })
    
    
    output$hotspotMap <- renderLeaflet({ # create leaflet map
        mydf <- fatal
        m <- leaflet(data = mydf) %>% addTiles() %>% setView(144.95486, -37.02244, 7)  %>%
            addProviderTiles(providers$Esri.WorldStreetMap)
        
        if(nrow(mydf) != 0){
            m <- m %>%
                addCircleMarkers(~Long, ~Lat,
                                 label = ~as.character(Accident_No),
                                 radius = 3,
                                 stroke = FALSE,
                                 layerId = ~Accident_No)
            # get_shapes(m, mydf)
        }
        m
    })
    
    
    output$table <- renderDataTable({
        # Print the table
        fatal
    }, options = list(scrollX = TRUE) # https://stackoverflow.com/questions/32149487/controlling-table-width-in-shiny-datatableoutput
    )
    
    output$Note <- renderText({
        paste(
            "<font color=\"#8B0000\">Note: Due to large data, hotspots will only load once you click apply filters</font>"
        )
    })
    
}



shinyApp(ui, server)