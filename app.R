
library(shiny)
library(leaflet)
library(shinydashboard)
#library(leaflet.extras)
library(tidyverse)
# library(sf)

greater_ldn <- readRDS(file="greater_ldn.rds")

greater_ldn$distance_from_work <- as.numeric(greater_ldn$distance_from_work)

ui <- dashboardPage(dashboardHeader(title = 'Where to have lunch?'), 
sidebar <- dashboardSidebar(width=200, actionButton("recalc", "Generate lunch option"),
                    numericInput("dist", "Distance:", value=500, min = 100, max = 1000)),
body <- dashboardBody(  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    ")),
    tags$style(
      HTML('
             h3 {
font-family: "Georgia", Times, "Times New Roman", serif;
             font-weight: bold;
             }
             ')),
    tags$style(
      HTML('
        .skin-blue .main-header .logo {
    font-family: "Lobster", Times, "Times New Roman", cursive;
             font-size: 20px;
             color: #fff;
             background-color: #dd0031;
        }
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #dd0031;
                              }
             
             /* other links in the sidebarmenu */
             .skin-blue .main-sidebar .sidebar .sidebar-menu a{
             background-color: #53a9cd;
             color: #fff;
             }
             /* toggle button when hovered  */                    
             .skin-blue .main-header .navbar .sidebar-toggle:hover{
             background-color: #53a9cd;
             }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #dd0031;
        }   
        /* main sidebar */
        .skin-blue .main-sidebar {
             background-color: 	#C0C0C0;
             }
  
       /* logo when hovered */
        .skin-blue .main-header .logo:hover {
          background-color: #dd0031;
        }')
    )),
  fluidRow(
    column(12,h3(textOutput("selected_var"))),
  column(12,leafletOutput("mymap"))
  )
  )
)

# Define server 
server <- function(input, output, session) {
  # create the interactive map...
  output$mymap <- renderLeaflet({
    cov_gar <- greater_ldn %>%
      dplyr::filter(as.numeric(distance_from_work)<input$dist) 
    
    points <- eventReactive(input$recalc, {
      
      sample_n(cov_gar,1)
    }, ignoreNULL = FALSE)
    
    output$selected_var <- renderText({ 
      paste0("Maybe you should go to ",points()$name, " for lunch? It is ", points()$distance_from_work, "m from the office.") 
      
      
    })
    
    leaflet(padding = 0, options= leafletOptions( minZoom=8, maxZoom=18) ) %>% 
      addTiles()  %>%
      addMarkers( group = "The office",
                  lng = -0.10640000000000782,
                  lat = 51.5133, 
                  popup="The office") %>% 
      # addCircleMarkers( group = "All lunch places",
      #                   lng = st_coordinates(cov_gar)[,1],
      #                   lat = st_coordinates(cov_gar)[,2],
      #                   radius = 8, weight = 0.25,
      #                   stroke = TRUE, opacity = 75,
      #                   fill = TRUE, fillColor = "deeppink",
      #                   fillOpacity = 100,
      #                   popup =data$label,
      #                   color = "white") %>%     
      addCircleMarkers( group = "All lunch places",
                        lng = cov_gar$X,
                        lat = cov_gar$Y,
                        radius = 8, weight = 0.25,
                        stroke = TRUE, opacity = 75,
                        fill = TRUE, fillColor = "deeppink",
                        fillOpacity = 100,
                        popup =cov_gar$label,
                        color = "white") %>%
      addCircleMarkers(group="Random lunch place", 
                                         lng = points()$X,
                                         lat = points()$Y,
                       radius = 8, weight = 0.25,
                       stroke = TRUE, opacity = 100,
                       fill = TRUE, fillColor = "purple",
                       fillOpacity = 100,     
                       popup = points()$label,
                       color = "white") %>% 
      addLayersControl(
        overlayGroups = c("All lunch places", "Random lunch place"),
        options = layersControlOptions(collapsed = FALSE))
    
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)