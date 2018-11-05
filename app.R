#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(sp)
library(magrittr)
library(maps)
library(htmltools)
library(rgdal)
library(data.table)
library(tidyverse)
library(shiny)
food<-read_csv(file="mayorsfoodcourt.csv")
CITY<-na.omit(unique(food$CITY))
temp<-c()
for(i in 1:length(CITY)){
  temp<-c(temp,CITY[i])
}
level2<-c("*","**","***")
back<-c("Stamen.Toner","CartoDB.Positron","Esri.NatGeoWorldMap")
# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  # Application title
  titlePanel("Inspections in BOSTON restuarants"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    sliderInput("num", "Select the number of Inspections:", 
                min=1, max=120,value=50),
    selectInput("level","Select the Violence level",choices=level2),
    selectInput("back","Select the Background Map",choices=back,selected = "Stamen.Toner"),
     checkboxGroupInput("region", "Select the region:", 
                temp,selected = "Boston"),
   
    textInput("notation", "Notation:",FALSE)
  ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    leafletOutput("mapping"),
    h3(textOutput("notation"))
  )
)





# Define server logic required to draw a histogram
server <- function(input, output,session) {
  output$notation <- renderPrint({
    if(input$level=="*"){
      print("The worst inspection in these restuarants is in * level")
    }
    else if(input$level=="**"){
      paste0("The worst inspection in these restuarants is in ** level")
    }
    else if(input$level=="***"){
      paste0("The worst inspection in these restuarants is in *** level")
    }
  })
  
  
  output$mapping <- renderLeaflet({
    region = input$region
    n <- input$num
    level<-input$level
    back<-input$back
    food_Boston <-NA
    map_3st <- food %>%
      filter(CITY %in% c("Boston", "BOSTON") & !is.na(Location) & ViolLevel=="***") %>%
      group_by(businessName, Location) %>%
      summarise(count=n()) %>%
      separate(Location, c("lat", "long"), ", ") %>%
      mutate(lat=gsub("\\(", "", lat), long=gsub("\\)", "", long))
    map_2st <- food %>%
      filter(CITY %in% c("Boston", "BOSTON") & !is.na(Location) & ViolLevel=="**") %>%
      group_by(businessName, Location) %>%
      summarise(count=n()) %>%
      separate(Location, c("lat", "long"), ", ") %>%
      mutate(lat=gsub("\\(", "", lat), long=gsub("\\)", "", long)) %>%
      anti_join(map_3st, by=c("businessName", "lat", "long"))
    map_1st <- food %>%
      filter(CITY %in% c("Boston", "BOSTON") & !is.na(Location) & ViolLevel=="*") %>%
      group_by(businessName, Location) %>%
      summarise(count=n()) %>%
      separate(Location, c("lat", "long"), ", ") %>%
      mutate(lat=gsub("\\(", "", lat), long=gsub("\\)", "", long)) %>%
      anti_join(map_3st, by=c("businessName", "lat", "long")) %>%
      anti_join(map_2st, by=c("businessName", "lat", "long"))
    if(level=="***"){
    food_Boston <-  map_3st 
    leaflet(data = food_Boston) %>% 
      addTiles() %>%
      addAwesomeMarkers(~as.numeric(long), ~as.numeric(lat), popup = ~businessName, 
                        label = ~businessName, icon=awesomeIcons(
                          icon = 'ios-close',
                          iconColor = 'black',
                          library = 'ion',
                          markerColor = "red")) %>% 
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>% 
      addProviderTiles(back) 
    }
    else if(level=="**"){
      food_Boston <- map_2st
      
      leaflet(data = food_Boston) %>% 
        addTiles() %>%
        addAwesomeMarkers(~as.numeric(long), ~as.numeric(lat), popup = ~businessName, 
                          label = ~businessName, icon=awesomeIcons(
                            icon = 'ios-close',
                            iconColor = 'black',
                            library = 'ion',
                            markerColor = "blue")) %>% 
        setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>% 
        addProviderTiles(back) 
    }
    else if(level=="*"){
      food_Boston <- map_1st
      
      leaflet(data = food_Boston) %>% 
        addTiles() %>%
        addAwesomeMarkers(~as.numeric(long), ~as.numeric(lat), popup = ~businessName, 
                          label = ~businessName, icon=awesomeIcons(
                            icon = 'ios-close',
                            iconColor = 'black',
                            library = 'ion',
                            markerColor = "green")) %>% 
        setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>% 
        addProviderTiles(back) 
    }
   
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

