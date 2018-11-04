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
    checkboxGroupInput("level","Select the Violence level",level2),
    checkboxGroupInput("back","Select the Background Map",back,selected = "Stamen.Toner"),
     checkboxGroupInput("region", "Select the region:", 
                temp,selected = "Boston"),
   
    textInput("summary", "Summary:","")
  ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    leafletOutput("mapping")
  )
)





# Define server logic required to draw a histogram
server <- function(input, output,session) {

  output$mapping <- renderLeaflet({
    region = input$region
    n <- input$num
    level<-input$level
    back<-input$back
    food_Boston <- food %>% 
      filter(CITY %in% region & !is.na(Location) & ViolLevel =="***") %>%
      group_by(businessName, Location) %>% 
      summarise(count=n()) %>% 
      filter(count>n) %>%
      separate(Location, c("lat", "long"), ", ") %>%
      mutate(lat=gsub("\\(", "", lat), long=gsub("\\)", "", long))
     leaflet(data = food_Boston) %>% 
      addTiles() %>%
      addMarkers(~as.numeric(long), ~as.numeric(lat), popup = ~businessName, 
                 label = ~businessName) %>% 
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>% 
      addProviderTiles(back)
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

