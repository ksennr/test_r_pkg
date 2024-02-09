#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(getChurnProb)
library(data.table)


data_setup <- function() {
  
  ##### 1. Setup R and read in data (5 min)
  path <- "C:\\Users\\kilia\\Documents\\R Seminar\\Day 5\\"
  file1 <- "data_customer.csv"
  file2 <- "data_personal.csv"
  
  data_customer <- fread(paste0(path, file1))
  data_personal <- fread(paste0(path, file2))
  
  ##### 2. Prepare data for analysis (5 min)
  data_merged <- merge(data_customer, data_personal, by.x = "CustomerId", by.y = "CustomerId", all=TRUE)
  
  data_merged$Exited <- as.factor(data_merged$Exited)
  data_merged$Gender <- as.factor(data_merged$Gender)
  
  ##### 3. Predict churn probability (20 min)
  glm_fit <- glm(
    formula = Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
    family = "binomial",
    data = data_merged
  )
  
  data_merged$churn_rate_pred <- predict(glm_fit, data_merged, type = "response")
  
  return(data_merged)
}




# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Churn Prediction"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("custID", label="", value = "Customer ID"),
      verbatimTextOutput("churn_rate_pred")
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_merged <- data_setup()

  output$map <- renderLeaflet({
    temp <- setorder(data_merged, -churn_rate_pred)[1:100,]
    
    zips <- as.matrix(temp[, list(zip_longitude, zip_latitude)])
    
    map <- leaflet()
    map <- addTiles(map)
    map <- addMarkers(map, data = zips, clusterOptions = markerClusterOptions())
    map <- setView(map, lat= 43, lng= -79, zoom = 3)
    
    
  })
  
  output$churn_rate_pred <- renderPrint({
    getChurnProb::get_cust_id(data_merged, as.numeric(input$custID))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
