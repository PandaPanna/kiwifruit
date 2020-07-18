###Data wrangling####
# Read data file
myData <- read.table('kfdata.csv',  sep=',', header=T)  
# Tidy up data 
library('tidyverse')
library('tidyr')

myData <- myData[myData$Indicator=='Soil status',]
data2009 <- myData[myData$Year==2009,]
data <- data2009 %>% 
  spread(Attribute, Value) 

names <- c("BulkDensity", "Carbon","Nitrogen","OlsenP", "pH")  

####R shiny###

color1 <- rgb(0, 0, 255, max = 255, alpha = 125)
color2 <- rgb(255, 192, 203, max = 255, alpha = 125)
color3 <- rgb(218, 164, 32, max = 255, alpha = 125)
color4 <- rgb(60, 179, 113, max = 255, alpha = 125)
color5 <- rgb(75, 0, 130, max = 255, alpha = 125)
color6 <- rgb(128, 128, 128, max = 255, alpha = 125)
color7 <- rgb(220, 20, 60, max = 255, alpha = 125)
color8 <- rgb(0, 206, 209, max = 255, alpha = 125)
color9 <- rgb(255, 165, 0, max = 255, alpha = 125)

palette(c(color1, color2, color3, color4,
          color5, color6, color7, color8, color9))

library(shiny)


ui <- fluidPage(
  headerPanel('Kiwi fruit farm k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names),
    selectInput('ycol', 'Y Variable', names),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)
