#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Temperature and Rainfall in India"),
   
   # # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
   #       sliderInput("bins",
   #                   "Number of bins:",
   #                   min = 1,
   #                   max = 50,
   #                   value = 30)
   #    ),
   
   selectInput("select", label = h3("Type of Graph"), 
               choices = list("Temperature Line Graph" = "Temperature Line Graph", "Rainfall"="Rainfall", "Motion Graph" = "Motion Graph", "Decade" = "Decade"), 
               selected = 1),
   
 
   selectInput("Type", label = h3("Chart Type"), 
               choices = list("Line" = "Line", "Bar" = "Bar"), 
               selected = 1),
   hr(),
   fluidRow(column(3, verbatimTextOutput("value"))),
   
        
  
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("tempplot"),
         htmlOutput("Motion")
      )
   )
)


# Define server logic required to draw a histogram

library(googleVis)
library(ggplot2)
TempData <- readRDS("data/Temperature.rds")
decade<- readRDS("data/Decade.rds")
WeatherData<- readRDS("data/weatherdata.rds")


server <- shinyServer(function(input, output) {
      
   output$value <- renderPrint({  
         if (input$select=="Temperature Line Graph")
            return("This is Temperature Chart since 1901 till 2014 for India")
         else if (input$select=="Motion Graph")
               return("The Motion Chart for temperature opens in new browser window")
         else if (input$select=="Decade")
               return("Below Chart shows temperature variation across decades")
         })
      
   
   output$tempplot <- renderPlot({
      # generate bins based on input$bins from ui.R
         #Motion<- gvisMotionChart(TempData, idvar = "Recording", timevar = "YEAR")
         if (input$select=="Temperature Line Graph") 
            return(ggplot(TempData,aes(YEAR, Temperature))+geom_line(aes(color=TempType))+geom_smooth(method="lm",aes(color=TempType))+facet_grid(. ~ Season)+ylab("Temperature in centigrade"))
         #plot(Motion) 
         else if (input$select=="Decade" && input$Type=="Bar") 
            return( ggplot(decade,aes(Decade,Temperature))+geom_bar(stat="identity",aes(fill=TempType))+facet_grid(TempType~Season) )
         else if (input$select=="Decade" && input$Type=="Line") 
              return(ggplot(decade,aes(Decade,Temperature))+geom_line(stat="identity",aes(color=TempType))+facet_grid(TempType~Season))
   })
   output$Motion<- renderGvis(
         {
               Motion<- gvisMotionChart(TempData, idvar = "Recording", timevar = "YEAR")
               if (input$select=="Motion Graph")
               plot(Motion)
         }
   )
})

# Run the application 
shinyApp(ui = ui, server = server)

