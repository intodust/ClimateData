#
# This is a Shiny web application. Created by Deepak Raja, for plotting Temperature and Rainfall data since 1901 in India
# It takes the data from data.gov.in and saves Rds after some data cleaning, and this simple app generates the charts. 
# Most of the code is self explainatory. 

library(shiny)


ui <- shinyUI(fluidPage(theme = "bootstrap.css",
   
   # Application title
   fluidRow(column(12, offset =3, titlePanel("Temperature and Rainfall in India Since 1901"))),
   
   fluidRow(column(4,selectInput("Data", label = h3("Data to be charted"), 
                                 choices = list("Temperature" = "Temperature", "Rainfall"="Rainfall"), 
                                 selected = 1)),
            
            
            column(4,selectInput("Period", label = h3("Period"), 
                                 choices = list("Annual" = "Annual", "Decade" = "Decade"), 
                                 selected = 1)),
            
            
            (column(4, selectInput("Type", label = h3("Chart Type"), 
                                   choices = list("Line" = "Line", "Bar" = "Bar", "Motion ( opened below other chart)" = "Motion"), 
                                   selected = 1)))),
   hr(),
   
         plotOutput("tempplot"),
         htmlOutput("Motion"),
  
  
   hr(),
   fluidRow(column(12, verbatimTextOutput("value")))
   
        
  
     
     
   )
)


# Define server logic required to draw a histogram

library(googleVis)
library(ggplot2)
library(ggthemes)

tempdata <- readRDS("temperature.Rds")
decade<- readRDS("decade.Rds")
weatherdata<- readRDS("weatherdata.Rds")
raindecade<- readRDS("raindecade.Rds")

server <- shinyServer(function(input, output) {
      
   output$value <- renderPrint({  
      
                     if (input$Data=="Temperature")
                        return(paste("@Deepak Raja - This is Temperature Chart since 1901 till 2014 for India"," Data: https://data.gov.in/ministrydepartment/ministry-earth-sciences"))
                     else if (input$Data=="Rainfall")
                        return(paste("@Deepak Raja - This is Rainfall Chart since 1901 till 2014 for India"," Data: https://data.gov.in/ministrydepartment/ministry-earth-sciences"))
                     
         })
      
   
   output$tempplot <- renderPlot({
     
          if (input$Data=="Temperature" && input$Period=="Annual" && input$Type=="Line") 
                        return(ggplot(tempdata,aes(YEAR, Temperature))+geom_line(aes(color=TempType))+geom_smooth(method="lm",aes(color=TempType))+facet_grid(. ~ Season)+ylab("Temperature in centigrade")+theme(legend.position="bottom")+ggtitle("Temperature in India 1901-2014"))
            else if(input$Data=="Temperature" && input$Period=="Annual" && input$Type=="Bar")
                        return(ggplot(tempdata,aes(YEAR, Temperature))+geom_bar(stat="identity",aes(fill=TempType))+geom_smooth(method="lm",aes(color=TempType))+facet_grid(TempType ~ Season)+ylab("Temperature in centigrade")+theme(legend.position="bottom")+ggtitle("Temperature in India 1901-2014"))
            else if (input$Data=="Temperature" && input$Period=="Decade" && input$Type=="Bar") 
                        return( ggplot(decade,aes(Decade,Temperature))+geom_bar(stat="identity",aes(fill=TempType))+facet_grid(TempType~Season)+ ylab("Temperature in centigrade")+theme(legend.position="bottom")+ggtitle("Temperature in India 1901-2014"))
            else if (input$Data=="Temperature" && input$Period=="Decade" && input$Type=="Line") 
                        return(ggplot(decade,aes(Decade,Temperature))+geom_line(stat="identity",aes(color=TempType))+facet_grid(TempType~Season)+ylab("Temperature in centigrade")+theme(legend.position="bottom")+ggtitle("Temperature in India 1901-2014"))
          
          else if (input$Data=="Rainfall" && input$Period=="Annual" && input$Type=="Line") 
                  return(ggplot(weatherdata,aes(YEAR, Rainfall))+geom_line(aes(color=Season))+geom_smooth(method="lm",aes(color=Season))+facet_grid(. ~ Season)+ylab("Rainfall in mm")+theme(legend.position="bottom")+ggtitle("Rainfall in India 1901-2014"))
          else if(input$Data=="Rainfall" && input$Period=="Annual" && input$Type=="Bar")
                  return(ggplot(weatherdata,aes(YEAR, Rainfall))+geom_bar(stat="identity",aes(fill=Season))+geom_smooth(method="lm",aes(color=Season))+facet_grid(. ~ Season)+ylab("Rainfall in mm")+theme(legend.position="bottom")+ggtitle("Rainfall in India 1901-2014"))
          else if (input$Data=="Rainfall" && input$Period=="Decade" && input$Type=="Bar") 
                  return( ggplot(raindecade,aes(Decade,Rainfall))+geom_bar(stat="identity",aes(fill=Season))+facet_grid(.~Season)+ylab("Rainfall in mm")+theme(legend.position="bottom")+ggtitle("Decade wise Rainfall in India"))
          else if (input$Data=="Rainfall" && input$Period=="Decade" && input$Type=="Line") 
                return(ggplot(raindecade,aes(Decade,Rainfall))+geom_line(stat="identity",aes(color=Season))+facet_grid(.~Season)+ylab("Rainfall in mm")+theme(legend.position="bottom")+ggtitle("Decade wise Rainfall in India"))
         else if(input$Type=="Motion")
               return(ggplot(tempdata,aes(YEAR, Temperature))+geom_line(aes(color=TempType))+geom_smooth(method="lm",aes(color=TempType))+facet_grid(. ~ Season)+ylab("Temperature in centigrade")+theme(legend.position="bottom")+ggtitle("Temperature in India 1901-2014"))
               
   })
 
   output$Motion<- renderGvis(
         {
               
               Motion<- gvisMotionChart(weatherdata, idvar = "Season", timevar = "YEAR")
                                 return(Motion)
         }
   )
})

# Run the application 
shinyApp(ui = ui, server = server)

