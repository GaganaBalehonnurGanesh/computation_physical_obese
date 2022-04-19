library(DT)
library(shiny)
library(tidyverse)
library(bslib)
library(gridExtra)
library(shinythemes)
library(shinyWidgets)
library(tableHTML)
library(leaflet)

source("www/functions/readingdata.R")
source("www/functions/ObesityTop10.R")
source("www/functions/OverWeight.R")
source("www/functions/PyramidTrend.R")


################################################################################
ui <- fluidPage(
  #theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  #shinyjs::inlineCSS(list(body = "color:#336600")),
  #setSliderColor("#DAA520",1),
  #setBackgroundColor(
  #color = ("#FFF8DC"),
  #gradient = "linear"
  #),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "comp.css")
  ),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  theme = shinytheme("sandstone"),
  navbarPage(title = "Physical Activity & Obesity",
             tabsetPanel(
               tabPanel(
                 "Obesity Vs Physical Inactivity",
                 mainPanel(
                   width = 12,
                   fluidRow(splitLayout(
                     cellWidths = c("80%", "20%"),
                     column(11, uiOutput("slider1")),
                     column(
                       1,
                       actionButton(
                         class = "reset-btn",
                         inputId = "reset",
                         label = "Reset Data",
                         icon = icon("refresh"),
                         align = "center"
                       )
                     )
                   ),),
                   fluidRow(class = "plot-row",
                            splitLayout(
                              column(4, plotOutput(
                                "plot1", width = "450px", height = "500px"
                              )),
                              column(4, plotOutput(
                                "plot2", width = "450px", height = "500px"
                              )),
                              column(4, plotOutput(
                                "plot3", width = "450px", height = "500px"
                              ))
                            ), )
                 )
               ),
               tabPanel(
                 "Socio-economic Influence on Overweight",
                 mainPanel(
                   width = 12,
                   fluidRow(splitLayout(
                     cellWidths = c("80%", "20%"),
                     column(11, uiOutput("slider2")),
                     column(
                       1,
                       actionButton(
                         class = "reset-btn",
                         inputId = "reset",
                         label = "Reset Data",
                         icon = icon("refresh"),
                         align = "center"
                       )
                     )
                   ),),
                   fluidRow(
                     class = "plot-row",
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       column(6, plotOutput(
                         "OWP1", width = "550px", height = "500px"
                       )),
                       column(6, plotOutput(
                         "OWP4", width = "550px", height = "500px"
                       ))
                     ),
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       column(6, plotOutput(
                         "OWP3", width = "550px", height = "500px"
                       )),
                       column(4, plotOutput(
                         "OWP2", width = "550px", height = "500px"
                       ))
                     ),
                   )
                 )
               ),
               tabPanel(
                 "Physical Activity Pyramid",
                 mainPanel(
                   width = 12,
                   fluidRow(splitLayout(
                     cellWidths = c("45%", "35%", "20%"),
                     column(5, uiOutput("slider3")),
                     column(5, uiOutput("race")),
                     column(
                       2,
                       actionButton(
                         class = "reset-btn",
                         inputId = "reset",
                         label = "Reset Data",
                         icon = icon("refresh"),
                         align = "center"
                       )
                     )
                   ),),
                   fluidRow(class = "plot-row",
                            splitLayout(
                              cellWidths = c("50%", "50%"),
                              column(6, plotOutput(
                                "tab1", width = "550px", height = "500px"
                              )),
                              column(6, plotOutput(
                                "tab2", width = "550px", height = "500px"
                              ))
                            ),)
                 )
               )
             ))
)

server <- function(input, output) {
  data <- readingdata()
  
  output$slider1 <- renderUI({
    sliderInput(
      inputId = "sliderinput1",
      label = "Year:",
      min = 2011,
      max = 2020,
      value = 2011,
      sep = ""
    )
  })
  
  output$slider2 <- renderUI({
    sliderInput(
      inputId = "sliderinput2",
      label = "Year:",
      min = 2011,
      max = 2020,
      value = 2011,
      sep = ""
    )
  })
  
  output$slider3 <- renderUI({
    sliderInput(
      inputId = "sliderinput3",
      label = "Year:",
      min = 2011,
      max = 2020,
      value = 2011,
      sep = ""
    )
  })
  
  output$race <- renderUI({
    pickerInput(
      inputId = "dropdown1",
      label = "Race/Ethnicity:",
      choices = c(
        "2 or more races",
        "American Indian/Alaska Native",
        "Asian",
        "Hawaiian/Pacific Islander",
        "Hispanic",
        "Non-Hispanic Black",
        "Non-Hispanic White",
        "Other"
      )
    )
  })
  
  output$plot1 <- renderPlot({
    ObesityTop10(data, input$sliderinput1)
  })
  
  output$plot2 <- renderPlot({
    physicalTop10(data, input$sliderinput1)
  })
  
  output$plot3 <- renderPlot({
    jointdata(data)
  })
  
  output$OWP1 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[1]]
  })
  
  output$OWP2 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[2]]
  })
  
  output$OWP3 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[3]]
  })
  
  output$OWP4 <- renderPlot({
    OWList <- OverWeight(data, input$sliderinput2)
    OWList[[4]]
  })
  
  output$tab1 <- renderPlot({
    gender(data, input$sliderinput3)
  })
  
  output$tab2 <- renderPlot({
    yearTrend(data,input$dropdown1)
  })
  
}

shinyApp(ui, server)
