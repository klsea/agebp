#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Load data
data <- read.csv('data/SV_DND_long_web.csv')
pics <- read.csv('data/brainPics.csv')
data$Age2 <- data$Age*data$Age
data$Study_Sex <- interaction(data$Study, data$Sex)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Regional differences in dopamine binding potential by age"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("roi",
                  "Region of Interest:",
                  choices = data$name, 
                  selected = "Straight gyrus.png"
      ), 
      checkboxInput('linear',
                    'Linear fit', 
                    value = TRUE), 
      checkboxInput('quadratic', 
                    'Quadratic fit', 
                    value = TRUE),
      tags$head(tags$style(
        type="text/css",
        "#image1 img {max-width: 100%; width: 100%; height: auto}"
      )),
      imageOutput('image1')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", 
                           h4(textOutput('ntext')),
                           plotOutput("roiPlot"),
                           h4(span(textOutput('percChangeText'))),
                           span(textOutput("linearText"), style="color:red"), 
                           span(textOutput("quadText"), style="color:blue"), 
                           textOutput('anovaText'), style="color:black"),
                  tabPanel("Regressions", 
                           span(verbatimTextOutput('baseline')), 
                           span(verbatimTextOutput('linear')),
                           span(verbatimTextOutput('quadratic'))), 
                  tabPanel("Credits", 
                           h4("Hammer's Atlas:"),
                           span(verbatimTextOutput('atlas')), 
                           h4('3D ROI Visualization:'),
                           span(verbatimTextOutput('anatroi')))
      )
    )
  ),
  hr(),
  h4('by Kendra Seaman, Motivated Cognition and Aging Brain Lab, Duke University')
))
