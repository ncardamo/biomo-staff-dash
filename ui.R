require(shiny)
require(shinydashboard)
require(DT)
require(plotly)
require(shinyWidgets)
require(stringr)
require(tidyverse)
require(shinyFiles)
require(reticulate)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "EDA Dashboard"),
        dashboardSidebar(
            sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Patient Search..."),
            shinyDirButton("dir", "Input directory", "Upload"),
            verbatimTextOutput("direct", placeholder = TRUE),
            fileInput("file2", multiple = TRUE,"Choose Daylio CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            )
    ),
    dashboardBody(
    # App title ----
    titlePanel("Patient Name"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Select metric to plot
            checkboxGroupButtons("med", 
                           "Metrics:", 
                           choices = c("Emotional Activity" = "EDA",
                                       "Skin Temperature" = "TEMP",
                                       "Heart Rate" = "BVP",
                                       "HR Variability" = "IBI"), selected = "EDA", justified = TRUE, checkIcon = list(yes = icon("circle"), no = NULL), size = "sm"),
        


            # Select layers to add
            radioGroupButtons("layer", "Layers:",
                         c("None" = "none",
                           "Activity & Mood" = "activity")),
            
        
        
            
            # br() element to introduce extra vertical spacing ----
            br(),
            
            
            radioGroupButtons("tick", choices = c("second", "minute", "hour"), label = "Granulation", choiceValues = c("seconds", "minutes", "hours"), selected = "minute"),
          
              br(),
            
            
            radioGroupButtons(inputId = "layout", choices = c("standard", "circle", "interactive"), label = "Plot Layout", selected = "standard"),
            
            br(),
    
            
            # Input: Tabs that display the calendar, summary, and table.
            
            tabsetPanel(type = "tabs",
                        tabPanel("Calendar", DTOutput("calendar")),
                        tabPanel("Summary",
                                 fluidRow(column(width = 10, valueBoxOutput("mode", width = 10), valueBoxOutput("stress", width = 10), valueBoxOutput("emotion", width = 10)))
                                 ),
                        tabPanel("Table", DTOutput("table")))
        ),
        

            

        
        # Main panel for displaying outputs ----
        mainPanel(fluidRow(
            uiOutput("edaBox"),
            uiOutput("hrvBox"),
            uiOutput("tempBox"),
            uiOutput("bvpBox"),
            uiOutput("interactivebox")
            ),
    ))
    
    
    
    
    
    
    
)
)
)


