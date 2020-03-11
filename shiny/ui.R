library(shiny)
library(shinyBS)

shinyUI(fluidPage(
        
        bsTooltip("search", "Enter key words for data", placement = "bottom", trigger = "hover",
            options = NULL),
        
        bsTooltip("go", "Search for your key words", placement = "bottom", trigger = "hover",
                  options = NULL),
        
        bsTooltip("load", "Load selected datasets to browse", placement = "bottom", trigger = "hover",
                  options = NULL),
        
        navbarPage(title = "Niwot Data Utility",
                   collapsible = TRUE,
                   selected = "Search",
                   id = "x1",
        
                   
          tabPanel(title = "Search",
                   fluidRow(
                     column(1,textInput(label = "", inputId = "search", placeholder = "e.g. Soil Texture"), style = "width:100vw;"),
                     style = "width:100vw"
                   ),
                   fluidRow(
                     column(1, actionButton("Search", inputId = "go"), align = "left", style = "width:5vw"),
                     column(2, uiOutput(outputId = "lb"), align = "left", style = "width:100px; position: absolute; left: 120px;")
                   ),
                   
                   hr(),
                   
                   fluidRow(
                     column(1, plotlyOutput(outputId = "c1"), style = "width: 45vw"),
                     column(2, plotlyOutput(outputId = "c2"), style = "width: 45vw")
                   ),
                   
                   fluidRow(
                     column(1, checkboxGroupInput(inputId = "to_view", label = "",choices = c()), style = "width:25vw;"),
                     column(2, tableOutput(outputId = "t1"), style = "width:65vw;")
                   )
                 ),
          tabPanel(title = "View",
                   fluidRow(
                     column(1, actionButton(inputId = "Prev", label = "Previous Dataset"), style = "width: 205px"),
                     column(2, actionButton(inputId = "Next", label = "Next Dataset"), style = "width: 205px")
                   ),
                   fluidRow(
                     column(1, selectInput(inputId = "p1", label = "", choices = c()), style = "width: 205px"),
                     column(2, selectInput(inputId = "p2", label = "", choices = c()), style = "width: 205px")
                   ),
                   
                   hr(),
                   
                   fluidRow(
                     column(1, plotlyOutput(outputId = "c3"), style = "width: 45vw"),
                     column(2, plotlyOutput(outputId = "c4"), style = "width: 45vw")
                   ),
                   
                   fluidRow(align = 'center',
                     column(1, tableOutput(outputId = "t2"), style = "width:90vw;")
                   )
                   
                 ),
          tabPanel(title = "Download",
                   fluidRow(
                     column(1,downloadButton(outputId = "dl", label = "Download"))
                   ),
                   
                   hr(),
                   
                   fluidRow(
                     column(1, checkboxGroupInput(inputId = "to_dl", label = "",choices = c()), style = "width:50vw")
                   )
                            
                 ), 
          tags$head(tags$style("#search{color: gray;font-size: 1.1em;font-style: italic; border-radius: 1px;width: 35vw;}", 
                               "#go{color:gray; border-radius: 1px; font-style: bold; font-size: 1.1em; width:100px}",
                               "#load{color:gray; border-radius: 1px; font-style: bold; font-size: 1.1em; width:100px;}",
                               "#to_view{color:gray; border-radius: 1px;}",
                               "#dl{border-radius: 1px; color: gray}",
                               "#to_dl{width:50vw; left-margin:25vw}",
                               "* {font-family: Tahoma, Geneva, sans-serif; !important;}",
                               ".navbar{height: 50px;font-size:1.1em;}",
                               ".selectize-input {border-radius:0px; width: 200px; color:gray}",
                               "#Next{width:200px;color:gray;border-radius:1px;font-size:1.1em}",
                               "#Prev{width:200px;color:gray;border-radius:1px;font-size:1.1em}",
                               ".checkbox-input {transform: scale(3);border-radius:1px;}"
                               )
      )
    ) 
  )
)





