library(shiny)
source("shiny_functions.R")

fluidPage(
  sidebarLayout(
    sidebarPanel(
                h3("Niwot Data Utility", style = "margin:0px;background-color:lightSteelBlue;height:50px;text-align:center;padding:10px 0;border-radius:5px;border-style:solid;border-color:rgba(0.2,0.2,0.2,0.2)"),
                 fluidRow(
                   column(1,
                          textInput("search", label = ""), style = 'width:340px; margin:0px'
                   )
                 ),
                 fluidRow(
                   column(2,
                          actionButton("go", label = "Search"), style = "width:80px;"
                   ),
                   column(1,
                          checkboxInput("filter", label = "Exact match"), style = "width:150px;margin-left:20px;"    
                   ) 
                 ),
                 hr(style = "margin:5px;"),
                 fluidRow(
                   column(1,
                          h3("Download", style = "margin:0px;margin-top:2px;")
                   ),
                   column(2,
                          downloadButton("download", ""), style = "width:100px;margin-left:90px;" 
                   )
                          ),
                 fluidRow(
                   column(1,
                          checkboxGroupInput("to_dl", label = "Select Data sets to download", choices = c()), style = "width:340px; margin-top:10px;" 
                   )
                 ),
                 hr(style = "margin:5px; margin-top:10px;"),
                 fluidRow(
                   column(1,
                          h3("View", style = "margin:0px;")),
                   column(2,
                          actionButton("View", label = "View"), style = 'margin-left:30px;')
                   
                 ),
                 fluidRow(
                   column(1,
                          checkboxGroupInput("to_view", label = "Select Data sets to view", choices = c()), style = "width:340px; margin-top:10px;" 
                   )
                 ),
                 fluidRow(
                   column(1, 
                          actionButton("Prev", label = "Previous Dataset"), style = 'margin-right:10px; width:120px;', value = 1),
                   column(2,
                          actionButton("Next", label = "Next Dataset"), style = 'margin-left:10px; width:120px;', value = 1), style = 'margin-top:10px;'
                   
                 ),
                 fluidRow(
                   column(width = 6,
                          selectInput("plotting1",label = "", choices = ""), style = 'margin:0px;'),
                   column(width = 6,
                          selectInput("plotting2",label = "", choices = ""), style = 'margin:0px;'), style = "margin:-10px;"
                   
                 ),
                 style = 'width:350px;border-style:solid;background-color:rgba(0.6,0.6,0.6,0.2);margin-top:15px;border-color:rgba(0.2,0.2,0.2,0.2)'
                 
    ),
    mainPanel(
      fluidRow(
               dataTableOutput("t1")),
      fluidRow(
               plotOutput("hist1")
      ),
              style = 'width:900px;margin-left:19px;'), fluid = FALSE
  )
)
