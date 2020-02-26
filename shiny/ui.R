library(shiny)
source("shiny_functions.R")

fluidPage(
  sidebarLayout(
    sidebarPanel(
                 fluidRow(
                   column(1,
                          textInput("search", label = "Search Terms"), style = 'width:200px;'
                   )
                 ),
                 fluidRow(
                   column(1,
                          actionButton("go", label = "Search"), style = "width:80px;"
                   ),
                   column(2,
                          checkboxInput("filter", label = "Exact match"), style = "width:150px;margin-left:20px;"    
                   ) 
                 ),
                 fluidRow(
                   column(1,
                          downloadButton("download", "Download"), style = "width:100px;margin-left:0px;" 
                   )
                 ),
                 fluidRow(
                   column(1,
                          checkboxGroupInput("to_dl", label = "Select Data sets to download", choices = c()), style = "width:500px;" 
                   )
                 ),
                 style = 'width:600px;border-style:none;'
                 
    ),
    mainPanel(position = 'bottom',
              dataTableOutput("t1"),
              fluidRow(
                column(1, 
                       actionButton("Prev", label = "Previous Dataset"), style = 'margin-right:10px; width:120px;', value = 1),
                column(2,
                       actionButton("Next", label = "Next Dataset"), style = 'margin-left:10px; width:120px;', value = 1), style = 'margin-top:50px;'

              ),
              fluidRow(
                column(1,
                       selectInput("plotting1",label = "", choices = ""), style = 'margin-left:0px; margin-top:25px; width:170px;'),
                column(2,
                       selectInput("plotting2",label = "", choices = ""), style = 'margin-left:0px; margin-top:25px; width:170px;')
                 
              ),
              plotOutput("hist1"),
              style = 'width:900px;margin-left:19px;'), fluid = FALSE
  )
)
