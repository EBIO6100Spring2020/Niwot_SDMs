library(shiny)
source("shiny_functions.R")


shinyServer(function(input, output, session){
  data_index = 1
  
  observe(if (input$go != 0){
    print("searching")
    
    search_terms = strsplit(input$search, split = " ")
    print(unlist(search_terms))
    data_list <<- batch_pull(unlist(search_terms), filter = input$filter)
    
    
    
    output$t1 = renderDataTable(data_list[[data_index]], options = list(searching = FALSE, pageLength = 15))
    
    updateCheckboxGroupInput(session, inputId = "to_dl", choices = names(data_list))
    updateSelectInput(session, inputId = "plotting1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "plotting2", choices = c("None", names(data_list[[data_index]])), selected = "None")
    
    
    plot_o = reactive({
      
      if (input$plotting1 != "None" & input$plotting2 == "None"){
        hist(data_list[[data_index]][,input$plotting1], main = input$plotting, breaks = 20, xlab = input$plotting1)
      } else if (input$plotting1 == "None" & input$plotting2 != "None"){
        hist(data_list[[data_index]][,input$plotting2], main = input$plotting, breaks = 20, xlab = input$plotting2)
      } else if (input$plotting1 != "None" & input$plotting2 != "None"){
        plot(data_list[[data_index]][,input$plotting2] ~ data_list[[data_index]][,input$plotting1], ylab = input$plotting2, xlab = input$plotting1)
      }
    })
    
    
    output$hist1 <- renderPlot({
      plot_o()
    }, width = 900, height = 450)
    
  })
  
  observeEvent(input$Next,{
    print(input$Next)
    data_index <<- data_index + 1
    output$t1 = renderDataTable(data_list[[data_index]], options = list(pageLength = 15))
    updateSelectInput(session, inputId = "plotting1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "plotting2", choices = c("None", names(data_list[[data_index]])), selected = "None")
  })

  observeEvent(input$Prev,{
    data_index <<- data_index - 1
    output$t1 = renderDataTable(data_list[[data_index]], options = list(pageLength = 15))
    updateSelectInput(session, inputId = "plotting1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "plotting2", choices = c("None", names(data_list[[data_index]])), selected = "None")
  })
  
  
  
  output$download <- downloadHandler(
    filename = function(){
      paste(input$search, ".Rdata", sep = "")
    },
    content = function(file){
      download_list = data_list[input$to_dl]
      save(download_list, "data_list", file = file)
    }
  )
  
})




