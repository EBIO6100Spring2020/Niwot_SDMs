library(shiny)
source("shiny_functions.R")


shinyServer(function(input, output, session){
  data_index = 1
  
  observeEvent(input$go,{
    print("searching")
    
    search_terms = strsplit(input$search, split = " ")
    print(unlist(search_terms))
    search_results <<- batch_search(unlist(search_terms), filter = input$filter)
    
    output$t1 = renderDataTable(search_results, options = list(searching = FALSE, pageLength = 25))
    updateCheckboxGroupInput(session, inputId = "to_view", choices = c("None", as.character(search_results$title)), selected = "None")
    
    output$hist1 = renderPlot(timeline(search_results$begindate, search_results$enddate, search_results$title))
 
   })
  
  observeEvent(input$View,{
    data_list <<- batch_pull(search_results[search_results$title %in% input$to_view,])
    updateCheckboxGroupInput(session, inputId = "to_dl", choices = names(data_list))
    updateSelectInput(session, inputId = "plotting1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "plotting2", choices = c("None", names(data_list[[data_index]])), selected = "None")
    output$t1 = renderDataTable(data_list[[data_index]], options = list(searching = FALSE, pageLength = 15))
    
    plot_o = reactive({
      
      if (input$plotting1 != "None" & input$plotting2 == "None"){
        hist(data_list[[data_index]][,input$plotting1], main = input$plotting, 
             breaks = 20, 
             xlab = input$plotting1, 
             cex.axis = 1.4)
      } else if (input$plotting1 == "None" & input$plotting2 != "None"){
        hist(data_list[[data_index]][,input$plotting2], main = input$plotting, 
             breaks = 20, 
             xlab = input$plotting2, 
             cex.axis = 1.4,
             cex.lab = 1.6)
      } else if (input$plotting1 != "None" & input$plotting2 != "None"){
        par(mfrow = c(1,2))
        hist(data_list[[data_index]][,input$plotting1], main = input$plotting1, 
             breaks = 20, 
             xlab = input$plotting1, 
             cex.axis = 1.4,
             cex.lab = 1.6)
        hist(data_list[[data_index]][,input$plotting2], 
             breaks = 20,
             col = rgb(52/255,235/255,222/255, 0.6),
             add = T)
        plot(data_list[[data_index]][,input$plotting2] ~ data_list[[data_index]][,input$plotting1], 
             ylab = input$plotting2, 
             xlab = input$plotting1,
             pch = 19,
             col = rgb(52/255,235/255,222/255),
             cex.axis = 1.4,
             cex.lab = 1.6)
      }
    })
    
      output$hist1 <- renderPlot({
        plot_o()
      }, width = 900, height = 450)

    
    
  })
  
  observeEvent(input$Next,{
    if (data_index < length(data_list)){
      data_index <<- data_index + 1
    }
    output$t1 = renderDataTable(data_list[[data_index]], options = list(pageLength = 15))
    updateSelectInput(session, inputId = "plotting1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "plotting2", choices = c("None", names(data_list[[data_index]])), selected = "None")
  })

  observeEvent(input$Prev,{
    if (data_index > 1){
      data_index <<- data_index - 1
    }
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




