library(shiny)
source("shiny_functions.R")


shinyServer(function(input, output, session){
  data_index = 1
  

  
  observeEvent(input$go,{
    print("searching")
    
    
    search_terms = strsplit(input$search, split = " ")
    search_results <<- batch_search(unlist(search_terms), filter = input$filter)
    colors_touse = get_colors(nrow(search_results))
    
    if (nrow(search_results) > 0){
      output$t1 = DT::renderDataTable(search_results, options = list(searching = FALSE, pageLength = 25))
      updateCheckboxGroupInput(session, inputId = "to_view", choices = as.character(search_results$title))
      
      output$hist1 = renderPlotly(timeline2(search_results))
      output$map1 = renderPlotly(make_map(map_emall(search_results)))
    }
    
   })
  
  
  observeEvent(input$View,{
    data_list <<- batch_pull(search_results[search_results$title %in% input$to_view,])
    updateCheckboxGroupInput(session, inputId = "to_dl", choices = names(data_list), selected = names(data_list))
    updateSelectInput(session, inputId = "plotting1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "plotting2", choices = c("None", names(data_list[[data_index]])), selected = "None")
    output$t1 = DT::renderDataTable(data_list[[data_index]], options = list(searching = FALSE, pageLength = 15))
  

    output$hist1 = renderPlotly(timeline2(search_results[search_results$title %in% input$to_view, ]))
    output$map1 = renderPlotly(make_map(map_emall(search_results[search_results$title %in% input$to_view, ])))
    
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
  
  observeEvent(input$collapse1,{

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




