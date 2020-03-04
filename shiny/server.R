library(shiny)
library(shinyBS)
source("shiny_functions.R")


shinyServer(function(input, output, session){
  data_index = 1
  data_list = list()
  
  
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
  
    


    output$map1 = renderPlotly(make_map(map_emall(search_results[search_results$title %in% input$to_view, ])))
    
    output$downloadbutton = renderUI({
      tagList(downloadButton("download", ""))
    })
    
    
  })

  observeEvent(input$plotting1,{
    if(input$plotting1 != "" & input$plotting1 != "None" & input$plotting2 == "None"){
      tmp_df = data_list[[data_index]]
      output$hist1 = renderPlotly(hist_plot(tmp_df[, input$plotting1], title = input$plotting1, color = 1))
    } else if(input$plotting1 == "None" &  input$plotting2 != "None"){
      tmp_df = data_list[[data_index]]
      output$hist1 = renderPlotly(hist_plot(tmp_df[, input$plotting1], title = input$plotting1, color = 2))
    } 
    

  })
  
  observeEvent(input$plotting2,{
    if(input$plotting2 != "" & input$plotting2 != "None" & input$plotting1 == "None"){
      tmp_df = data_list[[data_index]]
      output$hist1 = renderPlotly(hist_plot(tmp_df[, input$plotting2], title = input$plotting2, color = 2))
    } else if(input$plotting2 == "None" &  input$plotting1 != "None"){
      tmp_df = data_list[[data_index]]
      output$hist1 = renderPlotly(hist_plot(tmp_df[, input$plotting1], title = input$plotting1, color = 2))
    } 
    
    
  })
  
  observeEvent(input$plotting2,{
    if(input$plotting2 != "" & input$plotting2 != "None" & input$plotting1 != "" & input$plotting1 != "None"){
      tmp_df = data_list[[data_index]]
      output$hist1 = renderPlotly(scatter_plot(tmp_df[, input$plotting1], tmp_df[, input$plotting2], input$plotting1, input$plotting2))
    }  
    
    
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
      if(length(input$to_dl) != 0){
        download_list = data_list[input$to_dl]
        save(download_list, "data_list", file = file)
      }

    }
  )
  
})




