library(shiny)
source("shiny_functions.R")

shinyServer(function(input, output, session){
  observeEvent(input$go,{
    print(input$search)
    if (input$search != ""){
      print("searching")
      
      
      search_terms = strsplit(input$search, split = " ")
      search_results <<- batch_search(unlist(search_terms), filter = FALSE)
      
      if (nrow(search_results) > 0){
        output$t1 = renderTable(search_results)
        updateCheckboxGroupInput(session, inputId = "to_view", choices = as.character(search_results$title))
        
        output$c1 = renderPlotly(timeline2(search_results))
        output$c2 = renderPlotly(make_map(map_emall(search_results)))
        
        output$lb = renderUI(actionButton(inputId = "load", label = "Load"))
      }
    }
  })
  
  observeEvent(input$load,{
    data_index <<- 1
    data_list <<- batch_pull(search_results[search_results$title %in% input$to_view,])
    updateCheckboxGroupInput(session, inputId = "to_dl", choices = names(data_list), selected = names(data_list))
    updateNavbarPage(session, inputId = "x1", selected = "View")
    updateSelectInput(session, inputId = "p1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "p2", choices = c("None", names(data_list[[data_index]])), selected = "None")
    output$t2 = renderTable(data_list[[data_index]], options = list(searching = FALSE, pageLength = 15))
  })
  
  observeEvent(input$p1,{
    if(input$p1 != "" & input$p1 != "None" & input$p2 == "None"){
      tmp_df = data_list[[data_index]]
      output$c3 = renderPlotly(hist_plot(tmp_df[, input$p1], title = input$p1, color = 1))
    } else if(input$p1 == "None" &  input$p2 != "None"){
      tmp_df = data_list[[data_index]]
      output$c3 = renderPlotly(hist_plot(tmp_df[, input$plotting1], title = input$plotting1, color = 2))
    } 
  })
  
  observeEvent(input$p2,{
    if(input$p2 != "" & input$p2 != "None" & input$p1 == "None"){
      tmp_df = data_list[[data_index]]
      output$c3 = renderPlotly(hist_plot(tmp_df[, input$p2], title = input$p2, color = 2))
    } else if(input$p2 == "None" &  input$p1 != "None"){
      tmp_df = data_list[[data_index]]
      output$c3 = renderPlotly(hist_plot(tmp_df[, input$p1], title = input$p1, color = 2))
    } 
  })
  
  observeEvent(input$p2,{
    if(input$p2 != "" & input$p2 != "None" & input$p1 != "" & input$p1 != "None"){
      tmp_df = data_list[[data_index]]
      output$c3 = renderPlotly(scatter_plot(tmp_df[, input$p1], tmp_df[, input$p2], input$p1, input$p2))
    }  
  })
  
  observeEvent(input$Next,{
    if (data_index < length(data_list)){
      data_index <<- data_index + 1
    }
    output$t2 = renderTable(data_list[[data_index]], options = list(pageLength = 15))
    updateSelectInput(session, inputId = "p1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "p2", choices = c("None", names(data_list[[data_index]])), selected = "None")
  })
  
  observeEvent(input$Prev,{
    if (data_index > 1){
      data_index <<- data_index - 1
    }
    output$t2 = renderTable(data_list[[data_index]], options = list(pageLength = 15))
    updateSelectInput(session, inputId = "p1", choices = c("None", names(data_list[[data_index]])), selected = "None")
    updateSelectInput(session, inputId = "p2", choices = c("None", names(data_list[[data_index]])), selected = "None")
  })
  
  output$dl <- downloadHandler(
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