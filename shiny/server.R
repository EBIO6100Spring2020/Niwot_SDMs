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
    updateSelectInput(session, inputId = "plotting", choices = names(data_list[[data_index]]))
    
    histo = reactive({
      hist(data_list[[1]][,input$plotting])
    })
    
    
    output$hist1 <- renderPlot({
      histo()
    }, width = 900, height = 450)
    
  })
  
  observeEvent(input$Next,{
    print(input$Next)
    data_index <<- data_index + 1
    output$t1 = renderDataTable(data_list[[data_index]], options = list(pageLength = 15))
  })

  observeEvent(input$Prev,{
    data_index <<- data_index - 1
    output$t1 = renderDataTable(data_list[[data_index]], options = list(pageLength = 15))
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




