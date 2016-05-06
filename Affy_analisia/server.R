library(shiny)
library(simpleaffy)
library(affy)
library(ggplot2)


options(shiny.maxRequestSize = 100*1024^2)
shinyServer(function(input, output) {
   
  
  # REACTIVE VALUES ----------------------------------------------------
  
  rvalues <- reactiveValues()
  rvalues$file_names <- NULL
  rvalues$directory <- NULL
  rvalues$raw.data <- NULL
  rvalues$processed.data <- NULL
  
  # DATA UPDATING ------------------------------------------------------
  
  getRawData <- reactive({
    data <- NULL
    # Check for changes in the loaded files
    if (!is.null(input$data_loader) & (is.null(rvalues$file_names) ||
                                       length(input$data_loader$names)!=length(rvalues$file_names) || 
                                       all(sort(input$data_loader$names)==sort(rvalues$file_names)))) {
      # Create a temporary directory
      rvalues$directory <- paste0(tempdir(),"\\",gsub(":","_",date()))
      dir.create(rvalues$directory)
      
      # Rename the files to reset their original name
      sapply(1:nrow(input$data_loader), 
             FUN=function(i) {
               file.copy(input$data_loader$datapath[i], paste0(rvalues$directory, "\\",
                                                               input$data_loader$name[i]))
             })
      
      # Update the rvalues object
      rvalues$file_names <- input$data_loader$name
      rvalues$raw.data <- read.affy(path=rvalues$directory)
      data <- rvalues$raw.data
    }
    return(data)
  })
  
  # OUTPUTS ------------------------------------------------------------
  
  output$file_list <- renderPlot({
    raw.data <- getRawData()
    res <- NULL
    if(!is.null(rvalues$directory)){
      res <- boxplot(raw.data)
    }
    print(res)
  })
  
})
