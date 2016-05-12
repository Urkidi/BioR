library(shiny)
library(simpleaffy)
library(affy)
library(ggplot2)


options(shiny.maxRequestSize = 100*1024^2)
server <- function(input, output) {
  
  #variables
  medians <- NULL
  id.ref <- NULL
   
  
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
  
  plotMA <- function (data, index) { 
    m <- exprs(data[,index]) - exprs(data[,id.ref])
    a <- (exprs(data[,index]) + exprs(data[,id.ref]))/2
    ma.plot(a,m,cex=0.75,lwd=3)
  }
  
  #plotMA <- function (data, index, ref, subsampling=NULL, ...) { 
      # Function to create the MA plots
      # ARGS:
      #   data        - AffyBatch object with the data to create the plot
      #   index       - Index of the array to be plotted
      #   ref         - Refrence against which the plot will be created
      #   subsampling - Number of random values to create the plots
      #   ...         - Additional arguments for the geom_point function
      #
      # RETURN:
      #   A ggplot2 object with the MA plot
      #
    # ref <- ref.array
    # expr <- exprs(data[,index])
    # if (!is.null(subsampling)) {
    #   id <- sample(1:length(expr), subsampling)
    #   expr <- expr[id]
    #   ref  <- ref[id]
    # }
      
    #m <- expr - ref
      #a <- (expr + ref) / 2
      #  
      #df <- data.frame(A=a, M=m)
      #ggplot(df, aes(x=A, y=M)) + geom_point(...) + geom_smooth()
      #}
  

  # OUTPUTS ------------------------------------------------------------
  
  output$fileName <- renderUI({ 
    
    selectInput("fileName", "choice a file",grep(".CEL",rvalues$file_names, value=T) )
      
  })
  output$summary <- renderPrint({
    if(!is.null(rvalues$directory)){
      dataset <- rvalues$file_names 
    }
    
    list(dataset)
  })
  
  output$file_list <- renderPlot({
    raw.data <- getRawData()
    res <- NULL
    if(!is.null(rvalues$directory)){
      res <- boxplot(raw.data)
    }
    print(res)
  })
  output$table <- renderPlot({
    hist(getRawData())
  })
  output$file_list2 <- renderPlot({
    raw.data <- getRawData()
    res <- NULL
    data.deg <- AffyRNAdeg(raw.data)
    res <-plotAffyRNAdeg(data.deg)
    print(res)
  })
  output$qc <- renderPlot({
    raw.data <- getRawData()
    res <- NULL
    mas5.data <- call.exprs(raw.data,"mas5")
    qcs <- qc(raw.data,mas5.data)
    res <-plot(qcs)
    print(res)
  })
  
  output$plotMA <- renderPlot({
    raw.data <- getRawData()
    num.arrays <- length(raw.data)
    res <- NULL
    if(!is.null(rvalues$directory)){
      medians <- sapply(1:num.arrays, 
                        FUN=function(i) {
                          return(median(exprs(raw.data[,i])))
                        })
      id.ref <- order(medians)[num.arrays/2]
      res <- plotMA(raw.data, 1)
    }
    print(res)
  })


  
}
shinyServer(server)
