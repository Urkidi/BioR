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
    observeEvent(input$delete, { 
      browser()
      eliminate ( match(input$filedelete, rvalues$raw.data) )})
  
  plotMA <- function (data, index, ref, subsampling=NULL, ...) { 
    #ref <- ref.array
    expr <- exprs(data[,index])
    if (!is.null(subsampling)) {
      id <- sample(1:length(expr), subsampling)
      expr <- expr[id]
      ref  <- ref[id]
    }
    
    m <- expr - ref
    a <- (expr + ref) / 2
    
    df <- data.frame(A=a, M=m)
    ggplot(df, aes(x=A, y=M)) + geom_point(...) + geom_smooth()
  }

  eliminate <- function(pos){
    rvalues$raw.data <- getRawData
    rvalues$raw.data <- rvalues$raw.data[-pos]
  }
  

  # OUTPUTS ------------------------------------------------------------
  
  
  
  output$fileName <- renderUI({ 
    
    selectInput("fileName", "choice a file",grep(".CEL",rvalues$file_names, value=T))
      
  })
  output$fileDelete <- renderUI({
    selectInput(inputId = "filedelete", "choice a file",grep(".CEL",rvalues$file_names, value=T))
  })
  
  output$file_list <- renderPlot({
    getRawData()
    res <- NULL
    if(!is.null(rvalues$directory)){
      res <- boxplot(rvalues$raw.data)
    }
    print(res)
  })
  output$table <- renderPlot({
    hist(getRawData())
  })
  output$file_list2 <- renderPlot({
    res <- NULL
    data.deg <- AffyRNAdeg(rvalues$raw.data)
    res <-plotAffyRNAdeg(data.deg)
    print(res)
  })
  output$qc <- renderPlot({
    res <- NULL
    mas5.data <- call.exprs(rvalues$raw.data,"mas5")
    qcs <- qc(rvalues$raw.data,mas5.data)
    res <-plot(qcs)
    print(res)
  })
  output$rma <- renderPlot({
    res <- NULL
    rma.data <- call.exprs(rvalues$raw.data,"rma")
    res <-boxplot(exprs(rvalues$raw.data))
    print(res)
  })
  
  output$plotMA <- renderPlot({
    num.arrays <- length(rvalues$raw.data)
    res <- NULL
    if(!is.null(rvalues$directory)){
      ref.array <- createRefArray(rvalues$raw.data)
      res <- plotMA(rvalues$raw.data, 1, ref=ref.array, subsampling=10000, size=5, alpha=0.5)
      
    }
    print(res)
  })
  output$densrma <- renderPlot({
    res <- NULL
    rma.data <- call.exprs(rvalues$raw.data,"rma")
    plot(density(exprs(rma.data[,1])))
  })


  
}
shinyServer(server)
