library(shiny)
library(simpleaffy)
library(affy)
library(ggplot2)


options(shiny.maxRequestSize = 100*1024^2)
server <- function(input, output) {
  
  #variables
  medians <- NULL
  id.ref <- NULL
  
  
  # REACTIVE VALUES —------------------------------------------------—
  
  rvalues <- reactiveValues()
  rvalues$file_names <- NULL
  rvalues$directory <- NULL
  rvalues$raw.data <- NULL
  rvalues$processed.data <- NULL
  
  # DATA UPDATING —----------------------------------------------------
  
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
  
  # NORMALIZED DATA —---------------------------------------------------------
  rma.data <- reactive ({ 
    raw.data <- rvalues$raw.data
    call.exprs(raw.data,"rma")
  })
  
  # PLOT MA —----------------------------------------------------------------
  
  createRefArray <- function(data) {
    expr <- data@assayData$exprs
    median.expr <- apply(expr, MARGIN=1, FUN=median)
    return(median.expr)
  }
  
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
  
  indize <- reactive({
    (which(rvalues$file_names==input$fileName))-1
  })
  
  #---------------------------------------------------------------—

  observeEvent(
    input$delete,
    { 
    eliminate (which(input$data_loader$name==input$filedelete)-1)
    }
    )
  
  
  eliminate <- function(pos){

    rvalues$raw.data <- rvalues$raw.data[-pos]
    rvalues$file_names <- rvalues$file_names[-pos]
    #rvalues$file_names <- grep(input$filedelete,rvalues$file_names, ignore.case = TRUE)
    ##getRawData
    
  }
  
  # OUTPUTS —----------------------------------------------------------
  
  output$pos <- renderText({
    pos <- 0
    pos <- (which(rvalues$file_names==input$filedelete))-1
    paste("You have selected", pos)
  })
  
  
  output$fileName <- renderUI({ 
    
    selectInput("fileName", "choice a file",grep(".CEL",rvalues$file_names, value=T))
    
  })
  output$fileDelete <- renderUI({
    selectInput(inputId = "filedelete", "choice a file",grep(".CEL",rvalues$file_names, value=T))
  })
  
  output$plot <- renderPlot({
    res <- NULL
    if (is.null(rvalues$file_names)){
          raw.data <- getRawData()
          if(!is.null(rvalues$directory)){
            res <- boxplot(raw.data, col=raw.data@phenoData@data$Type)
          }
          print(res)
    }else{
      raw.data <- rvalues$raw.data
      if(!is.null(rvalues$directory)){
        res <- boxplot(raw.data, col=raw.data@phenoData@data$Type)
      }
      print(res)
    }

  })
  
  output$hist <- renderPlot({
    hist(rvalues$raw.data)
  })
  
  output$rna <- renderPlot({
    raw.data <- rvalues$raw.data
    res <- NULL
    data.deg <- AffyRNAdeg(raw.data)
    res <-plotAffyRNAdeg(data.deg)
    print(res)
  })
  
  output$qc <- renderPlot({
    raw.data <- rvalues$raw.data
    res <- NULL
    mas5.data <- call.exprs(raw.data,"mas5")
    qcs <- qc(raw.data,mas5.data)
    res <-plot(qcs)
    print(res)
  })
  
  output$rma <- renderPlot({
    raw.data <- rvalues$raw.data
    rma <- rma.data()
    res <-boxplot(exprs(rma), col=rma@phenoData@data$Type)
    print(res)
  })
  
  output$image <- renderPlot({
    raw.data <- rvalues$raw.data
    num.arrays <- length(raw.data)
    res <- NULL
    if(!is.null(rvalues$directory)){
      
      res <- image(raw.data[,indize()])
      
    }
    print(res)
  })
  
  output$plotMA <- renderPlot({
    raw.data <- rvalues$raw.data
    num.arrays <- length(raw.data)
    res <- NULL
    if(!is.null(rvalues$directory)){
      ref.array <- createRefArray(raw.data)
      res <- plotMA(raw.data, indize(), ref=ref.array, subsampling=10000, size=5, alpha=0.5)
      
    }
    print(res)
  })
  
  output$densrma <- renderPlot({
    rma <- rma.data()
    plot(density(exprs(rma[,indize()])))
  })
  
  
  
}
shinyServer(server)