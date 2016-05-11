
library(shiny)


header <- h1("Affymetrix Array Data Analysis")

fileLoader <- fileInput (inputId="data_loader", 
                         label="Upload Files", 
                         multiple=TRUE)
ui <- fluidPage(
  header, 
  fileLoader,
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
      sidebarPanel(
        radioButtons("dist", "Distribution type:",
                     c("Normal" = "norm",
                       "Uniform" = "unif",
                       "Log-normal" = "lnorm",
                       "Exponential" = "exp")),
        br()
        
      ),
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput(outputId="file_list", width="500px")), 
                  tabPanel("Plot2", plotOutput(outputId="file_list2", width="500px")),
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Hist", plotOutput(outputId="table", width="500px"))
      )
    )
  )
  
)
shinyUI(ui)
