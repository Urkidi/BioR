
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
  navbarPage("Affymetrix",
             tabPanel("General",
                sidebarLayout(
                  sidebarPanel(
                    #selectInput("fileName", "Select your choice", choices = filename, selected = 1),
                    br(),
                    verbatimTextOutput("summary"),
                    br()
                    
                  ),
                  # Show a tabset that includes a plot, summary, and table view
                  # of the generated distribution
                  mainPanel(
                    tabsetPanel(type = "tabs", 
                                tabPanel("Plot", plotOutput(outputId="file_list", width="500px")),
                                tabPanel("Hist", plotOutput(outputId="table", width="500px")),
                                tabPanel("RNA degradation", plotOutput(outputId="file_list2", width="500px"))
                    )
                  )
                )
            ),
            tabPanel("Especific",
                     sidebarLayout(
                       sidebarPanel(
                         #selectInput("fileName", "Select your choice", choices = filename, selected = 1),
                         br(),
                         verbatimTextOutput("summary2"),
                         br()
                         
                       ),
                       # Show a tabset that includes a plot, summary, and table view
                       # of the generated distribution
                       mainPanel(
                         tabsetPanel(type = "tabs", 
                                     tabPanel("PlotMA", plotOutput(outputId="plotMA", width="500px"))
                         )
                       )
                     )
            )
          
  )
)
  
shinyUI(ui)
