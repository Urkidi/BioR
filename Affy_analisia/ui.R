library(shiny)


header <- h1("Affymetrix Array Data Analysis")

fileLoader <- fileInput (inputId="data_loader", 
                         label="Upload Files", 
                         multiple=TRUE)
ui <- fluidPage(
  theme = "flatly.css",#themes installed:"slate.css" "darks.css" "flatly.css" "sandstone.css" 
  header, 
  fileLoader,
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  navbarPage("Affymetrix",
             tabPanel("General",
                      sidebarLayout(
                        sidebarPanel(
                          br(),
                          uiOutput("fileDelete"),
                          actionButton("delete","Delete"),
                          textOutput(outputId="pos"), 
                          br()
                          
                        ),
                        # Show a tabset that includes a plot, summary, and table view
                        # of the generated distribution
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("Plot", plotOutput(outputId="plot", width="500px")),
                                      tabPanel("Hist", plotOutput(outputId="hist", width="500px")),
                                      tabPanel("RNA degradation", plotOutput(outputId="rna", width="500px")),
                                      tabPanel("QC", plotOutput(outputId="qc", width="500px")),
                                      tabPanel("Normalized", plotOutput(outputId="rma", width="500px"))
                          )
                        )
                      )
             ),
             tabPanel("Especific",
                      sidebarLayout(
                        sidebarPanel(
                          br(),
                          uiOutput("fileName"),
                          br()
                        ),
                        # Show a tabset that includes a plot, summary, and table view
                        # of the generated distribution
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("Image", plotOutput(outputId="image", width="500px")),
                                      tabPanel("PlotMA", plotOutput(outputId="plotMA", width="500px")),
                                      tabPanel("Density Normalized", plotOutput(outputId="densrma", width="500px"))
                          )
                        )
                      )
             )
             
  )
)

shinyUI(ui)