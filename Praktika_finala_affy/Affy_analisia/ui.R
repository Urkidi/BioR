
library(shiny)


header <- h1("Affymetrix Array Data Analysis")

fileLoader <- fileInput (inputId="data_loader", 
                         label="Upload Files", 
                         multiple=TRUE)
shinyUI(fluidPage(
  header, 
  fileLoader,
  plotOutput(outputId="file_list", width="500px")
))
