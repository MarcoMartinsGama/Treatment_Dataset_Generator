library(shiny)
library(shinyjs)
library(DT)

fluidPage(
  useShinyjs(),
  titlePanel("Treatment Dataset Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("msstatsFile", "MSstats results-annotated.txt"),
      fileInput("ttestFile", "T-test.txt"),
      actionButton("generate", "Generate dataset"),
      textOutput("output_text")
    ),
    mainPanel(
      fluidRow(style = 'overflow-x: auto', DT::dataTableOutput("mergedTable")),
      downloadButton("download_mergedTable", "Download Treatment Raw results Table"),
      downloadButton("download_final_results", "Download Treatment Dataset Results")
    )
  )
)