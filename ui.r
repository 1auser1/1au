source("global.R")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("Simple Data Cleaner MVP"),
  sidebarLayout(
    sidebarPanel(
      dataInputUI("data_upload"),
      hr(),
      columnReorderUI("column_reorder"),
      hr(),
      columnControlsUI("column_controls"),
      hr(),
      h5("NA Count Summary:"),
      tableOutput("na_summary")
    ),
    mainPanel(
      DTOutput("data_preview"),
      tags$div(style = "display: none;", textInput("trigger", "Trigger", value = 0)),
      # Use the simple hover module
      simpleHoverUI("simple_hover")
    )
  )
)