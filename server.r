source("global.R")

server <- function(input, output, session) {
  
  # Reactive values to store data and state
  values <- reactiveValues(
    data = NULL,
    col_types = list(),
    edit_count = 0
  )
  
  # Call data input module
  dataInputServer("data_upload", values)
  
  # Call column REORDERING module
  columnReorderServer("column_reorder", values, session)
  
  # Call column CONTROLS module
  columnControlsServer("column_controls", values, input, session)
  
  # Call data processing module
  processed_data <- dataProcessingServer("data_processing", values, input)
  
  # Call SIMPLE hover module
  hover_module <- simpleHoverServer("simple_hover", values)
  
  # Display data preview
  output$data_preview <- renderDT({
    req(processed_data())
    datatable(processed_data(), 
              options = list(
                scrollX = TRUE,
                dom = 'lftip'
              ),
              editable = TRUE,
              selection = 'none')
  })
  
  # Display NA summary
  output$na_summary <- renderTable({
    req(processed_data())
    input$trigger
    na_summary_table(processed_data())
  })
  
  # SIMPLE HOVER DETECTION - Add this
  observeEvent(input$data_preview_cell_info, {
    # This ensures the table is rendered before we try to access it
    delay(1000, {
      runjs('
        // Add hover effect to table headers
        $("#data_preview thead th").hover(
          function() {
            $(this).css("background-color", "#e3f2fd");
            var colName = $(this).text().trim();
            Shiny.setInputValue("hovered_column", colName);
          },
          function() {
            $(this).css("background-color", "");
            Shiny.setInputValue("hovered_column", null);
          }
        );
      ')
    })
  })
  
  # Observe the hovered column
  observeEvent(input$hovered_column, {
    req(input$hovered_column)
    hover_module$update_selection(input$hovered_column)
  })
  
  observe({
    if (is.null(input$hovered_column)) {
      hover_module$update_selection(NULL)
    }
  })
}