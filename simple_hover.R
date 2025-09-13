# Simple Hover Module - More reliable approach
simpleHoverUI <- function(id) {
  ns <- NS(id)
  div(
    id = ns("hover_container"),
    style = "position: fixed; bottom: 20px; right: 20px; width: 300px; background: white; border: 2px solid #007bff; border-radius: 8px; padding: 15px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 1000;",
    h4("Data Preview", style = "margin-top: 0; color: #007bff;"),
    p("Hover over column names in the table to see distributions", 
      style = "font-size: 12px; color: #666; margin-bottom: 15px;"),
    plotOutput(ns("hover_plot"), height = "200px")
  )
}

simpleHoverServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a reactive for the selected column
    selected_col <- reactiveVal(NULL)
    
    # Update selected column when data changes
    observeEvent(values$data, {
      req(values$data)
      if (!is.null(selected_col())) {
        if (!selected_col() %in% names(values$data)) {
          selected_col(NULL)
        }
      }
    })
    
    # Generate plot based on selected column
    output$hover_plot <- renderPlot({
      req(values$data)
      
      if (is.null(selected_col())) {
        # Show instruction message
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Hover over any column name\nin the table above", 
                   size = 5, color = "gray", lineheight = 0.8) +
          theme_void()
      } else if (selected_col() %in% names(values$data)) {
        df <- values$data
        col_name <- selected_col()
        
        if (is.numeric(df[[col_name]])) {
          # Boxplot for numeric columns
          ggplot(df, aes(y = .data[[col_name]])) +
            geom_boxplot(fill = "lightblue", alpha = 0.7) +
            labs(title = col_name, y = NULL) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 12))
        } else {
          # Message for non-numeric columns
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = paste("Column type:", class(df[[col_name]])[1]), 
                     size = 5, color = "gray") +
            theme_void()
        }
      }
    })
    
    # Return function to update from outside
    list(
      update_selection = function(col_name) {
        selected_col(col_name)
      }
    )
  })
}