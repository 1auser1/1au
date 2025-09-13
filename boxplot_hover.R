# Boxplot on Hover Module
boxplotHoverUI <- function(id) {
  ns <- NS(id)
  div(
    id = ns("hover_plot_container"),
    style = "position: fixed; bottom: 20px; right: 20px; width: 300px; background: white; border: 1px solid #ccc; border-radius: 8px; padding: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); z-index: 1000; display: none;",
    h5("Column Distribution", style = "margin-top: 0;"),
    plotOutput(ns("hover_plot"), height = "200px"),
    div(style = "font-size: 11px; color: #666; text-align: center;", 
        "Hover over a column name to see distribution")
  )
}

boxplotHoverServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to store the currently hovered column
    hovered_column <- reactiveVal(NULL)
    
    # JavaScript to handle hover events on column headers
    observeEvent(values$data, {
      req(values$data)
      
      # Inject JavaScript to add hover events to column headers
      runjs(sprintf('
        $(document).on("mouseenter", ".dataTables_scrollHead th", function() {
          var colIndex = $(this).index();
          var colName = $(this).text().trim();
          Shiny.setInputValue("%s", {index: colIndex, name: colName, nonce: Math.random()});
        });
        
        $(document).on("mouseleave", ".dataTables_scrollHead th", function() {
          Shiny.setInputValue("%s", null);
        });
      ', ns("column_hover"), ns("column_hover")))
    })
    
    # Observe hover events
    observeEvent(input$column_hover, {
      req(input$column_hover, values$data)
      hovered_column(input$column_hover$name)
      
      # Show the plot container
      runjs(sprintf('$("#%s").fadeIn(200);', ns("hover_plot_container")))
    })
    
    observe({
      if (is.null(input$column_hover)) {
        hovered_column(NULL)
        # Hide the plot container
        runjs(sprintf('$("#%s").fadeOut(200);', ns("hover_plot_container")))
      }
    })
    
    # Generate the boxplot
    output$hover_plot <- renderPlot({
      req(hovered_column(), values$data)
      col_name <- hovered_column()
      df <- values$data
      
      # Check if the column is numeric
      if (is.numeric(df[[col_name]])) {
        ggplot(df, aes(y = .data[[col_name]])) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          labs(title = col_name, y = NULL) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
      } else {
        # For non-numeric columns, show a bar plot or message
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Not a numeric column", 
                   size = 6, color = "gray") +
          theme_void()
      }
    })
  })
}