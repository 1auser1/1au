# Column Reordering Module - IMPROVED (Instant drag-and-drop)
columnReorderUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Drag to Reorder Columns"),
    uiOutput(ns("drag_container")),
    # Removed the apply button - changes are now instant
    div(style = "font-size: 12px; color: #666; margin-top: 5px;",
        "Drag column names to reorder. Changes are applied instantly.")
  )
}

columnReorderServer <- function(id, values, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render the drag-and-drop bucket list
    output$drag_container <- renderUI({
      req(values$data)
      bucket_list(
        header = NULL, # No header for cleaner look
        group_name = ns("column_bucket"),
        orientation = "vertical",
        add_rank_list(
          text = "Column Order (Drag to Reorder)",
          labels = names(values$data),
          input_id = ns("column_order")
        )
      )
    })
    
    # OBSERVE THE DRAG-AND-DROP INPUT DIRECTLY - NO BUTTON NEEDED
    observeEvent(input$column_order, {
      req(values$data, input$column_order)
      
      # Only proceed if the order has actually changed
      current_names <- names(values$data)
      if (!identical(current_names, input$column_order)) {
        
        # Get the new order of column NAMES from the drag-and-drop list
        new_order_names <- input$column_order
        
        # Find the numeric INDEX for each name in the original data
        new_order_indices <- match(new_order_names, current_names)
        
        # Use the numeric indices to reorder the dataframe
        values$data <- values$data[, new_order_indices, drop = FALSE]
        
        # Also reorder the col_types to match!
        if (length(values$col_types) > 0) {
          values$col_types <- values$col_types[new_order_names]
        }
        
        # Update the trigger to force other reactives to update
        values$edit_count <- values$edit_count + 1
        updateTextInput(parent_session, "trigger", value = values$edit_count)
      }
    })
    
    # Return the new order for use in other modules if needed
    return(reactive(input$column_order))
  })
}