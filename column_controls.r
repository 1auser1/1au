# Column Controls Module
columnControlsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("column_controls_ui"))
}

columnControlsServer <- function(id, values, parent_input, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render column controls UI
    output$column_controls_ui <- renderUI({
      req(values$data)
      input$trigger <- parent_input$trigger  # <- ADD THIS to react to reordering
      
      df <- values$data
      current_types <- values$col_types
      
      tagList(
        h4("Column Controls"),
        lapply(names(df), function(col_name) {
          current_type <- current_types[[col_name]] %||% "Unknown"
          
          wellPanel(
            h5(col_name),
            textInput(ns(paste0("rename_", col_name)),
                      "New Name:", value = col_name),
            div(style = "margin-bottom: 10px;", strong("Change Type:")),
            div(style = "display: flex; gap: 5px; margin-bottom: 10px;",
                actionButton(ns(paste0("type_num_", col_name)), "Num",
                            style = ifelse(current_type == "Numeric", 
                                         "background-color: #007bff; color: white;", 
                                         "background-color: #f8f9fa;")),
                actionButton(ns(paste0("type_char_", col_name)), "Char",
                            style = ifelse(current_type == "Character", 
                                         "background-color: #28a745; color: white;", 
                                         "background-color: #f8f9fa;")),
                actionButton(ns(paste0("type_date_", col_name)), "Date",
                            style = ifelse(current_type == "Date", 
                                         "background-color: #dc3545; color: white;", 
                                         "background-color: #f8f9fa;"))
            ),
            div(style = "font-size: 12px; color: #666;",
                paste("Current:", current_type))
          )
        })
      )
    })
    
    # Observe type change buttons
    observe({
      req(values$data)
      df <- values$data
      
      for (col_name in names(df)) {
        local({
          col_name_local <- col_name
          observeEvent(input[[paste0("type_num_", col_name_local)]], {
            values$col_types[[col_name_local]] <- "Numeric"
            values$edit_count <- values$edit_count + 1
            updateTextInput(parent_session, "trigger", 
                           value = values$edit_count)
          })
          
          observeEvent(input[[paste0("type_char_", col_name_local)]], {
            values$col_types[[col_name_local]] <- "Character"
            values$edit_count <- values$edit_count + 1
            updateTextInput(parent_session, "trigger", 
                           value = values$edit_count)
          })
          
          observeEvent(input[[paste0("type_date_", col_name_local)]], {
            values$col_types[[col_name_local]] <- "Date"
            values$edit_count <- values$edit_count + 1
            updateTextInput(parent_session, "trigger", 
                           value = values$edit_count)
          })
        })
      }
    })
  })
}