# Data Processing Module
dataProcessingServer <- function(id, values, parent_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Process data with all changes
    processed_data <- reactive({
      req(values$data, values$col_types)
      parent_input$trigger
      
      df <- values$data
      current_types <- values$col_types
      
      # Apply changes to each column
      for (col_name in names(df)) {
        # Apply renaming
        rename_input <- parent_input[[paste0("rename_", col_name)]]
        if (!is.null(rename_input) && rename_input != "" && rename_input != col_name) {
          df <- df %>% rename(!!rename_input := !!col_name)
          col_name <- rename_input
        }
        
        # Apply type conversion
        target_type <- current_types[[col_name]]
        if (!is.null(target_type)) {
          tryCatch({
            if (target_type == "Numeric") {
              df[[col_name]] <- suppressWarnings(as.numeric(df[[col_name]]))
            } else if (target_type == "Character") {
              df[[col_name]] <- as.character(df[[col_name]])
            } else if (target_type == "Date") {
              df[[col_name]] <- as.Date(df[[col_name]], 
                                      tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y"))
            }
          }, error = function(e) {
            print(paste("Error converting", col_name, "to", target_type, ":", e$message))
          })
        }
      }
      df
    })
    
    return(processed_data)
  })
}