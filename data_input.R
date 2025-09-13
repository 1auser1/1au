# Data Input Module
dataInputUI <- function(id) {
  ns <- NS(id)
  fileInput(ns("file_upload"), "Choose CSV File", accept = ".csv")
}

dataInputServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$file_upload, {
      req(input$file_upload)
      
      # First, detect the maximum number of columns
      first_pass <- read_lines(input$file_upload$datapath, n_max = 10)
      max_cols <- max(count_fields(input$file_upload$datapath, 
                                   tokenizer = tokenizer_csv()))
      
      # Create column names for the maximum number of columns
      col_names <- paste0("col_", seq_len(max_cols))
      
      # Read with all columns accounted for
      df <- read_csv(
        input$file_upload$datapath,
        col_names = col_names,
        skip = 1,
        show_col_types = FALSE,
        na = c("", "NA", "NULL", "N/A", "NaN", " ", "NAN", ".."),
        trim_ws = TRUE
      )
      
      values$data <- df
      
      # Initialize column types
      initial_types <- sapply(df, function(x) {
        if (is.numeric(x)) "Numeric"
        else if (is.character(x)) "Character" 
        else if (inherits(x, "Date")) "Date"
        else "Unknown"
      })
      values$col_types <- as.list(initial_types)
    })
  })
}