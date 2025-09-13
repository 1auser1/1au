# Load all required libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(DT)
library(sortable) # <- ADD THIS LINE

# Source all module files
source("modules/data_input.R")
source("modules/column_controls.R")
source("modules/data_processing.R")
source("modules/column_reorder.R")
source("modules/simple_hover.R")

# Helper functions
na_summary_table <- function(data) {
  data %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Column", values_to = "NA Count")
}