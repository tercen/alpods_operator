# Test script for ALPODs operator
# This script can be used for local testing without Tercen

library(dplyr)
library(tidyr)
library(tibble)

# Mock Tercen context
mock_ctx <- function() {
  # Create sample data
  set.seed(42)
  n_events <- 100
  n_channels <- 5
  
  # Create main data
  data_tbl <- expand.grid(
    .ci = 0:(n_events-1),
    .ri = 0:(n_channels-1)
  ) %>%
    mutate(.y = rnorm(n_events * n_channels))
  
  # Create row data (channels)
  row_data <- data.frame(
    logicle..variable = paste0("Channel", 1:n_channels),
    logicle..channel_description = paste0("Marker", 1:n_channels),
    .ri = 0:(n_channels-1)
  )
  
  # Create column data (events)
  col_data <- data.frame(
    logicle..event_id = 1:n_events,
    .ci = 0:(n_events-1)
  )
  
  # Create mock context
  ctx <- list(
    select = function() data_tbl,
    rselect = function() row_data,
    cselect = function() col_data,
    save = function(df) {
      cat("Saving data frame with", nrow(df), "rows\n")
      print(head(df))
    },
    addNamespace = function() {
      return(list(
        select = function(df) {
          return(df)
        }
      ))
    },
    op.value = function(name, default = NULL, type = NULL) {
      # Mock operator properties
      properties <- list(
        verbose = TRUE,
        min_samples = 5,
        purity_threshold = 0.9,
        effect_size_threshold = 0.5
      )
      
      if (name %in% names(properties)) {
        value <- properties[[name]]
        if (!is.null(type)) {
          value <- type(value)
        }
        return(value)
      }
      
      return(default)
    }
  )
  
  return(ctx)
}

# Set global context for testing
ctx <- mock_ctx()

# Source the main script
source("main.R")