# Test script for hypergate operator
# This script can be used to test the operator locally

library(tercen)
library(dplyr)
library(tidyr)
library(tibble)
library(hypergate)

# Mock data generation for testing
generate_test_data <- function(n_events = 1000, n_markers = 5) {
  # Generate random marker names
  marker_names <- paste0("Marker", 1:n_markers)
  
  # Generate random event IDs
  event_ids <- 0:(n_events-1)
  
  # Generate random expression data
  set.seed(123)
  expression_data <- matrix(
    runif(n_events * n_markers, 0, 1),
    nrow = n_events,
    ncol = n_markers,
    dimnames = list(NULL, marker_names)
  )
  
  # Create main data in long format
  main_data <- data.frame()
  for (i in 1:n_markers) {
    marker_data <- tibble(
      .ci = rep(event_ids, each = 1),
      .ri = rep(i-1, n_events),
      .y = expression_data[, i]
    )
    main_data <- rbind(main_data, marker_data)
  }
  
  # Create row data (marker information)
  row_data <- tibble(
    .ri = 0:(n_markers-1),
    logicle..variable = paste0("Channel", 1:n_markers),
    logicle..channel_description = marker_names
  )
  
  # Create column data (event information)
  col_data <- tibble(
    .ci = event_ids,
    logicle..event_id = paste0("Event", 1:n_events)
  )
  
  list(
    main_data = main_data,
    row_data = row_data,
    col_data = col_data
  )
}

# Generate test data
test_data <- generate_test_data(1000, 5)

# Mock Tercen context
mock_ctx <- list(
  op.value = function(name) {
    switch(
      name,
      "high_expression" = 0.7,
      "low_expression" = 0.3,
      "expressed_marker" = "Marker1",
      "not_expressed_marker" = "Marker5",
      NULL
    )
  },
  addNamespace = function(df) df,
  save = function(df) {
    cat("Would save data with dimensions:", nrow(df), "x", ncol(df), "\n")
    print(head(df))
    invisible(df)
  }
)

# Mock select, cselect, rselect functions
select <- function(ctx) test_data$main_data
cselect <- function(ctx) test_data$col_data
rselect <- function(ctx) test_data$row_data

# Set verbose flag for detailed logging
verbose <- TRUE

# Function to log messages if verbose is TRUE
log_message <- function(message) {
  if (verbose) {
    cat(paste0("[Hypergate Operator] ", message, "\n"))
  }
}

log_message("Starting Hypergate Operator Test")

# Get operator properties
high_expression_threshold <- mock_ctx$op.value('high_expression')
low_expression_threshold <- mock_ctx$op.value('low_expression')
expressed_markers_str <- mock_ctx$op.value('expressed_marker')
not_expressed_markers_str <- mock_ctx$op.value('not_expressed_marker')

log_message(paste0("High expression threshold: ", high_expression_threshold))
log_message(paste0("Low expression threshold: ", low_expression_threshold))
log_message(paste0("Expressed markers: ", expressed_markers_str))
log_message(paste0("Not expressed markers: ", not_expressed_markers_str))

# Parse comma-separated marker lists
expressed_markers <- if (expressed_markers_str != "") {
  strsplit(expressed_markers_str, ",")[[1]]
} else {
  character(0)
}

not_expressed_markers <- if (not_expressed_markers_str != "") {
  strsplit(not_expressed_markers_str, ",")[[1]]
} else {
  character(0)
}

# Get data from mock context
log_message("Extracting data from mock context")

# Get main data with .y values
main_data <- select(mock_ctx)

# Get column projections (event IDs)
col_data <- cselect(mock_ctx)

# Get row projections (variable/channel information)
row_data <- rselect(mock_ctx)

log_message(paste0("Main data rows: ", nrow(main_data)))
log_message(paste0("Column data rows: ", nrow(col_data)))
log_message(paste0("Row data rows: ", nrow(row_data)))

# Transform data from long to wide format
log_message("Transforming data to wide format")

# Join main data with row data to get marker information
data_with_markers <- main_data %>%
  left_join(row_data, by = ".ri")

# Reshape to wide format where rows are events and columns are markers
wide_data <- data_with_markers %>%
  pivot_wider(
    id_cols = .ci,
    names_from = logicle..channel_description,
    values_from = .y,
    values_fn = list(.y = mean)  # Use mean if multiple values per cell
  )

log_message(paste0("Wide data dimensions: ", nrow(wide_data), " x ", ncol(wide_data)))

# Identify population of interest based on marker expression
log_message("Identifying population of interest")

# Initialize a logical vector for population membership
population_membership <- rep(TRUE, nrow(wide_data))

# Apply high expression thresholds
for (marker in expressed_markers) {
  if (marker %in% colnames(wide_data)) {
    marker_values <- wide_data[[marker]]
    threshold <- quantile(marker_values, high_expression_threshold, na.rm = TRUE)
    population_membership <- population_membership & (marker_values >= threshold)
    log_message(paste0("Applied high expression threshold for ", marker, ": ", threshold))
  } else {
    log_message(paste0("Warning: Marker ", marker, " not found in data"))
  }
}

# Apply low expression thresholds
for (marker in not_expressed_markers) {
  if (marker %in% colnames(wide_data)) {
    marker_values <- wide_data[[marker]]
    threshold <- quantile(marker_values, low_expression_threshold, na.rm = TRUE)
    population_membership <- population_membership & (marker_values <= threshold)
    log_message(paste0("Applied low expression threshold for ", marker, ": ", threshold))
  } else {
    log_message(paste0("Warning: Marker ", marker, " not found in data"))
  }
}

# Count events in population of interest
n_population <- sum(population_membership, na.rm = TRUE)
log_message(paste0("Population of interest contains ", n_population, " events"))

# Prepare data for hypergate
log_message("Preparing data for hypergate")

# Remove .ci column for hypergate input
xp_data <- wide_data %>%
  select(-(.ci)) %>%
  as.matrix()

# Create gate vector (1 for population of interest, 0 for others)
gate_vector <- ifelse(population_membership, 1, 0)

# Apply hypergate algorithm
log_message("Applying hypergate algorithm")

# Handle potential errors in hypergate
tryCatch({
  # Run hypergate
  hg_result <- hypergate(
    xp = xp_data,
    gate_vector = gate_vector,
    level = 1,  # Our population of interest is labeled as 1
    verbose = verbose,
    beta = 1  # Equal weight to precision and recall
  )
  
  # Apply the gate to get membership
  gate_membership <- subset_matrix_hg(hg_result, xp_data)
  
  log_message(paste0("Hypergate identified ", sum(gate_membership), " events in the gate"))
  
  # Prepare output data
  output_data <- tibble(
    .ci = wide_data$.ci,
    .hypergate_member = as.integer(gate_membership)
  )
  
}, error = function(e) {
  # If hypergate fails, use the initial population membership
  log_message(paste0("Hypergate error: ", e$message))
  log_message("Using initial population membership as fallback")
  
  output_data <- tibble(
    .ci = wide_data$.ci,
    .hypergate_member = as.integer(population_membership)
  )
})

# Verify output has same number of rows as column projection
if (nrow(output_data) != nrow(col_data)) {
  log_message("Warning: Output rows don't match column projection rows")
}

# Save results
mock_ctx$save(output_data)

log_message("Hypergate operator test completed successfully")