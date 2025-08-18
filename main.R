library(tercen)
library(dplyr)
library(tidyr)
library(tibble)

# Get Tercen context
#http://127.0.0.1:5400/kumo_test/w/0b4ec8f92d47f8a388859024bb0186e7/ds/9c93405f-d024-4047-b056-1cfe02f78ac0
options("tercen.workflowId"= "0b4ec8f92d47f8a388859024bb0186e7")
options("tercen.stepId"= "9c93405f-d024-4047-b056-1cfe02f78ac0")
ctx <- tercenCtx(username = "test", password = "test", serviceUri = "http://127.0.0.1:5400")

# Get operator properties
verbose <- ctx$op.value('verbose', default = TRUE, type = logical)
min_samples <- ctx$op.value('min_samples', default = 10, type = as.numeric)
purity_threshold <- ctx$op.value('purity_threshold', default = 0.95, type = as.numeric)
effect_size_threshold <- ctx$op.value('effect_size_threshold', default = 0.5, type = as.numeric)

# Function to log messages if verbose is TRUE
log_message <- function(message) {
  if (verbose) {
    cat(paste0("[ALPODs Operator] ", message, "\n"))
  }
}

log_message("Starting ALPODs Operator")

# Load data from Tercen
log_message("Loading data from Tercen")
data_tbl <- ctx %>% select()
row_data <- ctx %>% rselect()
col_data <- ctx %>% cselect()

# Add index columns to projections
log_message("Processing projections")
row_data <- row_data %>% mutate(.ri = row_number() - 1)
col_data <- col_data %>% mutate(.ci = row_number() - 1)

# Check if data is available
if (nrow(data_tbl) == 0) {
  stop("No data available in the main projection.")
}
if (nrow(row_data) == 0) {
  stop("No data available in the row projection.")
}
if (nrow(col_data) == 0) {
  stop("No data available in the column projection.")
}

log_message(paste("Loaded", nrow(data_tbl), "data points"))
log_message(paste("Loaded", nrow(row_data), "row variables"))
log_message(paste("Loaded", nrow(col_data), "column variables"))

# Transform data for ALPODs
log_message("Transforming data for ALPODs")

# Create a wide format data frame with events as rows and channels as columns
wide_data <- data_tbl %>%
  pivot_wider(
    id_cols = .ci,
    names_from = .ri,
    values_from = .y
  )

# Get channel names from row projection
channel_names <- row_data %>%
  select(logicle..variable, logicle..channel_description, .ri) %>%
  mutate(channel_name = paste(logicle..variable, logicle..channel_description, sep = "_"))

# Rename columns in wide_data using channel names
colnames(wide_data)[-1] <- channel_names$channel_name[match(as.numeric(colnames(wide_data)[-1]), channel_names$.ri)]

# Add event IDs from column projection
wide_data <- wide_data %>%
  left_join(col_data %>% select(.ci, logicle..event_id), by = ".ci")

log_message("Data transformation complete")

# ALPODs algorithm implementation
log_message("Implementing ALPODs algorithm")

# Node class for decision tree
Node <- function(data, depth = 0) {
  node <- list(
    data = data,
    left = NULL,
    right = NULL,
    split_feature = NULL,
    split_value = NULL,
    depth = depth,
    is_leaf = FALSE,
    prediction = NULL,
    rule = ""
  )
  class(node) <- "Node"
  return(node)
}

# Function to calculate entropy
calculate_entropy <- function(labels) {
  if (length(labels) == 0) return(0)
  
  proportions <- table(labels) / length(labels)
  -sum(proportions * log2(proportions))
}

# Function to find the best split
find_best_split <- function(data, features) {
  best_gain <- -Inf
  best_feature <- NULL
  best_value <- NULL
  
  current_entropy <- calculate_entropy(data$cell_type)
  
  for (feature in features) {
    # Skip if feature is not numeric
    if (!is.numeric(data[[feature]])) next
    
    # Get unique values for the feature
    values <- sort(unique(data[[feature]]))
    
    # Try each value as a potential split
    for (i in 1:(length(values) - 1)) {
      split_value <- (values[i] + values[i + 1]) / 2
      
      # Split the data
      left_indices <- data[[feature]] <= split_value
      right_indices <- !left_indices
      
      # Skip if either split is too small
      if (sum(left_indices) < min_samples || sum(right_indices) < min_samples) next
      
      # Calculate entropy for each split
      left_entropy <- calculate_entropy(data$cell_type[left_indices])
      right_entropy <- calculate_entropy(data$cell_type[right_indices])
      
      # Calculate weighted entropy
      weighted_entropy <- (sum(left_indices) * left_entropy + sum(right_indices) * right_entropy) / nrow(data)
      
      # Calculate information gain
      gain <- current_entropy - weighted_entropy
      
      # Update best split if this one is better
      if (gain > best_gain) {
        best_gain <- gain
        best_feature <- feature
        best_value <- split_value
      }
    }
  }
  
  return(list(feature = best_feature, value = best_value, gain = best_gain))
}

# Function to build the decision tree
build_tree <- function(data, features, depth = 0, max_depth = 10) {
  # Create a new node
  node <- Node(data, depth)
  
  # Check stopping criteria
  if (depth >= max_depth || 
      nrow(data) < min_samples || 
      length(unique(data$cell_type)) == 1 ||
      max(table(data$cell_type) / nrow(data)) >= purity_threshold) {
    
    # Make this a leaf node
    node$is_leaf <- TRUE
    node$prediction <- names(which.max(table(data$cell_type)))
    
    # Create a simple rule for this leaf
    node$rule <- paste("Leaf", depth, ":", node$prediction)
    
    return(node)
  }
  
  # Find the best split
  split <- find_best_split(data, features)
  
  # If no good split was found, make this a leaf node
  if (is.null(split$feature) || split$gain <= 0) {
    node$is_leaf <- TRUE
    node$prediction <- names(which.max(table(data$cell_type)))
    node$rule <- paste("Leaf", depth, ":", node$prediction)
    return(node)
  }
  
  # Set the split information
  node$split_feature <- split$feature
  node$split_value <- split$value
  
  # Split the data
  left_indices <- data[[split$feature]] <= split$value
  right_indices <- !left_indices
  
  # Create the child nodes
  node$left <- build_tree(data[left_indices, ], features, depth + 1, max_depth)
  node$right <- build_tree(data[right_indices, ], features, depth + 1, max_depth)
  
  # Create a rule for this node
  node$rule <- paste(split$feature, "<=", round(split$value, 4))
  
  return(node)
}

# Function to predict using the decision tree
predict_tree <- function(node, data) {
  if (node$is_leaf) {
    return(list(
      prediction = rep(node$prediction, nrow(data)),
      rule = rep(node$rule, nrow(data))
    ))
  }
  
  # Split the data
  left_indices <- data[[node$split_feature]] <= node$split_value
  right_indices <- !left_indices
  
  # Predict for each split
  left_predictions <- predict_tree(node$left, data[left_indices, ])
  right_predictions <- predict_tree(node$right, data[right_indices, ])
  
  # Combine predictions
  predictions <- rep(NA, nrow(data))
  rules <- rep(NA, nrow(data))
  
  predictions[left_indices] <- left_predictions$prediction
  predictions[right_indices] <- right_predictions$prediction
  
  rules[left_indices] <- left_predictions$rule
  rules[right_indices] <- right_predictions$rule
  
  return(list(prediction = predictions, rule = rules))
}

# Function to extract rules from the decision tree
extract_rules <- function(node, current_rule = "") {
  if (node$is_leaf) {
    return(data.frame(
      cell_type = node$prediction,
      rule = ifelse(current_rule == "", node$rule, paste(current_rule, "AND", node$rule))
    ))
  }
  
  # Build rules for left and right branches
  left_rule <- ifelse(current_rule == "", 
                     node$rule, 
                     paste(current_rule, "AND", node$rule))
  
  right_rule <- ifelse(current_rule == "", 
                      paste("NOT (", node$rule, ")"), 
                      paste(current_rule, "AND NOT (", node$rule, ")"))
  
  # Recursively extract rules
  left_rules <- extract_rules(node$left, left_rule)
  right_rules <- extract_rules(node$right, right_rule)
  
  # Combine rules
  return(rbind(left_rules, right_rules))
}

# Main ALPODs function
alpods <- function(data, features) {
  log_message("Building decision tree")
  
  # Build the decision tree
  root <- build_tree(data, features)
  
  log_message("Extracting rules")
  
  # Extract rules
  rules <- extract_rules(root)
  
  log_message("Making predictions")
  
  # Make predictions
  predictions <- predict_tree(root, data)
  browser()
  # Create result dataframe
  result <- data.frame(
    .ci = as.integer(data$.ci),
    cell_type = predictions$prediction,
    rule = predictions$rule
  )
  
  return(result)
}

# Prepare data for ALPODs
log_message("Preparing data for ALPODs")

# For demonstration, we'll create a simple clustering to get cell types
# In a real implementation, this might come from another source or be part of the ALPODs algorithm
set.seed(42)
k <- min(5, nrow(wide_data) - 1)  # Ensure k is less than the number of data points
cluster_result <- kmeans(wide_data[, -which(names(wide_data) %in% c(".ci", "logicle..event_id"))], centers = k)

# Add cluster assignments to the data
wide_data$cell_type <- paste0("Cluster_", cluster_result$cluster)

# Get feature names (excluding metadata columns)
feature_names <- setdiff(names(wide_data), c(".ci", "logicle..event_id", "cell_type"))

# Run ALPODs algorithm
log_message("Running ALPODs algorithm")
result <- alpods(wide_data, feature_names)

# Prepare output
log_message("Preparing output")
output <- result %>%
  select(.ci, cell_type, rule)

# Return result to Tercen
log_message("Returning result to Tercen")
browser()
output %>%
  ctx$addNamespace() %>%
  ctx$save()

log_message("ALPODs Operator completed successfully")