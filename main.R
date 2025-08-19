library(tercen)
library(dplyr)
library(tidyr)
library(tibble)

# Get Tercen context
#http://127.0.0.1:5400/kumo_test/w/0b4ec8f92d47f8a388859024bb0186e7/ds/9c93405f-d024-4047-b056-1cfe02f78ac0
ctx <- tercenCtx()

# Get operator properties
verbose <- TRUE
min_samples_pct <- ctx$op.value('min_samples_pct', default = 0.02, type = as.numeric)
effect_size_threshold <- ctx$op.value('effect_size_threshold', default = 0.4, type = as.numeric)
abc_threshold_a <- ctx$op.value('abc_threshold_a', default = 0.8, type = as.numeric)
abc_threshold_b <- ctx$op.value('abc_threshold_b', default = 0.95, type = as.numeric)
max_populations <- ctx$op.value('max_populations', default = 10, type = as.numeric)
simpson_bins <- ctx$op.value('simpson_bins', default = 50, type = as.numeric)

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

# Function to calculate Simpson Index
calculate_simpson_index <- function(data, variable, target_class) {
  if (length(data[[variable]]) == 0) return(0)
  
  # Get target and non-target data
  target_data <- data[[variable]][data$cell_type == target_class]
  non_target_data <- data[[variable]][data$cell_type != target_class]
  
  # Remove NA values
  target_data <- target_data[!is.na(target_data)]
  non_target_data <- non_target_data[!is.na(non_target_data)]
  
  # Check if we have enough data points for density estimation
  if (length(target_data) < 2 || length(non_target_data) < 2) {
    log_message(paste("  Insufficient data for density estimation: target =", length(target_data), 
                     "non-target =", length(non_target_data)))
    return(0)
  }
  
  # Check if data has any variation
  if (var(target_data) == 0 || var(non_target_data) == 0) {
    log_message("  No variation in data - returning 0")
    return(0)
  }
  
  # Create probability density estimates using kernel density estimation
  range_min <- min(c(target_data, non_target_data), na.rm = TRUE)
  range_max <- max(c(target_data, non_target_data), na.rm = TRUE)
  
  # If range is too small, return 0
  if (range_max - range_min < 1e-10) {
    log_message("  Data range too small - returning 0")
    return(0)
  }
  
  # Create evaluation points
  eval_points <- seq(range_min, range_max, length.out = simpson_bins)
  
  # Calculate density estimates with error handling
  target_dens <- density(target_data, from = range_min, to = range_max, n = simpson_bins)
  target_density <- approx(target_dens$x, target_dens$y, eval_points, rule = 2)$y
  
  non_target_dens <- density(non_target_data, from = range_min, to = range_max, n = simpson_bins)
  non_target_density <- approx(non_target_dens$x, non_target_dens$y, eval_points, rule = 2)$y
  
  # Check for valid density estimates
  if (any(is.na(target_density)) || any(is.na(non_target_density))) {
    log_message("  Invalid density estimates - returning 0")
    return(0)
  }
  
  # Normalize densities to probabilities
  target_density <- target_density / (sum(target_density) + 1e-10)
  non_target_density <- non_target_density / (sum(non_target_density) + 1e-10)
  
  # Calculate Simpson Index as the conjunction (minimum) of probabilities
  conjunction <- pmin(target_density, non_target_density)
  simpson_index <- sum(conjunction) * (range_max - range_min) / simpson_bins
  
  return(simpson_index)
}

# Function to find Bayesian optimal split point
find_bayesian_split <- function(data, variable, target_classes) {
  if (!is.numeric(data[[variable]])) return(NULL)
  
  target_data <- data[[variable]][data$cell_type %in% target_classes]
  non_target_data <- data[[variable]][!data$cell_type %in% target_classes]
  
  # Remove NA values
  target_data <- target_data[!is.na(target_data)]
  non_target_data <- non_target_data[!is.na(non_target_data)]
  
  # Check if we have enough data points for density estimation
  if (length(target_data) < 2 || length(non_target_data) < 2) {
    log_message(paste("  Insufficient data for Bayesian split: target =", length(target_data), 
                     "non-target =", length(non_target_data)))
    return(NULL)
  }
  
  # Check if data has any variation
  if (var(target_data) == 0 || var(non_target_data) == 0) {
    log_message("  No variation in data for Bayesian split")
    return(NULL)
  }
  
  # Calculate priors
  prior_target <- length(target_data) / (length(target_data) + length(non_target_data))
  prior_non_target <- 1 - prior_target
  
  # Get evaluation range
  range_min <- min(c(target_data, non_target_data), na.rm = TRUE)
  range_max <- max(c(target_data, non_target_data), na.rm = TRUE)
  
  # If range is too small, return middle point
  if (range_max - range_min < 1e-10) {
    log_message("  Data range too small for Bayesian split")
    return((range_min + range_max) / 2)
  }
  
  eval_points <- seq(range_min, range_max, length.out = 1000)
  
  # Calculate density estimates with error handling
  target_dens <- density(target_data, from = range_min, to = range_max, n = 1000)
  target_density <- approx(target_dens$x, target_dens$y, eval_points, rule = 2)$y
  
  non_target_dens <- density(non_target_data, from = range_min, to = range_max, n = 1000)
  non_target_density <- approx(non_target_dens$x, non_target_dens$y, eval_points, rule = 2)$y
  
  # Check for valid density estimates
  if (any(is.na(target_density)) || any(is.na(non_target_density))) {
    log_message("  Invalid density estimates for Bayesian split")
    return((range_min + range_max) / 2)
  }
  
  # Calculate posterior probabilities
  posterior_target <- target_density * prior_target
  posterior_non_target <- non_target_density * prior_non_target
  
  # Find point where posteriors are most equal
  diff_posteriors <- abs(posterior_target - posterior_non_target)
  min_diff_idx <- which.min(diff_posteriors)
  
  return(eval_points[min_diff_idx])
}

# Function to find the best variable using Simpson Index
find_best_variable <- function(data, features) {
  best_simpson <- -Inf
  best_feature <- NULL
  best_split <- NULL
  
  # Calculate dynamic minimum samples based on dataset size
  min_samples <- max(10, ceiling(nrow(data) * min_samples_pct))
  
  # Get unique target classes
  target_classes <- unique(data$cell_type)
  
  for (i in seq_along(features)) {
    feature <- features[i]
    log_message(paste("Evaluating feature", i, "of", length(features), ":", feature))
    
    if (!is.numeric(data[[feature]])) {
      log_message(paste("Skipping non-numeric feature:", feature))
      next
    }
    
    # Calculate Simpson Index for each target class
    total_simpson <- 0
    for (j in seq_along(target_classes)) {
      target_class <- target_classes[j]
      log_message(paste("  Computing Simpson Index for class", j, "of", length(target_classes), ":", target_class))
      simpson_val <- calculate_simpson_index(data, feature, target_class)
      log_message(paste("  Simpson Index value:", round(simpson_val, 4)))
      total_simpson <- total_simpson + simpson_val
    }
    
    log_message(paste("Total Simpson Index for", feature, ":", round(total_simpson, 4)))
    
    # Find optimal Bayesian split point
    split_point <- find_bayesian_split(data, feature, target_classes[1])
    
    if (!is.null(split_point)) {
      # Check if split produces adequate sample sizes
      left_size <- sum(data[[feature]] <= split_point, na.rm = TRUE)
      right_size <- sum(data[[feature]] > split_point, na.rm = TRUE)
      
      if (left_size >= min_samples && right_size >= min_samples) {
        if (total_simpson > best_simpson) {
          best_simpson <- total_simpson
          best_feature <- feature
          best_split <- split_point
        }
      }
    }
  }
  
  return(list(feature = best_feature, value = best_split, simpson = best_simpson))
}

# Function to build the ALPODS decision tree (DAG)
build_alpods_tree <- function(data, features, depth = 0, max_depth = 15) {
  # Create a new node
  node <- Node(data, depth)
  
  # Calculate dynamic minimum samples
  min_samples <- max(10, ceiling(nrow(data) * min_samples_pct))
  
  # ALPODS stopping criteria
  if (depth >= max_depth || 
      nrow(data) < min_samples || 
      length(unique(data$cell_type)) == 1) {  # Complete purity (ALPODS standard)
    
    # Make this a leaf node
    node$is_leaf <- TRUE
    node$prediction <- names(which.max(table(data$cell_type)))
    
    # Create ALPODS-style rule
    node$rule <- paste("Population", depth, ":", node$prediction)
    
    return(node)
  }
  
  # Find the best variable using Simpson Index
  best_var <- find_best_variable(data, features)
  
  # If no good variable was found, make this a leaf node
  if (is.null(best_var$feature) || best_var$simpson <= 0) {
    node$is_leaf <- TRUE
    node$prediction <- names(which.max(table(data$cell_type)))
    node$rule <- paste("Population", depth, ":", node$prediction)
    return(node)
  }
  
  # Set the split information
  node$split_feature <- best_var$feature
  node$split_value <- best_var$value
  
  # Split the data
  left_indices <- data[[best_var$feature]] <= best_var$value
  right_indices <- !left_indices
  
  # Create the child nodes
  node$left <- build_alpods_tree(data[left_indices, ], features, depth + 1, max_depth)
  node$right <- build_alpods_tree(data[right_indices, ], features, depth + 1, max_depth)
  
  # Create ALPODS-style rule
  node$rule <- paste(best_var$feature, "<=", round(best_var$value, 4))
  
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

# Function to calculate Cohen's d effect size
calculate_cohens_d <- function(group1, group2) {
  if (length(group1) < 2 || length(group2) < 2) return(0)
  
  # Calculate means
  mean1 <- mean(group1, na.rm = TRUE)
  mean2 <- mean(group2, na.rm = TRUE)
  
  # Calculate standard deviations
  sd1 <- sd(group1, na.rm = TRUE)
  sd2 <- sd(group2, na.rm = TRUE)
  
  # Calculate sample sizes
  n1 <- length(group1)
  n2 <- length(group2)
  
  # Calculate pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  
  # Avoid division by zero
  if (pooled_sd == 0) return(0)
  
  # Calculate Cohen's d
  cohens_d <- abs(mean1 - mean2) / pooled_sd
  
  return(cohens_d)
}

# Function to extract rules with effect sizes
extract_rules_with_effect_size <- function(node, original_data, current_rule = "", current_indices = rep(TRUE, nrow(original_data))) {
  if (node$is_leaf) {
    # Get subpopulation data
    subpop_data <- original_data[current_indices, ]
    
    if (nrow(subpop_data) == 0) {
      return(data.frame(
        cell_type = node$prediction,
        rule = ifelse(current_rule == "", node$rule, paste(current_rule, "AND", node$rule)),
        effect_size = 0,
        population_size = 0
      ))
    }
    
    # Calculate effect size for each feature
    feature_names <- setdiff(names(original_data), c(".ci", "logicle..event_id", "cell_type"))
    max_effect_size <- 0
    
    for (feature in feature_names) {
      if (!is.numeric(original_data[[feature]])) next
      
      # Get target population data for this feature
      target_data <- subpop_data[[feature]][subpop_data$cell_type == node$prediction]
      
      # Get non-target population data for this feature
      non_target_data <- original_data[[feature]][original_data$cell_type != node$prediction]
      
      if (length(target_data) > 1 && length(non_target_data) > 1) {
        effect_size <- calculate_cohens_d(target_data, non_target_data)
        max_effect_size <- max(max_effect_size, effect_size)
      }
    }
    
    return(data.frame(
      cell_type = node$prediction,
      rule = ifelse(current_rule == "", node$rule, paste(current_rule, "AND", node$rule)),
      effect_size = max_effect_size,
      population_size = nrow(subpop_data)
    ))
  }
  
  # Build rules for left and right branches
  left_rule <- ifelse(current_rule == "", 
                     node$rule, 
                     paste(current_rule, "AND", node$rule))
  
  right_rule <- ifelse(current_rule == "", 
                      paste("NOT (", node$rule, ")"), 
                      paste(current_rule, "AND NOT (", node$rule, ")"))
  
  # Calculate indices for left and right branches
  left_indices <- current_indices & (original_data[[node$split_feature]] <= node$split_value)
  right_indices <- current_indices & (original_data[[node$split_feature]] > node$split_value)
  
  # Recursively extract rules
  left_rules <- extract_rules_with_effect_size(node$left, original_data, left_rule, left_indices)
  right_rules <- extract_rules_with_effect_size(node$right, original_data, right_rule, right_indices)
  
  # Combine rules
  return(rbind(left_rules, right_rules))
}

# Function to perform ABC analysis
abc_analysis <- function(values) {
  if (length(values) == 0) return(list(A = c(), B = c(), C = c()))
  
  # Sort values in descending order with indices
  sorted_indices <- order(values, decreasing = TRUE)
  sorted_values <- values[sorted_indices]
  
  # Calculate cumulative sum and percentages
  total_sum <- sum(sorted_values)
  if (total_sum == 0) return(list(A = c(), B = c(), C = c()))
  
  cumulative_sum <- cumsum(sorted_values)
  cumulative_percentage <- cumulative_sum / total_sum
  
  # Find ABC categories based on thresholds
  a_indices <- which(cumulative_percentage <= abc_threshold_a)
  b_indices <- which(cumulative_percentage > abc_threshold_a & cumulative_percentage <= abc_threshold_b)
  c_indices <- which(cumulative_percentage > abc_threshold_b)
  
  return(list(
    A = sorted_indices[a_indices],
    B = sorted_indices[b_indices], 
    C = sorted_indices[c_indices]
  ))
}

# Function to filter relevant subpopulations using effect size and ABC analysis
filter_relevant_populations <- function(rules_df) {
  if (nrow(rules_df) == 0) return(rules_df)
  
  # Filter by effect size threshold
  significant_rules <- rules_df[rules_df$effect_size >= effect_size_threshold, ]
  
  if (nrow(significant_rules) == 0) {
    log_message("No populations meet the effect size threshold")
    return(rules_df[1:min(3, nrow(rules_df)), ])  # Return top 3 if none meet threshold
  }
  
  # Perform ABC analysis on effect sizes
  abc_result <- abc_analysis(significant_rules$effect_size)
  
  # Select A-category populations (most important)
  relevant_indices <- abc_result$A
  
  # If A-category is empty, include B-category
  if (length(relevant_indices) == 0) {
    relevant_indices <- abc_result$B
  }
  
  # Apply 7±2 population limit for human comprehension
  max_pops <- min(max_populations, length(relevant_indices))
  if (max_pops > 0) {
    relevant_indices <- relevant_indices[1:max_pops]
    filtered_rules <- significant_rules[relevant_indices, ]
  } else {
    # Fallback: return top populations by effect size
    top_indices <- order(significant_rules$effect_size, decreasing = TRUE)[1:min(max_populations, nrow(significant_rules))]
    filtered_rules <- significant_rules[top_indices, ]
  }
  
  return(filtered_rules)
}

# Function to convert hard rules to fuzzy rules
generate_fuzzy_rules <- function(rules_df, data, features) {
  if (nrow(rules_df) == 0) return(rules_df)
  
  fuzzy_rules <- rules_df
  fuzzy_rules$fuzzy_rule <- ""
  fuzzy_rules$confidence <- 0
  
  for (i in 1:nrow(rules_df)) {
    rule_text <- rules_df$rule[i]
    cell_type <- rules_df$cell_type[i]
    
    # Parse the rule to extract conditions
    # Simple parsing for demonstration - in practice this would be more sophisticated
    if (grepl("<=", rule_text)) {
      # Extract feature name and threshold
      parts <- strsplit(rule_text, " AND ")[[1]]
      fuzzy_conditions <- c()
      
      for (part in parts) {
        if (grepl("<=", part) && !grepl("NOT", part)) {
          # Positive condition
          tokens <- strsplit(part, " <= ")[[1]]
          if (length(tokens) == 2) {
            feature_name <- trimws(tokens[1])
            threshold <- as.numeric(tokens[2])
            
            if (feature_name %in% features && !is.na(threshold)) {
              # Calculate fuzzy membership
              feature_data <- data[[feature_name]]
              feature_median <- median(feature_data, na.rm = TRUE)
              
              if (threshold <= feature_median) {
                fuzzy_conditions <- c(fuzzy_conditions, paste("low", feature_name))
              } else {
                fuzzy_conditions <- c(fuzzy_conditions, paste("moderate", feature_name))
              }
            }
          }
        } else if (grepl("NOT", part)) {
          # Negative condition - convert to fuzzy
          tokens <- strsplit(gsub("NOT \\(|\\)", "", part), " <= ")[[1]]
          if (length(tokens) == 2) {
            feature_name <- trimws(tokens[1])
            threshold <- as.numeric(tokens[2])
            
            if (feature_name %in% features && !is.na(threshold)) {
              feature_data <- data[[feature_name]]
              feature_median <- median(feature_data, na.rm = TRUE)
              
              if (threshold <= feature_median) {
                fuzzy_conditions <- c(fuzzy_conditions, paste("high", feature_name))
              } else {
                fuzzy_conditions <- c(fuzzy_conditions, paste("moderate-to-high", feature_name))
              }
            }
          }
        }
      }
      
      # Create fuzzy rule
      if (length(fuzzy_conditions) > 0) {
        fuzzy_rules$fuzzy_rule[i] <- paste("IF", paste(fuzzy_conditions, collapse = " AND "), "THEN", cell_type)
      } else {
        fuzzy_rules$fuzzy_rule[i] <- paste("DEFAULT:", cell_type)
      }
      
      # Calculate confidence based on population purity
      target_count <- sum(data$cell_type == cell_type, na.rm = TRUE)
      total_count <- nrow(data)
      if (total_count > 0) {
        fuzzy_rules$confidence[i] <- target_count / total_count
      }
    } else {
      fuzzy_rules$fuzzy_rule[i] <- paste("SIMPLE:", cell_type)
      fuzzy_rules$confidence[i] <- 0.5
    }
  }
  
  return(fuzzy_rules)
}

# Main ALPODS function with proper methodology
alpods <- function(data, features, target_variable = NULL) {
  log_message("Starting ALPODS algorithm")
  
  # If no target variable specified, REQUIRE it - no fallback to K-means
  if (is.null(target_variable) && !"cell_type" %in% names(data)) {
    stop("ALPODS requires target cell types. Please provide 'cell_type' column or specify target_variable parameter.")
  }
  
  # If cell_type column doesn't exist, create it from target_variable
  if (!"cell_type" %in% names(data)) {
    if (!is.null(target_variable) && target_variable %in% names(data)) {
      data$cell_type <- data[[target_variable]]
      log_message(paste("Using", target_variable, "as target variable"))
    } else {
      stop(paste("Target variable", target_variable, "not found in data columns"))
    }
  }
  
  # Validate that we have multiple cell types for ALPODS
  unique_types <- unique(data$cell_type)
  if (length(unique_types) < 2) {
    stop("ALPODS requires at least 2 different cell types for classification")
  }
  
  log_message(paste("Found", length(unique_types), "cell types:", paste(unique_types, collapse = ", ")))
  
  log_message("Building ALPODS decision network (DAG)")
  
  # Build the ALPODS decision tree using Simpson Index and Bayesian splits
  root <- build_alpods_tree(data, features)
  
  log_message("Extracting rules with effect size analysis")
  
  # Extract rules with effect size calculations
  all_rules <- extract_rules_with_effect_size(root, data)
  
  log_message(paste("Found", nrow(all_rules), "potential subpopulations"))
  
  # Filter relevant populations using effect size and ABC analysis
  log_message("Filtering relevant subpopulations using ABC analysis")
  relevant_rules <- filter_relevant_populations(all_rules)
  
  log_message(paste("Selected", nrow(relevant_rules), "relevant subpopulations"))
  
  # Generate fuzzy rules for better interpretability
  log_message("Generating fuzzy rules for enhanced interpretability")
  fuzzy_rules <- generate_fuzzy_rules(relevant_rules, data, features)
  
  # Make predictions for all data points
  log_message("Making predictions for all data points")
  predictions <- predict_tree(root, data)
  
  # Create enhanced result dataframe with population information
  result <- data.frame(
    .ci = as.integer(data$.ci),
    cell_type = predictions$prediction,
    rule = predictions$rule
  )
  
  # Add population relevance information
  result$is_relevant <- result$cell_type %in% relevant_rules$cell_type
  
  # Add effect size information where available
  result$effect_size <- 0
  for (i in 1:nrow(relevant_rules)) {
    matching_indices <- result$cell_type == relevant_rules$cell_type[i]
    result$effect_size[matching_indices] <- relevant_rules$effect_size[i]
  }
  
  log_message("ALPODS analysis completed")
  
  return(list(
    predictions = result,
    population_rules = relevant_rules,
    fuzzy_rules = fuzzy_rules,
    all_populations = all_rules
  ))
}

# Prepare data for ALPODS
log_message("Preparing data for ALPODS")

# Get feature names (excluding metadata columns)
feature_names <- setdiff(names(wide_data), c(".ci", "logicle..event_id"))
log_message(paste("Using all", length(feature_names), "features"))

# Since ALPODS requires target cell types, create initial populations first
# This is temporary - in production, you would have predefined cell types
log_message("Creating initial cell type assignments (temporary for testing)")
if (!"cell_type" %in% names(wide_data)) {
  feature_data <- wide_data[, feature_names, drop = FALSE]
  feature_data <- feature_data[complete.cases(feature_data), ]
  
  if (nrow(feature_data) > 0) {
    k <- min(5, nrow(feature_data) - 1)
    if (k > 0) {
      set.seed(42)
      kmeans_result <- kmeans(feature_data, centers = k, iter.max = 100)
      wide_data$cell_type <- paste0("Initial_Pop_", kmeans_result$cluster)
      log_message(paste("Created", k, "initial populations using K-means"))
    } else {
      wide_data$cell_type <- "Single_Population"
    }
  } else {
    wide_data$cell_type <- "Single_Population"
  }
}

# Run ALPODS algorithm with the initial cell types
log_message("Running ALPODS algorithm")
alpods_result <- alpods(wide_data, feature_names)

# Extract main results
result_data <- alpods_result$predictions
population_rules <- alpods_result$population_rules

# Log population analysis results
log_message(paste("ALPODS identified", nrow(population_rules), "relevant subpopulations"))
if (nrow(population_rules) > 0) {
  log_message("Population summary:")
  for (i in 1:nrow(population_rules)) {
    log_message(paste(" -", population_rules$cell_type[i], 
                     "| Effect size:", round(population_rules$effect_size[i], 3),
                     "| Size:", population_rules$population_size[i]))
  }
}

# Prepare output (only essential columns for Tercen)
log_message("Preparing output")
output <- result_data %>%
  select(.ci, cell_type, rule)
# browser()
# Return result to Tercen
log_message("Returning result to Tercen")
output %>%
  ctx$addNamespace() %>%
  ctx$save()

log_message("ALPODS Operator completed successfully")