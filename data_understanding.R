# Return an object that reports some information about the given dataset
data_understanding <- function(dataset) {
  
  # Information about rows
  instances <- nrow(dataset)
  num_of_trials <- 6
  chars_for_trial <- 5
  iterations_for_trial <- 10
  
  # Information about columns
  features <- ncol(dataset) - 2
  num_of_channels <- 8
  channels <- c("Fz","Cz","Pz","Oz","P3","P4","PO7","PO8")
  samples_for_channels <- 204
  
  # Search for duplicated
  is_dup <- duplicated(dataset)
  num_of_duplicated <- length(Filter(isTRUE, is_dup))
  
  # Search for missing values
  count_missing <- function(values) {
    is_missing <- !complete.cases(values)
    return(length(Filter(isTRUE, is_missing)))
  }
  
  column_missing_values <- sapply(dataset, count_missing)
  missing_values <- sum(column_missing_values)
  
  # Search for outliers
  detect_outliers <- function(values) {
    bp <- boxplot(values, plot=F)
    outliers <- bp$out
    outliers_fraction <- round(length(outliers)/length(values), 4)
    return(list(outliers, outliers_fraction))
  }
  
  features_outliers <- lapply(dataset[,1:(ncol(dataset)-2)], detect_outliers)
  
  # Speller used to communicate
  r1 <- c("A", "B", "C", "D", "E", "F")
  r2 <- c("G", "H", "I", "J", "K", "L")
  r3 <- c("M", "N", "O", "P", "Q", "R")
  r4 <- c("S", "T", "U", "V", "W", "X")
  r5 <- c("Y", "Z", "1", "2", "3", "4")
  r6 <- c("5", "6", "7", "8", "9", "_")
  speller <- rbind(r1, r2, r3, r4, r5, r6)
  rownames(speller) <- NULL
  
  # Extract trials
  trials <- sapply(1:(num_of_trials * chars_for_trial), function(i) {
    flash_for_trial <- 12 * iterations_for_trial
    inf <- flash_for_trial * i - flash_for_trial + 1
    sup <- flash_for_trial * i
    # Select instances of Y corresponding to the trial
    trial_labels <- dataset$label[inf:sup]
    target_labels_positions <- which(trial_labels == 1)
    # Select row and column corresponding to y = +1
    target_row_column <- unique(dataset$stimulus_type[target_labels_positions])
    row_index <- target_row_column[1]
    col_index <- target_row_column[2] - 6
    return(speller[row_index, col_index])
  })
  
  output <- list(instances, num_of_trials, chars_for_trial, iterations_for_trial,
                 features, num_of_channels, channels, samples_for_channels,
                 num_of_duplicated, missing_values, features_outliers, speller,
                 trials)
  names(output) <- c("Instances", "Number of Trials", "Characters for Trial",
                     "Iterations for Trial", "Number of Features",
                     "Number of Channels", "Channels", "Samples for Channel",
                     "Number of Duplicated", "Missing Values",
                     "Outliers for each feature", "Speller", "Trials")
  return(output)
}