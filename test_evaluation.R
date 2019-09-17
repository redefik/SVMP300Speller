library(data.table)

# Evaluates the SVM linear model on a test set consisting in three files: 
# an X file, an Y file and a C file. Before evaluating the model, the test set
# is standardized using the statistics of the training set (the last two arguments)
evaluate_test <- function(filename_X, filename_Y, filename_C, linear_model, 
                          speller, training_center, training_scale, rows_for_char) {
  # Import test file
  x <- as.data.frame(fread(filename_X))
  y <- as.data.frame(fread(filename_Y))
  c <- as.data.frame(fread(filename_C))
  # Test data binding
  test_set <- cbind(x,y,c)
  test_set_names <- names(test_set)
  test_set_names[length(test_set_names)-1] <- "label"
  test_set_names[length(test_set_names)] <- "stimulus_type"
  names(test_set) <- test_set_names
  
  num_of_characters <- nrow(test_set) / rows_for_char
  
  # Extract test characters
  test_chars <- sapply(1:num_of_characters, function(i) {
    inf <- rows_for_char * (i - 1) + 1
    sup <- rows_for_char * i
    # Select instances of Y corresponding to the char
    char_labels <- test_set$label[inf:sup]
    target_labels_positions <- which(char_labels == 1)
    # Select row and column corresponding to y = +1
    target_row_column <- unique(test_set$stimulus_type[target_labels_positions])
    row_index <- target_row_column[1]
    col_index <- target_row_column[2] - 6
    return(speller[row_index, col_index])
  })
  # Standardize test set
  scaled_test_set <- as.data.frame(scale(test_set[,1:1632], training_center, training_scale))
  
  test_prediction <- predict(linear_model, scaled_test_set, decisionValues=TRUE)
  
  # Extract the predicted characters from the test_prediction object
  predicted_chars <- sapply(1:length(test_chars), function(i) {
    inf <- rows_for_char * (i - 1) + 1
    sup <- rows_for_char * i
    # Decision Values for the instances corresponding to the current character
    chr_dv <- test_prediction$decisionValues[inf:sup, ]
    # Stimuli type (rows or columns) for the instances corresponding to 
    # the current character
    chr_stimuli <- test_set$stimulus_type[inf:sup]
    # Each row/column of the matrix obtains a score that is the average of the 
    # decision values assigned to the instances corresponding to row/column
    # intensification
    row_scores <- sapply(1:6, function(i) {
      row_flash_pos <- which(chr_stimuli == i)
      return(mean(chr_dv[row_flash_pos]))
    })
    col_scores <- sapply(7:12, function(j) {
      col_flash_pos <- which(chr_stimuli == j)
      return(mean(chr_dv[col_flash_pos]))
    })
    # Best row and best column give the predicted character
    best_row <- which.max(row_scores)
    best_col <- which.max(col_scores)
    return(speller[best_row, best_col])
  })
  
  # Printing results
  correct_predictions <- length(Filter(isTRUE, predicted_chars == test_chars))
  accuracy <- round(correct_predictions/length(test_chars), 4)
  cat("Observed characters")
  print(test_chars)
  cat("Predicted characters")
  print(predicted_chars)
  cat("Accuracy:", 100 * accuracy, "%\n")
  
}