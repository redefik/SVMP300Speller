library(LiblineaR)

# Trains a linear SVM with parameter C on the training set with instances train_x
# and labels train_y. Then, evaluates the model on test_x computing for each
# instance the decision value. Finally, for each iteration and for each row or
# column the average decision value is evaluated. The row and the column with
# the highest score identify the predicted character.
linear_SVM <- function(train_x, train_y, test_x, test_chars, test_stimuli,
                       c, rows_for_char, matrix_speller, verbose) {
  set.seed(12345)
  # type=1 means that the dual formulation is used.
  linear_model <- LiblineaR(data=train_x, target=train_y, type=1, cost=c,
                            bias=TRUE, verbose=FALSE)
  test_prediction <- predict(linear_model, test_x, decisionValues=TRUE)
  
  # Extract the predicted characters from the test_prediction object
  predicted_chars <- sapply(1:length(test_chars), function(i) {
    inf <- rows_for_char * (i - 1) + 1
    sup <- rows_for_char * i
    # Decision Values for the instances corresponding to the current character
    chr_dv <- test_prediction$decisionValues[inf:sup, ]
    # Stimuli type (rows or columns) for the instances corresponding to 
    # the current character
    chr_stimuli <- test_stimuli[inf:sup]
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
    return(matrix_speller[best_row, best_col])
  })
  
  correct_predictions <- length(Filter(isTRUE, predicted_chars == test_chars))
  accuracy <- round(correct_predictions/length(test_chars), 4)
  if (verbose) {
    cat("Observed characters")
    print(test_chars)
    cat("Predicted characters")
    print(predicted_chars)
    cat("Accuracy:", 100 * accuracy, "%\n")
  }
  
  return(accuracy)
}