# Trains a linear SVM with parameter c on the training set with instances train_x
# and labels train_y. Then, evaluates the model on test_x computing for each
# instance the decision value. Finally, it infers the predicted character 
# averaging on the iterations and compare the results with test_chars.
# Returns the fraction of correctly predicted characters.

library(LiblineaR)

linear_SVM <- function(train_x, train_y, test_x, test_chars, test_stimuli,
                       c, rows_for_char, matrix_speller, verbose) {
  set.seed(12345)
  linear_model <- LiblineaR(data=train_x, target=train_y, type=2, cost=c,
                            bias=TRUE, verbose=FALSE)
  test_prediction <- predict(linear_model, test_x, decisionValues=TRUE)
  
  predicted_chars <- sapply(1:length(test_chars), function(i) {
    inf <- rows_for_char * i - rows_for_char + 1
    sup <- rows_for_char * i
    # Decision Values for the instances corresponding to the current trial
    chr_dv <- test_prediction$decisionValues[inf:sup, ]
    # Stimuli type (rows or columns) for the instances corresponding to 
    # the current trial
    chr_stimuli <- test_stimuli[inf:sup]
    # Each row/column of the matrix obtains a score that is the average of the 
    # decision Values assigned to the instances corresponding to row/column
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
  
  if (verbose) {
    cat("Observed characters")
    print(test_chars)
    cat("Predicted characters")
    print(predicted_chars)
  }
  
  return(round(correct_predictions/length(test_chars), 4))
}