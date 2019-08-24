source("linear_SVM.R")

# Tests the C parameters in the provided vector performing a k-fold cross-valudation
# Returns a list, containing the accuracies obtained with a linear SVM using
# each value of C.
cross_validation <- function(c_vector, data_split, rows_for_char, speller) {
  set.seed(123456)
  folds <- 5
  # Generate a vector that indicates for each character the iteration into which
  # it will be a test one.
  when_test <- sample(rep(1:folds, length.out = length(data_split$train_chars)))
  
  pb <- txtProgressBar(min=0, max=length(c_vector), style=3)
  
  # output will contain for each value of c the accuracy obtained by the
  # cross-validation
  output <- lapply(c_vector, function(c) {
    # accuracies[i] contains the accuracy obtained at i-th iteration
    accuracies <- sapply(1:folds, function(k) {
       test_chars_positions <- which(when_test == k)
       train_chars_positions <- which(when_test != k)
       test_chars <- data_split$train_chars[test_chars_positions]
       union <- cbind(data_split$train_x, data_split$train_y, 
                      data_split$train_stimuli)
       test_blocks <- lapply(test_chars_positions, function(i) {
         inf <- rows_for_char * i - rows_for_char + 1
         sup <- rows_for_char * i
         return(union[inf:sup,])
       })
       test_set <- do.call("rbind", test_blocks)
       test_x <- test_set[,1:(ncol(test_set)-2)]
       test_stimuli <- test_set[,ncol(test_set)]
       train_blocks <- lapply(train_chars_positions, function(i) {
         inf <- rows_for_char * i - rows_for_char + 1
         sup <- rows_for_char * i
         return(union[inf:sup,])         
       })
       training_set <- do.call("rbind", train_blocks)
       train_x <- training_set[,1:(ncol(training_set)-2)]
       train_y <- training_set[,(ncol(training_set)-1)]
       
       acc <- linear_SVM(train_x, train_y, test_x, test_chars, test_stimuli, c, 
                         rows_for_char, speller, verbose=FALSE)
       return(acc)
    })
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
    return(mean(accuracies))
  })
  names(output) <- c_vector
  
  close(pb)
  
  return(output)
}
