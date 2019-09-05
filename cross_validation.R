library(parallel)

source("linear_SVM.R")

# Performs a multicore cross-validation. i represents the number of iterations
multi_cross_validation <- function(c_vector, data_split, rows_for_char, speller, i) {
  
  cat("Performing Cross-Validation: the operation could require few minutes...\n")
  
  if (i <= 1) {
    return(cross_validation(c_vector, data_split, rows_for_char, speller))
  }
  
  # Configure the cluster for the parallel computation.
  # The multi-core approach allows to reduce execution time.
  num_of_cores <- detectCores() - 1
  cluster <- makeCluster(num_of_cores)
  # Exporting needed variables and library (Windows only)
  clusterExport(cluster, c("c_vector", "data_split", "rows_for_char", "speller",
                           "cross_validation"), envir=environment())
  clusterEvalQ(cluster, library(LiblineaR))
  clusterEvalQ(cluster, source("linear_SVM.R"))
  # "L'Ecuyer-CMRG" RNG is set to avoid random sequence overlapping among workers.
  clusterSetRNGStream(cluster, 123456)
  
  partial_accuracies <- parLapply(cluster, 1:i, function(j) {
    return(cross_validation(c_vector, data_split, rows_for_char, speller))
  })
  
  # Release resources allocated for parallel computation
  stopCluster(cluster)
  
  output <- lapply(1:length(c_vector), function(c) {
    return(mean(sapply(partial_accuracies, function(l){return(l[[c]])})))
  })
  names(output) <- c_vector
  
  cat("Done\n")
  
  return(output)
}

# Tests the C parameters in the provided vector performing a k-fold cross-validation
# Returns a list, containing the accuracies obtained with a linear SVM using
# each value of C.
cross_validation <- function(c_vector, data_split, rows_for_char, speller) {
  folds <- 5
  # Generate a vector that indicates for each character the iteration into which
  # it will be a test one.
  when_test <- sample(rep(1:folds, length.out = length(data_split$train_chars)))
  
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
       # Extract from the training set the istances that will be the test set
       # during this iteration
       test_blocks <- lapply(test_chars_positions, function(i) {
         inf <- rows_for_char * (i - 1) + 1
         sup <- rows_for_char * i
         return(union[inf:sup,])
       })
       test_set <- do.call("rbind", test_blocks)
       test_x <- test_set[,1:(ncol(test_set)-2)]
       test_stimuli <- test_set[,ncol(test_set)]
       # Extract from the training set the istances that will be the training set
       # during this iteration
       train_blocks <- lapply(train_chars_positions, function(i) {
         inf <- rows_for_char * (i - 1) + 1
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
    return(mean(accuracies))
  })
  names(output) <- c_vector
  
  return(output)
}
