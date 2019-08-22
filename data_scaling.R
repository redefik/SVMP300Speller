# Standardize training and test set.
scale_split <- function(data_split){

  scaled_train_x <- scale(data_split$train_x)
  # Important: to avoid contamination between training and test set, 
  # standardization is performed using training set statistics on the test too.
  scaled_test_x <- scale(data_split$test_x,
                         attr(scaled_train_x, "scaled:center"),
                         attr(scaled_train_x, "scaled:scale"))
  
  output <- list(scaled_train_x, data_split$train_y, data_split$train_chars, scaled_test_x, data_split$test_chars)
  names(output) <- c("train_x", "train_y", "train_chars", "test_x", "test_chars")
  return(output)
}