# Returns:
# Training X
# Training Y
# Training Characters
# Training Stimuli Type (rows or columns)
# Test X
# Test Characters
# Test Stimuli Type (rows or columns)
split_training_test <- function(dataset, characters, rows_for_characters) {
  train_chars_size <- floor(0.7 * length(characters))
  train_chars <- characters[1:train_chars_size]
  test_chars <- characters[(train_chars_size + 1):length(characters)]
  
  train_dataset_size <- train_chars_size * rows_for_characters
  train_x <- dataset[1:train_dataset_size, 1:(ncol(dataset) - 2)]
  train_y <- dataset[1:train_dataset_size, ncol(dataset) - 1]
  train_rc <- dataset[1:train_dataset_size, ncol(dataset)]
  test_x <- dataset[(train_dataset_size + 1):nrow(dataset), 1:(ncol(dataset) - 2)]
  test_rc <- dataset[(train_dataset_size + 1):nrow(dataset), ncol(dataset)]
  
  output <- list(train_x, train_y, train_chars, train_rc, test_x, test_chars, test_rc)
  names(output) <- c("train_x", "train_y", "train_chars", "train_stimuli", 
                     "test_x", "test_chars", "test_stimuli")
  return(output)
}