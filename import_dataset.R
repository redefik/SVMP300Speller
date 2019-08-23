library(data.table) # To speed-up file reading


# Return a data frame containing the available dataset
# The "X" file contains the EEG samples recorded by the 8 channels. 
# For each one 204 samples are available.
# Y[i] = +1 if X[i] was recorded after a target stimulus, Y[i] = -1 otherwise
# C[i] indicates the row or column intensified for each EEG response 
# (values between 1 and 6 represent rows, values between 7 and 12 represent columns)
import_dataset <- function(filename_X, filename_Y, filename_C) {
  x <- as.data.frame(fread(filename_X))
  y <- as.data.frame(fread(filename_Y))
  c <- as.data.frame(fread(filename_C))
  output <- cbind(x, y, c)
  output_names <- names(output)
  output_names[length(output_names)-1] <- "label"
  output_names[length(output_names)] <- "stimulus_type"
  names(output) <- output_names
  return(output)
}