# Return an object that reports some information about the given dataset
data_understanding <- function(dataset) {
  
    cat("Data Understanding...\n")
    pb <- txtProgressBar(min=0, max=100, style=3)
    
    # Information about rows
    instances <- nrow(dataset)
    num_of_words <- 6
    chars_for_word <- 5
    iterations_for_char <- 10
    rows_for_char <- 12 * iterations_for_char
    
    # Information about columns
    features <- ncol(dataset) - 2
    num_of_channels <- 8
    channels <- c("Fz","Cz","Pz","Oz","P3","P4","PO7","PO8")
    samples_for_channels <- 204
    
    # Search for duplicated
    is_dup <- duplicated(dataset)
    num_of_duplicated <- length(Filter(isTRUE, is_dup))
    
    setTxtProgressBar(pb, 25)
    
    # Search for missing values
    count_missing <- function(values) {
      is_missing <- !complete.cases(values)
      return(length(Filter(isTRUE, is_missing)))
    }
    column_missing_values <- sapply(dataset, count_missing)
    missing_values <- sum(column_missing_values)
    
    setTxtProgressBar(pb, 50)
    
    # Search for outliers
    detect_outliers <- function(values) {
      bp <- boxplot(values, plot=F)
      outliers <- bp$out
      outliers_fraction <- round(length(outliers)/length(values), 4)
      return(list(outliers, outliers_fraction))
    }
    features_outliers <- lapply(dataset[,1:(ncol(dataset)-2)], detect_outliers)
    
    setTxtProgressBar(pb, 75)
    
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
    characters <- sapply(1:(num_of_words * chars_for_word), function(i) {
      inf <- rows_for_char * (i - 1) + 1
      sup <- rows_for_char * i
      # Select instances of Y corresponding to the char
      char_labels <- dataset$label[inf:sup]
      target_labels_positions <- which(char_labels == 1)
      # Select row and column corresponding to y = +1
      target_row_column <- unique(dataset$stimulus_type[target_labels_positions])
      row_index <- target_row_column[1]
      col_index <- target_row_column[2] - 6
      return(speller[row_index, col_index])
    })
    
    output <- list(instances, num_of_words, chars_for_word, iterations_for_char,
                   rows_for_char, features, num_of_channels, channels,
                   samples_for_channels, num_of_duplicated, missing_values,
                   features_outliers, speller, characters)
    names(output) <- c("Instances", "Number of Words", "Characters for Word",
                       "Iterations for Character", "Rows for Character", "Number of Features",
                       "Number of Channels", "Channels", "Samples for Channel",
                       "Number of Duplicated", "Missing Values",
                       "Outliers for each feature", "Speller", "Characters")
    
    setTxtProgressBar(pb, 100)
    close(pb)
    
    cat("Done\n")
    
    return(output)
}