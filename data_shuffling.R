# Scrambles the spelled characters and shuffles the dataset accordingly.
data_shuffling <- function(dataset, characters, rows_for_character) {
  set.seed(123456789)
  chars_permutation <- sample(1:length(characters), length(characters))
  scrambled_blocks <- lapply(chars_permutation, function(i) {
    inf <- (i - 1) * rows_for_character + 1
    sup <- i * rows_for_character
    return(dataset[inf:sup,])
  })
  shuffled_dataset <- do.call("rbind", scrambled_blocks)
  shuffled_characters <- characters[chars_permutation]
  output <- list(shuffled_dataset, shuffled_characters)
  names(output) <- c("instances", "characters")
  return(output)
}