# Apply k-means with k=6 to cluster the provided EEG signals. For each word
# the assignments of the corresponding signals are plotted.
cluster_signals <- function(signals, num_of_words, rows_for_char, chars_for_word) {
  cat("Clustering EEG signals: the operation could require few minutes...\n")
  set.seed(20)
  kmeans_output <- kmeans(scale(signals), centers=num_of_words, nstart=20, iter.max=20)
  for (i in 1:num_of_words) {
    inf <- rows_for_char * chars_for_word * (i - 1) + 1
    sup <- rows_for_char * chars_for_word * i
    x_label <- sprintf("Word %d signals", i)
    y_label <- "clusters"
    plot(inf:sup, kmeans_output$cluster[inf:sup], xlab=x_label, ylab=y_label)
    title("Word vs Corresponding Signals Assignments")
  }
  cat("Done\n")
}