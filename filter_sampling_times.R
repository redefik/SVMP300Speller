library(CORElearn)

# A ReliefF-based approach is used: for each feature a score is computed using
# ReliefF. Then, for each sampling time (from 1 to 204) the average score among
# channels is evaluated. Finally, sampling times with negative scores are 
# returned.
filter_sampling_times <- function(train_x, train_y, samples_for_channel, 
                                  num_of_channels) {
  
  set.seed(432167)
  
  dataset <- as.data.frame(list(train_x, train_y))
  dataset_names <- names(dataset)
  dataset_names[length(dataset_names)] <- "train_y"
  names(dataset) <- dataset_names
  
  feature_scores <- attrEval(train_y ~ ., dataset, estimator="ReliefFexpRank",
                           kNearestExpRank=70, ReliefIterations=0)
  
  sampling_time_scores <- sapply(1:samples_for_channel, function(i) {
    i_positions <- seq(i, num_of_channels * samples_for_channel,
                       samples_for_channel)
    return(mean(feature_scores[i_positions]))
  })
  return(which(sampling_time_scores < 0))
}