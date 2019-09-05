library(CORElearn)

# A ReliefF-based approach is used: the dataset is reduced to 8 feature, one for 
# each channel. For i = 1..8, the i-th attribute is the mean of 204 samples for 
# the i-th channel.
# Then ReliefF is applied on the reducted dataset.
filter_channels <- function(train_x, train_y, num_of_channels, samples_for_channel) {
  
  cat("Performing Channel Selection: the operation could require few seconds...\n")
  set.seed(134599771)
  
  reducted_train <- sapply(1:num_of_channels, function(i) {
    inf <- (i - 1) * samples_for_channel + 1
    sup <- i * samples_for_channel
    samples <- train_x[, inf:sup]
    return(apply(samples, 1, mean))
  })
  reducted_dataset <- as.data.frame(list(reducted_train, train_y))
  names(reducted_dataset) <- c("1","2","3","4","5","6","7","8","train_y")
  
  # ReliefFexpRank estimator is used to take into account conditional
  # dependencies among attributes
  # ReliefIterations=0 means that the iterations are datasize
  # 70 is the default value for kNearestExpRank
  feature_rank <- attrEval(train_y ~ ., reducted_dataset, 
                           estimator="ReliefFexpRank", kNearestExpRank=70,
                           ReliefIterations=0) 
  top_feature <- which(feature_rank >= 0)
  names(top_feature) <- NULL
  cat("Done\n")
  return(sort(top_feature))
}