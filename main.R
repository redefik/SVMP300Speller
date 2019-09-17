################################################################################
# A dataset with complete record of P300 evoked potentials is given.           #                          
# An user with amyotrphic lateral sclerosis (ALS) focused on one out of        #
# 36 different characters.                                                     #
# Objective: to predict the correct character in each of the provided character#
# selection steps (trials).                                                    #
################################################################################

# Authors: Federico Viglietta and Tommaso Villa

source("import_dataset.R")
source("data_understanding.R")
source("data_shuffling.R")
source("data_splitting.R")
source("channel_selection.R")
source("filter_sampling_times.R")
source("cross_validation.R")
source("linear_SVM.R")
source("signal_clustering.R")
source("test_evaluation.R")
library(LiblineaR)

# Importing dataset
# The three arguments are the training files
dataset <- import_dataset("X.txt", "Y.txt", "C.txt")

# Data Understanding
data_summary <- data_understanding(dataset)

characters <- data_summary$Characters
iterations_for_char <- data_summary$`Iterations for Character`
rows_for_char <- data_summary$`Rows for Character`
samples_for_channel <- data_summary$`Samples for Channel`
num_of_channels <- data_summary$`Number of Channels`

# Data Exploration results in no missing values and no duplicated instances.
# Instead, few outliers have been detected. Since we are not able to attest 
# whether these values are anomalous or not, we don't replace them.
# Furthermore, characters have not been scrambled, i.e. we have first the
# chars of the first word, then the chars of the second one...and so on.

# To produce an unbiased model, we scramble the spelled characters and shuffle
# the dataset accordingly
shuffled_data <- data_shuffling(dataset, characters, rows_for_char)

# Generate Training Set and Test Set
data_split <- split_training_test(shuffled_data$instances, 
                                  shuffled_data$characters, rows_for_char)

# Feature Selection

# A first attempt of feature extraction is made trying to select the most
# relevant channels
top_channels <- filter_channels(data_split$train_x, data_split$train_y, 
                                num_of_channels, samples_for_channel)
# With the used approach it seems that no channels can be considered irrelevant

# A second attempt of feature extraction is made trying to select the most
# relevant sampling times
bad_sampling_times <- filter_sampling_times(data_split$train_x,
                                            data_split$train_y,
                                            samples_for_channel,
                                            num_of_channels)
# With the used approach it seems that no sampling times can be considered 
# irrelevant

# Disclaimer: the feature analysis has been made with different seed values and
#             always returned the same results.

# To check if Ensemble Learning could be a viable approach, we find 6 clusters
# (i.e. the number of provided words) for the available instances
cluster_signals(dataset[,1:(ncol(dataset)-2)], data_summary$`Number of Words`,
                rows_for_char, data_summary$`Characters for Word`)

# It seems that signals corresponding to the same run are not homogeneus.
# Therefore, we abandon the idea to follow an Ensemble Learning approach.

# Standardize training set
scaled_train <- scale(data_split$train_x)
train_center <- attr(scaled_train, "scaled:center")
train_scale <- attr(scaled_train, "scaled:scale")
data_split$train_x <- as.data.frame(scaled_train)

# Cross-validation is used to tune the C parameter of SVM. The process is
# repeated with different training-validation splits in order to perform a 
# robust tuning choice.
c_vector <- 10^seq(-6, 6, 1)
c_accuracies <- multi_cross_validation(c_vector, data_split, rows_for_char, 
                                 data_summary$Speller, 5)

# In light of cross-validation's results, best choice for C seems to be: 10^-3

# Before being evaluated, test set is scaled using training set statistics.
data_split$test_x <- as.data.frame(scale(data_split$test_x, train_center,
                                         train_scale))
# Evaluate the model on the test set.
test_accuracy <- linear_SVM(data_split$train_x, data_split$train_y, 
                            data_split$test_x, data_split$test_chars,
                            data_split$test_stimuli, 10^-3, rows_for_char,
                            data_summary$Speller, verbose=TRUE)

# Eventually, the final model is trained on the entire dataset

# Standardize dataset
scaled_dataset <- scale(shuffled_data$instances[,1:(ncol(dataset)-2)])
dataset_center <- attr(scaled_dataset, "scaled:center")
dataset_scale <- attr(scaled_dataset, "scaled:scale")

# Disclaimer: use the above statistics to scale test set
final_model <- LiblineaR(data=scaled_dataset, 
                         target=shuffled_data$instances[,(ncol(dataset)-1)],
                         type=1, cost=10^-3, bias=TRUE, verbose=FALSE)

# Evaluate test data
# The first tree arguments will be test files
evaluate_test("X.txt", "Y.txt", "C.txt", final_model, data_summary$Speller, 
              dataset_center, dataset_scale, rows_for_char)