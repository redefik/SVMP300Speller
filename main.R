################################################################################
# A dataset with complete record of P300 evoked potentials is given.           #                          
# An user with amyotrphic lateral sclerosis (ALS) focused on one out of        #
# 36 different characters.                                                     #
# Objective: to predict the correct character in each of the provided character#
# selection steps (trials).                                                    #
################################################################################

# Authors:

source("import_dataset.R")
source("data_understanding.R")
source("data_shuffling.R")
source("data_splitting.R")
source("channel_selection.R")
source("filter_sampling_times.R")
source("cross_validation.R")
source("linear_SVM.R")

dataset <- import_dataset("X.txt", "Y.txt", "C.txt")

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

# Select the most relevant channels trying to reduce the dimensionality of the
# problem
top_channels <- filter_channels(data_split$train_x, data_split$train_y, 
                                num_of_channels, samples_for_channel)
# With this approach it seems that no channels can be considered irrelevant

# Let's see another approach: evaluating not channels but sampling times.
bad_sampling_times <- filter_sampling_times(data_split$train_x, 
                                            data_split$train_y, 
                                            samples_for_channel, 
                                            num_of_channels)
# With this approach it seems that no sampling times can be considered 
# irrelevant

# Disclaimer: the feature analysis has been made with different seed values.

# Standardize training set
scaled_train <- scale(data_split$train_x)
train_center <- attr(scaled_train, "scaled:center")
train_scale <- attr(scaled_train, "scaled:scale")
data_split$train_x <- as.data.frame(scaled_train)

# Since we use a linear SVM we have to choose the value of the C parameter
# Tested C are sampled from the set: 2^-5, 2^-3, 2^-1, ..., 2^15.
c_vector <- 2^seq(-5, 15, 2)
c_accuracies <- cross_validation(c_vector, data_split, rows_for_char, 
                                 data_summary$Speller)

# For example, for C=0.03125 we obtain 90% accuracy, so a possible configuration
# is: Linear SVM with C=0.03125

# First, we have to scale test set using training set statistics
data_split$test_x <- as.data.frame(scale(data_split$test_x, train_center,
                                         train_scale))
# Then, we invoke linear_SVM to evaluate the model on the test set
test_accuracy <- linear_SVM(data_split$train_x, data_split$train_y, 
                            data_split$test_x, data_split$test_chars,
                            data_split$test_stimuli, 0.03125, rows_for_char,
                            data_summary$Speller, verbose=TRUE)