################################################################################
# A dataset with complete record of P300 evoked potentials is given.           #                          
# An user with amyotrphic lateral sclerosis (ALS) focused on one out of        #
# 36 different characters.                                                     #
# Objective: to predict the correct character in each of the provided character#
# selection steps (trial).                                                     #
################################################################################

source("import_dataset.R")
source("data_understanding.R")

dataset <- import_dataset("X.txt", "Y.txt", "C.txt")

# Data Exploration. data_summary object contains the highlights of the available
# dataset.
data_summary <- data_understanding(dataset)

# Data Exploration results in no missing values and no duplicated instances.
# Instead, few outliers are detected. Since we are not able to attest the anomaly
# of these values, we avoid to replace them.
