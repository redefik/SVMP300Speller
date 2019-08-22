#########################################################################################################
# A dataset with complete record of P300 evoked potentials is given.                                    #
# An user with amyotrphic lateral sclerosis (ALS) focused on one out of 36 different characters.        #
# Objective: to predict the correct character in each of the provided character selection steps (trial).#
#########################################################################################################

source("import_dataset.R")

dataset <- import_dataset("X.txt", "Y.txt", "C.txt")

