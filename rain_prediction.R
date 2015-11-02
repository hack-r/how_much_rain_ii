# ------------------------------------------------------------------------
# File:        rain_report.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script runs the best model on the unlabeled test data
# ------------------------------------------------------------------------


# Predict Outlier Prob ----------------------------------------------------


# Imputation --------------------------------------------------------------
test <- myAmelia(test, iterations = 10)

# Break apart test data into manageable pieces ----------------------------
# Make sure all obs for the same ID are in the same object
test <- test[order(test$Id),]
n    <- 1

for(i in (length(unique(test$Id))/5)){
 test$subfile <- n
 n <- n + 1
}

test.1 <- test[test$subfile == 1, ]
test.2 <- test[test$subfile == 2, ]
test.3 <- test[test$subfile == 3, ]
test.4 <- test[test$subfile == 4, ]
test.5 <- test[test$subfile == 5, ]