# ------------------------------------------------------------------------
# File:        rain_report.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script runs the best model on the unlabeled test data
# ------------------------------------------------------------------------

# Load --------------------------------------------------------------------
test <- readRDS("test.RDS")
id   <- test$Id

# Imputation --------------------------------------------------------------
#final.imputed.ppm <- MyAmelia(test = test, iterations = 3)
test <- na.roughfix(test)


# Add MP ------------------------------------------------------------------
test$mp <- mpalmer(ref = test$Ref, minutes_past = test$minutes_past)

# Predict Outlier Prob ----------------------------------------------------
test.mad.scores            <- scores(test,  type = "mad", prob = 1)
test.mad.scores$Id         <- NULL
test.mad.scores$Id_mad     <- NULL
colnames(test.mad.scores)  <- paste(colnames(test.mad.scores), "_mad", sep = "")
test             <- cbind(test, test.mad.scores)
rm(test.mad.scores)

# Run prediction ----------------------------------------------------------
predicted  <- predict(GRF,  test)

# Write out results -------------------------------------------------------
test$Prediction <- Prediction
res <- sqldf("select Id, max(minutes_past) as max_min, Prediction as Expected from test group by Id")
res$max_min <- NULL
write.csv(res, "id_forecast.csv")
