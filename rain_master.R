# ------------------------------------------------------------------------
# File:        rain_master.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This is the main file for running scripts related to 
#               the Kaggle.com "How Much Did It Rain? II" competition
#               sponsored by the  Artificial Intelligence Committee of the 
#               American Meteorological Society. 
#               Climate Corporation is providing the prize pool.
# ------------------------------------------------------------------------

# Load functions, libraries, options --------------------------------------
source("code//rain_functions.R")

# Load data and perform ETL -----------------------------------------------
source("..//code//rain_data.R")

# Run analysis, modeling --------------------------------------------------
source("..//code//rain_analysis.R")

# Generate statistics for the report/paper --------------------------------
source("..//code//rain_report.R")