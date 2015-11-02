# ------------------------------------------------------------------------
# File:        rain_functions.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This loads libraries, functions, and options
# ------------------------------------------------------------------------

# Options -----------------------------------------------------------------
setwd("data")

mod     <- 4 # Model number to run
perfect <- T # Load only naturally non-missing training data
useRDS  <- T # T = use RDS saved objects to save time
useRAW  <- F # T = load "raw" data from .csv
useMP   <- T # Incorporate Marshall Palmer
predict <- F # Use prediction dataset (test.csv)
fullSet <- T # use FULL training sample (slow)

# Libraries ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, data.table, devtools, dplyr, hydroGOF, 
               outliers, radar, RRF, sqldf) #raincpc, RDSTK

# Functions ---------------------------------------------------------------
myAmelia <- function (test, iterations = 10) {
  # Matthew T. Boulanger
  # University of Missouri Research Reactor
  
  # Modifications by Jason D. Miller, hack-r.com / millerintllc.com
  
  #Set no. of iterations
  iter = iterations
  
  #End user-defined variables
  #-------------------------------->
  
  #Read in data
  data.ppm <- read.table(test, sep=",", header=TRUE, na.strings="NA", row.names="ANID")
  
  #Create Vector of ANIDs
  anid.list <- c(rep(row.names(data.ppm), iter))
  
  #Log transform data
  data.log <- log10(data.ppm)
  
  #Impute missing values
  imputed.log <- amelia(data.log, m = iter)
  
  #Write each imputed test to files
  write.amelia(obj=imputed.log, file.stem = "imputed_")
  
  #Create list of the imputed files
  imputed.files = list.files(pattern = 'imputed_*')
  
  #Read in all imputed files
  imputed.list = lapply(imputed.files, read.csv, header=TRUE, row.names = "X")
  
  #Compile all imputed data into one big data frame
  big.list <- ldply(imputed.list, data.frame)
  
  #Set variable y, which is the base-10 log concentration of each specimen
  y = big.list
  
  #Use variable y to convert data back to ppm
  imputed.ppm <- 10^y
  
  #Convert imputed data to data frame
  imputed.ppm <- data.frame(imputed.ppm, row.names=NULL)
  
  #Insert ANIDs into imputed data data frame
  imputed.ppm$ANID <- anid.list
  
  #Calculate mean of all imputations based on ANID
  final.imputed.ppm <- aggregate(imputed.ppm[,!(colnames(imputed.ppm) == "ANID")],list(imputed.ppm$ANID), mean)
  
  return(final.imputed.ppm)
}

mpalm  <- function(ref, minutes_past) {
  #Credit to Troy Walters for this
  # small edits by Jason Miller, hack-r.com
  
  # order reflectivity values and minutes_past
  sort_min_index = order(minutes_past)
  minutes_past <- minutes_past[sort_min_index]
  ref <- ref[sort_min_index]
  
  # calculate the length of time for which each reflectivity value is valid
  valid_time <- rep(0, length(minutes_past))
  valid_time[1] <- minutes_past[1]
  if (length(valid_time) > 1) {
    for (i in seq(2, length(minutes_past))) {
      valid_time[i] <- minutes_past[i] - minutes_past[i-1]
    }
    valid_time[length(valid_time)] = valid_time[length(valid_time)] + 60 - sum(valid_time)
  } else {
    # if only 1 observation, make it valid for the entire hour
    valid_time <- 60
  }
  
  valid_time = valid_time / 60
  
  # calculate hourly rain rates using marshall-palmer weighted by valid times
  sum <- 0
  for (i in seq(length(ref))) {
    if (!is.na(ref[i])) {
      mmperhr <- ((10^(ref[i]/10))/200) ^ 0.625
      sum <- sum + mmperhr * valid_time[i]
    }
  }
  
  return(sum)
  
}
mpalmer <- function(ref, minutes_past) {
  
  # order reflectivity values and minutes_past
  sort_min_index = order(minutes_past)
  minutes_past <- minutes_past[sort_min_index]
  ref <- ref[sort_min_index]
  
  # calculate the length of time for which each reflectivity value is valid
  valid_time <- rep(0, length(minutes_past))
  valid_time[1] <- minutes_past[1]
  if (length(valid_time) > 1) {
    for (i in seq(2, length(minutes_past))) {
      valid_time[i] <- minutes_past[i] - minutes_past[i-1]
    }
    valid_time[length(valid_time)] = valid_time[length(valid_time)] + 60 - sum(valid_time)
  } else {
    # if only 1 observation, make it valid for the entire hour
    valid_time <- 60
  }
  
  valid_time = valid_time / 60
  
  # calculate hourly rain rates using marshall-palmer weighted by valid times
  sum <- 0
  for (i in seq(length(ref))) {
    if (!is.na(ref[i])) {
      mmperhr <- ((10^(ref[i]/10))/200) ^ 0.625
      sum <- sum + mmperhr * valid_time[i]
    }
  }
  
  return(sum)
  
}

#  results <- test %>% group_by(Id) %>% summarize(Expected=sum)
