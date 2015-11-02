# Script to multiply impute missing values in a test using a bootstrapping algorithm of AMELIA II
# December 19, 2012
# Matthew T. Boulanger
# University of Missouri Research Reactor

# Modifications by Jason D. Miller, hack-r.com / millerintllc.com

#Script uses AMELIA II to impute missing values in a test.
#Automatically creates a file entitled "imputed_data.csv" in your working directory
#Option added for user to set the number of iterations

#-------------------------------->
#Load required libraries
library("Amelia")
library("dplyr")

#-------------------------------->
#Begin user-defined variables
# Set working directory
setwd("")


myAmelia <- function (test, iterations = 10) {
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
  final.imputed.ppm <- aggregate(imputed.ppm[,-34],list(imputed.ppm$ANID), mean)
}

#Write imputed data to disk
write.csv(final.imputed.ppm, file="imputed_data.csv")
