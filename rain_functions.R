# ------------------------------------------------------------------------
# File:        rain_functions.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This loads libraries, functions, and options
# ------------------------------------------------------------------------

# Libraries ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(Amelia,caret, data.table, devtools, fitdistrplus, dplyr, hydroGOF,
               outliers, radar, rms, RRF, sqldf, MASS, clusterGeneration, lme4,
               fmsb) #raincpc, RDSTK
#install_bitbucket("mkuhn/parallelRandomForest", ref="parallelRandomForest")
#install_github("topepo/caret/pkg/caret")

# Options -----------------------------------------------------------------
#setwd("data")

mod     <- 4 # Model number to run
perfect <- T # Load only naturally non-missing training data
useRDS  <- T # T = use RDS saved objects to save time
useRAW  <- F # T = load "raw" data from .csv
useMP   <- T # Incorporate Marshall Palmer
predict <- F # Use prediction dataset (test.csv)
fullSet <- T # use FULL training sample (slow)
hpc     <- T # attempt to use HPC

# HPC Setup ---------------------------------------------------------------
if (hpc){
  p_load(doParallel, foreach, parallel, snow)
  cores_2_use <- detectCores() - 2
  cl          <- makePSOCKcluster(cores_2_use, useXDR = F)
  clusterSetRNGStream(cl, 9956)
  registerDoParallel(cl, cores_2_use)
}

# Functions ---------------------------------------------------------------
myAmelia <- function (test, iterations = 10) {
  # Matthew T. Boulanger
  # University of Missouri Research Reactor

  # Modifications by Jason D. Miller, hack-r.com / millerintllc.com

  #Set no. of iterations
  iter = iterations

  #End user-defined variables
  #-------------------------------->

  rn <- paste( "ANID", seq(1:nrow(test)), sep = " ")
  row.names(test) <- rn

  #Create Vector of ANIDs
  anid.list <- c(rep(row.names(test), iter))

  #Log transform data
  #data.log <- log10(test)

  #Impute missing values
  imputed <- amelia(test,p2s =2, m = iter, parallel = "snow", cl = parallel::makePSOCKcluster(20), ncpus = 20, idvars = c("Id")) #cl = cl,

  #Write each imputed test to files
  write.amelia(obj=imputed, file.stem = "imputed_")

  #Create list of the imputed files
  imputed.files = list.files(pattern = 'imputed_*')

  #Read in all imputed files
  imputed.list = lapply(imputed.files, read.csv, header=TRUE, row.names = "X")

  #Compile all imputed data into one big data frame
  imputed.ppm <- ldply(imputed.list, data.frame, row.names=NULL)

  #Set variable y, which is the base-10 log concentration of each specimen
  #y = big.list

  #Use variable y to convert data back to ppm
  #imputed.ppm <- 10^y

  #Convert imputed data to data frame
  #imputed.ppm <- data.frame(imputed.ppm, row.names=NULL)

  #Insert ANIDs into imputed data data frame
  imputed.ppm$ANID <- anid.list

  #Calculate mean of all imputations based on ANID
  final.imputed.ppm <- aggregate(imputed.ppm[,!(colnames(imputed.ppm) == "ANID")],list(imputed.ppm$ANID), mean)

  return(final.imputed.ppm)
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
    valid_time[2:length(minutes_past)] = diff(minutes_past)
    valid_time[length(valid_time)] = valid_time[length(valid_time)] + 60 - sum(valid_time)
  } else {
    # if only 1 observation, make it valid for the entire hour
    valid_time <- 60
  }

  valid_time = valid_time / 60

  # calculate hourly rain rates using marshall-palmer weighted by valid times
  sum = sum(((10^(ref/10))/200) ^ 0.625 * valid_time, na.rm = T)

  return(sum)

}

estBetaParams <- function(mu, var) {
  # from https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


vif_func<-function(in_frame,thresh=10,trace=T,...){

  require(fmsb)

  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)

  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))

  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{

    in_dat<-in_frame

    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){

      vif_vals<-NULL
      var_names <- names(in_dat)

      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]

      vif_max<-as.numeric(vif_vals[max_row,2])

      if(vif_max<thresh) break

      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }

      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]

    }

    return(names(in_dat))

  }

}
