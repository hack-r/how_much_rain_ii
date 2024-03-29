# ------------------------------------------------------------------------
# File:        rain_analysis.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script performs analysis, model building and testing
# ------------------------------------------------------------------------


# HPC RF ------------------------------------------------------------------
if(hpc){
  source("..//code//rain_mod4_hpc.R")
}

# RF ----------------------------------------------------------------------
if(!(hpc) & mod == 0){
  RF    <- RRF(x, y, flagReg = 0, ntree = 50, mtry = (ncol(x)),
               keep.forest=T, ytest=y1, xtest=x1,#strata = x$,
               corr.bias = F, do.trace = T)
  # In Sample CV
  pred0  <- predict(RF,  x1)
  summary(round((pred0)))
  summary(round((y1)))
  mae(pred0, y1)

  # Out of Sample test
  pred_oos   <- predict(RF, train.oos)
  summary(round((pred_oos)))
  summary(round((train.oos$Expected)))
  res <- mae(pred_oos, train.oos$Expected)
  cat(res)
  saveRDS(res, "mod0_res.RDS")
}

# RRF ---------------------------------------------------------------------
if(!(hpc) & !(hpc) & mod == 1){
  RF    <- RRF(x, y, flagReg = 1, ntree = 50, mtry = (ncol(x)),
               keep.forest=T, ytest=y1, xtest=x1,#strata = x$,
               corr.bias = F, do.trace = T)
  # In Sample CV
  pred0  <- predict(RF,  x1)
  summary(round((pred0)))
  summary(round((y1)))
  mae(pred0, y1)

  # Out of Sample test
  pred_oos   <- predict(RF,train.oos)
  summary(round((pred_oos)))
  summary(round((train.oos$Expected)))
  res <- mae(pred_oos,train.oos$Expected)
  cat(res)
  saveRDS(res, "mod1_res.RDS")
}

# RF-GRF ------------------------------------------------------------------
if(!(hpc) & mod == 2){
  RF    <- RRF(x, y, flagReg = 0, ntree = 50, mtry = (ncol(x)),
               keep.forest=T, ytest=y1, xtest=x1,#strata = x$,
               corr.bias = F, do.trace = T)
  # In Sample CV
  pred0  <- predict(RF,  x1)
  summary(round((pred0)))
  summary(round((y1)))
  mae(pred0, y1)

  # Out of Sample test
  pred_oos   <- predict(RF,train.oos)
  summary(round((pred_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_oos,train.oos$Expected)

  print(length(RF$feaSet))

  # GRF
  imp     <- RF$importance[,"IncNodePurity"]
  impRF   <- imp/max(imp)
  head(as.data.frame(impRF[order(impRF, decreasing = T)]), 20) # Just to make it easy to read
  gamma   <- .9
  coefReg <- (1-gamma) + gamma * impRF
  GRF     <- RRF(x, y, flagReg = 0, coefReg = coefReg, mtry = (ncol(x)), ntree = 50,
                 keep.forest=TRUE, #ytest=y1, xtest=x1 strata = x$carrera_grupo,
                 corr.bias = T, do.trace = T)
  print(length(GRF$feaSet))

  pred_grf_oos   <- predict(GRF,train.oos)

  summary(round((pred_grf_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_grf_oos,train.oos$Expected)

  res <- mae(pred_grf_oos,train.oos$Expected)
  cat(res)
  saveRDS(res, "mod2_res.RDS")
}

# RRF-GRF -----------------------------------------------------------------
if(!(hpc) & mod == 3){
  RF    <- RRF(x, y, flagReg = 1, ntree = 50, mtry = (ncol(x)),
               keep.forest=T, ytest=y1, xtest=x1,#strata = x$,
               corr.bias = F, do.trace = T)
  # In Sample CV
  pred0  <- predict(RF,  x1)
  summary(round((pred0)))
  summary(round((y1)))
  mae(pred0, y1)

  # Out of Sample test
  pred_oos   <- predict(RF,train.oos)
  summary(round((pred_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_oos,train.oos$Expected)

  print(length(RF$feaSet))

  # GRF
  imp     <- RF$importance[,"IncNodePurity"]
  impRF   <- imp/max(imp)
  head(as.data.frame(impRF[order(impRF, decreasing = T)]), 20) # Just to make it easy to read
  gamma   <- .9
  coefReg <- (1-gamma) + gamma * impRF
  GRF     <- RRF(x, y, flagReg = 0, coefReg = coefReg, mtry = (ncol(x)), ntree = 50,
                 keep.forest=TRUE, #ytest=y1, xtest=x1 strata = x$carrera_grupo,
                 corr.bias = T, do.trace = T)
  print(length(GRF$feaSet))

  pred_grf_oos   <- predict(GRF,train.oos)

  summary(round((pred_grf_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_grf_oos,train.oos$Expected)

  res <- mae(pred_grf_oos,train.oos$Expected)
  cat(res)
  saveRDS(res, "mod3_res.RDS")
}

# RF-GRRF -----------------------------------------------------------------
if(!(hpc) & mod == 4){

system.time(
  RF    <- RRF(x, y, flagReg = 0, ntree = 10, mtry = (ncol(x)),
                 keep.forest=T, #strata = x$, ytest=y1, xtest=x1,
                 corr.bias = T, do.trace = T) # should be under 90 $Vay(y) by iter 10
)
  # In Sample CV
  pred0  <- predict(RF,  x1)
  summary(round((pred0)))
  summary(round((y1)))
  mae(pred0, y1) # 3.673331 with 45 var x, 100 trees and strata = x$min_minutes_past_t2
  saveRDS(RF, "RF.RDS") # don't save unless it's better

  # Out of Sample test
  train.oos  <- sample_n(train.samp, 10000)
  pred_oos   <- predict(RF,train.oos)
  summary(round((pred_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_oos,train.oos$Expected)
    # 2.174568 with 45 var x, 100 trees and strata = x$min_minutes_past_t2
    # 1.864744 with 92 varx, 10 tree no strata

  # GRF
  imp     <- RF$importance[,"IncNodePurity"]
  impRF   <- imp/max(imp)
  head(as.data.frame(impRF[order(impRF, decreasing = T)]), 20) # Just to make it easy to read
  gamma   <- .95
  coefReg <- (1-gamma) + gamma * impRF
  system.time(
    GRF     <- RRF(x, y, flagReg = 1, coefReg = coefReg, mtry = (ncol(x)), ntree = 20,
                   keep.forest=TRUE, strata = x$min_minutes_past_t2, #ytest=y1, xtest=x1 strata = x$carrera_grupo,
                   corr.bias = T, do.trace = T)
   )
  saveRDS(GRF, "GRF.RDS")
  pred_grf_x1    <- predict(GRF,x1)
  summary(round((pred_grf_x1)))
  summary(round((y1)))
  mae(pred_grf_x1,y1)

  pred_grf_oos   <- predict(GRF,train.oos)
  summary(round((pred_grf_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_grf_oos,train.oos$Expected)

  res <- mae(pred_grf_oos,train.oos$Expected)
  cat(res)
  saveRDS(res, "mod4_res.RDS")
}

# RRF-GRRF ----------------------------------------------------------------
if(!(hpc) & mod == 5){
  RF    <- RRF(x, y, flagReg = 1, ntree = 50, mtry = (ncol(x)),
               keep.forest=T, ytest=y1, xtest=x1,#strata = x$,
               corr.bias = F, do.trace = T)
  # In Sample CV
  pred0  <- predict(RF,  x1)
  summary(round((pred0)))
  summary(round((y1)))
  mae(pred0, y1)

  # Out of Sample test
  train.oos  <- sample_n(train.samp,10000)
  pred_oos   <- predict(RF,train.oos)
  summary(round((pred_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_oos,train.oos$Expected)

  # GRF
  imp     <- RF$importance[,"IncNodePurity"]
  impRF   <- imp/max(imp)
  head(as.data.frame(impRF[order(impRF, decreasing = T)]), 20) # Just to make it easy to read
  gamma   <- .9
  coefReg <- (1-gamma) + gamma * impRF
  GRF     <- RRF(x, y, flagReg = 1, coefReg = coefReg, mtry = (ncol(x)), ntree = 50,
                 keep.forest=TRUE, #ytest=y1, xtest=x1 strata = x$carrera_grupo,
                 corr.bias = T, do.trace = T)
  print(length(GRF$feaSet))

  pred_grf_oos   <- predict(GRF,train.oos)

  summary(round((pred_grf_oos)))
  summary(round((train.oos$Expected)))
  mae(pred_grf_oos,train.oos$Expected)

  res <- mae(pred_grf_oos,train.oos$Expected)
  cat(res)
  saveRDS(res, "mod5_res.RDS")
}
