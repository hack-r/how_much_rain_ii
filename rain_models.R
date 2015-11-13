# ------------------------------------------------------------------------
# File:        rain_analysis.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script performs analysis, model building and testing
# ------------------------------------------------------------------------


# Gamma GLM ---------------------------------------------------------------
glmGamma <- glm(formula = "Expected ~ mp + md + Ref_5x5_10th_t1 + RhoHV_5x5_90th_t1 + Zdr_5x5_90th_t1 + min_minutes_past_t2 + RefComposite_5x5_10th_t2 + RefComposite_5x5_90th_t2  + RhoHV_5x5_90th_t2 + Kdp_5x5_50th_t2",
                  family = Gamma(link = "identity"), data = train.roughfix)
summary(glmGamma)
library(MASS)
myshape <- gamma.shape(glmGamma)
gampred <- predict(glmGamma , type = "response", se = T, dispersion = 1/myshape$alpha)
summary(glmGamma, dispersion = 1/myshape$alpha)
saveRDS(glmGamma, "glmGamma.RDS")

# VIF Factor Reduction ----------------------------------------------------
xgampred     <- predict(glmGamma , newdata = x, type = "response", se=T, dispersion = 1/myshape$alpha)
x1gampred    <- predict(glmGamma , newdata = x1, type = "response",se=T, dispersion = 1/myshape$alpha)
x$gampred    <- xgampred$fit
x1$gampred   <- x1gampred$fit

vx <- vif_func(x1);saveRDS(vx, "vx.RDS")
x  <- x[,colnames(x) %in% vx]
x1 <- x1[,colnames(x1) %in% vx]
identical(colnames(x), colnames(x1))

# Gamma RRF only:
x  <- x[,c("mp", "md", "gampred","RefComposite_5x5_90th_t2", "min_minutes_past_t2")]
x1 <- x1[,c("mp", "md", "gampred","RefComposite_5x5_90th_t2", "min_minutes_past_t2")]
x  <- na.omit(x)
x1  <- na.omit(x1)

# RRF ---------------------------------------------------------------------
system.time(
  RF    <- RRF(x, y, flagReg = 1, ntree = 250, #mtry = (ncol(x)),
                 keep.forest=T, strata = x$md ,ytest=y1, xtest=x1, #
                corr.bias = T, do.trace = T) # should be under 90 $Vay(y) by iter 10
)
# In Sample CV
pred0  <- predict(RF,  x1)
summary(round((pred0)))
summary(round((y1)))
mae(pred0, y1) # 3.673331 with 45 var x, 100 trees and strata = x$min_minutes_past_t2
saveRDS(RF, "RF.RDS")

# Out of Sample test
train.oos  <- sample_n(train.samp, 10000)
train.oos  <- na.roughfix(train.oos)
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
gamma   <- .5
coefReg <- (1-gamma) + gamma * impRF
system.time(
  GRF     <- RRF(x[,colnames(x) %in% names(coefReg)], y, flagReg = 1,
                 coefReg = coefReg[names(coefReg) %in% colnames(x)],
                 mtry = (ncol(x[,colnames(x) %in% names(coefReg)])), ntree = 250,
                 keep.forest=TRUE, strata = x$md,
                 corr.bias = T, do.trace = T)
 )
saveRDS(GRF, "GRF.RDS")
pred_grf_x1    <- predict(GRF,x1)
summary(round((pred_grf_x1)))
summary(round((y1)))
mae(pred_grf_x1,y1)
plot(density(pred_grf_x1))

pred_grf_oos   <- predict(GRF,train.oos)
summary(round((pred_grf_oos)))
summary(round((train.oos$Expected)))
mae(pred_grf_oos,train.oos$Expected)

res <- mae(pred_grf_oos,train.oos$Expected)
cat(res)
saveRDS(res, "mod4_res.RDS")


# caret tree --------------------------------------------------------------
cl <- makePSOCKcluster(detectCores());
clusterEvalQ(cl, library(foreach, caret)); registerDoParallel(cl)
rf_train <- train(y=y, x=x[,colnames(x) ],
                  method="parRF",  tuneGrid = data.frame(mtry = ncol(x)),#ncol(x[,colnames(x) %in% keep])
                  na.action = na.omit, #ntree = 100,
                  trControl=trainControl(method='oob', allowParallel = TRUE)
)
saveRDS(rf_train, "rf_train.rds")

stopCluster(cl); stopImplicitCluster()
gc()

pred0  <- predict(rf_train,  x1)
summary(round((pred0)))
summary(round((y1)))
mae(pred0, y1) #3.53 caret oob mtry=ncol

train.oos  <- sample_n(train, 10000)
pred_oos   <- predict(rf_train,train.oos)
summary(round((pred_oos)))
summary(round((train.oos$Expected)))
mae(pred_oos,train.oos$Expected)
  # 2.17 with 45 var x, 100 trees and strata = x$min_minutes_past_t2, n=?
  # 3.56 with 57 var,  parRF,  22.3k rows, mtry = 3 (tree?)
  # 3.58 with 57 var,  parRF,  22.3k rows, mtry = ncol (tree?)


imp     <- randomForest::importance(rf_train$finalModel)[,"IncNodePurity"]
impRF   <- imp/max(imp)
head(as.data.frame(impRF[order(impRF, decreasing = T)]), 20) # Just to make it easy to read
# caret-based GRRF
gamma   <- .9
coefReg <- (1-gamma) + gamma * impRF
system.time(
  GRF     <- RRF(x, y, flagReg = 1, coefReg = coefReg, mtry = (ncol(x)), ntree = 250,
                 keep.forest=TRUE, strata = x$min_minutes_past_t2,ytest=y1, xtest=x1, # strata = x$carrera_grupo,
                 corr.bias = T, do.trace = T)
)
saveRDS(GRF, "GRF.RDS")
pred_grf_x1    <- predict(GRF,x1)
summary(round((pred_grf_x1)))
summary(round((y1)))
mae(pred_grf_x1,y1)
  #3.89 10 tree, gamma .9, mtry = 17, caret based

pred_oos   <- predict(GRF,train.oos)
summary(round((pred_oos)))
summary(round((train.oos$Expected)))
mae(pred_oos,train.oos$Expected)
  #3.82