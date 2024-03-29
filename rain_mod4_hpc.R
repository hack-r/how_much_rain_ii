# ------------------------------------------------------------------------
# File:        rain_mod4_hpc.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script takes advantage of doParallel for the trees and
#                 is based on the best small-data performing model, mod4
# ------------------------------------------------------------------------
train.sample <- sample_n(train.samp, 25000)
id <- train.sample$Id
x  <- train.sample[,!(colnames(train.sample) %in% c("Id", "Expected"))]
y  <- train.sample$Expected

system.time(
rf <- foreach(ntree=rep(50, 12), .combine=combine, .multicombine=TRUE,
              .packages='RRF') %dopar% {
                RF    <- RRF(x, y, flagReg = 0, ntree = ntree, mtry = (ncol(x)),
                             keep.forest=T, ytest=y1, xtest=x1,#strata = x$,
                             corr.bias = F, do.trace = T)
              }
)