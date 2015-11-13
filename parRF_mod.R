
parRF_mod$fit <- function (x, y, wts, param, lev, last, classProbs, ...)
{
  # added the requirement of foreach
  require(foreach)
  workers <- getDoParWorkers()
  theDots <- list(...)
  theDots$ntree <- 100
  theDots$x <- x
  theDots$y <- y
  theDots$mtry <- param$mtry
  theDots$ntree <- ceiling(theDots$ntree/workers)
  out <- foreach(ntree = 1:workers, .combine = combine) %dopar%
  {
    library(randomForest)
    do.call("randomForest", theDots)
  }
  out$call["x"] <- "x"
  out$call["y"] <- "y"
  out
}
