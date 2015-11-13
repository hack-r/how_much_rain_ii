setwd("P://")
setwd("T://RNA//Baltimore//Jason//ad_hoc//cats")
p_load(HiddenMarkov)
demo("gamma", package="HiddenMarkov")
sample <- read.csv("sample_solution.csv")
mae(sample$Expected, jitter(sample$Expected, amount = 25))
train <- readRDS("train_clean.RDS")
train.samp  <- sample_n(train, 20000)
train.samp1 <- sample_n(train, 10000)
while(t.test(train.samp$Expected, train$Expected)$p.value < .6){train.samp <- sample_n(train, 20000)}
while(t.test(train.samp1$Expected, train.samp$Expected)$p.value < .6){train.samp1 <- sample_n(train, 10000)}
summary(train$Expected)
summary(train.samp$Expected)
summary(train.samp1$Expected)

# train.samp$outlier1 <- scores(train.samp$Expected, type = "z", prob = 1)#, lim = 100
# plot(density(train.samp$Expected[train.samp$outlier1 < .468]))
#
# train.samp$outlier <- scores(train.samp$mp, type = "z", prob = 1)#, lim = 100
# plot(density(train.samp$Expected[train.samp$outlier < .363]))

system.time(
  for(i in unique(train.samp$Id)){
    train.samp$mp[train.samp$Id == i] <- mpalmer(train.samp$Ref[train.samp$Id == i], minutes_past = train.samp$minutes_past[train.samp$Id == i])
  }
)
for(i in unique(train.samp1$Id)){
  train.samp1$mp[train.samp1$Id == i] <- mpalmer(train.samp1$Ref[train.samp1$Id == i], minutes_past = train.samp1$minutes_past[train.samp1$Id == i])
}


y  <- train.samp$Expected
y1 <- train.samp1$Expected
x  <- train.samp[,!colnames(train.samp) %in% c("Expected", "outlier", "outlier1", "Id")]
x1 <- train.samp1[,!colnames(train.samp1) %in% c("Expected", "outlier", "outlier1", "Id")]
x  <- na.roughfix(x)
x1 <- na.roughfix(x1)

vx <- vif_func(x1)
x  <- x[,colnames(x) %in% vx]
x1 <- x1[,colnames(x1) %in% vx]

# x$Expected  <- jitter(train.samp$Expected, amount = 1)
# x1$Expected <- jitter(train.samp1$Expected, amount =1)

x$max_min <-0
x$max_min[x$minutes_past == 59] <- 1

x1$max_min <-0
x1$max_min[x1$minutes_past == 59] <- 1


RF    <- RRF(x, y, flagReg = 1, ntree = 500, mtry = (ncol(x)),
                    keep.forest=T, strata = x$max_min, corr.bias=T,
                    do.trace = T)

# train.oos  <- sample_n(train, 10000)
# system.time(
#   for(i in unique(train.oos$Id)){
#     train.oos$mp[train.oos$Id == i] <- mpalmer(train.oos$Ref[train.oos$Id == i], minutes_past = train.oos$minutes_past[train.oos$Id == i])
#   }
# )
#

# In Sample CV
pred0  <- predict(RF,  x1)
summary(round((pred0)))
summary(round((y1)))
mae(pred0, y1) #
saveRDS(RF, "RF_crazy_idea.RDS") #


# GRF
imp     <- RF$importance[,"IncNodePurity"]
impRF   <- imp/max(imp)
head(as.data.frame(impRF[order(impRF, decreasing = T)]), 20) # Just to make it easy to read
gamma   <- .8
coefReg <- (1-gamma) + gamma * impRF
system.time(
  GRF     <- RRF(x, y, flagReg = 1, coefReg = coefReg, ntree = 500,
                 keep.forest=TRUE, strata = x$min_minutes_past_t2, #ytest=y1, xtest=x1 strata = x$carrera_grupo,
                 corr.bias = T, do.trace = T)
)
saveRDS(GRF, "GRF_crazy_idea.RDS")

test.wide <- readRDS("test_wide.RDS")
RF <- readRDS("RF.RDS")
GRF <- readRDS("GRF.RDS")

RF.out <- readRDS("RF_outlier.RDS")

sample <- read.csv("sample_solution.csv")

test.wide$Expected <- sample$Expected
test.wide$max_min <-0
test.wide$max_min[test.wide$minutes_past == 59] <- 1

names(test.wide) = sub("*.t1","",names(test.wide))
test.wide$minutes_past <- test.wide$max_minutes_past

test.wide$prediction <- predict(RF, test.wide)
summary(test.wide$prediction)
plot(density(test.wide$prediction))
#plot(density(sample$Expected))

grf_pred <- predict(GRF, test.wide)
plot(density(grf_pred))
summary(grf_high_pred)

test.wide$prediction[test.wide$prediction < 0 ] <- 0

res <- sqldf("select Id, prediction as Expected from 'test.wide'") # group by Id
write.csv(res, "crazy_idea.csv", row.names = F)
