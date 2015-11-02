# ------------------------------------------------------------------------
# File:        rain_data.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script performs ETL
# ------------------------------------------------------------------------

# Load raw data -----------------------------------------------------------
if(useRDS == F & useRAW){
  train.raw <- read.csv("train.csv")
  test.raw  <- read.csv("test.csv")
  saveRDS(train.raw, "train.RDS")
  saveRDS(test.raw, "test.RDS")
}

if(useRDS & useRAW){
  train.raw <- readRDS("train.RDS")
  test.raw  <- readRDS("test.RDS")
}

# Clean -------------------------------------------------------------------
# Delete ID's with completely non-NULL data (other than minutes and radar distance)
if(useRDS == F){
  train.clean <- train.raw
  rm(train.raw)
  train.clean$delete                         <- 0
  train.clean$delete[is.na(train.clean$Ref)] <- 1

  drop           <- aggregate(train.clean$delete, by=list(train.clean$Id), sum)
  colnames(drop) <- c("Id", "delete")
  clean          <- train.clean[train.clean$Id %in% drop$Id[drop$delete == 0],]
  clean$delete   <- NULL
  saveRDS(clean, "train_clean.RDS")
  train          <- clean
  rm(train.clean, drop, clean) # from here on out, I use the RDS object
} else{
  train <- readRDS("train_clean.RDS")
}

# Non-missing, non-imputed only -------------------------------------------
if(perfect){
  train   <- readRDS("train_clean.RDS")
  train   <- na.omit(train)
}

# Sample training data ----------------------------------------------------
if(fullset){
  n           <- nrow(train) - 25000
  n1          <- n + 1
  train.samp  <- train[1:n, ]
  train.samp1 <- train[n1:nrow(train), ]
  rm(train, n, n1)
}else {
  train.samp  <- sample_n(train, 20000)
  train.samp1 <- sample_n(train, 20000)
  train.oos   <- sample_n(train, 20000)
}

# Outlier Correction ------------------------------------------------------
# Based on analysis of density plots, I want to use the outlier scores that
#   exclude Expected values > 107

summary(train.samp$Expected)
summary(train.samp1$Expected)

train.samp$outlier  <- scores(train.samp$Expected, type = "iqr")
train.samp          <- train.samp[train.samp$outlier < 26,]
train.samp          <- train.samp[!(train.samp$Expected < 0),]
d                   <- density(train.samp$Expected)
plot(d)

train.samp1$outlier  <- scores(train.samp1$Expected, type = "iqr", prob = 1)
train.samp1          <- train.samp1[train.samp1$outlier < 26,]
train.samp1          <- train.samp1[!(train.samp1$Expected < 0),]
d                    <- density(train.samp1$Expected)
plot(d)

summary(train.samp$Expected)
summary(train.samp1$Expected)

train.samp$outlier  <- NULL
train.samp1$outlier <- NULL

train.samp.scores  <- scores(train.samp,  type = "mad", prob = 1)
train.samp1.scores <- scores(train.samp1, type = "mad", prob = 1)

train.samp.scores$Id1  <- train.samp$Id
train.samp1.scores$Id1 <- train.samp1$Id
train.samp.scores$Id   <- NULL
train.samp1.scores$Id  <- NULL

colnames(train.samp.scores)  <- paste(colnames(train.samp.scores), "_mad", sep = "")
colnames(train.samp1.scores) <- paste(colnames(train.samp1.scores), "_mad", sep = "")

train.samp             <- cbind(train.samp, train.samp.scores)
train.samp1            <- cbind(train.samp1, train.samp1.scores)
train.samp.scores$Id1  <- NULL
train.samp1.scores$Id1 <- NULL

# Incorporate Marshall Palmer ---------------------------------------------
if(useMP & !(fullSet)){
  train.samp      <- train.samp[order(train.samp$Id),]
  train.samp1     <- train.samp1[order(train.samp1$Id),]
  train.oos       <- train.oos[order(train.oos$Id),]
  #train.samp$mp   <- aggregate(train.samp$Ref, by=list(train.samp$Id), FUN = mpalme, minutes_past = train.samp$minutes_past)
  #train.samp1$mp  <- aggregate(train.samp1$Ref, by=list(train.samp1$Id), FUN = mpalm, minutes_past = train.samp1$minutes_past)

  for(i in unique(train.samp$Id)){
    train.samp$mp[train.samp$Id == i] <- mpalmer(train.samp$Ref[train.samp$Id == i], minutes_past = train.samp$minutes_past[train.samp$Id == i])
  }
  for(i in unique(train.samp1$Id)){
    train.samp1$mp[train.samp1$Id == i] <- mpalmer(train.samp1$Ref[train.samp1$Id == i], minutes_past = train.samp1$minutes_past[train.samp1$Id == i])
  }
  for(i in unique(train.oos$Id)){
    train.oos$mp[train.oos$Id == i] <- mpalmer(train.oos$Ref[train.oos$Id == i], minutes_past = train.oos$minutes_past[train.oos$Id == i])
  }

}
if(useMP & fullSet){
  train.samp      <- train.samp[order(train.samp$Id),]
  train.samp1     <- train.samp1[order(train.samp1$Id),]

  train.samp.id    <- unique(train.samp$Id)
  train.samp.id.1  <- train.samp.id[1:25000]
  train.samp.id.2  <- train.samp.id[25001:50000]
  train.samp.id.3  <- train.samp.id[50001:75000]
  train.samp.id.4  <- train.samp.id[75001:100000]
  train.samp.id.6  <- train.samp.id[100001:125000]
  train.samp.id.5  <- train.samp.id[125001:150000]
  train.samp.id.7  <- train.samp.id[150001:175000]
  train.samp.id.8  <- train.samp.id[175001:200000]
  train.samp.id.9  <- train.samp.id[200001:length(train.samp.id)]

  train.samp.1 <- train.samp[train.samp$Id %in% train.samp.id.1,]
  train.samp.2 <- train.samp[train.samp$Id %in% train.samp.id.2,]
  train.samp.3 <- train.samp[train.samp$Id %in% train.samp.id.3,]
  train.samp.4 <- train.samp[train.samp$Id %in% train.samp.id.4,]
  train.samp.5 <- train.samp[train.samp$Id %in% train.samp.id.5,]
  train.samp.6 <- train.samp[train.samp$Id %in% train.samp.id.6,]
  train.samp.7 <- train.samp[train.samp$Id %in% train.samp.id.7,]
  train.samp.8 <- train.samp[train.samp$Id %in% train.samp.id.8,]
  train.samp.9 <- train.samp[train.samp$Id %in% train.samp.id.9,]

  system.time(
    for(i in unique(train.samp.1$Id)){
      train.samp.1$mp[train.samp.1$Id == i] <- mpalmer(train.samp.1$Ref[train.samp.1$Id == i], minutes_past = train.samp.1$minutes_past[train.samp.1$Id == i])
    })
  system.time(
    for(i in unique(train.samp.2$Id)){
      train.samp.2$mp[train.samp.2$Id == i] <- mpalmer(train.samp.2$Ref[train.samp.2$Id == i], minutes_past = train.samp.2$minutes_past[train.samp.2$Id == i])
    })
  system.time(
    for(i in unique(train.samp.3$Id)){
      train.samp.3$mp[train.samp.3$Id == i] <- mpalmer(train.samp.3$Ref[train.samp.3$Id == i], minutes_past = train.samp.3$minutes_past[train.samp.3$Id == i])
    })
  system.time(
    for(i in unique(train.samp.4$Id)){
      train.samp.4$mp[train.samp.4$Id == i] <- mpalmer(train.samp.4$Ref[train.samp.4$Id == i], minutes_past = train.samp.4$minutes_past[train.samp.4$Id == i])
    })
  system.time(
    for(i in unique(train.samp.5$Id)){
      train.samp.5$mp[train.samp.5$Id == i] <- mpalmer(train.samp.5$Ref[train.samp.5$Id == i], minutes_past = train.samp.5$minutes_past[train.samp.5$Id == i])
    })
  system.time(
    for(i in unique(train.samp.6$Id)){
      train.samp.6$mp[train.samp.6$Id == i] <- mpalmer(train.samp.6$Ref[train.samp.6$Id == i], minutes_past = train.samp.6$minutes_past[train.samp.6$Id == i])
    })
  system.time(
    for(i in unique(train.samp.7$Id)){
      train.samp.7$mp[train.samp.7$Id == i] <- mpalmer(train.samp.7$Ref[train.samp.7$Id == i], minutes_past = train.samp.7$minutes_past[train.samp.7$Id == i])
    })
  system.time(
    for(i in unique(train.samp.8$Id)){
      train.samp.8$mp[train.samp.8$Id == i] <- mpalmer(train.samp.8$Ref[train.samp.8$Id == i], minutes_past = train.samp.8$minutes_past[train.samp.8$Id == i])
    })
  system.time(
    for(i in unique(train.samp.9$Id)){
      train.samp.9$mp[train.samp.9$Id == i] <- mpalmer(train.samp.9$Ref[train.samp.9$Id == i], minutes_past = train.samp.9$minutes_past[train.samp.9$Id == i])
    })

  train.samp <- rbind(train.samp.1, train.samp.2, train.samp.3, train.samp.4,
                      train.samp.5, train.samp.6, train.samp.7, train.samp.8,
                      train.samp.9)

  rm(train.samp.1, train.samp.2, train.samp.3, train.samp.4,
     train.samp.5, train.samp.6, train.samp.7, train.samp.8,
     train.samp.9)
  saveRDS(train.samp, "full_train_with_mp.RDS")

  system.time(
    for(i in unique(train.samp1$Id)){
      train.samp1$mp[train.samp1$Id == i] <- mpalmer(train.samp1$Ref[train.samp1$Id == i], minutes_past = train.samp1$minutes_past[train.samp1$Id == i])
    }
  )

}

# Transform for ML --------------------------------------------------------
id <- train.samp$Id
id1<- train.samp1$Id
x  <- train.samp[,!(colnames(train.samp) %in% c("Id", "Expected"))]
x1 <- train.samp1[,!(colnames(train.samp1) %in% c("Id", "Expected"))]
y  <- train.samp$Expected
y1 <- train.samp1$Expected

# Imputation --------------------------------------------------------------
# Use Amelia II
x  <- na.roughfix(x)
x1 <- na.roughfix(x1)