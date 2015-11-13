# ------------------------------------------------------------------------
# File:        rain_data.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script performs ETL
# ------------------------------------------------------------------------

# Load raw data -----------------------------------------------------------
if(useRDS == F & useRAW){
  train  <- read.csv("train.csv")
  test   <- read.csv("test.csv")
  saveRDS(train, "train.RDS")
  saveRDS(test, "test.RDS")
}

if(useRDS & useRAW){
  train <- readRDS("train.RDS")
  test  <- readRDS("test.RDS")
}

# Clean -------------------------------------------------------------------
# Delete ID's with completely non-NULL data (other than minutes and radar distance)

# Using only complete data
if(useRDS == F){
  train.clean <- train
  rm(train)
  train.clean$delete                         <- 0
  train.clean$delete[is.na(train.clean$Ref)] <- 1

  drop           <- aggregate(train.clean$delete, by=list(train.clean$Id), sum)
  colnames(drop) <- c("Id", "delete")
  clean          <- train.clean[train.clean$Id %in% drop$Id[drop$delete == 0],]
  clean$delete   <- NULL
  train          <- clean
  train$Ref_5x5_50th[which(train$Ref_5x5_50th < 0)] <- NA
  train$Ref_5x5_90th[which(train$Ref_5x5_90th < 0)] <- NA
  train$RefComposite[which(train$RefComposite < 0)] <- NA
  train$RefComposite_5x5_50th[which(train$RefComposite_5x5_50th_5x5_50th < 0)] <- NA
  train$RefComposite_5x5_90th[which(train$RefComposite_5x5_50th_5x5_90th < 0)] <- NA
  train$Ref[which(train$Ref < 0)] <- NA
  train <- train[!(is.na(train$Ref)),]
  saveRDS(train, "train_clean.RDS")
  rm(train.clean, drop, clean) # from here on out, I use the RDS object
} else{
  train <- readRDS("train_clean.RDS")
}

# Non-missing, non-imputed only -------------------------------------------
if(perfect){
  train   <- readRDS("train_clean.RDS")
  train   <- na.omit(train)
}

# Outlier Correction ------------------------------------------------------
train <- subset(train, Expected < 69)

# Reshape -----------------------------------------------------------------
train.wide  <- sqldf("select Id, max(minutes_past) as max_minutes_past, radardist_km,Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th,RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th,RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th,Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th,Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th,Expected from 'train' group by Id")
train.wide2 <- sqldf("select Id, min(minutes_past) as min_minutes_past, radardist_km,Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th,RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th,RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th,Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th,Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th from 'train' group by Id")
rm(train)
colnames(train.wide)   <- paste(colnames(train.wide), "_t1", sep = "")
colnames(train.wide2)  <- paste(colnames(train.wide2), "_t2", sep = "")
train.wide2$Id_t2      <- NULL
train.wide$Id          <- train.wide$Id_t1
train.wide$Id_t1       <- NULL

train <- cbind(train.wide, train.wide2)
rm(train.wide, train.wide2)
#train <- na.omit(train)

train$Expected_t2 <- NULL
train$Expected    <- train$Expected_t1
train$Expected_t1 <- NULL
saveRDS(train, "train_wide_clean.RDS")

# Sample training data ----------------------------------------------------
if(fullset){
  train.samp  <- sample_n(train,(nrow(train) * .9))
  train.samp1 <- train[!(train$Id %in% train.samp$Id),]#sample_n(train,10000)
  train
}else {
  while(t.test(train.samp$Expected, train$Expected)$p.value < .8 | (t.test(train.samp$mp, train$mp)$p.value < .8)){train.samp <- sample_n(train, 30000)}
  while(t.test(train.samp1$Expected, train.samp$Expected)$p.value < .8 | (t.test(train.samp$mp, train$mp)$p.value < .8)){train.samp1 <- sample_n(train, 7500)}
}
summary(train.samp$Expected)
summary(train.samp1$Expected)
summary(train.samp$mp)
summary(train.samp1$mp)

# Incorporate Marshall Palmer ---------------------------------------------
if(useMP & !(fullSet)){
  train.samp      <- train.samp[order(train.samp$Id),]
  train.samp1     <- train.samp1[order(train.samp1$Id),]
  #train.oos       <- train.oos[order(train.oos$Id),]
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
  train.samp$Ref  <- train.samp$Ref_t1
  train.samp1$Ref <- train.samp1$Ref_t1
  train.samp$minutes_past  <- train.samp$max_minutes_past_t1
  train.samp1$minutes_past <- train.samp1$max_minutes_past_t1

  train.samp$Ref_t1  <- NULL
  train.samp1$Ref_t1 <- NULL
  train.samp$max_minutes_past_t1  <- NULL
  train.samp1$max_minutes_past_t1 <- NULL

  train.samp.id    <- unique(train.samp$Id)
  train.samp.id.1  <- train.samp.id[1:50000]
  train.samp.id.2  <- train.samp.id[50001:75000]
  train.samp.id.3  <- train.samp.id[75001:100000]
  train.samp.id.4  <- train.samp.id[100001:150000]
  train.samp.id.5  <- train.samp.id[150001:175000]
  train.samp.id.6  <- train.samp.id[175001:200000]
  train.samp.id.7  <- train.samp.id[200001:250000]
  train.samp.id.8  <- train.samp.id[250001:300000]
  train.samp.id.9  <- train.samp.id[300001:length(train.samp.id)]

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
    }
    )
  system.time(
    for( i in unique(train.samp.2$Id)){
      train.samp.2$mp[train.samp.2$Id == i] <- mpalmer(train.samp.2$Ref[train.samp.2$Id == i], minutes_past = train.samp.2$minutes_past[train.samp.2$Id == i])
    })
  system.time(
    for( i in unique(train.samp.3$Id)){
      train.samp.3$mp[train.samp.3$Id == i] <- mpalmer(train.samp.3$Ref[train.samp.3$Id == i], minutes_past = train.samp.3$minutes_past[train.samp.3$Id == i])
    })
  system.time(
    for( i in unique(train.samp.4$Id)){
      train.samp.4$mp[train.samp.4$Id == i] <- mpalmer(train.samp.4$Ref[train.samp.4$Id == i], minutes_past = train.samp.4$minutes_past[train.samp.4$Id == i])
    })
  system.time(
    for( i in unique(train.samp.5$Id)){
      train.samp.5$mp[train.samp.5$Id == i] <- mpalmer(train.samp.5$Ref[train.samp.5$Id == i], minutes_past = train.samp.5$minutes_past[train.samp.5$Id == i])
    })
  system.time(
    for( i in unique(train.samp.6$Id)){
      train.samp.6$mp[train.samp.6$Id == i] <- mpalmer(train.samp.6$Ref[train.samp.6$Id == i], minutes_past = train.samp.6$minutes_past[train.samp.6$Id == i])
    })
  system.time(
    for( i in unique(train.samp.7$Id)){
      train.samp.7$mp[train.samp.7$Id == i] <- mpalmer(train.samp.7$Ref[train.samp.7$Id == i], minutes_past = train.samp.7$minutes_past[train.samp.7$Id == i])
    })
  system.time(
    for( i in unique(train.samp.8$Id)){
      train.samp.8$mp[train.samp.8$Id == i] <- mpalmer(train.samp.8$Ref[train.samp.8$Id == i], minutes_past = train.samp.8$minutes_past[train.samp.8$Id == i])
    })
  system.time(
    for( i in unique(train.samp.9$Id)){
      train.samp.9$mp[train.samp.9$Id == i] <- mpalmer(train.samp.9$Ref[train.samp.9$Id == i], minutes_past = train.samp.9$minutes_past[train.samp.9$Id == i])
    })

  train.samp <- rbind(train.samp.1, train.samp.2, train.samp.3, train.samp.4,
                      train.samp.5, train.samp.6, train.samp.7, train.samp.8,
                      train.samp.9)

  rm(train.samp.1, train.samp.2, train.samp.3, train.samp.4,
     train.samp.5, train.samp.6, train.samp.7, train.samp.8,
     train.samp.9)
  saveRDS(train.samp, "full_train_samp_with_mp.RDS")
  summary(train.samp$mp)
  system.time(
    for(i in unique(train.samp1$Id)){
      train.samp1$mp[train.samp1$Id == i] <- mpalmer(train.samp1$Ref[train.samp1$Id == i], minutes_past = train.samp1$minutes_past[train.samp1$Id == i])
    }
  )
  saveRDS(train.samp1, "full_samp1_with_mp.RDS")
  train <- rbind(train.samp, train.samp1)
  #saveRDS(train.samp, "full_train_with_mp.RDS")
}

# Test out MAD predictors... ----------------------------------------------
# train.samp.scores  <- scores(train.samp[,!(colnames(train.samp) %in% c("Id", "Expected"))],  type = "mad", prob = 1)
# train.samp1.scores <- scores(train.samp1[,!(colnames(train.samp1) %in% c("Id", "Expected"))], type = "mad", prob = 1)
#
# train.samp.scores$Id1  <- train.samp$Id
# train.samp1.scores$Id1 <- train.samp1$Id
# train.samp.scores$Id   <- NULL
# train.samp1.scores$Id  <- NULL
# train.samp.scores$Id_mad   <- NULL
# train.samp1.scores$Id_mad  <- NULL
#
# train.samp.scores$Expected_mad  <- NULL
# train.samp1.scores$Expected_mad <- NULL
#
# colnames(train.samp.scores)  <- paste(colnames(train.samp.scores), "_mad", sep = "")
# colnames(train.samp1.scores) <- paste(colnames(train.samp1.scores), "_mad", sep = "")
#
# train.samp             <- cbind(train.samp, train.samp.scores)
# train.samp1            <- cbind(train.samp1, train.samp1.scores)
# train.samp.scores$Id1  <- NULL
# train.samp1.scores$Id1 <- NULL
# train.samp$Id_mad      <- NULL
# train.samp1$Id_mad     <- NULL
# train.samp$Id1_mad      <- NULL
# train.samp1$Id1_mad     <- NULL
# train.samp$Expected_mad  <- NULL
# train.samp1$Expected_mad <- NULL

# NEW VARIABLES -----------------------------------------------------------
# move this code later
train$md                <- train$minutes_past - train$min_minutes_past_t2
train$Zdr_5x5_50th_diff <- train$Zdr_5x5_50th_t1     - train$Zdr_5x5_50th_t2
train.samp$md                <- train.samp$minutes_past - train.samp$min_minutes_past_t2
train.samp$Zdr_5x5_50th_diff <- train.samp$Zdr_5x5_50th_t1     - train.samp$Zdr_5x5_50th_t2
train.samp1$md                <- train.samp1$minutes_past - train.samp1$min_minutes_past_t2
train.samp1$Zdr_5x5_50th_diff <- train.samp1$Zdr_5x5_50th_t1     - train.samp1$Zdr_5x5_50th_t2

saveRDS(train, "train_final.RDS")
saveRDS(train.samp, "train_samp_final.RDS")
saveRDS(train.samp1, "train_samp1_final.RDS")

# Transform for ML --------------------------------------------------------
# train.samp  <- sample_n(train, 22331); train.samp <- train
# train.samp1 <- sample_n(train.samp, 7500)

train.samp  <- na.omit(train.samp)
train.samp1 <- na.omit(train.samp1)
id  <- train.samp$Id
id1 <- train.samp1$Id
x   <- train.samp[,!(colnames(train.samp) %in% c("Id", "Id1", "Expected", "Expected_mad"))]
x1  <- train.samp1[,!(colnames(train.samp1) %in% c("Id", "Id1", "Expected", "Expected_mad"))]
y   <- train.samp$Expected
y1  <- train.samp1$Expected

# Just to be safe...
x$Id1      <- NULL
x$Id       <- NULL
x$Id_mad   <- NULL
x1$Id1_mad <- NULL
x1$Id1     <- NULL
x1$Id      <- NULL
x1$Id_mad  <- NULL
x1$Id1_mad <- NULL
x1$outlier <- NULL
x$outlier  <- NULL
x$Expected_mad   <- NULL
x1$Expected_mad  <- NULL
x$Expected       <- NULL
x1$Expected      <- NULL

#x <- na.roughfix(x)
#x1 <- na.roughfix(x1)