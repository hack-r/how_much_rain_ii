# ------------------------------------------------------------------------
# File:        rain_report.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script runs the best model on the unlabeled test data
# ------------------------------------------------------------------------

# Load --------------------------------------------------------------------
test <- readRDS("test.RDS")
id   <- test$Id
GRF  <- readRDS("GRF.RDS")

# Imputation --------------------------------------------------------------
test <- na.roughfix(test)
saveRDS(test, "test_roughfix.RDS")

# Reshape -----------------------------------------------------------------
test.wide  <- sqldf("select Id, max(minutes_past) as max_minutes_past, radardist_km,Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th,RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th,RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th,Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th,Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th from test group by Id")
test.wide2 <- sqldf("select Id, min(minutes_past) as min_minutes_past, radardist_km,Ref,Ref_5x5_10th,Ref_5x5_50th,Ref_5x5_90th,RefComposite,RefComposite_5x5_10th,RefComposite_5x5_50th,RefComposite_5x5_90th,RhoHV,RhoHV_5x5_10th,RhoHV_5x5_50th,RhoHV_5x5_90th,Zdr,Zdr_5x5_10th,Zdr_5x5_50th,Zdr_5x5_90th,Kdp,Kdp_5x5_10th,Kdp_5x5_50th,Kdp_5x5_90th from test group by Id")
rm(test)
colnames(test.wide)   <- paste(colnames(test.wide), "_t1", sep = "")
colnames(test.wide2)  <- paste(colnames(test.wide2), "_t2", sep = "")
test.wide2$Id_t2      <- NULL
test.wide$Id          <- test.wide$Id_t1
test.wide$Id_t1       <- NULL
test.wide$mp          <- test.wide$mp_t1 #if it exists already
test.wide$mp_t1        <- NULL

test <- cbind(test.wide, test.wide2)
rm(test.wide, test.wide2)
id.wide <- test$Id
saveRDS(id.wide, "id_wide.RDS")
saveRDS(test, "test_wide.RDS")

# Add MP ------------------------------------------------------------------
uid    <- unique(test$Id)
test.1 <- test[test$Id %in% uid[1:10000],]
test.2 <- test[test$Id %in% uid[10001:20000],]
test.3 <- test[test$Id %in% uid[20001:30000],]
test.4 <- test[test$Id %in% uid[30001:40000],]
test.5 <- test[test$Id %in% uid[40001:50000],]
test.6 <- test[test$Id %in% uid[50001:60000],]
test.7 <- test[test$Id %in% uid[60001:70000],]
test.8 <- test[test$Id %in% uid[70001:80000],]
test.9 <- test[test$Id %in% uid[80001:90000],]
test.10 <- test[test$Id %in% uid[90001:100000],]
test.11 <- test[test$Id %in% uid[100001:110000],]
test.12 <- test[test$Id %in% uid[110001:120000],]
test.13 <- test[test$Id %in% uid[120001:130000],]
test.14 <- test[test$Id %in% uid[130001:140000],]
test.15 <- test[test$Id %in% uid[140001:150000],]
test.16 <- test[test$Id %in% uid[150001:160000],]
test.17 <- test[test$Id %in% uid[160001:170000],]
test.18 <- test[test$Id %in% uid[170001:180000],]
test.19 <- test[test$Id %in% uid[180001:190000],]

test.20 <- test[test$Id %in% uid[190001:200000],]
test.21 <- test[test$Id %in% uid[200001:210000],]
test.22 <- test[test$Id %in% uid[210001:220000],]
test.23 <- test[test$Id %in% uid[220001:230000],]
test.24 <- test[test$Id %in% uid[230001:240000],]
test.25 <- test[test$Id %in% uid[240001:250000],]
test.26 <- test[test$Id %in% uid[250001:260000],]
test.27 <- test[test$Id %in% uid[260001:270000],]
test.28 <- test[test$Id %in% uid[270001:280000],]
test.29 <- test[test$Id %in% uid[280001:290000],]

test.30 <- test[test$Id %in% uid[290001:300000],]
test.31 <- test[test$Id %in% uid[300001:310000],]
test.32 <- test[test$Id %in% uid[310001:320000],]
test.33 <- test[test$Id %in% uid[320001:330000],]
test.34 <- test[test$Id %in% uid[330001:340000],]
test.35 <- test[test$Id %in% uid[340001:350000],]
test.36 <- test[test$Id %in% uid[350001:360000],]
test.37 <- test[test$Id %in% uid[360001:370000],]
test.38 <- test[test$Id %in% uid[370001:380000],]
test.39 <- test[test$Id %in% uid[380001:390000],]

test.40 <- test[test$Id %in% uid[390001:400000],]
test.41 <- test[test$Id %in% uid[400001:410000],]
test.42 <- test[test$Id %in% uid[410001:420000],]
test.43 <- test[test$Id %in% uid[420001:430000],]
test.44 <- test[test$Id %in% uid[430001:440000],]
test.45 <- test[test$Id %in% uid[440001:450000],]
test.46 <- test[test$Id %in% uid[450001:460000],]
test.47 <- test[test$Id %in% uid[460001:470000],]
test.48 <- test[test$Id %in% uid[470001:480000],]
test.49 <- test[test$Id %in% uid[480001:490000],]

test.50 <- test[test$Id %in% uid[490001:550000],]
test.51 <- test[test$Id %in% uid[550001:600000],]
test.52 <- test[test$Id %in% uid[600001:650000],]
test.53 <- test[test$Id %in% uid[650001:700000],]
test.54 <- test[test$Id %in% uid[700001:length(uid)],]


#mp <- clusterApply(cl = cl, test$Ref,1, mpalmer, minutes_past = test$minutes_past)
system.time(mp1 <- parLapply(cl = cl, test.1$Ref, mpalmer, minutes_past = test.1$minutes_past))
system.time(mp2 <- parLapply(cl = cl, test.2$Ref, mpalmer, minutes_past = test.2$minutes_past))
system.time(mp3 <- parLapply(cl = cl, test.3$Ref, mpalmer, minutes_past = test.3$minutes_past))
system.time(mp4 <- parLapply(cl = cl, test.4$Ref, mpalmer, minutes_past = test.4$minutes_past))
system.time(mp5 <- parLapply(cl = cl, test.5$Ref, mpalmer, minutes_past = test.5$minutes_past))
system.time(mp6 <- parLapply(cl = cl, test.6$Ref, mpalmer, minutes_past = test.6$minutes_past))
system.time(mp7 <- parLapply(cl = cl, test.7$Ref, mpalmer, minutes_past = test.7$minutes_past))
system.time(mp8 <- parLapply(cl = cl, test.8$Ref, mpalmer, minutes_past = test.8$minutes_past))
system.time(mp9 <- parLapply(cl = cl, test.9$Ref, mpalmer, minutes_past = test.9$minutes_past))
system.time(mp10 <- parLapply(cl = cl, test.10$Ref, mpalmer, minutes_past = test.10$minutes_past))

system.time(mp11 <- parLapply(cl = cl, test.11$Ref, mpalmer, minutes_past = test.11$minutes_past))
system.time(mp12 <- parLapply(cl = cl, test.12$Ref, mpalmer, minutes_past = test.12$minutes_past))
system.time(mp13 <- parLapply(cl = cl, test.13$Ref, mpalmer, minutes_past = test.13$minutes_past))
system.time(mp14 <- parLapply(cl = cl, test.14$Ref, mpalmer, minutes_past = test.14$minutes_past))
system.time(mp15 <- parLapply(cl = cl, test.15$Ref, mpalmer, minutes_past = test.15$minutes_past))
system.time(mp16 <- parLapply(cl = cl, test.16$Ref, mpalmer, minutes_past = test.16$minutes_past))
system.time(mp17 <- parLapply(cl = cl, test.17$Ref, mpalmer, minutes_past = test.17$minutes_past))
system.time(mp18 <- parLapply(cl = cl, test.18$Ref, mpalmer, minutes_past = test.18$minutes_past))
system.time(mp19 <- parLapply(cl = cl, test.19$Ref, mpalmer, minutes_past = test.19$minutes_past))

system.time(mp20 <- parLapply(cl = cl, test.20$Ref, mpalmer, minutes_past = test.20$minutes_past))
system.time(mp21 <- parLapply(cl = cl, test.21$Ref, mpalmer, minutes_past = test.21$minutes_past))
system.time(mp22 <- parLapply(cl = cl, test.22$Ref, mpalmer, minutes_past = test.22$minutes_past))
system.time(mp23 <- parLapply(cl = cl, test.23$Ref, mpalmer, minutes_past = test.23$minutes_past))
system.time(mp24 <- parLapply(cl = cl, test.24$Ref, mpalmer, minutes_past = test.24$minutes_past))
system.time(mp25 <- parLapply(cl = cl, test.25$Ref, mpalmer, minutes_past = test.25$minutes_past))
system.time(mp26 <- parLapply(cl = cl, test.26$Ref, mpalmer, minutes_past = test.26$minutes_past))
system.time(mp27 <- parLapply(cl = cl, test.27$Ref, mpalmer, minutes_past = test.27$minutes_past))
system.time(mp28 <- parLapply(cl = cl, test.28$Ref, mpalmer, minutes_past = test.28$minutes_past))
system.time(mp29 <- parLapply(cl = cl, test.29$Ref, mpalmer, minutes_past = test.29$minutes_past))

system.time(mp30 <- parLapply(cl = cl, test.30$Ref, mpalmer, minutes_past = test.30$minutes_past))
system.time(mp31 <- parLapply(cl = cl, test.31$Ref, mpalmer, minutes_past = test.31$minutes_past))
system.time(mp32 <- parLapply(cl = cl, test.32$Ref, mpalmer, minutes_past = test.32$minutes_past))
system.time(mp33 <- parLapply(cl = cl, test.33$Ref, mpalmer, minutes_past = test.33$minutes_past))
system.time(mp34 <- parLapply(cl = cl, test.34$Ref, mpalmer, minutes_past = test.34$minutes_past))
system.time(mp35 <- parLapply(cl = cl, test.35$Ref, mpalmer, minutes_past = test.35$minutes_past))
system.time(mp36 <- parLapply(cl = cl, test.36$Ref, mpalmer, minutes_past = test.36$minutes_past))
system.time(mp37 <- parLapply(cl = cl, test.37$Ref, mpalmer, minutes_past = test.37$minutes_past))
system.time(mp38 <- parLapply(cl = cl, test.38$Ref, mpalmer, minutes_past = test.38$minutes_past))
system.time(mp39 <- parLapply(cl = cl, test.39$Ref, mpalmer, minutes_past = test.39$minutes_past))

system.time(mp40 <- parLapply(cl = cl, test.40$Ref, mpalmer, minutes_past = test.40$minutes_past))
system.time(mp41 <- parLapply(cl = cl, test.41$Ref, mpalmer, minutes_past = test.41$minutes_past))
system.time(mp42 <- parLapply(cl = cl, test.42$Ref, mpalmer, minutes_past = test.42$minutes_past))
system.time(mp43 <- parLapply(cl = cl, test.43$Ref, mpalmer, minutes_past = test.43$minutes_past))
system.time(mp44 <- parLapply(cl = cl, test.44$Ref, mpalmer, minutes_past = test.44$minutes_past))
system.time(mp45 <- parLapply(cl = cl, test.45$Ref, mpalmer, minutes_past = test.45$minutes_past))
system.time(mp46 <- parLapply(cl = cl, test.46$Ref, mpalmer, minutes_past = test.46$minutes_past))
system.time(mp47 <- parLapply(cl = cl, test.47$Ref, mpalmer, minutes_past = test.47$minutes_past))
system.time(mp48 <- parLapply(cl = cl, test.48$Ref, mpalmer, minutes_past = test.48$minutes_past))
system.time(mp49 <- parLapply(cl = cl, test.49$Ref, mpalmer, minutes_past = test.49$minutes_past))

system.time(mp50 <- parLapply(cl = cl, test.50$Ref, mpalmer, minutes_past = test.50$minutes_past))
system.time(mp51 <- parLapply(cl = cl, test.51$Ref, mpalmer, minutes_past = test.51$minutes_past))
system.time(mp52 <- parLapply(cl = cl, test.52$Ref, mpalmer, minutes_past = test.52$minutes_past))
system.time(mp53 <- parLapply(cl = cl, test.53$Ref, mpalmer, minutes_past = test.53$minutes_past))
system.time(mp54 <- parLapply(cl = cl, test.54$Ref, mpalmer, minutes_past = test.54$minutes_past))

mp1 <- as.numeric(mp1)
mp2 <- as.numeric(mp2)
mp3 <- as.numeric(mp3)
mp4 <- as.numeric(mp4)
mp5 <- as.numeric(mp5)
mp6 <- as.numeric(mp6)
mp7 <- as.numeric(mp7)
mp8 <- as.numeric(mp8)
mp9 <- as.numeric(mp9)
mp10 <- as.numeric(mp10)
mp11 <- as.numeric(mp11)
mp12 <- as.numeric(mp12)
mp13 <- as.numeric(mp13)
mp14 <- as.numeric(mp14)
mp15 <- as.numeric(mp15)
mp16 <- as.numeric(mp16)
mp17 <- as.numeric(mp17)
mp18 <- as.numeric(mp18)
mp19 <- as.numeric(mp19)
mp20 <- as.numeric(mp20)
mp21 <- as.numeric(mp21)
mp22 <- as.numeric(mp22)
mp23 <- as.numeric(mp23)
mp24 <- as.numeric(mp24)
mp25 <- as.numeric(mp25)
mp26 <- as.numeric(mp26)
mp27 <- as.numeric(mp27)
mp28 <- as.numeric(mp28)
mp29 <- as.numeric(mp29)
mp30 <- as.numeric(mp30)
mp31 <- as.numeric(mp31)
mp32 <- as.numeric(mp32)
mp33 <- as.numeric(mp33)
mp34 <- as.numeric(mp34)
mp35 <- as.numeric(mp35)
mp36 <- as.numeric(mp36)
mp37 <- as.numeric(mp37)
mp38 <- as.numeric(mp38)
mp39 <- as.numeric(mp39)
mp40 <- as.numeric(mp40)
mp41 <- as.numeric(mp41)
mp42 <- as.numeric(mp42)
mp43 <- as.numeric(mp43)
mp44 <- as.numeric(mp44)
mp45 <- as.numeric(mp45)
mp46 <- as.numeric(mp46)
mp47 <- as.numeric(mp47)
mp48 <- as.numeric(mp48)
mp49 <- as.numeric(mp49)
mp50 <- as.numeric(mp50)
mp51 <- as.numeric(mp51)
mp52 <- as.numeric(mp52)
mp53 <- as.numeric(mp53)
mp54 <- as.numeric(mp54)

mp <-      c(mp1,  mp2,  mp3,  mp4,  mp5,  mp6,  mp7,  mp8,  mp9,  mp10, mp11, mp12,
            mp13, mp14, mp15, mp16, mp17, mp18, mp19, mp20, mp21, mp22, mp23, mp24,
            mp25, mp26, mp27, mp28, mp29, mp30, mp31, mp32, mp33, mp34, mp35, mp36,
            mp37, mp38, mp39, mp40, mp41, mp42, mp43, mp44, mp45, mp46, mp47, mp48,
            mp49, mp50, mp51, mp52, mp53, mp54)

test$mp <- mp
rm(list=ls(pattern="test."))
gc(T)
saveRDS(test, "test_mp_wide.RDS")

# Predict Outlier Prob ----------------------------------------------------
# IF using MAD variables
test.mad.scores            <- scores(test,  type = "mad", prob = 1)
test.mad.scores            <- parApply(cl=cl,test, 2, scores, type = "mad", prob = 1)
test.mad.scores$Id         <- NULL
test.mad.scores$Id_t1      <- NULL
test.mad.scores$Id_mad_t1  <- NULL
test.mad.scores$Id_mad     <- NULL
colnames(test.mad.scores)  <- paste(colnames(test.mad.scores), "_mad", sep = "")
test             <- cbind(test, test.mad.scores)
rm(test.mad.scores)
saveRDS(test, "test_final.RDS")

# Run prediction ----------------------------------------------------------
predicted  <- predict(GRF,  test)

# Write out results -------------------------------------------------------
test$Prediction <- predicted

res <- sqldf("select Id, Prediction as Expected from test") # group by Id
#res$max_min <- NULL
write.csv(res, "id_forecast.csv", row.names = F)
