train_prehex <-

train[,.(
  dist   = mean(radardist_km, na.rm = T),
  refArea5   = mean(Ref_5x5_50th, na.rm = T),
  refArea9  = mean(Ref_5x5_90th, na.rm = T),
  meanRefcomp = mean(RefComposite,na.rm=T),
  meanRefcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
  meanRefcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
  zdr   = mean(Zdr, na.rm = T),
  zdr5   = mean(Zdr_5x5_50th, na.rm = T),
  zdr9   = mean(Zdr_5x5_90th, na.rm = T),
  target = log1p(mean(Expected)),
  meanRef = mean(Ref,na.rm=T),
  sumRef = sum(Ref,na.rm=T),
  records = .N,
  naCounts = sum(is.na(Ref))
),Id][records>naCounts,]

train_prehex <- na.roughfix(train_prehex)
write.csv(train_prehex, "train_prehex.csv", row.names = F)


test<-fread("test.csv",select=c(1,2,3,4,6,7,8,10,11,16,18, 19))
#test <- readRDS("test.RDS"); test <- test[,colnames(test) %in% c(colnames(train), "Id")]
#Cut off Ref values < 0
test$Ref_5x5_50th[which(test$Ref_5x5_50th < 0)] <- NA
test$Ref_5x5_90th[which(test$Ref_5x5_90th < 0)] <- NA
test$RefComposite[which(test$RefComposite < 0)] <- NA
test$RefComposite_5x5_50th[which(test$RefComposite_5x5_50th < 0)] <- NA
test$RefComposite_5x5_90th[which(test$RefComposite_5x5_90th < 0)] <- NA
test$Ref[which(test$Ref < 0)] <- NA

test <- as.data.table(test)
test_prehex <- test[,.(
              dist   = mean(radardist_km, na.rm = T),
              refArea5   = mean(Ref_5x5_50th, na.rm = T),
              refArea9  = mean(Ref_5x5_90th, na.rm = T),
              meanRefcomp = mean(RefComposite,na.rm=T),
              meanRefcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
              meanRefcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
              zdr   = mean(Zdr, na.rm = T),
              zdr5   = mean(Zdr_5x5_50th, na.rm = T),
              zdr9   = mean(Zdr_5x5_90th, na.rm = T),

              meanRef = mean(Ref,na.rm=T),
              sumRef = sum(Ref,na.rm=T),
              records = .N,
              naCounts = sum(is.na(Ref))
              ),Id]

test_prehex <- na.roughfix(test_prehex)
write.csv(test_prehex, "test_prehex.csv", row.names = F)

pred   <- fread("C:\\users\\jmiller\\Downloads\\pred5k.csv")
sample <- read.csv("sample_solution.csv")
sub    <- sample
summary(sample$Expected)

pred$new <- expm1(pred$predict)
summary(pred$new)
plot(density(pred$new))
lines(density(sample$Expected))

identical(sub$Expected, sample$Expected)
sub$Expected <- (sample$Expected * .25) + (pred$new * .75)
summary(sub$Expected)
plot(density(sub$Expected[sub$Expected <15]))
lines(density(sample$Expected[sample$Expected <15]))
#sub$Expected <- round(sub$Expected / 0.254) * 0.254
#summary(sub$Expected)
#plot(density(sub$Expected[sub$Expected <15]))

write.csv(sub, "sat710pm.csv", row.names = F)

