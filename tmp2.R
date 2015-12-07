predictions <- as.data.frame(h2o.predict(rfHex,trainHex))
summary(predictions$predict)
Expected <- expm1(predictions$predict)
summary(Expected)


train.samp          <- as.data.frame(trainHex)
train.samp$h2orf    <- Expected
train.samp$Expected <- expm1(train.samp$target)
summary(train.samp$h2orf)
summary(train.samp$Expected)

train.samp <- na.omit(train.samp)

hybridGamma <- glm(formula = "Expected ~ h2orf",
                family = Gamma(link = "identity"), data = train.samp)

summary(hybridGamma)
library(MASS)
myshape <- gamma.shape(hybridGamma)
gampred <- predict(hybridGamma , type = "response", se = T, dispersion = 1/myshape$alpha)
summary(hybridGamma, dispersion = 1/myshape$alpha)
saveRDS(hybridGamma, "hybridGamma.RDS")

test.h2orf <- read.csv("rfv3cn3.csv")
test <- readRDS("test_final.RDS")
test$h2orf <- test.h2orf$Expected

answer <- predict(hybridGamma, newdata = test , type = "response", se = T, dispersion = 1/myshape$alpha)
#train2 <- sqldf("select a.*, b.predict from 'train.samp' a join predictions b on a.Id = b.Id")
plot(density(answer$fit))
saveRDS(answer, "answer.RDS")

GRF <- readRDS("GRF.RDS")
test <- na.roughfix(test)
test.grf <- predict(GRF, newdata = test)
summary(test.grf)

submission          <- read.csv("sample_solution.csv")
submission$Expected <- answer$fit #+ (.125 * submission$Expected) + (.125 * test.grf)
submission$Expected[submission$Expected < 0 ] <- 0
summary(submission)
plot(density(submission$Expected))
write.csv(submission,"hybrid3.csv",row.names=F)

