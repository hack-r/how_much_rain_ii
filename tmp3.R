setwd("T://RNA//Baltimore//Jason//ad_hoc//cats")
dir(patter=".csv")
one <- read.csv("answer.csv")
two <- read.csv("bd.csv")
three <- read.csv("cleaner_gammaglmrf.csv")
four  <- read.csv("gammaglmrf.csv")
five  <- read.csv("jdm.csv")
six   <- read.csv("rfv3cn3.csv")
seven <- read.csv("sample_solution.csv")

mu <- cbind(one[,], two[,2], three[,2], four[,2], five[,2], six[,2], seven[,2])

submission <- as.data.frame(mu$Id)
mu$Id <- NULL
submission$Expected <- rowMeans(mu)
names(submission) <- c("Id", "Expected")
write.csv(submission,"averageprior.csv", row.names = F)
