# ------------------------------------------------------------------------
# File:        rain_report.R
# Author:      Jason D. Miller
# Contact:     millerintllc@gmail.com
# Description: This script summarizes the model and has stats for the paper
# ------------------------------------------------------------------------


# Examine distribution ----------------------------------------------------
plotdist(train.sample$Expected, histo = TRUE, demp = TRUE)
plotdist(pred_oos, histo = TRUE, demp = TRUE)

descdist(train.samp1$Expected, boot = 100)

    # summary statistics
    # ------
    # min:  0.01   max:  103.3781
    # median:  2.032001
    # mean:  4.299148
    # estimated sd:  8.162936
    # estimated skewness:  5.704044
    # estimated kurtosis:  48.76989

# Seems to fit Beta better than Gamma
# Let's estimate the Beta distribution shape parameters
estBetaParams(mu = 4.182381, 59.95115)  # for train.samp (2 million rows)
    # $alpha
    # [1] -5.110924
    #
    # $beta
    # [1] 3.888911
estBetaParams(mu = 4.299148, 66.63352)  # for train.samp1
    # $alpha
    # [1] -5.214259 # for train.samp1
    #
    # $beta
    # [1] 4.0014     # for train.samp1

#fb <- fitdist(train.samp1$Expected, "beta")
fw <- fitdist(train.samp1$Expected, "weibull")
summary(fw)
fg <- fitdist(train.samp1$Expected, "gamma")
fln <- fitdist(train.samp1$Expected, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend,  xlegend = "bottomright")
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)


# Create CSV for submission on Kaggle -------------------------------------
