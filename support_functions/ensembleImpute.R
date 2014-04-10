naiveImputation = function(x){
  require(zoo)
  nobserved = length(na.omit(x))
  n = length(x)
  type = ifelse(nobserved == 0, "none",
    ifelse(nobserved == 1, "repeat", "naive"))
  switch(type,
         "none" = {tmp = rep(NA, n)},
         "repeat" = {tmp = rep(na.omit(x), n)},
         "naive" = {tmp = na.locf(na.locf(na.approx(x, na.rm = FALSE),
            na.rm = FALSE), na.rm = FALSE, fromLast = TRUE)}
         )
  as.numeric(tmp)
}

ensembleImpute = function(x, plot = FALSE){
    missIndex = which(is.na(x))
    T = length(x)
    time = 1:T
    n.miss = length(missIndex)
    n.obs = T - n.miss
    if(n.miss > 0){
        if(n.obs >= 5 & var(x, na.rm = TRUE) != 0){
            ## Start fitting
            meanFit = rep(mean(x, na.rm = TRUE), T)
            meanFitError = 1/sum(abs(x - meanFit), na.rm = TRUE)

            lmFit = predict(lm(formula = x ~ time),
                newdata = data.frame(time = time))
            lmFit[lmFit < 0] = 0
            lmFitError = 1/sum(abs(x - lmFit), na.rm = TRUE)

            ## lm2Fit = predict(lm(formula = x ~ poly(time, 2)),
            ##     newdata = data.frame(time = time))
            ## lm2Fit[lm2Fit < 0] = 0
            ## lm2FitError = 1/sum(abs(x - lm2Fit), na.rm = TRUE)

            loessFit = try(predict(loess(formula = x ~ time,
                control = loess.control(surface = "direct"), span = 0.75,
                degree = 1), newdata = data.frame(time)))
            ## print(sum(abs(x - loessFit), na.rm = TRUE))
            ## print(x)
            if(!inherits(loessFit, "try-error") &
               sum(abs(x - loessFit), na.rm = TRUE) > 0.1 &
               n.obs/T >= 0.5){
                loessFit[loessFit < 0] = 0
                loessFitError = 1/sum(abs(x - loessFit), na.rm = TRUE)
            } else {
                loessFit = rep(0, T)
                loessFitError = 0
            }

            xmax = max(x, na.rm = TRUE)
            x.scaled = x/xmax
            logisticFit = predict(glm(formula = x.scaled ~ time,
                family = "binomial"), newdata = data.frame(time = time),
                type = "response") *
                    xmax
            logisticFitError = 1/sum(abs(x - logisticFit), na.rm = TRUE)

            naiveFit = naiveImputation(x)
            naiveFitError =
                mean(c(meanFitError, lmFitError, logisticFitError),
                     na.rm = TRUE)

            ## Construct the ensemble
            ## weights =
            ##     c(mean = meanFitError, lm = lmFitError, lm2 = lm2FitError,
            ##       logistic = logisticFitError, naive = naiveFitError)/
            ##         sum(c(meanFitError, lmFitError, lm2FitError,
            ##               logisticFitError, naiveFitError), na.rm = TRUE)
            weights =
                c(mean = meanFitError, lm = lmFitError, loess = loessFitError,
                  logistic = logisticFitError, naive = naiveFitError)^2/
                    sum(c(meanFitError, lmFitError, loess = loessFitError,
                          logisticFitError, naiveFitError)^2, na.rm = TRUE)
            weights[is.na(weights)] = 0
            print(weights)
            finalFit = (meanFit * weights["mean"] +
                        lmFit * weights["lm"] +
                        loessFit * weights["loess"] +
                        logisticFit * weights["logistic"] +
                        naiveFit * weights["naive"])
            ## finalFit = (meanFit * weights["mean"] +
            ##             lmFit * weights["lm"] +
            ##             logisticFit * weights["logistic"] +
            ##             naiveFit * weights["naive"])

            if(plot){
                plot(x ~ time,
                     ylim = c(0, max(c(x, lmFit, logisticFit, loessFit),
                         na.rm = TRUE)))
                lines(meanFit, col = "red")
                lines(lmFit, col = "orange")
                ## lines(lm2Fit, col = "brown")
                lines(loessFit, col = "brown")
                lines(logisticFit, col = "green")
                lines(naiveFit, col = "blue")
                lines(finalFit, col = "steelblue", lwd = 3)
                legend("topleft", legend = c("mean", "linear",
                                      "logistic", "naive", "final"),
                       col = c("red", "orange", "green", "blue",
                           "steelblue"), lwd = c(rep(1, 4), 3), bty = "n",
                       lty = 1)

            }
            x[missIndex] = finalFit[missIndex]
        } else {
            x = naiveImputation(x)
        }
    }
    round(x, 3)
}





