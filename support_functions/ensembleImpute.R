ensembleImpute = function(x, plot = FALSE){
    missIndex = which(is.na(x))
    T = length(x)
    time = 1:T
    n.miss = length(missIndex)
    n.obs = T - n.miss
    if(n.miss > 0){
        if(n.obs >= 5 & var(x, na.rm = TRUE) != 0){

            ## Global mean
            meanFit = rep(mean(x, na.rm = TRUE), T)
            meanFitError = 1/sum(abs(x - meanFit), na.rm = TRUE)

            ## linear regression
            lmFit = predict(lm(formula = x ~ time),
                newdata = data.frame(time = time))
            lmFit[lmFit < 0] = 0
            lmFitError = 1/sum(abs(x - lmFit), na.rm = TRUE)

            ## Mars
            x.tmp = na.omit(x)
            time.tmp = time[-attr(x.tmp, "na.action")]
            marsFit = try(predict(earth(x.tmp ~ time.tmp),
                newdata = data.frame(time.tmp = time)))
            if(!inherits(marsFit, "try-error")){
                marsFit[marsFit < 0] = 0
                marsFitError = 1/sum(abs(x - marsFit), na.rm = TRUE)
            } else {
                marsFit = rep(0, T)
                marsFitError = 0
            }

            ## Exponential
            expFit = exp(predict(lm(formula = log(x + 1) ~ time),
                newdata = data.frame(time = time)))
            expFitError = ifelse(n.obs/T >= 0.75 &
                length(na.omit(tail(x, 5))) > 0,
                1/sum(abs(x - expFit), na.rm = TRUE), 0)

            ## locally smooth lienar
            loessFit = try(predict(loess(formula = x ~ time,
                control = loess.control(surface = "direct"),
                span = ifelse(n.obs/T >= 0.5, 0.3,
                    ifelse(n.obs <= 5, 1, 0.75)), degree = 1),
                newdata = data.frame(time)))
            if(!inherits(loessFit, "try-error") &
               sum(abs(x - loessFit), na.rm = TRUE) > 0.1 &
               n.obs/T >= 0.5){
                loessFit[loessFit < 0] = 0
                loessFitError = 1/sum(abs(x - loessFit), na.rm = TRUE)
            } else {
                loessFit = rep(0, T)
                loessFitError = 0
            }

            ## logistic
            xmax = max(x, na.rm = TRUE)
            x.scaled = x/xmax
            logisticFit = predict(glm(formula = x.scaled ~ time,
                family = "binomial"), newdata = data.frame(time = time),
                type = "response") *
                    xmax
            logisticFitError = 1/sum(abs(x - logisticFit), na.rm = TRUE)

            ## Arima
            arimaFit = na.approx(x, na.rm = FALSE)
            nonMissIndex = which(!is.na(arimaFit))
            fit = auto.arima(na.omit(arimaFit))
            arimaFit[nonMissIndex] = fitted(fit)
            if(var(arimaFit, na.rm = TRUE) > 1e-3){
                obs = which(!is.na(arimaFit))
                numberForward =
                    length(which(is.na(arimaFit[max(obs):length(arimaFit)])))
                if(numberForward > 0)
                    arimaFit[(max(obs) + 1):length(arimaFit)] =
                        c(forecast(fit, h = numberForward)$mean)
                numberBackward =
                    length(which(is.na(arimaFit[min(obs):1])))
                if(numberBackward > 0)
                    arimaFit[(min(obs) - 1):1] =
                        c(forecast(auto.arima(rev(arimaFit),
                                              seasonal = FALSE),
                                   h = numberBackward)$mean)
                arimaFit[arimaFit < 0]  = 0
                arimaFitError =
                    arimaFitError = 1/sum(abs(x - arimaFit),
                        na.rm = TRUE)
            } else {
                arimaFit = rep(0, T)
                arimaFitError = 0
            }

            ## Niave
            naiveFit = naiveImputation(x)
            naiveFitError = mean(c(meanFitError, lmFitError, marsFitError,
                           expFitError[expFitError != 0],
                           loessFitError[loessFitError != 0],
                           logisticFitError), na.rm = TRUE)

            ## Construct the ensemble
            weights =
                c(mean = meanFitError,
                  lm = lmFitError,
                  mars = marsFitError,
                  exp = expFitError,
                  loess = loessFitError,
                  logistic = logisticFitError,
                  ## arima = arimaFitError,
                  naive = naiveFitError)^2/
                    sum(c(mean = meanFitError,
                          lm = lmFitError,
                          mars = marsFitError,
                          exp = expFitError,
                          loess = loessFitError,
                          logisticFitError,
                          ## arima = arimaFitError,
                          naive = naiveFitError)^2, na.rm = TRUE)
            weights[is.na(weights)] = 0
            finalFit = (meanFit * weights["mean"] +
                        lmFit * weights["lm"] +
                        marsFit * weights["mars"] +
                        expFit * weights["exp"] +
                        loessFit * weights["loess"] +
                        logisticFit * weights["logistic"] +
                        ## arimaFit * weights["arima"] +
                        naiveFit * weights["naive"])

            ## Plot the result
            if(plot){
                plot(x ~ time,
                     ylim = c(0,
                         max(c(x,
                               lmFit,
                               logisticFit,
                               arimaFit,
                               loessFit
                               ),
                         na.rm = TRUE)),
                     pch = 19)
                lines(meanFit, col = "red")
                lines(lmFit, col = "orange")
                lines(marsFit, col = "maroon")
                lines(expFit, col = "gold")
                lines(loessFit, col = "brown")
                lines(logisticFit, col = "green")
                lines(arimaFit, col = "purple")
                lines(naiveFit, col = "blue")
                lines(finalFit, col = "steelblue", lwd = 3)
                legend("topleft", legend =
                       paste0(c("mean", "linear", "mars", "exponential",
                               "loess", "logistic", "arima",
                               "naive", "final"), " (",
                             round(c(weights, 1), 3) * 100, "%)"),
                       col = c("red", "orange", "maroon", "gold", "brown",
                           "green", "purple", "blue",
                           "steelblue"), lwd = c(rep(1, 8), 3), bty = "n",
                       lty = 1)

            }
            x[missIndex] = finalFit[missIndex]
        } else {
            x = naiveImputation(x)
        }
    }
    as.numeric(x)
}





