##' Default Logistic Model for the Ensemble.
##' 
##' The model fit is \eqn{ Production = A/(1 + exp(-B(time-C))) }, and
##' fitting is done via non-linear least squares (see ?nls).  If this fit fails
##' to converge, than A is fixed to the maximum Production value and the model
##' is fit via glm().
##' 
##' @note If the midpoint of the logistic regression model is outside the range
##' of the data, then a vector of NA's is returned (to prevent poor
##' extrapolation).
##'
##' @param x A numeric vector to be imputed.
##' @return A numeric vector with the estimated logistic regression model.
##' @export

defaultLogistic = function(x){
    time = 1:length(x)
    xmax = max(x, na.rm = TRUE)
    #If all values are 0 or NA's, then return no prediction
    if(is.na(xmax) | xmax==0)
        return( rep(NA, length(x)) )
    x.scaled = x/xmax
    logisticModel = try( nls(
        formula = log(x+1) ~ log(A) - log(1 + exp( -B*(time-C) )),
        start = list(A=xmax, B=1, C=mean(time)) ),
        silent = TRUE
        )
    if(!inherits(logisticModel, "try-error")){
        logisticFit = exp( predict(logisticModel, newdata = data.frame(time = time)) )
        midpoint = coef(logisticModel)[3]
    } else {
        logisticModel = glm(formula = x.scaled ~ time, family = "binomial")
        logisticFit =
            predict(logisticModel, 
                    newdata = data.frame(time = time),
                    type = "response") * xmax
        midpoint = - coef(logisticModel)[1]/coef(logisticModel)[2]
    }
    if(length(na.omit(x[time < midpoint])) < 1 |
       length(na.omit(x[time > midpoint])) < 1 )
        logisticFit = rep(NA, length(x))
    logisticFit
}