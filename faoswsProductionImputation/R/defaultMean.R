##' The default mean model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultMean = function(x){
    yearCount = length(x)
    if(all(is.na(x)))
        return(as.numeric(rep(NA, yearCount)))
    meanFit = rep(mean(x, na.rm = !all(is.na(x))), yearCount)
    meanFit
}
