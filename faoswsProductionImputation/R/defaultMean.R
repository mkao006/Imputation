##' The default mean model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultMean = function(x){
    yearCount = length(x)
    meanFit = rep(mean(x, na.rm = !all(is.na(x))), yearCount)
    meanFit
}
