##' The default spline model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export

defaultSpline = function(x){
    yearCount = length(x)
    time = 1:length(x)
    splineFit = spline(time, x, n = yearCount * 5 - 4, xout = time,
        method = "natural")$y
    splineFit[splineFit < 0] = 0
    splineFit
}
