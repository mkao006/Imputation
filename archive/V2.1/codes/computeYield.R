##' This function computes the yield given area and production
##'
##' This function avoids Inf, NAN when calculating the yield
##'
##' @param production The production time series
##' @param area The area time series
##'
##' @export


computeYield = function(production, area){
  if(length(production) != length(area))
    stop("Length of prodduction is not the same as area")
  ifelse(production != 0 & area != 0, production/area, NA)
}
