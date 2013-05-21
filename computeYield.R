########################################################################
## Title: Function to compute yield when area and production can be
##        zero or NA's
## Date: 2013-05-09
########################################################################

computeYield = function(production, area){
  if(length(production) != length(area))
    stop("Length of prodduction is not the same as area")
  ifelse(production != 0 & area != 0, production/area, NA)
}
