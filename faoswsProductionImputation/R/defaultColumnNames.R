##' Default Column Names
##'
##' This function generates a named vector of column names.  It's useful as a
##' starting point for the columnNames argument required by many functions in
##' this package.
##'
##' @return A named vector, typically to be passed to another function.  It
##' may be necessary to modify some values of this vector so that they are
##' appropriate for your particular dataset.
##'
##' @export
##' 

defaultColumnNames = function(){
    c( productionValue = "productionValue",
       productionObservationFlag = "productionFlag",
       productionMethodFlag = "productionFlag2",
       areaHarvestedValue = "areaHarvestedValue",
       areaHarvestedObservationFlag = "areaHarvestedFlag",
       areaHarvestedMethodFlag = "areaHarvestedFlag2",
       yieldValue = "yieldValue",
       yieldObservationFlag = "yieldFlag",
       yieldMethodFlag = "yieldFlag2",
       yearValue = "year",
       byKey = "areaCode" )
}