##' The function removes conflicting zeroes between production and
##' area harvested.
##'
##' @param productionValue The name of the production variable.
##' @param areaHarvestedValue The name of the area harvested variable.
##' @param yieldValue The name of the yield variable.
##' @param productionObservationFlag The observation flag of production.
##' @param areaHarvestedObservationFlag The observation flag of area
##' harvested.
##' @param yieldObservationFlag The observation flag of yield.
##' @param naFlag Flag value for missing values.
##' @param data The data table objest
##'
##' @export

removeZeroConflict = function(productionValue, areaHarvestedValue,
    yieldValue, productionFlag, areaHarvestedFlag, yieldFlag,
    naFlag = "M", data){

    setnames(x = data,
             old = c(productionValue,
                     areaHarvestedValue,
                     yieldValue,
                     productionFlag,
                     areaHarvestedFlag,
                     yieldFlag),
             new = c("productionValue",
                     "areaHarvestedValue",
                     "yieldValue",
                     "productionFlag",
                     "areaHarvestedFlag",
                     "yieldFlag"))
             
    data[productionValue == 0 & areaHarvestedValue,
         `:=`(areaHarvestedValue = NA,
              yieldValue = NA,
              areaHarvestedFlag = naFlag,
              yieldFlag = naFlag)]

    data[areaHarvestedValue == 0 & productionValue,
         `:=`(productionValue = NA,
              yieldValue = NA,
              productionFlag = naFlag,
              yieldFlag = naFlag)]

    setnames(x = data,
             old = c("productionValue",
                     "areaHarvestedValue",
                     "yieldValue",
                     "productionFlag",
                     "areaHarvestedFlag",
                     "yieldFlag"),
             new = c(productionValue,
                     areaHarvestedValue,
                     yieldValue,
                     productionFlag,
                     areaHarvestedFlag,
                     yieldFlag))
    
}
