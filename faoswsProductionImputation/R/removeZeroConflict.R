##' The function removes conflicting zeroes between production and
##' area harvested.
##'
##' @param columnNames See columnNames argument at ?imputeProductionDomain.
##' @param naFlag Flag value for missing values.
##' @param data The data table objest
##'
##' @export

removeZeroConflict = function(columnNames, naFlag = "M", data){
    
    ### Ensure inputs are as expected
    stopifnot( is(data, "data.table") )
    testColumnNames( columnNames = columnNames, data = data )
    assignColumnNames( columnNames = columnNames, environment = environment() )
    
    setnames(x = data,
             old = c(productionValue,
                     areaHarvestedValue,
                     yieldValue,
                     productionObservationFlag,
                     areaHarvestedObservationFlag,
                     yieldObservationFlag),
             new = c("productionValue",
                     "areaHarvestedValue",
                     "yieldValue",
                     "productionObservationFlag",
                     "areaHarvestedObservationFlag",
                     "yieldObservationFlag"))
             
    data[productionValue == 0 & areaHarvestedValue,
         `:=`(areaHarvestedValue = NA,
              yieldValue = NA,
              areaHarvestedObservationFlag = naFlag,
              yieldObservationFlag = naFlag)]

    data[areaHarvestedValue == 0 & productionValue,
         `:=`(productionValue = NA,
              yieldValue = NA,
              productionObservationFlag = naFlag,
              yieldObservationFlag = naFlag)]

    setnames(x = data,
             old = c("productionValue",
                     "areaHarvestedValue",
                     "yieldValue",
                     "productionObservationFlag",
                     "areaHarvestedObservationFlag",
                     "yieldObservationFlag"),
             new = c(productionValue,
                     areaHarvestedValue,
                     yieldValue,
                     productionObservationFlag,
                     areaHarvestedObservationFlag,
                     yieldObservationFlag))
    
}
