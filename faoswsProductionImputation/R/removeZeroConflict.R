##' The function removes conflicting zeroes between production and
##' area harvested.
##'
##' @param columnNames See columnNames argument at ?imputeProductionDomain.
##' @param naFlag Flag value for missing values.
##' @param data The data table objest
##'
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified.
##' 
##' @examples data = okrapd[1:10,]
##' data[,areaHarvestedValue := c(rep(0, 5), rep(100, 5))]
##' data[,productionValue := c(0, 0, 100, 100, 100, 0, 0, 100, 100, 100)]
##' removeZeroConflict(columnNames = defaultColumnNames(), data = data)
##' data[,.(areaHarvestedValue, areaHarvestedFlag, productionValue,
##'     productionFlag, yieldValue, yieldFlag)]
##'
##' @export

removeZeroConflict = function(columnNames, naFlag = "M", data){
    
    ### Data Quality Checks
    ensureData(data = data, columnNames = columnNames)
    ensureColumnNames(columnNames = columnNames, data = data)
    assignColumnNames(columnNames = columnNames, environment = environment())
    
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
             
    data[productionValue == 0 & areaHarvestedValue != 0,
         `:=`(areaHarvestedValue = NA,
              yieldValue = NA,
              areaHarvestedObservationFlag = naFlag,
              yieldObservationFlag = naFlag)]

    data[areaHarvestedValue == 0 & productionValue != 0,
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
