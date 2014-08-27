##' Function to compute and update yield
##'
##' @param productionValue The column name corresponding to production
##' value.
##' @param productionFlag The column name corresponding to the
##' observation flag of production.
##' @param areaHarvestedValue The column name corresponding to area
##' harvested value.
##' @param areaHarvestedFlag The column name corresponding to the
##' observation flag of area harvested.
##' @param yieldValue The columne name corresponding to yield value.
##' @param yieldFlag The column name corresponding to the observation
##' flag of yield.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##'
##' @export


computeYield = function(productionValue, productionFlag,
    areaHarvestedValue, areaHarvestedFlag, yieldValue, yieldFlag,
    flagTable = faoswsFlagTable, data){
    
    if(!yieldValue %in% colnames(data))
        data[, c(yieldValue) := NA]
    if(!yieldFlag %in% colnames(data))
        data[, c(yieldFlag) := NA]

    setnames(x = data,
             old = c(productionValue, productionFlag,
                     areaHarvestedValue, areaHarvestedFlag,
                     yieldValue, yieldFlag),
             new = c("productionValue", "productionFlag",
                     "areaHarvestedValue", "areaHarvestedFlag",
                     "yieldValue", "yieldFlag"))
    
    data[, yieldValue :=
         computeRatio(productionValue, areaHarvestedValue)]
    data[, yieldFlag :=
         aggregateObservationFlag(productionFlag, areaHarvestedFlag,
                                  flagTable = flagTable)]

    setnames(x = data,
             old = c("productionValue", "productionFlag",
                     "areaHarvestedValue", "areaHarvestedFlag",
                     "yieldValue", "yieldFlag"),
             new = c(productionValue, productionFlag,
                     areaHarvestedValue, areaHarvestedFlag,
                     yieldValue, yieldFlag))
}
