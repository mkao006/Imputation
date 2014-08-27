##' Function to compute production when new area harvested and yield
##' are given.
##'
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
##' 

balanceProduction = function(productionValue, productionFlag,
    areaHarvestedValue, areaHarvestedFlag, yieldValue, yieldFlag,
    flagTable = faoswsFlagTable, data){
    origName = c(productionValue, productionFlag,
        areaHarvestedValue, areaHarvestedFlag, yieldValue, yieldFlag)
    tmpName = c("pValue", "pFlag", "aValue", "aFlag", "yValue", "yFlag")
    setnames(data, old = origName, new = tmpName)
    data[is.na(pValue) & !is.na(aValue),
         c("pValue", "pFlag") :=
         list(aValue * yValue,
              aggregateObservationFlag(aFlag, yFlag,
                                       flagTable = flagTable))
         ]
    setnames(data, old = tmpName, new = origName)
    
}
