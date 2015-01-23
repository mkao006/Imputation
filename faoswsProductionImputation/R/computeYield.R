##' Function to compute and update yield
##'
##' @param productionValue The column name corresponding to production
##' value.
##' @param productionObservationFlag The column name corresponding to
##' the observation flag of production.
##' @param areaHarvestedValue The column name corresponding to area
##' harvested value.
##' @param areaHarvestedObservationFlag The column name corresponding
##' to the observation flag of area harvested.
##' @param yieldValue The columne name corresponding to yield value.
##' @param yieldObservationFlag The column name corresponding to the
##' observation flag of yield.
##' @param yieldMethodFlag
##' @param newMethodFlag 
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##' @param unitConversion Yield is computed as (production) / (area) and
##' multiplied by unitConversion.  This parameter defaults to 1.
##'
##' @export


computeYield = function(productionValue, productionObservationFlag,
    areaHarvestedValue, areaHarvestedObservationFlag, yieldValue,
    yieldObservationFlag, yieldMethodFlag, newMethodFlag,
    flagTable = faoswsFlagTable, data, unitConversion = 1){

    if(!yieldValue %in% colnames(data))
        data[, c(yieldValue) := NA]
    if(!yieldObservationFlag %in% colnames(data))
        data[, c(yieldObservationFlag) := NA]
    if(!yieldMethodFlag %in% colnames(data))
        data[, c(yieldMethodFlag) := NA]

    setnames(x = data,
             old = c(productionValue, productionObservationFlag,
                     areaHarvestedValue, areaHarvestedObservationFlag,
                     yieldValue, yieldObservationFlag, yieldMethodFlag),
             new = c("productionValue", "productionObservationFlag",
                 "areaHarvestedValue", "areaHarvestedObservationFlag",
                 "yieldValue", "yieldObservationFlag", "yieldMethodFlag"))

    ## Balance yield values only when they're missing
    missingYield = is.na(data[, yieldValue]) |
        data[, yieldObservationFlag] == "M"
    data[missingYield, yieldValue :=
         computeRatio(productionValue, areaHarvestedValue) * unitConversion]
    data[missingYield, yieldObservationFlag :=
         aggregateObservationFlag(productionObservationFlag,
                                  areaHarvestedObservationFlag,
                                  flagTable = flagTable)]
    data[missingYield, yieldMethodFlag := newMethodFlag]
    ## If yieldValue is still NA, make sure observation flag is "M".  Note:
    ## this can happen by taking 0 production / 0 area.
    data[is.na(yieldValue), yieldObservationFlag := "M"]

    setnames(x = data,
             old = c("productionValue", "productionObservationFlag",
                 "areaHarvestedValue", "areaHarvestedObservationFlag",
                 "yieldValue", "yieldObservationFlag", "yieldMethodFlag"),
             new = c(productionValue, productionObservationFlag,
                 areaHarvestedValue, areaHarvestedObservationFlag,
                 yieldValue, yieldObservationFlag, yieldMethodFlag))
}
