##' Function to compute area harvested when new production and yield
##' are given.
##'
##'
##' @param columnNames See same argument at ?imputeProductionDomain.
##' @param newMethodFlag The character value that should be assigned to
##' areaHarvestedObservationFlag when it is imputed.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##'
##' @export
##' 


balanceAreaHarvested = function(columnNames,
    newMethodFlag, flagTable = faoswsFlagTable, data){
    
    ### Data Quality Checks
    ensureData(data = data, columnNames = columnNames)
    assignColumnNames(columnNames = columnNames)
	ensureFlagTable(flagTable = flagTable, data = data, columnNames = columnNames)
    stopifnot(is(newMethodFlag, "character"))

    origName = c(productionValue, productionObservationFlag,
        areaHarvestedValue,
        areaHarvestedObservationFlag, areaHarvestedMethodFlag,
        yieldValue, yieldObservationFlag)
    tmpName = c("pValue", "pObsFlag", "aValue", "aObsFlag",
        "aMetFlag", "yValue", "yObsFlag")
    setnames(data, old = origName, new = tmpName)
    
    data[!is.na(pValue) & is.na(aValue) & !is.na(yValue),
         c("aValue", "aObsFlag", "aMetFlag") :=
         list(computeRatio(pValue, yValue),
              aggregateObservationFlag(pObsFlag, yObsFlag,
                                       flagTable = flagTable),
              newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}
