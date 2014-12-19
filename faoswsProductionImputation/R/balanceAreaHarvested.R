##' Function to compute area harvested when new production and yield
##' are given.
##'
##'
##' @param columnNames See same argument at ?imputeProductionDomain.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##'
##' @export
##' 


balanceAreaHarvested = function(columnNames,
    newMethodFlag, flagTable = faoswsFlagTable, data){
    
    ### Ensure inputs are as expected (and assign columnNames variables)
    stopifnot( is(data, "data.table") )
    testColumnNames(columnNames = columnNames, data = data)
    assignColumnNames(columnNames = columnNames, envir = environment() )
	testFlagTable( flagTable = flagTable, data = data,
        columnNames = columnNames )

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
