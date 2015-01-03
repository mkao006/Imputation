##' Function to compute area harvested when new production and yield
##' are given.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point. If
##' NULL, the parameters should have already been assigned (otherwise an error
##' will occur).
##'
##' @export
##' 


balanceAreaHarvested = function(data, imputationParameters = NULL){
    
    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned){
        stopifnot(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    }
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
	    ensureFlagTable(flagTable = flagTable, data = data)

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
