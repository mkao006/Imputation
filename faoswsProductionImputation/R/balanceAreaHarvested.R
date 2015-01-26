##' Function to compute area harvested when new production and yield
##' are given.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##'
##' @export
##' 


balanceAreaHarvested = function(data, imputationParameters,
                                processingParameters){
    
    ### Data Quality Checks
    if(!ensuredProcessingParameters)
        ensureProcessingParameters(processingParameters = processingParameters)
    if(!ensuredImputationParameters)
        ensureImputationParameters(imputationParameters = imputationParameters)
    if(!ensuredImputationData)
        ensureImputationData(data = data,
                             imputationParameters = imputationParameters)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = imputationParameters$flagTable,
                        data = data,
                        imputationParameters = imputationParameters)

    origName = c(processingParameters$productionValue,
                 processingParameters$productionObservationFlag,
                 processingParameters$areaHarvestedValue,
                 processingParameters$areaHarvestedObservationFlag,
                 processingParameters$areaHarvestedMethodFlag,
                 processingParameters$yieldValue,
                 processingParameters$yieldObservationFlag)
    tmpName = c("pValue", "pObsFlag", "aValue", "aObsFlag",
        "aMetFlag", "yValue", "yObsFlag")
    setnames(data, old = origName, new = tmpName)
    
    data[!is.na(pValue) & is.na(aValue) & !is.na(yValue),
         c("aValue", "aObsFlag", "aMetFlag") :=
         list(computeRatio(pValue, yValue),
              aggregateObservationFlag(pObsFlag, yObsFlag,
                                       Table = processingParameters$flagTable),
              newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}
