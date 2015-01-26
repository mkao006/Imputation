##' Function to compute production when new area harvested and yield
##' are given.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##'
##' @export
##' 

balanceProduction = function(data, imputationParameters){

    ### Data Quality Checks
    if(!ensuredImputationParameters)
        ensureImputationParameters(imputationParameters = imputationParameters)
    if(!ensuredImputationData)
        ensureImputationData(data = data,
                             imputationParameters = imputationParameters)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = imputationParameters$flagTable,
                        data = data,
                        imputationParameters = imputationParameters)
    
    origName = c(productionValue, productionObservationFlag,
        productionMethodFlag, areaHarvestedValue,
        areaHarvestedObservationFlag, 
        yieldValue, yieldObservationFlag)
    tmpName = c("pValue", "pObsFlag", "pMetFlag", "aValue", "aObsFlag",
        "yValue", "yObsFlag")
    setnames(data, old = origName, new = tmpName)
    
    # If data$pMetFlag is all NA's, it may be logical.  This could issue a
    # warning later but it's not really a problem.  To avoid confusion, 
    # coerce to a character here.
    if(is(data$pMetFlag, "logical"))
        data[, pMetFlag := as.character(data$pMetFlag)]
    data[!is.na(aValue) & is.na(pValue) & !is.na(yValue),
         c("pValue", "pObsFlag", "pMetFlag") :=
         list(aValue * yValue,
              aggregateObservationFlag(aObsFlag, yObsFlag,
                                       flagTable = flagTable),
              newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}
